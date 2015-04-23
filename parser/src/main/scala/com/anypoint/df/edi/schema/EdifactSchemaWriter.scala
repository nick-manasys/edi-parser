package com.anypoint.df.edi.schema

import java.io.OutputStream
import java.nio.charset.Charset
import java.util.Calendar
import java.util.GregorianCalendar
import scala.annotation.tailrec
import scala.collection.JavaConversions
import scala.collection.immutable.TreeMap
import scala.util.Try
import com.anypoint.df.edi.lexical.WriteException
import com.anypoint.df.edi.lexical.EdifactConstants._
import com.anypoint.df.edi.lexical.EdifactWriter

/** Configuration parameters for EDIFACT schema writer.
  */
case class EdifactWriterConfig(val syntax: SyntaxIdentifier, val version: SyntaxVersion, val subChar: Int,
  val decimalMark: Char, val charSet: Charset, val delims: String, val suffix: String) {
  if (delims.size != 0 && delims.size != 5) throw new IllegalArgumentException("delimiter string must be empty or 5 characters")
}

/** Provider interface for control numbers. */
trait EdifactNumberProvider {
  def contextToken(senderId: String, senderQual: String, receiverId: String, receiverQual: String): String
  def nextInterchange(context: String): String
  def nextGroup(context: String, senderId: String, senderQual: String, receiverId: String, receiverQual: String): String
  def nextMessage(context: String, msgType: String, msgVersion: String, msgRelease: String, agency: String): String
}

/** Writer for EDIFACT EDI documents.
  */
case class EdifactSchemaWriter(out: OutputStream, sc: EdiSchema, numprov: EdifactNumberProvider, config: EdifactWriterConfig)
  extends SchemaWriter(new EdifactWriter(out, config.charSet, config.version, config.syntax, config.delims,
    config.suffix, config.subChar, config.decimalMark), sc) with EdifactSchemaDefs {

  import EdiSchema._
  import SchemaJavaValues._
  
  val schemaDefs = versions(config.version)

  case class TransactionSet(val ident: String, val index: Int, val selfId: String, val partnerId: String,
    val agencyCode: String, val versionId: String, val data: ValueMap) {
    override def toString = s"transaction $ident at index $index"
  }

  var setCount = 0
  var setSegmentBase = 0
  var inGroup = false

  def logAndThrow(text: String, cause: Option[Throwable]) = {
    logger error text
    cause match {
      case Some(e) => throw new WriteException(text, e)
      case _ => throw new WriteException(text)
    }
  }

  /** Output interchange trailer segment(s) and finish with stream. */
  def term(props: ValueMap) = {
    writer.term(props)
  }

  /** Write start of a functional group (modifies passed in map). */
  def openGroup(functId: String, props: ValueMap) = {
    props put (groupHeadReferenceKey, functId)
    writeSegment(props, schemaDefs.segUNG)
    setCount = 0
  }

  /** Write close of a functional group (modifies passed in map). */
  def closeGroup(props: ValueMap) = {
    props put (groupTrailCountKey, Integer.valueOf(setCount))
    props put (groupTrailReferenceKey, props.get(groupHeadReferenceKey))
    writeSegment(props, schemaDefs.segUNE)
  }

  /** Write start of a transaction set (modifies passed in map). */
  def openSet(ident: String, props: ValueMap) = {
    props put (msgHeadReferenceKey, ident)
    setSegmentBase = writer.getSegmentCount
    writeSegment(props, schemaDefs.segUNH)
  }

  /** Write close of a transaction set (modifies passed in map). */
  def closeSet(props: ValueMap) = {
    props put (msgTrailCountKey, Integer.valueOf(writer.getSegmentCount - setSegmentBase + 1))
    props put (msgTrailReferenceKey, props.get(msgHeadReferenceKey))
    writeSegment(props, schemaDefs.segUNT)
    setCount += 1
  }

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == "ST" || segment.ident == "SE"

  /** Group transaction sets into lists for one or more interchanges based on the self and partner identification
    * information. This also verifies that the data for each transaction matches the transaction type of the containing
    * list, and saves information in the transaction set to refer back to the position in the input data. Note that the
    * list of transactions for each interchange is in reverse order relative to the original order of the transactions.
    * @param transMap transaction identifiers to lists of transactions
    * @return List(((interSelfQual, interSelf, interPartQual, interPart, interUsage), List(transSet))
    */
  def transactionInterchanges(transMap: ValueMap) = {
    def tupleKey(transet: TransactionSet) = try {
      (getRequiredString(transactionInterSelfQualId, transet.data),
        getRequiredString(transactionInterSelfId, transet.data),
        getRequiredString(transactionInterPartnerQualId, transet.data),
        getRequiredString(transactionInterPartnerId, transet.data),
        getRequiredString(transactionInterchangeUsage, transet.data))
    } catch {
      case e: IllegalArgumentException => logAndThrow(s"$transet ${e.getMessage}", None)
    }
    val scalaTrans = JavaConversions.mapAsScalaMap(transMap)
    val result = scalaTrans.foldLeft(TreeMap[(String, String, String, String, String), List[TransactionSet]]()) {
      case (acc, (transnum, transets)) => {
        val transbuff = JavaConversions.asScalaBuffer(transets.asInstanceOf[MapList])
        val sequence = (0 until transbuff.size) map { i =>
          val transdata = transbuff(i)
          try {
            val transid = getRequiredString(transactionId, transdata)
            if (transid != transnum) throw new IllegalArgumentException(s"$transactionId value '$transid'' does not match containing type")
            val selfid = getRequiredString(transactionGroupSelfId, transdata)
            val partnerid = getRequiredString(transactionGroupPartnerId, transdata)
            val agencyCode = getRequiredString(transactionGroupAgencyCode, transdata)
            val versionid = getRequiredString(transactionGroupVersionCode, transdata)
            TransactionSet(transnum, i, selfid, partnerid, agencyCode, versionid, transdata)
          } catch {
            case e: IllegalArgumentException => logAndThrow(s"transaction $transnum at index $i ${e.getMessage}", None)
          }
        }
        sequence.foldLeft(acc) ((acc, tset) => {
          val key = tupleKey(tset)
          val list = acc get (key) match {
            case Some(l) => l
            case None => Nil
          }
          acc + (key -> (tset :: list))
        })
      }
    }
    result map { case (tupkey, translist ) => (tupkey, translist reverse)} toList
  }

    /** Stores list of string values matching the simple value components. */
    def setStrings(values: List[String], comps: List[SegmentComponent], data: ValueMap) = {
      @tailrec
      def setr(vals: List[String], rem: List[SegmentComponent]): Unit = vals match {
        case h :: t => if (rem.nonEmpty) {
          if (h.size > 0) data put (rem.head.key, h)
          setr(t, rem.tail)
          }
        case _ =>
      }
      setr(values, comps)
    }

  /** Write the output message. */
  def write(map: ValueMap) = Try({
    val interchanges = transactionInterchanges(getRequiredValueMap(transactionsMap, map))
    if (interchanges.isEmpty) throw new WriteException("no transactions to be sent")
    interchanges foreach {
      case ((selfQual, selfId, partnerQual, partnerId, useIndicator), interlist) => {
        val interProps = new ValueMapImpl
        val context = numprov contextToken (selfId, selfQual, partnerId, partnerQual)
        /*
         *   val interHeadReferenceKey = "UNB05"
  val interHeadDateKey = "UNB0601"
  val interHeadTimeKey = "UNB0602"
  val interHeadApplicationKey = "UNB07"
  val interHeadPriorityKey = "UNB08"
  val interHeadAckreqKey = "UNB09"
  val interHeadAgreementKey = "UNB10"
  val interHeadTestKey = "UNB11"
         * 
         */
        interProps put (interHeadReferenceKey, Integer valueOf (numprov nextInterchange (context)))
        setStrings(List(config.syntax.code, config.version.code), schemaDefs.interHeadSyntax.components, interProps)
        setStrings(List(partnerId, partnerQual), schemaDefs.interHeadSender.components, interProps)
        setStrings(List(selfId, selfQual), schemaDefs.interHeadRecipient.components, interProps)
        val calendar = new GregorianCalendar
        interProps put (interHeadDateKey, calendar)
        val time = Integer.valueOf((calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000)
        interProps put (interHeadTimeKey, time)
        writer.init(interProps)
        writeSegment(interProps, schemaDefs.segUNB)
        val groups = interlist.groupBy(transet => {
          val transdef = schema.transactions(transet.ident)
          (transet selfId, transet partnerId, transet agencyCode, transet versionId, transdef.group.getOrElse(""))
        })
        groups foreach {
          case ((selfGroup, partnerGroup, agency, version, groupCode), grouplist) => {
            grouplist foreach (transet => try {
              val transdata = transet.data
              val setProps = new ValueMapImpl
              setProps put (msgHeadReferenceKey, numprov.nextMessage(context, transet.ident, "D", schema.version, "UN"))
              setProps put (msgHeadMessageTypeKey, transet.ident)
              setProps put (msgHeadMessageVersionKey, "D")
              setProps put (msgHeadMessageReleaseKey, schema.version)
              setProps put (msgHeadMessageAgencyKey, "UN")
              setProps put (msgHeadMessageAssignedKey, groupCode)
              if (transdata containsKey (transactionImplConventionRef)) setProps put (msgHeadImplIdentKey,
                transdata get (transactionImplConventionRef))
              openSet(transet ident, setProps)
              writeTransaction(transdata, schema transactions (transet ident))
              closeSet(setProps)
            } catch {
              case e @ (_: IllegalArgumentException | _: WriteException) => {
                logAndThrow(s"transaction ${transet ident} at index ${transet index} ${e.getMessage}", Some(e))
              }
            })
          }
        }
        term(interProps)
      }
    }
    writer close
  })
}