package com.anypoint.df.edi.schema

import java.io.OutputStream
import java.nio.charset.Charset
import java.util.Calendar
import java.util.GregorianCalendar
import scala.annotation.tailrec
import scala.collection.JavaConversions
import scala.util.Try
import com.anypoint.df.edi.lexical.WriteException
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.lexical.X12Writer

/** Configuration parameters for X12 schema writer.
  */
case class X12WriterConfig(val stringChars: CharacterSet, val subChar: Int, val charSet: Charset, val delims: String,
  val suffix: String) {
  if (delims.size < 4 || delims.size > 5) throw new IllegalArgumentException("delimiter string must be 4 or 5 characters")
  val repsep = if (delims(2) == 'U') -1 else delims(2)
}

/** Writer for X12 EDI documents.
  */
case class X12SchemaWriter(out: OutputStream, sc: EdiSchema, numprov: NumberProvider, config: X12WriterConfig)
  extends SchemaWriter(new X12Writer(out, config.charSet, config.delims(0), config.delims(1),
      config.repsep, config.delims(3), config.suffix, config.subChar, config.stringChars),
      sc.merge(X12Acknowledgment.trans997)) with X12SchemaDefs {

  import EdiSchema._
  import SchemaJavaValues._
  import X12SchemaValues._

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

  /** Configure lexical-level writer. */
//  def configureLexical = {
//    writer.asInstanceOf[X12Writer].configureX12(out, config.charSet, config.delims(0), config.delims(1),
//      config.repsep, config.delims(3))
//  }

  /** Output interchange trailer segment(s) and finish with stream. */
  def term(props: ValueMap) = {
    writer.term(props)
  }

  /** Write start of a functional group (modifies passed in map). */
  def openGroup(functId: String, props: ValueMap) = {
    props put (functionalIdentifierKey, functId)
    val calendar = new GregorianCalendar
    props put (groupDateKey, calendar)
    val time = Integer.valueOf((calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000)
    props put (groupTimeKey, time)
    writeSegment(props, GSSegment)
    setCount = 0
  }

  /** Write close of a functional group (modifies passed in map). */
  def closeGroup(props: ValueMap) = {
    props put (numberOfSetsKey, Integer.valueOf(setCount))
    props put (groupControlEndKey, props.get(groupControlKey))
    writeSegment(props, GESegment)
  }

  /** Write start of a transaction set (modifies passed in map). */
  def openSet(ident: String, props: ValueMap) = {
    props put (transactionSetIdentifierKey, ident)
    setSegmentBase = writer.getSegmentCount
    writeSegment(props, STSegment)
  }

  /** Write close of a transaction set (modifies passed in map). */
  def closeSet(props: ValueMap) = {
    props put (numberOfSegmentsKey, Integer.valueOf(writer.getSegmentCount - setSegmentBase + 1))
    props put (transactionSetControlEndKey, props.get(transactionSetControlKey))
    writeSegment(props, SESegment)
    setCount += 1
  }

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == "ST" || segment.ident == "SE"

  /** Group transaction sets into lists for one or more interchanges based on the self and partner identification
    * information. This also verifies that the data for each transaction matches the transaction type of the containing
    * list, and saves information in the transaction set to refer back to the position in the input data.
    * @param transMap transaction identifiers to lists of transactions
    */
  def transactionInterchanges(transMap: ValueMap) = {
    def tupleKey(transet: TransactionSet) = try {
      (getRequiredString(transactionInterSelfQualId, transet.data),
        getRequiredString(transactionInterSelfId, transet.data),
        getRequiredString(transactionInterPartnerQualId, transet.data),
        getRequiredString(transactionInterPartnerId, transet.data))
    } catch {
      case e: IllegalArgumentException => logAndThrow(s"$transet ${e.getMessage}", None)
    }
    val scalaTrans = JavaConversions.mapAsScalaMap(transMap)
    val result = scalaTrans.flatMap {
      case ((transnum, transets)) => {
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
        sequence.iterator.toList.groupBy(tupleKey(_))
      }
    }
    result.toList
  }

  /** Write the output message. */
  def write(map: ValueMap) = Try({
    val interchanges = transactionInterchanges(getRequiredValueMap(transactionsMap, map))
    if (interchanges.isEmpty) throw new WriteException("no transactions to be sent")
    interchanges foreach {
      case ((selfQual, selfId, partnerQual, partnerId), interlist) => {
        val interProps = new ValueMapImpl
        interProps put (INTER_CONTROL, Integer valueOf (numprov.nextInterchange))
        interProps put (RECEIVER_ID_QUALIFIER, partnerQual)
        interProps put (RECEIVER_ID, partnerId)
        interProps put (SENDER_ID_QUALIFIER, selfQual)
        interProps put (SENDER_ID, selfId)
        interProps put (VERSION_ID, getAs(interchangeVersionId, "00501", map))
        writer.init(interProps)
        val groups = interlist.groupBy(transet => {
          val transdef = schema.transactions(transet.ident)
          (transet selfId, transet partnerId, transet agencyCode, transet versionId, transdef group)
        })
        groups foreach {
          case ((selfGroup, partnerGroup, agency, version, groupCode), grouplist) => {
            val groupProps = new ValueMapImpl
            groupProps put (applicationSendersKey, selfGroup)
            groupProps put (applicationReceiversKey, partnerGroup)
            groupProps put (groupControlKey, Integer valueOf (numprov.nextGroup))
            groupProps put (responsibleAgencyKey, agency)
            groupProps put (versionIdentifierKey, version)
            openGroup(groupCode, groupProps)
            writer.countGroup
            grouplist foreach (transet => try {
              @tailrec
              def zeroPad(text: String, length: Int): String =
                if (text.length < length) zeroPad("0" + text, length) else text
              val transdata = transet.data
              val setProps = new ValueMapImpl
              setProps put (transactionSetControlKey, zeroPad(numprov.nextSet toString, 4))
              if (transdata containsKey (transactionImplConventionRef)) setProps put (implementationConventionKey,
                transdata get (transactionImplConventionRef))
              openSet(transet ident, setProps)
              writeTransaction(transdata, schema transactions (transet ident))
              closeSet(setProps)
            } catch {
              case e @ (_ : IllegalArgumentException | _ : WriteException) => {
                logAndThrow(s"transaction ${transet ident} at index ${transet index} ${e.getMessage}", Some(e))
              }
            })
            closeGroup(groupProps)
          }
        }
        term(interProps)
      }
    }
    writer close
  })
}