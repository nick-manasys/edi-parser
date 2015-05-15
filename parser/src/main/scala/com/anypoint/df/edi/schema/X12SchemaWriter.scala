package com.anypoint.df.edi.schema

import java.io.OutputStream
import java.nio.charset.Charset
import java.util.Calendar
import java.util.GregorianCalendar
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.util.Try
import com.anypoint.df.edi.lexical.WriteException
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.lexical.X12Writer

/** Configuration parameters for X12 schema writer.
  */
case class X12WriterConfig(val stringChars: CharacterRestriction, val subChar: Int, val charSet: Charset,
  val delims: String, val suffix: String) {
  if (delims.size < 4 || delims.size > 5) throw new IllegalArgumentException("delimiter string must be 4 or 5 characters")
  val repsep = if (delims(2) == 'U') -1 else delims(2)
}

/** Provider interface for control numbers. */
trait X12NumberProvider {
  def interchangIdentifier(senderQual: String, senderId: String, receiverQual: String, receiverId: String): String
  def nextInterchange(interchange: String): Int
  def nextGroup(interchange: String, senderCode: String, receiverCode: String): Int
  def nextSet(interchange: String, senderCode: String, receiverCode: String): String
}

/** Writer for X12 EDI documents.
  */
case class X12SchemaWriter(out: OutputStream, sc: EdiSchema, numprov: X12NumberProvider, config: X12WriterConfig)
  extends SchemaWriter(new X12Writer(out, config.charSet, config.delims(0), config.delims(1),
    config.repsep, config.delims(3), config.suffix, config.subChar, config.stringChars),
    sc.merge(X12Acknowledgment.trans997)) {

  import EdiSchema._
  import SchemaJavaValues._
  import X12SchemaDefs._

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
    props put (groupFunctionalIdentifierKey, functId)
    val calendar = new GregorianCalendar
    props put (groupDateKey, calendar)
    val time = Integer.valueOf((calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000)
    props put (groupTimeKey, time)
    writeSegment(props, GSSegment)
    setCount = 0
  }

  /** Write close of a functional group (modifies passed in map). */
  def closeGroup(props: ValueMap) = {
    props put (groupNumberSetsIncludedKey, Integer.valueOf(setCount))
    props put (groupControlNumberTrailerKey, props.get(groupControlNumberHeaderKey))
    writeSegment(props, GESegment)
  }

  /** Write start of a transaction set (modifies passed in map). */
  def openSet(ident: String, props: ValueMap) = {
    props put (setIdentifierCodeKey, ident)
    setSegmentBase = writer.getSegmentCount
    writeSegment(props, STSegment)
  }

  /** Write close of a transaction set (modifies passed in map). */
  def closeSet(props: ValueMap) = {
    props put (setNumberSegmentsIncludedKey, Integer.valueOf(writer.getSegmentCount - setSegmentBase + 1))
    props put (setControlNumberTrailerKey, props.get(setControlNumberHeaderKey))
    writeSegment(props, SESegment)
    setCount += 1
  }

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == "ST" || segment.ident == "SE"
  
  /** Group transaction sets into lists for one or more interchanges based on the self and partner identification
    * information. This also verifies that the data for each transaction matches the transaction type of the containing
    * list, and saves information in the transaction set to refer back to the position in the input data. Note that the
    * list of transactions for each interchange is in reverse order relative to the original order of the transactions.
    * @param rootMap message root
    * @return List(((interSelfQual, interSelf, interPartQual, interPart, interUsage), List(transSet))
    */
  def transactionInterchanges(rootMap: ValueMap) = {
    val interDflt = getAsMap(interchangeKey, rootMap)
    val groupDflt = getAsMap(groupKey, rootMap)
    def getInterchangeString(key: String, specific: ValueMap, dflt: String) =
      if (specific != null && specific.containsKey(key)) getAsString(key, specific)
      else if (interDflt != null && interDflt.containsKey(key)) getAsString(key, interDflt)
      else dflt
    def getGroupString(key: String, specific: ValueMap) =
      if (specific != null && specific.containsKey(key)) getAsString(key, specific)
      else if (groupDflt == null) null
      else getAsString(key, groupDflt)
    def tupleKey(transet: TransactionSet) = try {
      val specific = getAsMap(interchangeKey, transet.data)
      (getInterchangeString(SENDER_ID_QUALIFIER, specific, null),
        getInterchangeString(SENDER_ID, specific, null),
        getInterchangeString(RECEIVER_ID_QUALIFIER, specific, null),
        getInterchangeString(RECEIVER_ID, specific, null),
        getInterchangeString(TEST_INDICATOR, specific, null),
        getInterchangeString(VERSION_ID, specific, "00501"),
        getInterchangeString(ACK_REQUESTED, specific, "1"))
    } catch {
      case e: IllegalArgumentException => logAndThrow(s"$transet ${e.getMessage}", None)
    }
    val scalaTrans = getAsMap(transactionsMap, rootMap).asScala
    val result = scalaTrans.foldLeft(TreeMap[(String, String, String, String, String, String, String),
      List[TransactionSet]]()) {
      case (acc, (transnum, transets)) => {
        val transbuff = transets.asInstanceOf[MapList].asScala
        val sequence = (0 until transbuff.size) map { i =>
          val transdata = transbuff(i)
          try {
            val transid = getRequiredString(transactionId, transdata)
            if (transid != transnum) throw new IllegalArgumentException(s"$transactionId value '$transid'' does not match containing type")
            val specific = getAsMap(groupKey, transdata)
            val selfid = getGroupString(groupApplicationSenderKey, specific)
            val partnerid = getGroupString(groupApplicationReceiverKey, specific)
            val agencyCode = getGroupString(groupResponsibleAgencyKey, specific)
            val versionid = getGroupString(groupVersionReleaseIndustryKey, specific)
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

  /** Write the output message. */
  def write(map: ValueMap) = Try({
    val interchanges = transactionInterchanges(map)
    if (interchanges.isEmpty) throw new WriteException("no transactions to be sent")
    val interDflt = getAsMap(interchangeKey, map)
    val groupDflt = getAsMap(groupKey, map)
    interchanges foreach {
      case ((senderQual, senderId, receiverQual, receiverId, useIndicator, versionId, ackRequested), interlist) => {
        val interProps = new ValueMapImpl
        val providerId = numprov interchangIdentifier (senderQual, senderId, receiverQual, receiverId)
        interProps put (INTER_CONTROL, Integer valueOf (numprov nextInterchange (providerId)))
        interProps put (RECEIVER_ID_QUALIFIER, receiverQual)
        interProps put (RECEIVER_ID, receiverId)
        interProps put (SENDER_ID_QUALIFIER, senderQual)
        interProps put (SENDER_ID, senderId)
        interProps put (VERSION_ID, versionId)
        interProps put (ACK_REQUESTED, ackRequested)
        interProps put (TEST_INDICATOR, useIndicator)
        writer.init(interProps)
        if (map.containsKey(interchangeAcksToSend)) {
          val ta1list = getRequiredMapList(interchangeAcksToSend, map)
          ta1list.asScala.foreach { ta1map => writeSegment(ta1map, X12Acknowledgment.segTA1) }
          map.remove(interchangeAcksToSend)
        }
        val groups = interlist.groupBy(transet => {
          val transdef = schema.transactions(transet.ident)
          (transet selfId, transet partnerId, transet agencyCode, transet versionId, transdef.group.getOrElse(""))
        })
        groups foreach {
          case ((senderGroup, receiverGroup, agency, version, groupCode), grouplist) => {
            val groupProps = new ValueMapImpl
            groupProps put (groupApplicationSenderKey, senderGroup)
            groupProps put (groupApplicationReceiverKey, receiverGroup)
            groupProps put (groupControlNumberHeaderKey, Integer valueOf (numprov nextGroup (providerId, senderGroup, receiverGroup)))
            groupProps put (groupResponsibleAgencyKey, agency)
            groupProps put (groupVersionReleaseIndustryKey, version)
            openGroup(groupCode, groupProps)
            writer.countGroup
            grouplist foreach (transet => try {
              val transdata = transet.data
              val setProps =
                if (transdata.containsKey(setKey)) new ValueMapImpl(getRequiredValueMap(setKey, transdata))
                else new ValueMapImpl
              setProps put (setControlNumberHeaderKey, zeroPad(numprov nextSet (providerId, senderGroup, receiverGroup), 4))
              openSet(transet ident, setProps)
              writeTransaction(transdata, schema transactions (transet ident))
              closeSet(setProps)
            } catch {
              case e @ (_: IllegalArgumentException | _: WriteException) => {
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