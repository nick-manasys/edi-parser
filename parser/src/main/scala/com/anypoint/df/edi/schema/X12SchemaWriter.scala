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
    if (!props.containsKey(groupDateKey)) props put (groupDateKey, new GregorianCalendar)
    if (!props.containsKey(groupTimeKey)) {
      val calendar = new GregorianCalendar
      val time = Integer.valueOf((calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000)
      props put (groupTimeKey, time)
    }
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

  val transIndexKey = "$index$"

  /** Write the output message. */
  def write(map: ValueMap) = Try({
    val interchanges = getRequiredValueMap(transactionsMap, map).asScala.foldLeft(EmptySendMap) {
      case (acc, (ident, list)) => {
        val transMaps = list.asInstanceOf[MapList].asScala
        (0 until transMaps.size) map { i =>
          val transMap = transMaps(i)
          transMap put (transIndexKey, Integer.valueOf(i))
          if (transMap.containsKey(transactionId)) {
            if (ident != transMap.get(transactionId)) throw new WriteException("$ident at position $i has type ${transMap.get(transactionId)} (wrong message list)")
          } else transMap put (transactionId, ident)
        }
        groupSends(transMaps, SchemaJavaValues.interchangeKey, acc)
      }
    }
    if (interchanges.isEmpty) throw new WriteException("no transactions to be sent")
    val interRoot = getRequiredValueMap(interchangeKey, map)
    val groupRoot = getAsMap(groupKey, map)
    interchanges foreach {
      case (key, trans) => {
        val interProps = new ValueMapImpl(interRoot)
        key.foreach(interProps.putAll(_))
        val senderQual = getRequiredString(SENDER_ID_QUALIFIER, interProps)
        val senderId = getRequiredString(SENDER_ID, interProps)
        val receiverQual = getRequiredString(RECEIVER_ID_QUALIFIER, interProps)
        val receiverId = getRequiredString(RECEIVER_ID, interProps)
        if (!interProps.containsKey(VERSION_ID)) {
          interProps put (VERSION_ID, schema.version.substring(0, schema.version.length() - 1))
        }
        val providerId = numprov interchangIdentifier (senderQual, senderId, receiverQual, receiverId)
        interProps put (INTER_CONTROL, Integer valueOf (numprov nextInterchange (providerId)))
        writer.init(interProps)
        if (map.containsKey(interchangeAcksToSend)) {
          val ta1list = getRequiredMapList(interchangeAcksToSend, map)
          ta1list.asScala.foreach { ta1map => writeSegment(ta1map, X12Acknowledgment.segTA1) }
          map.remove(interchangeAcksToSend)
        }
        groupSends(trans, SchemaJavaValues.groupKey, EmptySendMap) foreach {
          case (key, trans) =>
            val transGroups = trans.groupBy { transMap =>
              val ident = getAsString(transactionId, transMap)
              schema.transactions.get(ident) match {
                case Some(t) => t.group.get
                case None => throw new WriteException("$ident at position $i has type ${transMap.get(transactionId)} (wrong message list)")
              }
            }
            transGroups.foreach{
              case (groupCode, transList) =>
                val groupProps = if (groupRoot == null) new ValueMapImpl else new ValueMapImpl(groupRoot)
                key.foreach(groupProps.putAll(_))
                val senderGroup = getRequiredString(groupApplicationSenderKey, groupProps)
                val receiverGroup = getRequiredString(groupApplicationReceiverKey, groupProps)
                groupProps put (groupControlNumberHeaderKey, Integer valueOf (numprov nextGroup (providerId, senderGroup, receiverGroup)))
                openGroup(groupCode, groupProps)
                writer.countGroup
                transList foreach (transet => try {
                  val setProps =
                    if (transet.containsKey(setKey)) new ValueMapImpl(getAsMap(setKey, transet))
                    else new ValueMapImpl
                  setProps put (setControlNumberHeaderKey, zeroPad(numprov nextSet (providerId, senderGroup, receiverGroup), 4))
                  val ident = getAsString(transactionId, transet)
                  openSet(ident, setProps)
                  writeTransaction(transet, schema transactions (ident))
                  closeSet(setProps)
                } catch {
                  case e@(_: IllegalArgumentException | _: WriteException) => {
                    logAndThrow(s"transaction ${getAsString(transactionId, transet)} at index ${getAsInt(transIndexKey, transet)} ${e.getMessage}", Some(e))
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