package com.anypoint.df.edi.schema

import java.io.OutputStream
import java.nio.charset.Charset
import java.util.Calendar
import java.util.GregorianCalendar
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.util.Try
import com.anypoint.df.edi.lexical.{ EdifactWriter, WriteException, WriterBase }
import com.anypoint.df.edi.lexical.EdifactConstants._

/** Configuration parameters for EDIFACT schema writer.
  */
case class EdifactWriterConfig(val syntax: SyntaxIdentifier, val version: SyntaxVersion, val subChar: Int,
  val decimalMark: Char, val charSet: Charset, val delims: String, val suffix: String, val forceUNOA: Boolean) {
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
    config.suffix, config.subChar, config.decimalMark), sc) {

  import EdiSchema._
  import SchemaJavaValues._
  import EdifactSchemaDefs._

  var setCount = 0
  var setSegmentBase = 0
  var inGroup = false
  
  /** Write top-level section of transaction. */
  def writeTopSection(index: Int, map: ValueMap, comps: List[TransactionComponent]) = comps match {
    case (ref: ReferenceComponent) :: tail if (ref.segment.ident == "UNS") => {
      writer.writeToken("UNS")
      writer.writeDataSeparator
      writer.writeToken(if (index == 1) "D" else "S")
      writer.writeSegmentTerminator
      writeSection(map, tail)
    }
    case _ => writeSection(map, comps)
  }

  /** Output interchange header segment(s). */
  def init(props: ValueMap) = {
    props put (interHeadSyntaxIdentKey, config.syntax.code)
    props put (interHeadSyntaxVersionKey, config.version.code)
    if (config.forceUNOA) props put (FORCE_UNA, java.lang.Boolean.TRUE)
    // TODO: set character encoding for v4, if it doesn't match syntax
    writer.init(props)
    writeSegment(props, unbSegment(config.version))
  }

  /** Output interchange trailer segment(s). */
  def term(props: ValueMap) = {
    writeSegment(props, segUNZ)
    writer.term(props)
  }

  /** Write start of a functional group (modifies passed in map). */
  def openGroup(functId: String, props: ValueMap) = {
    props put (groupHeadReferenceKey, functId)
    writeSegment(props, ungSegment(config.version))
    setCount = 0
  }

  /** Write close of a functional group (modifies passed in map). */
  def closeGroup(props: ValueMap) = {
    props put (groupTrailCountKey, Integer.valueOf(setCount))
    props put (groupTrailReferenceKey, props.get(groupHeadReferenceKey))
    writeSegment(props, segUNE)
  }

  /** Write start of a transaction set (modifies passed in map). */
  def openSet(ident: String, props: ValueMap) = {
    props put (msgHeadReferenceKey, ident)
    setSegmentBase = writer.getSegmentCount
    writeSegment(props, unhSegment(config.version))
  }

  /** Write close of a transaction set (modifies passed in map). */
  def closeSet(props: ValueMap) = {
    props put (msgTrailCountKey, Integer.valueOf(writer.getSegmentCount - setSegmentBase + 1))
    props put (msgTrailReferenceKey, props.get(msgHeadReferenceKey))
    writeSegment(props, segUNT)
  }

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = schema.ediForm.isEnvelopeSegment(segment.ident)

  /** Stores list of string values matching the simple value components. */
  def setStrings(values: List[String], comps: List[SegmentComponent], data: ValueMap) = {
    @tailrec
    def setr(vals: List[String], rem: List[SegmentComponent]): Unit = vals match {
      case h :: t => if (rem.nonEmpty) {
        if (h != null && h.size > 0) data put (rem.head.key, h)
        setr(t, rem.tail)
      }
      case _ =>
    }
    setr(values, comps)
  }

  val msgIndexKey = "$index$"

  /** Write the output message. */
  def write(map: ValueMap) = Try(try {
    val interchanges = getRequiredValueMap(messagesMap, map).asScala.foldLeft(EmptySendMap) {
      case (acc, (ident, list)) => {
        val msgMaps = list.asInstanceOf[MapList].asScala
        (0 until msgMaps.size) map { i =>
          val msgMap = msgMaps(i)
          msgMap put (msgIndexKey, Integer.valueOf(i))
          if (msgMap.containsKey(transactionId)) {
            if (ident != msgMap.get(transactionId)) {
              logAndThrow(s"$ident at position $i has type ${msgMap.get(transactionId)} (wrong message list)")
            }
          } else msgMap put (transactionId, ident)
        }
        groupSends(msgMaps, SchemaJavaValues.interchangeKey, acc)
      }
    }
    if (interchanges.isEmpty) throw new WriteException("no messages to be sent")
    val interRoot = if (map.containsKey(interchangeKey)) getRequiredValueMap(interchangeKey, map) else new ValueMapImpl
    interchanges foreach {
      case (key, msgs) => {
        val interProps = new ValueMapImpl(interRoot)
        key.foreach(interProps.putAll(_))
        val selfId = getRequiredString(interHeadSenderIdentKey, interProps)
        val selfQual = getAsString(interHeadSenderQualKey, interProps)
        val partnerId = getRequiredString(interHeadRecipientIdentKey, interProps)
        val partnerQual = getAsString(interHeadRecipientQualKey, interProps)
        val context = numprov contextToken (selfId, selfQual, partnerId, partnerQual)
        val interref = numprov nextInterchange (context)
        interProps put (interHeadReferenceKey, interref)
        setStrings(List(config.syntax.code, config.version.code), unbSyntax.components, interProps)
        setStrings(List(partnerId, partnerQual), unbSender.components, interProps)
        setStrings(List(selfId, selfQual), unbRecipient.components, interProps)
        if (!interProps.containsKey(interHeadDateKey)) {
          val calendar = new GregorianCalendar
          val yearnum = calendar.get(Calendar.YEAR)
          val basedate = calendar.get(Calendar.DAY_OF_MONTH) * 100 + calendar.get(Calendar.MONTH) + 1
          val datetime = unbSegment(config.version).components(3).asInstanceOf[CompositeComponent]
          val dateelem = datetime.composite.components(0).asInstanceOf[ElementComponent].element
          val date = if (dateelem.maxLength == 8) basedate * 10000 + yearnum else basedate * 100 + yearnum % 100
          interProps put (interHeadDateKey, Integer.valueOf(date))
        }
        if (!interProps.containsKey(interHeadTimeKey)) {
          val calendar = new GregorianCalendar
          val time = Integer.valueOf(calendar.get(Calendar.HOUR_OF_DAY) * 60 + calendar.get(Calendar.MINUTE))
          interProps put (interHeadTimeKey, time)
        }
        init(interProps)
        setCount = 0
        groupSends(msgs, SchemaJavaValues.groupKey, EmptySendMap) foreach {
          case (key, msgs) => {
            msgs foreach (msgData => try {
              val setProps =
                if (msgData.containsKey(messageHeaderKey)) new ValueMapImpl(getAsMap(messageHeaderKey, msgData))
                else new ValueMapImpl
              val msgType = getRequiredString(transactionId, msgData)
              setProps put (msgHeadMessageTypeKey, msgType)
              if (!setProps.containsKey(msgHeadMessageAgencyKey)) setProps put (msgHeadMessageAgencyKey, "UN")
              val msgAgency = getAsString(msgHeadMessageVersionKey, setProps)
              val fullVersion = schema.version.toUpperCase
              val msgVersion = fullVersion.substring(0, 1)
              val msgRelease = fullVersion.substring(1)
              setProps put (msgHeadMessageVersionKey, msgVersion)
              setProps put (msgHeadMessageReleaseKey, msgRelease)
              openSet(numprov.nextMessage(context, msgType, msgVersion, msgRelease, msgAgency), setProps)
              writeTransaction(msgData, schema transactions (msgType))
              closeSet(setProps)
              setCount += 1
            } catch {
              case e: Throwable => {
                logAndThrow(s"${e.getMessage} in ${msgData.get(transactionId)} at index ${msgData.get(msgIndexKey)}", e)
              }
            })
          }
        }
        val termprops = new ValueMapImpl
        termprops put (interTrailReferenceKey, interref)
        termprops put (interTrailCountKey, Integer.valueOf(setCount))
        term(termprops)
      }
    }
  } catch {
    case e: WriteException => throw e
    case e: Throwable => logAndThrow("Writer error ", e)
  } finally {
    try { close } catch { case e: Throwable => }
  })
}