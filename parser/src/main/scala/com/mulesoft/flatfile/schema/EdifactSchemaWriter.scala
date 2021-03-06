package com.mulesoft.flatfile.schema

import java.io.OutputStream
import java.math.{ BigDecimal, BigInteger }
import java.nio.charset.Charset
import java.util.{ Calendar, Date, GregorianCalendar }
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.util.Try
import com.mulesoft.flatfile.lexical.{ EdifactWriter, ErrorHandler, TypeFormat, WriteException }
import com.mulesoft.flatfile.lexical.EdifactConstants._
import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition
import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition._

/** Configuration parameters for EDIFACT schema writer.
  */
case class EdifactWriterConfig(val syntax: SyntaxIdentifier, val version: SyntaxVersion, val enforceChars: Boolean,
    val enforceRequires: Boolean, val subChar: Int, val decimalMark: Char, val charSet: Charset, val delims: String,
    val suffix: String, val forceUNA: Boolean) {
  if (delims != null && delims.size != 5) throw new IllegalArgumentException("delimiter string must be 5 characters")
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
case class EdifactSchemaWriter(out: OutputStream, numprov: EdifactNumberProvider, config: EdifactWriterConfig)
    extends DelimiterSchemaWriter(new EdifactWriter(out, config.charSet, config.version, config.syntax,
      config.enforceChars, config.delims, config.suffix, config.subChar, config.decimalMark), config.enforceRequires) {

  import EdiSchema._
  import SchemaJavaValues._
  import EdifactSchemaDefs._

  /** Typed writer, for access to format-specific conversions and support. */
  val writer = baseWriter.asInstanceOf[EdifactWriter]

  var setCount = 0
  var setSegmentBase = 0
  var inGroup = false

  /** Lexical error handler. */
  case object EdifactWriterErrorHandler extends ErrorHandler {
    // replace this with actual error accumulation
    def error(typ: TypeFormat, error: ErrorCondition, explain: java.lang.String): Unit = {
      error match {
        case TOO_LONG | TOO_SHORT | WRONG_TYPE | INVALID_VALUE => throw new WriteException(explain)
        case _ =>
      }
    }
  }

  /** Write top-level section of structure. */
  def writeTopSection(index: Int, map: ValueMap, seq: StructureSequence) = seq.items match {
    case (ref: ReferenceComponent) :: tail if (ref.segment.ident == "UNS") => {
      writer.writeToken("UNS")
      writer.writeDataSeparator
      writer.writeToken(if (index == 1) "D" else "S")
      writer.writeSegmentTerminator
      writeSection(map, tail)
    }
    case _ => writeSection(map, seq.items)
  }

  /** Output interchange header segment(s). */
  def init(props: ValueMap) = {
    props put (interHeadSyntaxIdentKey, config.syntax.code)
    props put (interHeadSyntaxVersionKey, config.version.code)
    if (config.forceUNA) props put (FORCE_UNA, java.lang.Boolean.TRUE)
    // TODO: set character encoding for v4, if it doesn't match syntax
    writer.init(props)
    writeSegment(props, unbSegment(config.version))
  }

  /** Output interchange trailer segment(s). */
  def term(interref: String) = {
    val props = new ValueMapImpl
    props put (interTrailReferenceKey, interref)
    props put (interTrailCountKey, Integer.valueOf(setCount))
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

  /** Write start of a structure set (modifies passed in map). */
  def openSet(ident: String, props: ValueMap) = {
    props put (msgHeadReferenceKey, ident)
    setSegmentBase = writer.getSegmentCount
    writeSegment(props, unhSegment(config.version))
  }

  /** Write close of a structure set (modifies passed in map). */
  def closeSet(props: ValueMap) = {
    props put (msgTrailCountKey, Integer.valueOf(writer.getSegmentCount - setSegmentBase + 1))
    props put (msgTrailReferenceKey, props.get(msgHeadReferenceKey))
    writeSegment(props, segUNT)
  }

  /** Check if an envelope segment (handled directly, outside of structure). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == "UNH" || segment.ident == "UNT"

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
    
    def openInterchange(optmap: Option[ValueMap], interRoot: ValueMap) = {
      val interProps = new ValueMapImpl
      optmap.foreach(interProps.putAll(_))
      val selfId = getRequiredString(interHeadSenderIdentKey, interProps)
      val selfQual = getAsString(interHeadSenderQualKey, interProps)
      val partnerId = getRequiredString(interHeadRecipientIdentKey, interProps)
      val partnerQual = getAsString(interHeadRecipientQualKey, interProps)
      val context = numprov contextToken (selfId, selfQual, partnerId, partnerQual)
      val interref = numprov nextInterchange (context)
      interProps put (interHeadReferenceKey, interref)
      setStrings(List(config.syntax.code, config.version.code), unbSyntax.components, interProps)
      setStrings(List(selfId, selfQual), unbSender.components, interProps)
      setStrings(List(partnerId, partnerQual), unbRecipient.components, interProps)
      if (!interProps.containsKey(interHeadDateKey)) {
        val calendar = new GregorianCalendar
        val yearnum = calendar.get(Calendar.YEAR)
        val basedate = calendar.get(Calendar.DAY_OF_MONTH) * 100 + calendar.get(Calendar.MONTH) + 1
        val datetime = unbSegment(config.version).components(3).asInstanceOf[CompositeComponent]
        val dateelem = datetime.composite.components(0).asInstanceOf[ElementComponent].element
        val date = if (dateelem.typeFormat.maxLength == 8) basedate * 10000 + yearnum else basedate * 100 + yearnum % 100
        interProps put (interHeadDateKey, Integer.valueOf(date))
      }
      if (!interProps.containsKey(interHeadTimeKey)) {
        val calendar = new GregorianCalendar
        val time = Integer.valueOf(calendar.get(Calendar.HOUR_OF_DAY) * 60 + calendar.get(Calendar.MINUTE))
        interProps put (interHeadTimeKey, time)
      }
      init(interProps)
      setCount = 0
      (context, interref, interProps)
    }
    
    def sendList(msgs: List[ValueMap], context: String, inter: ValueMap) = {
      msgs foreach (msgData => try {
        msgData.put(interchangeKey, inter)
        val setProps =
          if (msgData.containsKey(messageHeaderKey))  new ValueMapImpl(getAsMap(messageHeaderKey, msgData))
          else  new ValueMapImpl
        msgData.put(messageHeaderKey, setProps)
        val struct = getAsRequired[Structure](structureSchema, msgData)
        val msgType = getRequiredString(structureId, msgData)
        setProps put (msgHeadMessageTypeKey, msgType)
        if (!setProps.containsKey(msgHeadMessageAgencyKey)) setProps put (msgHeadMessageAgencyKey, "UN")
        val msgAgency = getAsString(msgHeadMessageAgencyKey, setProps)
        val (msgVersion, msgRelease) = if (msgType == "CONTRL") {
          val version = config.version.code
          (version, "1")
        } else {
          val fullVersion = struct.version.version.toUpperCase
          (fullVersion.substring(0, 1), fullVersion.substring(1))
        }
        setProps put (msgHeadMessageVersionKey, msgVersion)
        setProps put (msgHeadMessageReleaseKey, msgRelease)
        openSet(numprov.nextMessage(context, msgType, msgVersion, msgRelease, msgAgency), setProps)
        writeStructure(msgData, struct)
        closeSet(setProps)
        setCount += 1
      } catch {
        case e: WriteException => throw e
        case e: Throwable =>
          logAndThrow(s"${e.getMessage} in ${msgData.get(structureId)} at index ${msgData.get(msgIndexKey)}", e)
      })
    }
    
    val interchanges = getRequiredValueMap(messagesMap, map).asScala.foldLeft(EmptySendMap) {
      case (acc, (version, vmap)) => {
        vmap.asInstanceOf[ValueMap].asScala.foldLeft(acc) {
          case (acc, (ident, list)) => {
            val msgMaps = list.asInstanceOf[MapList].asScala.toIndexedSeq
            (0 until msgMaps.size) map { i =>
              val msgMap = msgMaps(i)
              msgMap put (msgIndexKey, Integer.valueOf(i))
              if (msgMap.containsKey(structureId)) {
                if (ident != msgMap.get(structureId)) {
                  logAndThrow(s"$ident at position $i has type ${msgMap.get(structureId)} (wrong message list)")
                }
              } else msgMap put (structureId, ident)
            }
            groupSends(msgMaps, SchemaJavaValues.interchangeKey, acc)
          }
        }
      }
    }
    writer.setHandler(EdifactWriterErrorHandler)
    val sendAcks = getAs(functionalAcksToSend,  new MapListImpl, map)
    if (interchanges.isEmpty && sendAcks.isEmpty) throw new WriteException("no messages to be sent")
    val interRoot = if (map.containsKey(interchangeKey)) getRequiredValueMap(interchangeKey, map) else  new ValueMapImpl
    val ackInterchanges = sendAcks.asScala.foldLeft(Map[Option[ValueMap], List[ValueMap]]()) { (acc, map) =>
      val key = Some(getRequiredValueMap(interchangeKey, map))
      acc.get(key) match {
        case Some(list) => acc + (key -> (map :: list))
        case None => acc + (key -> List(map))
      }
    }
    
    // send interchanges containing only acks
    ackInterchanges.keys.filter { !interchanges.contains(_) }.foreach { key =>
      val (context, interref, interprops) = openInterchange(key, interRoot)
      sendList(ackInterchanges(key), context, interprops)
      term(interref)
    }
    
    // send interchanges containing application messages and potentially also acks
    interchanges foreach {
      case (key, msgs) => {
        val (context, interref, interprops) = openInterchange(key, interRoot)
        ackInterchanges.get(key).foreach { sendList(_, context, interprops) }
        groupSends(msgs, SchemaJavaValues.groupKey, EmptySendMap) foreach {
          case (key, msgs) => sendList(msgs, context, interprops)
        }
        term(interref)
      }
    }
  } catch {
    case e: WriteException => throw e
    case e: Throwable => logAndThrow("Writer error ", e)
  } finally {
    try { close } catch { case e: Throwable => }
  })
}