package com.anypoint.df.edi.schema

import java.io.OutputStream
import java.nio.charset.Charset
import java.util.Calendar
import java.util.GregorianCalendar
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.util.Try
import com.anypoint.df.edi.lexical.{ ErrorHandler, TypeFormat, WriteException, X12Writer }
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition._

/** Configuration parameters for X12 schema writer.
  */
case class X12WriterConfig(val enforceRequires: Boolean, val stringChars: CharacterRestriction, val subChar: Int,
  val charSet: Charset, val delims: String, val suffix: String) {
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
case class X12SchemaWriter(out: OutputStream, numprov: X12NumberProvider, config: X12WriterConfig)
    extends DelimiterSchemaWriter(new X12Writer(out, config.charSet, config.delims(0), config.delims(1),
      config.repsep, config.delims(3), config.suffix, config.subChar, config.stringChars), config.enforceRequires) {

  import EdiSchema._
  import SchemaJavaValues._
  import X12SchemaDefs._

  /** Typed writer, for access to format-specific conversions and support. */
  val writer = baseWriter.asInstanceOf[X12Writer]

  var setCount = 0
  var setSegmentBase = 0
  var inGroup = false

  /** Lexical error handler. */
  case object X12WriterErrorHandler extends ErrorHandler {
    // replace this with actual error accumlation
    def error(typ: TypeFormat, error: ErrorCondition, explain: java.lang.String): Unit = {
      error match {
        case WRONG_TYPE => throw new WriteException(explain)
        case _ =>
      }
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

  /** Write top-level section of transaction. */
  def writeTopSection(index: Int, map: ValueMap, seq: StructureSequence) = writeSection(map, seq.items)

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == "ST" || segment.ident == "SE"

  val transIndexKey = "$index$"

  /** Write the output message. */
  def write(map: ValueMap) = Try(try {
    writer.setHandler(X12WriterErrorHandler)
    val interchanges = getRequiredValueMap(transactionsMap, map).asScala.foldLeft(EmptySendMap) {
      case (acc, (version, vmap)) => {
        vmap.asInstanceOf[ValueMap].asScala.foldLeft(acc) {
          case (acc, (ident, list)) => {
            val transMaps = list.asInstanceOf[MapList].asScala
            transMaps.zipWithIndex.foreach { case (map, index) =>
              map put (transIndexKey, Integer.valueOf(index))
              if (map.containsKey(structureId)) {
                if (ident != map.get(structureId)) {
                  throw new WriteException(s"$ident at position $index has type ${map.get(structureId)} (wrong message list)")
                }
              } else map put (structureId, ident)
            }
            groupSends(transMaps, SchemaJavaValues.interchangeKey, acc)
          }
        }
      }
    }
    if (interchanges.isEmpty) throw new WriteException("no transactions to be sent")
    val interRoot = if (map.containsKey(interchangeKey)) getRequiredValueMap(interchangeKey, map) else new ValueMapImpl
    val groupRoot = getAsMap(groupKey, map)
    interchanges foreach {
      case (key, trans) => {
        val interProps = new ValueMapImpl(interRoot)
        key.foreach(interProps.putAll(_))
        val senderQual = getRequiredString(SENDER_ID_QUALIFIER, interProps)
        val senderId = getRequiredString(SENDER_ID, interProps)
        val receiverQual = getRequiredString(RECEIVER_ID_QUALIFIER, interProps)
        val receiverId = getRequiredString(RECEIVER_ID, interProps)
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
            val transGroups = trans.groupBy { transMap => getAsRequired[Structure](structureSchema, transMap).group.get }
            transGroups.foreach {
              case (groupCode, transList) => try {
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
                  val ident = getAsString(structureId, transet)
                  openSet(ident, setProps)
                  writeStructure(transet, getAsRequired[Structure](structureSchema, transet))
                  closeSet(setProps)
                } catch {
                  case e: Throwable => {
                    logAndThrow(s"${e.getMessage} in transaction ${getAsString(structureId, transet)} at index ${getAsInt(transIndexKey, transet)}", e)
                  }
                })
                closeGroup(groupProps)
              } catch {
                case e: Throwable => logAndThrow(s"group $groupCode ${e.getMessage}", e)
              }
            }
        }
        term(interProps)
      }
    }
  } catch {
    case e: WriteException => throw e
    case e: Throwable => logAndThrow("Writer error ", e)
  } finally {
    try { close } catch { case e: Throwable => }
  })
}