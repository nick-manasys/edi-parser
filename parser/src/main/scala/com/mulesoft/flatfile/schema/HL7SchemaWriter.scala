package com.mulesoft.flatfile.schema

import java.io.OutputStream
import java.nio.charset.Charset
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Date
import java.util.GregorianCalendar
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.util.Try
import com.mulesoft.flatfile.lexical.{ ErrorHandler, HL7Writer, TypeFormat, WriteException, WriterBase }
import com.mulesoft.flatfile.lexical.EdiConstants.ItemType._
import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition
import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition._
import EdiSchema.Structure

/** Configuration parameters for HL7 schema writer.
  */
case class HL7WriterConfig(val enforceRequires: Boolean, val subChar: Int, val charSet: Charset, val delims: String) {
  if (delims.size != 5) throw new IllegalArgumentException("delimiter string must be 5 characters")
}

/** Provider interface for control numbers. */
trait HL7NumberProvider {

  /** Get next send message control identifier.
    * @param sender
    * @param receiver
    */
  def nextMessage(sender: HL7Identity.HL7IdentityInformation, receiver: HL7Identity.HL7IdentityInformation): String
}

/** Writer for HL7 EDI documents.
  */
case class HL7SchemaWriter(out: OutputStream, struct: Structure, numprov: HL7NumberProvider, config: HL7WriterConfig)
  extends DelimiterSchemaWriter(new HL7Writer(out, config.charSet, config.delims, config.subChar), config.enforceRequires)
  with UtilityBase {

  import EdiSchema._
  import HL7Identity._
  import HL7SchemaDefs._
  import SchemaJavaValues._

  /** Lexical error handler. */
  case object HL7WriterErrorHandler extends ErrorHandler {
    // replace this with actual error accumlation
    def error(typ: TypeFormat, error: ErrorCondition, explain: java.lang.String): Unit = {
      error match {
        case WRONG_TYPE => throw new WriteException(explain)
        case _ =>
      }
    }
  }

  /** Typed writer, for access to format-specific conversions and support. */
  val writer = baseWriter.asInstanceOf[HL7Writer]

  /** Write top-level section of structure. */
  def writeTopSection(index: Int, map: ValueMap, seq: StructureSequence) = writeSection(map, seq.items)

  /** Output message header segment. */
  def init(props: ValueMap) = {
    props put (mshEncodingCharsKey, config.delims.drop(1))
    if (!props.containsKey(mshDateTimeKey)) {
      val format = new SimpleDateFormat("yyyyMMddHHmm")
      props put (mshDateTimeKey, format.format(new Date))
    }
    val sender = buildIdentityInformation(mshSendingApplication, mshSendingFacility, props)
    val receiver = buildIdentityInformation(mshReceivingApplication, mshReceivingFacility, props)
    props put (mshControlKey, numprov.nextMessage(sender, receiver))
    writer.init(props)
    writeCompList(props, DATA_ELEMENT, false, segMSH.components.drop(1))
    writer.writeSegmentTerminator
  }

  /** Output message trailer segment(s). */
  def term(props: ValueMap) = writer.term(props)

  /** Check if an envelope segment (handled directly, outside of structure). */
  def isEnvelopeSegment(segment: Segment) = HL7.isEnvelopeSegment(segment.ident)

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

  /** Write the output message. */
  def write(map: ValueMap) = Try(try {
    writer.setHandler(HL7WriterErrorHandler)
    val mshmap = getRequiredValueMap(mshKey, map)
    val msgType = getRequiredString(structureId, map)
    val datamap = getRequiredValueMap(dataKey, map)
    mshmap put (mshStructureKey, msgType)
    init(mshmap)
    struct.heading.foreach { seq => writeTopSection(0, datamap, seq) }
  } catch {
    case e: WriteException => throw e
    case e: Throwable => logAndThrow("Writer error ", e)
  } finally {
    try { close } catch { case e: Throwable => }
  })
}