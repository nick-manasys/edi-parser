package com.mulesoft.flatfile.schema

import java.io.OutputStream
import java.math.{ BigDecimal, BigInteger }
import java.nio.charset.Charset
import java.text.SimpleDateFormat
import java.util.{ Calendar, Collections, Date }
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.util.Try
import com.mulesoft.flatfile.lexical.{ FlatFileWriter, IBM037, WriteException }
import com.mulesoft.flatfile.lexical.EdiConstants.ItemType
import com.mulesoft.flatfile.lexical.EdiConstants.ItemType._
import EdiSchema._
import SchemaJavaValues._

/** Configuration parameters for flat file schema writer.
  */
case class FlatFileWriterConfig(val enforceRequires: Boolean, val charSet: Charset, val segTerm: String,
  val missChar: Char)

/** Base writer for flat file documents. */
abstract class FlatFileWriterBase(out: OutputStream, config: FlatFileWriterConfig)
  extends SchemaWriter(new FlatFileWriter(out, IBM037.replaceCharset(config.charSet), config.segTerm),
    config.enforceRequires) with UtilityBase {

  /** Typed writer, for access to format-specific conversions and support. */
  val writer = baseWriter.asInstanceOf[FlatFileWriter]

  def userValue(usage: Usage) = usage match {
    case UnusedUsage | IgnoredUsage => false
    case _                          => true
  }

  override def startSegment(segment: Segment) = Unit

  /** Write a value from map. */
  override def writeValue(map: ValueMap, typ: ItemType, skip: Boolean, comp: SegmentComponent): Unit = {

    def writeSimple(value: Any, ec: ElementComponent) = {
      val element = ec.element
      val format = element.typeFormat
      if (value != null) format.write(value, writer)
      else if (ec.value.isDefined) format.write(ec.value.get, writer)
      else writer.writeChar(format.maxLength, config.missChar)
    }

    def skipComponentList(comps: List[SegmentComponent]): Unit = comps foreach { comp =>
      comp match {
        case cc: CompositeComponent => skipComponentList(cc.composite.components)
        case ec: ElementComponent   => writeSimple(null, ec)
      }
    }

    comp match {
      case cc: CompositeComponent =>
        val comp = cc.composite
        if (userValue(cc.usage) && map.containsKey(cc.key)) {
          writeCompList(getAsMap(cc.key, map), typ.nextLevel, true, comp.components)
        } else {
          if (cc.usage == MandatoryUsage) logAndThrow(s"missing required map value '${cc.key}'")
          skipComponentList(cc.composite.components)
        }
      case ec: ElementComponent =>
        if (ec.usage == MandatoryUsage && !map.containsKey(ec.key)) logAndThrow(s"missing required value '${ec.key}'")
        val value = if (userValue(ec.usage)) map.get(ec.key) else null
        writeSimple(value, ec)
    }
  }

  /** Write top-level section of structure. */
  def writeTopSection(index: Int, map: ValueMap, seq: StructureSequence) = writeSection(map, seq.items)

  /** Check if an envelope segment (handled directly, outside of structure). */
  def isEnvelopeSegment(segment: Segment) = false

  /** Write the output message. */
  def write(map: ValueMap): Try[Unit]
}

/** Writer for structured flat file documents. */
class FlatFileStructureWriter(out: OutputStream, structure: Structure, config: FlatFileWriterConfig)
  extends FlatFileWriterBase(out, config) {

  /** Write the output message. */
  def write(map: ValueMap) = Try(try {
    val datamap = getRequiredValueMap(dataKey, map)
    structure.heading.foreach { seq => writeTopSection(0, datamap, seq) }
  } catch {
    case e: WriteException => throw e
    case e: Throwable =>
      logger error e
      throw new WriteException("Writer error: " + e.getMessage, e)
  } finally {
    try { close } catch { case e: Throwable => }
  })
}

/** Writer for single repeated segment documents. */
class FlatFileSegmentWriter(out: OutputStream, segment: Segment, config: FlatFileWriterConfig)
  extends FlatFileWriterBase(out, config) {

  /** Write the output message. */
  def write(map: ValueMap) = Try(try {
    val data = getRequiredMapList(dataKey, map)
    foreachMapInList(data, { map => writeSegment(map, segment) })
  } catch {
    case e: WriteException => throw e
    case e: Throwable =>
      logger error e
      throw new WriteException("Writer error: " + e.getMessage, e)
  } finally {
    try { close } catch { case e: Throwable => }
  })
}