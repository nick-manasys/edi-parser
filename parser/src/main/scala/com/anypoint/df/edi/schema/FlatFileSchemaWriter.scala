package com.anypoint.df.edi.schema

import java.io.OutputStream
import java.math.{ BigDecimal, BigInteger }
import java.nio.charset.Charset
import java.text.SimpleDateFormat
import java.util.{ Calendar, Collections, Date }
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.util.Try
import com.anypoint.df.edi.lexical.{ FlatFileWriter, WriteException }
import com.anypoint.df.edi.lexical.EdiConstants.ItemType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import EdiSchema._
import SchemaJavaValues._

/**
 * Configuration parameters for flat file schema writer.
 */
case class FlatFileWriterConfig(val enforceRequires: Boolean, val charSet: Charset)

/** Base writer for flat file documents. */
abstract class FlatFileWriterBase(out: OutputStream, config: FlatFileWriterConfig)
  extends SchemaWriter(new FlatFileWriter(out, config.charSet), config.enforceRequires) with UtilityBase {

  /** Typed writer, for access to format-specific conversions and support. */
  val writer = baseWriter.asInstanceOf[FlatFileWriter]
  
  def userValue(usage: Usage) = usage match {
    case UnusedUsage | IgnoredUsage => false
    case _ => true
  }

  /** Write a value from map. */
  def writeValue(map: ValueMap, typ: ItemType, skip: Boolean, comp: SegmentComponent): Unit = {

    def writeSimple(value: Any, element: Element) =
      if (value == null) writer.writeBlank(element.typeFormat.maxLength)
      else element.typeFormat.write(value, writer)

    def skipComponentList(comps: List[SegmentComponent]): Unit = comps match {
      case h :: t => h match {
        case cc: CompositeComponent => skipComponentList(cc.composite.components)
        case ec: ElementComponent => writer.writeBlank(ec.element.typeFormat.maxLength)
      }
      case _ =>
    }

    comp match {
      case cc: CompositeComponent =>
        val comp = cc.composite
        if (userValue(cc.usage) && comp.components.exists { ccc => map containsKey ccc.key }) {
          writeCompList(map, typ.nextLevel, true, comp.components)
        } else {
          if (cc.usage == MandatoryUsage) logAndThrow(s"missing required value '${cc.name}'")
          skipComponentList(cc.composite.components)
        }
      case ec: ElementComponent =>
        val elem = ec.element
        val value = if (userValue(ec.usage)) map.get(ec.key) else null
        writeSimple(value, elem)
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
    writer.setTagField(structure.tagStart.get)
    val datamap = getRequiredValueMap(dataKey, map)
    structure.heading.foreach { seq => writeTopSection(0, datamap, seq) }
  } catch {
    case e: WriteException => throw e
    case e: Throwable => logAndThrow("Writer error ", e)
  } finally {
    try { close } catch { case e: Throwable => }
  })
}

/** Writer for single repeated segment documents. */
class FlatFileSegmentWriter(out: OutputStream, segment: Segment, config: FlatFileWriterConfig)
  extends FlatFileWriterBase(out, config) {

  /** Write the output message. */
  def write(map: ValueMap) = Try(try {
    writer.setTagField(-1)
    val data = getRequiredMapList(dataKey, map)
    foreachMapInList(data, { map => writeSegment(map, segment) })
  } catch {
    case e: WriteException => throw e
    case e: Throwable => logAndThrow("Writer error ", e)
  } finally {
    try { close } catch { case e: Throwable => }
  })
}