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
import com.anypoint.df.edi.lexical.EdiConstants.{ DataType, ItemType }
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.EdiConstants.DataType._
import EdiSchema._

/**
 * Configuration parameters for flat file schema writer.
 */
case class FlatFileWriterConfig(val enforceRequires: Boolean, val charSet: Charset)

/**
 * Writer for flat file documents.
 */
case class FlatFileSchemaWriter(out: OutputStream, structure: Structure, config: FlatFileWriterConfig)
  extends SchemaWriter(new FlatFileWriter(out, config.charSet), config.enforceRequires) with UtilityBase {

  import EdiSchema._
  import HL7Identity._
  import HL7SchemaDefs._
  import SchemaJavaValues._

  /** Typed writer, for access to format-specific conversions and support. */
  val writer = baseWriter.asInstanceOf[FlatFileWriter]

  /** Write a value from map. */
  def writeValue(map: ValueMap, typ: ItemType, skip: Boolean, comp: SegmentComponent): Unit = {

    def writeSimple(value: Any, dtype: DataType, min: Int, max: Int) =
      if (value == null) writer.writeBlank(max)
      else dtype match {
        case ALPHA => writer.writeAlpha(value.asInstanceOf[String], max, max)
        case ALPHANUMERIC => writer.writeSpacePadded(value.asInstanceOf[String], max, max)
        case DATE => value match {
          case calendar: Calendar => writer.writeDate(calendar, max, max)
          case date: Date => writer.writeDate(date, min, max)
          case _ => throw new WriteException(s"Date value must be Date or Calendar instance, not ${value.getClass.getName}")
        }
        case INTEGER => writer.writeInt(value.asInstanceOf[Integer].intValue, min, max)
        case NUMBER | REAL | NUMERIC => value match {
          case bigdec: BigDecimal => writer.writeDecimal(bigdec, min, max)
          case integer: Integer => writer.writeInt(integer, min, max)
          case long: Long => writer.writeLong(long, min, max)
          case bigint: BigInteger => writer.writeBigInteger(bigint, min, max)
          case _ => throw new WriteException(s"Value type ${value.getClass.getName} is not compatible with expected type BigDecimal")
        }
        case TIME => writer.writeTime(value.asInstanceOf[Integer], min, max)
        case typ: DataType if (typ.isDecimal) =>
          writer.writeImplicitDecimal(value.asInstanceOf[BigDecimal], typ.decimalPlaces, min, max)
        case _ => throw new WriteException(s"Flat files do not support $dtype data type")
      }

    def skipComponentList(comps: List[SegmentComponent]): Unit = comps match {
      case h :: t => h match {
        case cc: CompositeComponent => skipComponentList(cc.composite.components)
        case ec: ElementComponent => writer.writeBlank(ec.element.maxLength)
      }
      case _ =>
    }

    comp match {
      case cc: CompositeComponent =>
        val comp = cc.composite
        if (comp.components.exists { ccc => map containsKey ccc.key }) {
          writeCompList(map, typ.nextLevel, true, comp.components)
        } else {
          if (cc.usage == MandatoryUsage) logAndThrow(s"missing required value '${cc.name}'")
          skipComponentList(cc.composite.components)
        }
      case ec: ElementComponent =>
        val elem = ec.element
        writeSimple(map.get(ec.key), elem.dataType, elem.minLength, elem.maxLength)
    }
  }

  /** Write top-level section of structure. */
  def writeTopSection(index: Int, map: ValueMap, seq: StructureSequence) = writeSection(map, seq.items)

  /** Check if an envelope segment (handled directly, outside of structure). */
  def isEnvelopeSegment(segment: Segment) = false

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
    writeStructure(map, structure)
  } catch {
    case e: WriteException => throw e
    case e: Throwable => logAndThrow("Writer error ", e)
  } finally {
    try { close } catch { case e: Throwable => }
  })
}