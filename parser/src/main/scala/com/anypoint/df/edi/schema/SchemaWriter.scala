package com.anypoint.df.edi.schema

import java.io.OutputStream
import java.math.BigDecimal
import java.util.Calendar

import scala.collection.JavaConversions
import scala.util.Try

import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.EdiConstants.DataType._
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.WriteException
import com.anypoint.df.edi.lexical.WriterBase
import com.anypoint.df.edi.schema.EdiSchema._

/** Write EDI document based on schema. */

abstract class SchemaWriter(val writer: WriterBase, val schema: EdiSchema) extends SchemaJavaDefs {

  import SchemaJavaValues._

  /** Initialize writer and output interchange header segment(s). */
  def init(delims: String, encoding: String, props: ValueMap): Unit

  /** Output interchange trailer segment(s) and finish with stream. */
  def term(props: ValueMap): Unit

  /** Write a segment from a map of values. */
  protected def writeSegment(map: ValueMap, segment: Segment): Unit = {

    /** Write a value from map. */
    def writeValue(map: ValueMap, typ: ItemType, comp: SegmentComponent): Unit = {

      def writeSimple(value: Any, dtype: DataType, min: Int, max: Int) = dtype match {
        case ALPHA => writer.writeAlpha(value.asInstanceOf[String], min, max)
        case ALPHANUMERIC => writer.writeAlphaNumeric(value.asInstanceOf[String], min, max)
        case ID => writer.writeId(value.asInstanceOf[String], min, max)
        case DATE => writer.writeDate(value.asInstanceOf[Calendar], min, max)
        case INTEGER => writer.writeInt(value.asInstanceOf[Integer].intValue(), min, max)
        case NUMBER | REAL => writer.writeDecimal(value.asInstanceOf[BigDecimal], min, max)
        case TIME => writer.writeTime(value.asInstanceOf[Integer], min, max)
        case BINARY => throw new WriteException("Handling not implemented for binary values")
        case typ: DataType if (typ.isDecimal()) =>
          writer.writeImplicitDecimal(value.asInstanceOf[BigDecimal], typ.decimalPlaces, min, max)
      }

      def writeComponent(value: Object) = {
        typ match {
          case SEGMENT => writer.writeSegmentTerminator
          case DATA_ELEMENT => writer.writeDataSeparator
          case QUALIFIER => writer.writeSubDelimiter
          case REPETITION => writer.writeRepetitionSeparator
        }
        comp match {
          case ElementComponent(elem, _, _, _, _, _) =>
            writeSimple(value, elem.dataType, elem.minLength, elem.maxLength)
          case CompositeComponent(composite, _, _, _, _, _) =>
            writeCompList(value.asInstanceOf[ValueMap], QUALIFIER, composite.components)
        }
      }

      if (map.containsKey(comp.key)) {
        val value = map.get(comp.key)
        if (comp.count > 1) {
          if (!value.isInstanceOf[SimpleList]) throw new WriteException(s"expected list of values for property ${comp.name}")
          else {
            val iter = JavaConversions.asScalaIterator(value.asInstanceOf[SimpleList].iterator)
            iter.foreach(value => writeComponent(value))
          }
        } else writeComponent(value)
      } else comp.usage match {
        case MandatoryUsage => throw new WriteException(s"missing required value '${comp.name}'")
        case _ => typ match {
          case SEGMENT => writer.writeSegmentTerminator
          case DATA_ELEMENT => writer.skipElement
          case QUALIFIER => writer.skipSubElement
          case REPETITION =>
        }
      }
    }

    /** Write a list of components (which may be the segment itself, a repeated set of values, or a composite). */
    def writeCompList(map: ValueMap, typ: ItemType, comps: List[SegmentComponent]) =
      comps foreach { comp => writeValue(map, typ, comp) }

    writer.writeToken(segment.ident)
    writeCompList(map, DATA_ELEMENT, segment.components)
    writer.writeSegmentTerminator
  }

  /** Write a complete transaction. The supplied map has a maximum of five values: the transaction id and name, and
    * separate child maps for each of the three sections of a transaction (heading, detail, and summary). Each child map
    * is keyed by segment name (with the ID suffixed in parenthesis) or group id. For a segment with no repeats allowed
    * the associated value is the map of the values in the segment. For a segment with repeats allowed the value is a
    * list of maps, one for each occurrence of the segment. For a group the value is also a list of maps, with each map
    * of the same form as the child maps of the top-level result (so keys are segment or nested group names, values are
    * maps or lists).
    */
  def writeTransaction(map: ValueMap, transaction: Transaction) = {

    /** Write a (potentially) repeating segment from a list of maps. */
    def writeRepeatingSegment(list: MapList, segment: Segment, limit: Int): Unit = {
      val iter = JavaConversions.asScalaIterator(list.iterator)
      iter.foreach(map => writeSegment(map, segment))
    }

    /** Write a (potentially) repeating group from a list of maps. */
    def writeRepeatingGroup(list: MapList, group: GroupBase): Unit = {
      val iter = JavaConversions.asScalaIterator(list.iterator)
      iter.foreach(map => writeSection(map, group.items))
    }

    /** Write a portion of transaction data represented by a list of components (which may be segment references or
      * loops) from a map.
      */
    def writeSection(map: ValueMap, comps: List[TransactionComponent]): Unit = comps.foreach(comp => {
      def checkMissing() = comp.usage match {
        case MandatoryUsage => throw new WriteException(s"missing required value '${comp.key}'")
        case _ =>
      }
      val key = comp.key
      comp match {
        case ref: ReferenceComponent =>
          if (!isEnvelopeSegment(ref.segment)) {
            if (map.containsKey(key)) {
              val value = map.get(key)
              if (ref.count != 1) writeRepeatingSegment(getRequiredMapList(key, map), ref.segment, ref.count)
              else writeSegment(getRequiredValueMap(key, map), ref.segment)
            } else checkMissing()
          }
        case wrap: LoopWrapperComponent =>
          if (map.containsKey(key)) {
            val idmap = new ValueMapImpl
            idmap put (wrap.open.components.head.key, wrap.ident)
            idmap put (wrap.close.components.head.key, wrap.ident)
            writeSegment(idmap, wrap.open)
            writeSection(getRequiredValueMap(key, map), wrap.loopGroup :: Nil)
            writeSegment(idmap, wrap.close)
          }
        case group: GroupComponent =>
          var variant = false
          group.variants.foreach { gv => if (map.containsKey(gv.key)) {
            variant = true
            if (gv.count > 1) writeRepeatingGroup(getRequiredMapList(key, map), group)
            else writeSection(getRequiredValueMap(gv.key, map), gv.items)
          }}
          if (map.containsKey(key)) writeRepeatingGroup(getRequiredMapList(key, map), group)
          else if (!variant) checkMissing()
      }
    })

    if (!transaction.heading.isEmpty) writeSection(
      getRequiredValueMap(transactionHeading, map), transaction.heading)
    if (!transaction.detail.isEmpty) writeSection(
      getRequiredValueMap(transactionDetail, map), transaction.detail)
    if (!transaction.summary.isEmpty) writeSection(
      getRequiredValueMap(transactionSummary, map), transaction.summary)
  }

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment): Boolean

  /** Write the output message. */
  def write(map: ValueMap): Try[Unit]
}

trait NumberProvider {
  def nextInterchange: Int
}

object SchemaWriter {

  /** Factory function to create writer instances. */
  def create(out: OutputStream, schema: EdiSchema, numprov: NumberProvider) = Try {
    schema ediForm match {
      case EdiFact => throw new IllegalArgumentException()
      case X12 => new X12SchemaWriter(out, schema, numprov)
    }
  }
}