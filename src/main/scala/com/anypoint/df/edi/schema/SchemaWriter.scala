package com.anypoint.df.edi.schema

import java.math.BigDecimal
import java.math.BigInteger
import java.util.Date

import scala.collection.JavaConversions
import scala.util.Try

import com.anypoint.df.edi.lexical.WriteException
import com.anypoint.df.edi.lexical.WriterBase

import com.anypoint.df.edi.lexical.EdiConstants.ItemType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.LexerBase
import com.anypoint.df.edi.schema.EdiSchema._

/** Write EDI document based on schema. */

abstract class SchemaWriter(val writer: WriterBase, val schema: EdiSchema) extends SchemaJavaDefs {

  /** Initialize writer and output header segments. */
  protected def init(map: ValueMap): Unit

  /** Write a segment from a map of values. */
  protected def writeSegment(map: ValueMap, segment: Segment): Unit = {

    /** Write a value from map. */
    def writeValue(map: ValueMap, typ: ItemType, comp: SegmentComponent): Unit = {
      
      def writeSimple(value: Any, dtype: DataType, min: Int, max: Int) = dtype match {
        case AlphaType => writer.writeAlpha(value.asInstanceOf[String], min, max)
        case AlphaNumericType => writer.writeAlphaNumeric(value.asInstanceOf[String], min, max)
        case IdType => writer.writeId(value.asInstanceOf[String], min, max)
        case DateType => writer.writeDate(value.asInstanceOf[Date], min, max)
        case IntegerType => writer.writeInteger(value.asInstanceOf[BigInteger], min, max)
        case decimal: DecimalType =>
          writer.writeImplicitDecimal(value.asInstanceOf[BigDecimal], decimal.places, min, max)
        case NumberType | RealType => writer.writeDecimal(value.asInstanceOf[BigDecimal], min, max)
        case TimeType => writer.writeTime(value.asInstanceOf[Integer], min, max)
        case BinaryType => throw new WriteException("Handling not implemented for binary values")
      }
      
      def writeComponent(value: Object) = comp match {
        case ElementComponent(elem, name, use, count) =>
          writeSimple(value, elem.dataType, elem.minLength, elem.maxLength)
        case CompositeComponent(comp, name, use, count) =>
          writeCompList(value.asInstanceOf[ValueMap], QUALIFIER, comp.components)
      }

      if (map.containsKey(comp.name)) {
        val value = map.get(comp.name)
        if (comp.count > 1) {
          if (!value.isInstanceOf[SimpleList]) {
            throw new WriteException(s"expected list of values for property ${comp.name}")
          } else {
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
    def writeRepeatingGroup(list: MapList, group: GroupComponent): Unit = {
      val iter = JavaConversions.asScalaIterator(list.iterator)
      iter.foreach(map => writeSection(map, group.items))
    }
    
    def getKey(comp: TransactionComponent) = comp match {
      case ref: ReferenceComponent => ref.segment.name
      case group: GroupComponent => group.ident
    }

    /** Write a portion of transaction data represented by a list of components (which may be segment references or
      * loops) into a map.
      */
    def writeSection(map: ValueMap, comps: List[TransactionComponent]): Unit = comps.foreach(comp => {
      val key = getKey(comp)
      if (map.containsKey(key)) {
        val value = map.get(key)
        comp match {
        case ref: ReferenceComponent =>
          if (ref.count != 1) writeRepeatingSegment(value.asInstanceOf[MapList], ref.segment, ref.count)
          else writeSegment(value.asInstanceOf[ValueMap], ref.segment)
        case group: GroupComponent => 
          if (group.count != 1) writeRepeatingGroup(value.asInstanceOf[MapList], group)
          else writeSection(value.asInstanceOf[ValueMap], group.items)
        }
      } else comp.usage match {
        case MandatoryUsage => throw new WriteException(s"missing required value '$key'")
        case _ =>
      }
    })
  }

  /** Write start of a functional group. */
  def openGroup(props: ValueMap): Unit

  /** Write close of a functional group. */
  def closeGroup(props: ValueMap): Unit

  /** Write start of a transaction set. */
  def openSet(name: String, props: ValueMap): Unit

  /** Write close of a transaction set. */
  def closeSet(props: ValueMap): Unit

  /** Write the output message. */
  def write(map: ValueMap): Try[Unit] = Try({
    init(map.get("interchange").asInstanceOf[ValueMap])
  })
}