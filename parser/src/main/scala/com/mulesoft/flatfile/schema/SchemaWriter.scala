package com.mulesoft.flatfile.schema

import java.{ lang => jl, math => jm, util => ju }

import scala.annotation.tailrec
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.util.Try

import org.apache.log4j.Logger

import com.mulesoft.flatfile.lexical.{ DelimiterWriter, WriteException, WriterBase }
import com.mulesoft.flatfile.lexical.EdiConstants.ItemType
import com.mulesoft.flatfile.lexical.EdiConstants.ItemType._
import com.mulesoft.flatfile.schema.EdiSchema._

import SchemaJavaValues._

import javax.xml.datatype.XMLGregorianCalendar

/** Write document based on schema. */
abstract class SchemaWriter(val baseWriter: WriterBase, val enforceRequireds: Boolean) extends SchemaJavaDefs {

  import SchemaJavaValues._

  val logger = Logger.getLogger(getClass.getName)

  /** Log error and throw as WriteException if enforcing restrictions.
    */
  def logAndThrow(text: String) =
    if (enforceRequireds) {
      val e = new WriteException(text)
      logger error e
      throw e
    } else logger error text

  /** Log error and throw as WriteException if enforcing restrictions. If the cause is already a WriteException this
    * appends the text to the existing exception text and throws a new WriteException.
    */
  def logAndThrow(text: String, cause: Throwable) = {
    logger error (text, cause)
    if (enforceRequireds) cause match {
      case e: WriteException => throw new WriteException(e.getMessage + ' ' + text)
      case e: Throwable => throw new WriteException(text, e)
    }
  }

  /** Pad a string with leading zeroes to specified length. */
  def zeroPad(text: String, length: Int) = {
    @tailrec
    def zeroPadr(t: String): String =
      if (t.length < length) zeroPadr("0" + t) else t
    zeroPadr(text)
  }

  /** Map from interchange and/or group values to list of maps using those values. */
  type SendMap = Map[Option[ValueMap], List[ValueMap]]

  val EmptySendMap = Map[Option[ValueMap], List[ValueMap]]()

  /** Group maps of data to be sent based on equality of an envelope map, which may or may not be present.
    * @param sends send data maps
    * @param key sort map name
    * @param groups accumulated map to lists of send data maps
    */
  def groupSends(sends: Iterable[ValueMap], key: String, groups: SendMap) =
    sends.foldLeft(groups)((acc, map) => {
      val value = if (map.containsKey(key)) Some(getAsMap(key, map)) else None
      acc.get(value) match {
        case Some(list) => acc + (value -> (map :: list))
        case None => acc + (value -> (map :: Nil))
      }
    })

  /** Write a value. */
  def writeValue(map: ValueMap, typ: ItemType, skip: Boolean, comp: SegmentComponent): Unit
  
  /** Start writing a segment. */
  def startSegment(segment: Segment): Unit

  /** Write a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  @tailrec
  final def writeCompList(map: ValueMap, typ: ItemType, skip: Boolean, comps: List[SegmentComponent]): Unit = {
    comps match {
      case h :: t => {
        try {
          writeValue(map, typ, skip, h)
        } catch {
          case e: WriteException => throw e
          case e: Exception =>
            e.printStackTrace
            throw new WriteException(s"${e.getMessage} on component ${h.key}")
        }
        writeCompList(map, typ, false, t)
      }
      case _ =>
    }
  }

  /** Write a segment from a map of values. */
  def writeSegment(map: ValueMap, segment: Segment): Unit = {
    startSegment(segment)
    try {
      writeCompList(map, DATA_ELEMENT, false, segment.components)
    } catch {
      case e: Exception =>
        throw new WriteException(s"${e.getMessage} of segment ${segment.ident} at position ${baseWriter.getSegmentCount}", e)
    }
    baseWriter.writeSegmentTerminator
  }

  /** Write a portion of structure data represented by a list of components (which may be segment references or
    * loops) from a map.
    */
  def writeSection(map: ValueMap, comps: List[StructureComponent]): Unit = comps.foreach(comp => {

    /** Write a (potentially) repeating segment from a list of maps. */
    def writeRepeatingSegment(list: MapList, segment: Segment): Unit = {
      val iter = list.iterator
      while (iter.hasNext) writeSegment(iter.next, segment)
    }

    /** Write a (potentially) repeating group from a list of maps. */
    def writeRepeatingGroup(list: MapList, group: GroupBase): Unit = {
      val iter = list.iterator
      while (iter.hasNext) writeSection(iter.next, group.seq.items)
    }

    def checkMissing = comp.usage match {
      case MandatoryUsage => logAndThrow(s"missing required value '${comp.key}'")
      case _ =>
    }
    def writeGroup(key: String, group: GroupBase) =
      if (group.count == 1) writeSection(getRequiredValueMap(key, map), group.seq.items)
      else {
        val list = getRequiredMapList(key, map)
        if (group.count > 0 && list.size > group.count) logAndThrow(s"too many values present for group/loop ${group.key} (maximum ${group.count})")
        writeRepeatingGroup(list, group)
      }

    val key = comp.key
    comp match {
      case ref: ReferenceComponent =>
        if (!isEnvelopeSegment(ref.segment)) {
          if (map.containsKey(key)) {
            if (ref.count != 1) {
              val list = getRequiredMapList(key, map)
              if (list.isEmpty) ref.usage match {
                case MandatoryUsage => logAndThrow(s"no values present for segment ${ref.key}")
                case _ =>
              }
              else {
                if (ref.count > 0 && list.size > ref.count) logAndThrow(s"too many values present for segment ${ref.key} (maximum ${ref.count})")
                writeRepeatingSegment(list, ref.segment)
              }
            } else writeSegment(getRequiredValueMap(key, map), ref.segment)
          } else checkMissing
        }
      case wrap: LoopWrapperComponent =>
        if (map.containsKey(key)) {
          val idmap = new ValueMapImpl
          idmap put (wrap.open.components(0).key, wrap.groupId)
          idmap put (wrap.close.components(0).key, wrap.groupId)
          writeSegment(idmap, wrap.open)
          writeGroup(key, wrap.wrapped)
          writeSegment(idmap, wrap.close)
        }
      case group: GroupComponent =>
        var variant = false
        group.variants.foreach { gv =>
          if (map.containsKey(gv.key)) {
            variant = true
            writeGroup(gv.key, gv)
          }
        }
        if (map.containsKey(key)) writeGroup(key, group)
        else if (!variant) checkMissing
    }
  })

  /** Write top-level section of structure. */
  def writeTopSection(index: Int, map: ValueMap, seq: StructureSequence): Unit

  /** Write a complete structure. The supplied map has a maximum of five values: the structure id and name, and
    * separate child maps for each of the three sections of a structure (heading, detail, and summary). Each child map
    * is keyed by segment name (with the ID suffixed in parenthesis) or group id. For a segment with no repeats allowed
    * the associated value is the map of the values in the segment. For a segment with repeats allowed the value is a
    * list of maps, one for each occurrence of the segment. For a group the value is also a list of maps, with each map
    * of the same form as the child maps of the top-level result (so keys are segment or nested group names, values are
    * maps or lists).
    */
  def writeStructure(map: ValueMap, structure: Structure) {
    structure.heading.foreach { seq => writeTopSection(0, getRequiredValueMap(structureHeading, map), seq) }
    structure.detail.foreach { seq => writeTopSection(0, getRequiredValueMap(structureDetail, map), seq) }
    structure.summary.foreach { seq => writeTopSection(0, getRequiredValueMap(structureSummary, map), seq) }
  }

  /** Check if an envelope segment (handled directly, outside of structure). */
  def isEnvelopeSegment(segment: Segment): Boolean

  /** Close output, intended for testing rather than application. */
  def close = baseWriter.close

  /** Write the output message. */
  def write(map: ValueMap): Try[Unit]
}

/** Writer for delimited EDI formats using schemas. */
abstract class DelimiterSchemaWriter(val delimWriter: DelimiterWriter, enforceRequireds: Boolean)
  extends SchemaWriter(delimWriter, enforceRequireds) {
  import com.mulesoft.flatfile.lexical.EdiConstants.ItemType
  import com.mulesoft.flatfile.lexical.EdiConstants.ItemType._

  /** Write a value from map. */
  override def writeValue(map: ValueMap, typ: ItemType, skip: Boolean, comp: SegmentComponent): Unit = {

    /** Write a simple value. The type formats used with EDI delimited formats are not compatible with the spire numeric
     * values used by DataWeave, so this converts them before calling the format.
     */
    def writeSimple(value: Any, element: Element) = {
      value match {
        case n: spire.math.Number =>
          val numval = if (n.canBeInt) n.toInt
            else if (n.canBeLong) jl.Long.valueOf(n.toLong)
            else if (n.isWhole) n.toBigInt.bigInteger
            else n.toBigDecimal.bigDecimal
          element.typeFormat.write(numval, delimWriter)
        case _ => element.typeFormat.write(value, delimWriter)
      }
    }
    
    def writeSeparator(repeat: Boolean) =
      if (repeat) delimWriter.writeRepetitionSeparator
      else if (!skip) typ match {
        case SEGMENT => delimWriter.writeSegmentTerminator
        case DATA_ELEMENT => delimWriter.writeDataSeparator
        case COMPONENT => delimWriter.writeComponentSeparator
        case SUB_COMPONENT => delimWriter.writeSubcomponentSeparator
      }

    def writeComponent(value: Object, repeat: Boolean) = {
      writeSeparator(repeat)
      comp match {
        case ElementComponent(elem, _, _, _, _, _, _, _) =>
          writeSimple(value, elem)
        case CompositeComponent(composite, _, _, _, _, _) =>
          writeCompList(value.asInstanceOf[ValueMap], typ.nextLevel, true, composite.components)
      }
    }

    def skipAtLevel = {
      if (!skip) typ match {
      case SEGMENT => delimWriter.writeSegmentTerminator
      case DATA_ELEMENT => delimWriter.skipElement
      case COMPONENT => delimWriter.skipComponent
      case SUB_COMPONENT => delimWriter.skipSubcomponent
      case REPETITION =>
    }}

    def compCheckr(comp: CompositeComponent): Boolean =
      comp.composite.components.exists { ccc =>
        map.containsKey(ccc.key) || (ccc match {
          case cc: CompositeComponent => compCheckr(cc)
          case ec: ElementComponent => ec.value.isDefined
        })
      }

    comp match {
      case cc: CompositeComponent if (cc.count == 1) =>
        if (compCheckr(cc)) {
          writeComponent(map, false)
        } else {
          if (cc.usage == MandatoryUsage) logAndThrow(s"missing required value '${cc.name}'")
          skipAtLevel
        }
      case _ =>
        if (map.containsKey(comp.key)) {
          val value = map.get(comp.key)
          if (value == null) throw new WriteException(s"Value cannot be null for key ${comp.key}")
          if (comp.count != 1) {
            value match {
              case list: SimpleList =>
                if (list.isEmpty) {
                  if (comp.usage == MandatoryUsage) logAndThrow(s"no values present for property ${comp.name}")
                } else {
                  if (comp.count > 0 && list.size > comp.count) logAndThrow(s"too many values present for repeated component ${comp.key} (maximum ${comp.count})")
                  val iter = list.iterator
                  writeComponent(iter.next, false)
                  while (iter.hasNext) writeComponent(iter.next, true)
                }
              case _ => throw new WriteException(s"expected list of values for property ${comp.name}")
            }
          } else writeComponent(value, false)
        } else {
          if (comp.usage == MandatoryUsage) logAndThrow(s"missing required value '${comp.name}'")
          comp match {
            case cc: CompositeComponent => skipAtLevel
            case ec: ElementComponent =>
              if (ec.value.isDefined) {
                writeSeparator(false)
                writeSimple(ec.value.get, ec.element)
              } else skipAtLevel
          }
        }
    }
  }
  
  override def startSegment(segment: Segment) = delimWriter.writeSegmentTag(segment.ident)

}