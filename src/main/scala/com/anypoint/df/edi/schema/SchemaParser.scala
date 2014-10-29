package com.anypoint.df.edi.schema

import java.io.IOException
import java.io.InputStream
import scala.util.Success
import scala.util.Try
import com.anypoint.df.edi.parser.EdiFactParser
import com.anypoint.df.edi.parser.ParserBase
import com.anypoint.df.edi.parser.ParserBase.ItemType
import com.anypoint.df.edi.parser.ParserBase.ItemType._
import com.anypoint.df.edi.parser.X12Parser
import com.anypoint.df.edi.schema.EdiSchema._
import scala.annotation.tailrec
import scala.xml.dtd.REQUIRED

/** Parse EDI document based on schema.
  *
  * @author MuleSoft, Inc.
  */

abstract class SchemaParser(val baseParser: ParserBase, val schema: EdiSchema) {

  type ValueMap = java.util.Map[String, Object]
  type ValueMapImpl = java.util.HashMap[String, Object]
  type MapList = java.util.List[ValueMap]
  type MapListImpl = java.util.ArrayList[ValueMap]
  type RealNumber = java.math.BigDecimal
  type IntegerNumber = Integer

  /** Initialize parser and read header segments. */
  protected def init(): ValueMap

  /** Parse a segment to a map of values. The base parser must be positioned following the segment tag when this is
    * called.
    */
  protected def parseSegment(segment: Segment): ValueMap = {

    /** Parse a value, adding it to map. */
    def parseValue(comp: SegmentComponent, map: ValueMap): Unit = {
      comp match {
        case ElementComponent(elem, name, use, count) => {
          elem.dataType match {
            case AlphaType => map put (name, baseParser.parseAlpha(elem.minLength, elem.maxLength))
            case AlphaNumericType | IdType => map put (name, baseParser.parseAlphaNumeric(elem.minLength, elem.maxLength))
            case BinaryType => throw new IOException("Handling not implemented for binary values")
            case DateType => map put (name, baseParser.parseDate(elem.minLength, elem.maxLength))
            case IntegerType => map put (name, baseParser.parseInteger(elem.minLength, elem.maxLength))
            case decimal: DecimalType => map put (name, baseParser.parseImpliedDecimalNumber(decimal.places, elem.minLength, elem.maxLength))
            case NumberType | RealType => map put (name, baseParser.parseNumber(elem.minLength, elem.maxLength))
            case TimeType => map put (name, Integer.valueOf(baseParser.parseTime(elem.minLength, elem.maxLength)))
          }
        }
        case CompositeComponent(comp, name, use, count) => {
          def parseCompInst(): ValueMap = {
            val compmap = new ValueMapImpl()
            parseCompList(comp.components, QUALIFIER, compmap)
            compmap
          }
          if (count > 1) {
            val complist = new MapListImpl()
            map put (name, complist)
            (1 until count) foreach { index =>
              complist add (parseCompInst())
            }
          } else map put (name, parseCompInst())
        }
      }
    }

    /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
    def parseCompList(comps: List[SegmentComponent], expect: ItemType, map: ValueMap) = {
      comps foreach { comp =>
        if (expect == baseParser.nextType()) parseValue(comp, map)
        else baseParser.nextType() match {
          case SEGMENT | END =>
            if (comp.usage == MandatoryUsage) throw new IOException(s"Missing required value '${comp.name}'")
          case _ => throw new IOException(s"Wrong separator type")
        }
      }
    }

    val map = new ValueMapImpl()
    parseCompList(segment.components.tail, DATA_ELEMENT, map)
    map
  }

  def checkSegment(segment: Segment) = baseParser.currentType == segment.ident

  // value keys for top-level transaction parse result map
  val transactionId = "id"
  val transactionName = "name"
  val transactionHeading = "heading"
  val transactionDetail = "detail"
  val transactionSummary = "summary"

  /** Parse a complete transaction. The returned map has a maximum of five values: the transaction id and name, and
    * separate child maps for each of the three sections of a transaction (heading, detail, and summary). Each child map
    * is keyed by segment name (with the ID suffixed in parenthesis) or group id. For a segment with no repeats allowed
    * the associated value is the map of the values in the segment. For a segment with repeats allowed the value is a
    * list of maps, one for each occurrence of the segment. For a group the value is also a list of maps, with each map
    * of the same form as the child maps of the top-level result (so keys are segment or nested group names, values are
    * maps or lists).
    */
  def parseTransaction(transaction: Transaction) = {

    /** Parse a (potentially) repeating segment into a list of maps. */
    def parseRepeatingSegment(segment: Segment, limit: Int): MapList = {
      val list: MapList = new MapListImpl()
      @tailrec
      def parseRepeat(): Unit =
        if (checkSegment(segment)) {
          if (limit > 0 && limit <= list.size) throw new IllegalStateException(s"too many repetitions of segment ${segment.ident}")
          list add (parseSegment(segment))
          parseRepeat()
        }
      parseRepeat()
      list
    }

    /** Parse a (potentially) repeating group into a list of maps. */
    def parseRepeatingGroup(group: GroupComponent): MapList = {
      val list: MapList = new MapListImpl()
      val lead = group.items match {
        case (ref: ReferenceComponent) :: t => ref.segment
        case _ => throw new IllegalStateException(s"first item in group ${group.ident} is not a segment")
      }
      @tailrec
      def parseRepeat(): Unit =
        if (checkSegment(lead)) {
          if (group.count > 0 && group.count <= list.size) throw new IllegalStateException(s"too many repetitions of group ${group.ident}")
          list add (parseSection(group.items))
          parseRepeat()
        }
      parseRepeat()
      list
    }

    /** Parse a portion of transaction data represented by a list of components (which may be segment references or
      * loops) into a map.
      */
    def parseSection(comps: List[TransactionComponent]) = {
      val values = new ValueMapImpl
      @tailrec
      def parseComponents(comps: List[TransactionComponent]): Unit = comps match {
        case (ref: ReferenceComponent) :: tail => {
          val segment = ref.segment
          if (checkSegment(segment)) values put (segment.name,
            if (ref.count > 0) parseRepeatingSegment(segment, ref.count) else parseSegment(segment))
          else if (ref.usage == MandatoryUsage) throw new IllegalStateException(s"missing required segment ${segment ident}")
          else parseComponents(tail)
        }
        case (group: GroupComponent) :: tail => {
          val repeats = parseRepeatingGroup(group)
          if (repeats.size() > 0) values put (group.ident, repeats)
          else if (group.usage == MandatoryUsage) throw new IllegalStateException(s"missing required loop ${group ident}")
        }
        case _ =>
      }
      parseComponents(comps)
      values
    }

    val topMap: ValueMap = new ValueMapImpl
    topMap put (transactionId, transaction.ident)
    topMap put (transactionName, transaction.name)
    topMap put (transactionHeading, parseSection(transaction.heading))
    topMap put (transactionDetail, parseSection(transaction.detail))
    topMap put (transactionSummary, parseSection(transaction.summary))
    topMap
  }

  /** Parse start of a functional group. */
  def openGroup(): ValueMap

  /** Check if at functional group close segment. */
  def isGroupClose(): Boolean

  /** Parse close of a functional group. */
  def closeGroup(props: ValueMap)

  /** Parse start of a transaction set. */
  def openSet(): (String, ValueMap)

  /** Check if at transaction set close segment. */
  def isSetClose(): Boolean

  /** Parse close of a transaction set. */
  def closeSet(props: ValueMap)

  /** Parse the input message. */
  def parse(): Try[ValueMap] = Try({
    val map = new ValueMapImpl
    val interchange = init
    map.put("interchange", interchange)
    val group = openGroup
    map.put("group", group)
    val set = openSet
    map.put("set", set._2)
    schema.transactions(set._1) match {
      case t: Transaction => map.put("transaction", parseTransaction(t))
      case _ => throw new IllegalStateException(s"unknown transaction type ${set._1}")
    }
    map
  })
}

object SchemaParser {

  /** Parser for EDIFACT messages. */
  /*  private class EdiFactSchemaParser(in: InputStream, sc: EdiSchema) extends SchemaParser(new EdiFactParser(in), sc) {
    def init() = {
      val params = baseParser.init(new ValueMapImpl())
      val second = baseParser.requireNextItem(SEGMENT)
      params
    }
    def parse() = {
      Success(Map.empty())
    }
  } */

  /** Factory function to create initialized parser instances. */
  def create(in: InputStream, schema: EdiSchema) = Try {
    val parser = schema ediForm match {
      case EdiFact => throw new IllegalArgumentException()
      case X12 => new X12SchemaParser(in, schema)
    }
    parser.init
    parser
  }
}