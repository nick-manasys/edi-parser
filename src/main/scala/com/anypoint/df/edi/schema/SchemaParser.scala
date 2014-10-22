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

/** Parse EDI document based on schema.
  *
  * @author MuleSoft, Inc.
  */

sealed abstract class SchemaParser(val baseParser: ParserBase, val schema: EdiSchema) {

  type ValueMap = java.util.Map[String, Any]
  type ValueMapImpl = java.util.HashMap[String, Any]
  type MapList = java.util.List[ValueMap]
  type MapListImpl = java.util.ArrayList[ValueMap]
  type RealNumber = java.math.BigDecimal
  type IntegerNumber = Integer

  val realSet = RealDataTypes.toSet
  val integerSet = IntegerDataTypes.toSet
  val stringSet = StringDataTypes.toSet

  /** Initialize parser and read header segments. */
  protected def init(): Unit

  /** Parse a segment to a map of values. The base parser must be positioned following the segment tag when this is
    * called.
    */
  protected def parseSegment(segment: Segment): ValueMap = {

    /** Parse a value, adding it to map. */
    def parseValue(comp: SegmentComponent, map: ValueMap): Unit = comp match {
      case ElementComponent(id, name, use, count) => {
        val elem = schema.elementsByName(id)
        elem.dataType match {
          case AlphaType => map put (id, baseParser.parseAlpha())
          case AlphaNumericType | IdType => map put (id, baseParser.parseAlphanumeric())
          case BinaryType => throw new IOException("Handling not implemented for binary values")
          case DateType => map put (id, baseParser.parseDate())
          case IntegerType => map put (id, baseParser.parseInteger(elem.minLength, elem.maxLength))
          case x: DecimalType => map put (id, baseParser.parseImpliedDecimalNumber(x.places, elem.minLength, elem.maxLength))
          case NumberType | RealType => map put (id, baseParser.parseNumber(elem.minLength, elem.maxLength))
          case TimeType => map put (id, baseParser.parseTime())
        }
      }
      case CompositeComponent(id, name, use, count) => {
        val comp = schema.compositesByName(id)
        def parseCompInst(): ValueMap = {
          val compmap = new ValueMapImpl()
          parseCompList(comp.components, QUALIFIER, compmap)
          compmap
        }
        if (count > 1) {
          val complist = new MapListImpl()
          map put (id, complist)
          (1 until count) foreach { index =>
            complist add (parseCompInst())
          }
        } else map put (id, parseCompInst())
      }
    }

    /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite. */
    def parseCompList(comps: List[SegmentComponent], expect: ItemType, map: ValueMap) = {
      comps foreach { comp =>
        if (expect == baseParser.nextType()) parseValue(comp, map)
        else baseParser.nextType() match {
          case SEGMENT | END =>
            if (comp.usage == MandatoryUsage) throw new IOException(s"Missing required value '${comp.ident}'")
          case QUALIFIER | DATA_ELEMENT => throw new IOException(s"Wrong separator type")
        }
      }
    }

    val map = new ValueMapImpl()
    parseCompList(segment.components.tail, DATA_ELEMENT, map)
    map
  }

  /** Parse the input message. */
  def parse(): Try[MapList]
}

object SchemaParser {

  /** Parser for EDIFACT messages. */
  private class EdiFactSchemaParser(in: InputStream, sc: EdiSchema) extends SchemaParser(new EdiFactParser(in), sc) {
    def init() = {
      val params = baseParser.init()
      val second = baseParser.requireNextItem(SEGMENT)
    }
    def parse() = {
      Success(Map.empty())
    }
  }

  /** Parser for X.12 messages. */
  private class X12SchemaParser(in: InputStream, sc: EdiSchema) extends SchemaParser(new X12Parser(in), sc) {
    def init() = {
      val params = baseParser.init()
      baseParser.requireNextItem(SEGMENT) match {
        case "GS" =>
        case "IEA" =>
        case x => throw new IOException(s"Illegal segment '$x'")
      }
    }
    def parse() = {
      Success(Map.empty())
    }
  }

  /** Factory function to create initialized parser instances. */
  def create(in: InputStream, schema: EdiSchema) = Try {
    val parser = schema ediForm match {
      case EdiFact => new EdiFactSchemaParser(in, schema)
      case X12 => new X12SchemaParser(in, schema)
    }
    parser.init
    parser
  }
}