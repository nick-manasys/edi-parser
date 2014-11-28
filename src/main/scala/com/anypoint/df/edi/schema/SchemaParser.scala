package com.anypoint.df.edi.schema

import java.io.IOException
import java.io.InputStream

import scala.annotation.tailrec
import scala.util.Try

import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.EdiConstants.DataType._
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.LexerBase
import com.anypoint.df.edi.schema.EdiSchema._

/** Parse EDI document based on schema. */

abstract class SchemaParser(val lexer: LexerBase, val schema: EdiSchema) extends SchemaJavaDefs {

  import SchemaJavaValues._

  /** Initialize parser and read header segments. */
  protected def init(): ValueMap

  /** Read interchange trailer segment(s) and finish with stream. */
  protected def term(props: ValueMap): Unit

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  protected def parseSegment(segment: Segment): ValueMap = {

    /** Parse a value, adding it to map. */
    def parseValue(comp: SegmentComponent, map: ValueMap): Unit = {
      comp match {
        case ElementComponent(elem, name, pos, use, count) => {
          elem.dataType match {
            case ALPHA => map put (comp.key, lexer.parseAlpha(elem.minLength, elem.maxLength))
            case ALPHANUMERIC | ID => map put (comp.key, lexer.parseAlphaNumeric(elem.minLength, elem.maxLength))
            case BINARY => throw new IOException("Handling not implemented for binary values")
            case DATE => map put (comp.key, lexer.parseDate(elem.minLength, elem.maxLength))
            case INTEGER => map put (comp.key, lexer.parseInteger(elem.minLength, elem.maxLength))
            case NUMBER | REAL => map put (comp.key, lexer.parseNumber(elem.minLength, elem.maxLength))
            case TIME => map put (comp.key, Integer.valueOf(lexer.parseTime(elem.minLength, elem.maxLength)))
            case typ: DataType if (typ.isDecimal()) =>
              map put (comp.key, lexer.parseImpliedDecimalNumber(typ.decimalPlaces, elem.minLength, elem.maxLength))
          }
        }
        case CompositeComponent(composite, name, pos, use, count) => {
          def parseCompInst(): ValueMap = {
            val compmap = new ValueMapImpl()
            parseCompList(composite.components, QUALIFIER, compmap)
            compmap
          }
          if (count > 1) {
            val complist = new MapListImpl()
            map put (comp.key, complist)
            (1 until count) foreach { index =>
              complist add (parseCompInst())
            }
          } else map put (comp.key, parseCompInst())
        }
      }
    }

    /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
    def parseCompList(comps: List[SegmentComponent], expect: ItemType, map: ValueMap) = {
      comps foreach { comp =>
        if (expect == lexer.currentType) {
          if (lexer.token.length > 0) parseValue(comp, map)
          else if (comp.usage == MandatoryUsage) throw new IOException(s"missing required value '${comp.name}'")
          else lexer.advance
        } else lexer.currentType match {
          case SEGMENT | END =>
            if (comp.usage == MandatoryUsage) throw new IOException(s"missing required value '${comp.name}'")
          case _ => throw new IOException("wrong separator type")
        }
      }
    }

    val map = new ValueMapImpl()
    lexer.advance
    parseCompList(segment.components, DATA_ELEMENT, map)
    lexer.currentType match {
      case SEGMENT | END =>
      case _ => throw new IOException("too many values in segment")
    }
    map
  }

  def checkSegment(segment: Segment) = lexer.currentType == SEGMENT && lexer.token == segment.ident

  /** Parse a complete transaction body (not including any envelope segments). The returned map has a maximum of five
    * values: the transaction id and name, and separate child maps for each of the three sections of a transaction
    * (heading, detail, and summary). Each child map is keyed by segment name (with the ID suffixed in parenthesis) or
    * group id. For a segment with no repeats allowed the associated value is the map of the values in the segment. For
    * a segment with repeats allowed the value is a list of maps, one for each occurrence of the segment. For a group
    * the value is also a list of maps, with each map of the same form as the child maps of the top-level result (so
    * keys are segment or nested group names, values are maps or lists).
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
          if (!isEnvelopeSegment(segment)) {
            if (checkSegment(segment)) values put (segment.name,
              if (ref.count != 1) parseRepeatingSegment(segment, ref.count) else parseSegment(segment))
            else if (ref.usage == MandatoryUsage) throw new IllegalStateException(s"missing required segment ${segment ident}")
          }
          parseComponents(tail)
        }
        case (group: GroupComponent) :: tail => {
          val repeats = parseRepeatingGroup(group)
          if (repeats.size() > 0) values put (group.ident, repeats)
          else if (group.usage == MandatoryUsage) throw new IllegalStateException(s"missing required loop ${group ident}")
          parseComponents(tail)
        }
        case Nil =>
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

  /** Check if at functional group open segment. */
  def isGroupOpen(): Boolean

  /** Parse start of a functional group. */
  def openGroup(): ValueMap

  /** Check if at functional group close segment. */
  def isGroupClose(): Boolean

  /** Parse close of a functional group. */
  def closeGroup(props: ValueMap): Unit

  /** Check if at transaction set start segment. */
  def isSetOpen(): Boolean

  /** Parse start of a transaction set. */
  def openSet(): (String, ValueMap)

  /** Parse close of a transaction set. */
  def closeSet(props: ValueMap): Unit

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment): Boolean

  /** Parse the input message. */
  def parse(): Try[ValueMap] = Try({
    val map = new ValueMapImpl
    val interchange = init
    map.put(interchangeProperties, interchange)
    val builder = new StringBuilder
    builder.append(lexer.getDataSeparator)
    builder.append(lexer.getSubElement)
    builder.append(if (lexer.getRepetitionSeparator < 0) 'U' else lexer.getRepetitionSeparator.asInstanceOf[Char])
    builder.append(lexer.getSegmentTerminator)
    builder.append(if (lexer.getReleaseIndicator < 0) 'U' else lexer.getReleaseIndicator.asInstanceOf[Char])
    map.put(delimiterCharacters, builder.toString)
    val transLists = new ValueMapImpl().asInstanceOf[java.util.Map[String, MapList]]
    schema.transactions.keys foreach { key => transLists.put(key, new MapListImpl) }
    map.put(transactionsMap, transLists)
    while (isGroupOpen) {
      val group = openGroup
      map.put(groupProperties, group)
      lexer.countGroup()
      while (!isGroupClose) {
        val set = openSet
        map.put(setIdentifier, set._1)
        map.put(setProperties, set._2)
        schema.transactions(set._1) match {
          case t: Transaction => {
            val list = transLists.get(set._1)
            list.add(parseTransaction(t))
          }
          case _ => throw new IllegalStateException(s"unknown transaction type ${set._1}")
        }
        closeSet(set._2)
      }
      closeGroup(group)
    }
    term(interchange)
    map
  })
}

object SchemaParser {

  /** Parser for EDIFACT messages. */
  /*  private class EdiFactSchemaParser(in: InputStream, sc: EdiSchema) extends SchemaParser(new EdiFactLexer(in), sc) {
    def init() = {
      val params = lexer.init(new ValueMapImpl())
      val second = lexer.requireNextItem(SEGMENT)
      params
    }
    def parse() = {
      Success(Map.empty())
    }
  } */

  /** Factory function to create parser instances. */
  def create(in: InputStream, schema: EdiSchema) = Try {
    schema ediForm match {
      case EdiFact => throw new IllegalArgumentException()
      case X12 => new X12SchemaParser(in, schema)
    }
  }
}