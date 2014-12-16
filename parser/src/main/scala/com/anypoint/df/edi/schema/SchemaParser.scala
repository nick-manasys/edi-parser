package com.anypoint.df.edi.schema

import java.io.IOException
import java.io.InputStream
import org.apache.log4j.Logger
import scala.annotation.tailrec
import scala.util.Try
import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.EdiConstants.DataType._
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.LexerBase
import com.anypoint.df.edi.lexical.LexicalException
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.lexical.ErrorHandler
import com.anypoint.df.edi.lexical.ErrorHandler._
import scala.collection.mutable.Buffer

/** Parse EDI document based on schema. */
abstract class SchemaParser(val lexer: LexerBase, val schema: EdiSchema) extends SchemaJavaDefs {

  import SchemaJavaValues._

  val logger = Logger.getLogger(getClass.getName)

  /** Initialize parser and read header segments. */
  protected def init(): ValueMap

  /** Read interchange trailer segment(s) and finish with stream. */
  protected def term(props: ValueMap): Unit

  /** Discard remainder of current data element. */
  def discardElement() = while (lexer.currentType == QUALIFIER || lexer.currentType == REPETITION) lexer.advance

  /** Parse a segment component, which is either an element or a composite. */
  def parseComponent(comp: SegmentComponent, map: ValueMap): Unit = {
    comp match {
      case elemComp: ElementComponent => {
        val elem = elemComp.element
        val value = elem.dataType match {
          case ALPHA => lexer.parseAlpha(elem.minLength, elem.maxLength)
          case ALPHANUMERIC | ID => lexer.parseAlphaNumeric(elem.minLength, elem.maxLength)
          case BINARY => throw new IOException("Handling not implemented for binary values")
          case DATE => lexer.parseDate(elem.minLength, elem.maxLength)
          case INTEGER => lexer.parseInteger(elem.minLength, elem.maxLength)
          case NUMBER | REAL => lexer.parseNumber(elem.minLength, elem.maxLength)
          case TIME => Integer.valueOf(lexer.parseTime(elem.minLength, elem.maxLength))
          case typ: DataType if (typ.isDecimal()) =>
            lexer.parseImpliedDecimalNumber(typ.decimalPlaces, elem.minLength, elem.maxLength)
        }
        map put (comp.key, value)
      }
      case compComp: CompositeComponent => {
        val composite = compComp.composite
        def parseCompInst(): ValueMap = {
          val compmap = new ValueMapImpl()
          parseCompList(composite.components, QUALIFIER, compmap)
          compmap
        }
        if (comp.count > 1) {
          val complist = new MapListImpl()
          map put (comp.key, complist)
          (1 until comp.count) foreach { index =>
            complist add (parseCompInst())
          }
        } else map put (comp.key, parseCompInst())
      }
    }
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], expect: ItemType, map: ValueMap): Unit

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  def parseSegment(segment: Segment, group: Option[String]): ValueMap

  /** Check if at segment start. */
  def checkSegment(segment: Segment) = lexer.currentType == SEGMENT && lexer.token == segment.ident

  object ComponentErrors {
    sealed trait ComponentError
    case object TooManyLoops extends ComponentError
    case object TooManyRepetitions extends ComponentError
    case object MissingRequired extends ComponentError
    case object UnknownSegment extends ComponentError
    case object OutOfOrderSegment extends ComponentError
    case object UnusedSegment extends ComponentError
  }

  /** Report segment error. */
  def segmentError(ident: String, group: Option[String], error: ComponentErrors.ComponentError): Unit

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
    def parseRepeatingSegment(segment: Segment, limit: Int, group: Option[String]): MapList = {
      val list: MapList = new MapListImpl()
      @tailrec
      def parseRepeat(): Unit =
        if (checkSegment(segment)) {
          if (limit > 0 && limit <= list.size) segmentError(segment.ident, group, ComponentErrors.TooManyRepetitions)
          list add (parseSegment(segment, group))
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
          if (group.count > 0 && group.count <= list.size)
            segmentError(group.ident, Some(group.ident), ComponentErrors.TooManyLoops)
          list add (parseSection(group.items, Some(group.ident)))
          parseRepeat()
        }
      parseRepeat()
      list
    }

    def checkSegmentKnown(): Unit = lexer.currentType match {
      case SEGMENT =>
        if (!transaction.segmentIds.contains(lexer.token)) {
          val ident = lexer.token
          segmentError(ident, None, ComponentErrors.UnknownSegment)
          while (ident == lexer.token) discardSegment
        }
      case END =>
      case _ => throw new IllegalStateException("lexer in segment data")
    }

    /** Parse a portion of transaction data represented by a list of components (which may be segment references or
      * loops) into a map.
      */
    def parseSection(comps: List[TransactionComponent], group: Option[String]) = {
      val values = new ValueMapImpl
      @tailrec
      def parseComponents(comps: List[TransactionComponent]): Unit = comps match {
        case (ref: ReferenceComponent) :: tail => {
          val segment = ref.segment
          if (!isEnvelopeSegment(segment)) {
            if (checkSegment(segment)) {
              if (ref.usage == UnusedUsage) segmentError(segment.ident, group, ComponentErrors.UnusedSegment)
              val data =
                if (ref.count == 1) parseSegment(segment, group) else parseRepeatingSegment(segment, ref.count, group)
              values put (segment.name, data)
              checkSegmentKnown()
            } else if (ref.usage == MandatoryUsage) segmentError(segment.ident, group, ComponentErrors.MissingRequired)
          }
          parseComponents(tail)
        }
        case (group: GroupComponent) :: tail => {
          val repeats = parseRepeatingGroup(group)
          if (repeats.size() > 0) values put (group.ident, repeats)
          else if (group.usage == MandatoryUsage)
            segmentError(group.ident, Some(group.ident), ComponentErrors.MissingRequired)
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
    topMap put (transactionHeading, parseSection(transaction.heading, None))
    topMap put (transactionDetail, parseSection(transaction.detail, None))
    topMap put (transactionSummary, parseSection(transaction.summary, None))
    topMap
  }

  /** Discard input past end of current segment. */
  def discardSegment() =
    while (lexer.currentType() != ItemType.SEGMENT && lexer.currentType() != ItemType.END) lexer.advance()

  /** Discard input past end of current transaction. */
  def discardTransaction(): Unit

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment): Boolean

  /** Parse the input message. */
  def parse(): Try[ValueMap]
}