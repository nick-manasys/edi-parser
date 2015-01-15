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

  /** Discard current element. */
  def discardElement = {
    lexer.advance
    while (lexer.currentType == QUALIFIER || lexer.currentType == REPETITION) lexer.advance
  }

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
        if (comp.count > 1) {
          val complist = new MapListImpl()
          map put (comp.key, complist)
          // TODO: check this logic
          (1 until comp.count) foreach (index => {
            val compmap = new ValueMapImpl()
            parseCompList(composite.components, lexer.currentType(), QUALIFIER, compmap)
            complist add compmap
          })
        } else parseCompList(composite.components, lexer.currentType(), QUALIFIER, map)
      }
    }
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap): Unit

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  def parseSegment(segment: Segment, group: Option[String], position: String): ValueMap

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

    /** Get list of maps for key. If the list is not already set, this creates and returns a new one. */
    def getList(key: String, values: ValueMap) =
      if (values.containsKey(key)) values.get(key).asInstanceOf[MapList]
      else {
        val list = new MapListImpl
        values put (key, list)
        list
      }

    /** Parse a (potentially) repeating segment into a list of maps. */
    def parseRepeatingSegment(segment: Segment, limit: Int, group: Option[String], position: String): MapList = {
      val list: MapList = new MapListImpl()
      @tailrec
      def parseRepeat(): Unit =
        if (checkSegment(segment)) {
          if (limit > 0 && limit <= list.size) segmentError(segment.ident, group, ComponentErrors.TooManyRepetitions)
          list add (parseSegment(segment, group, position))
          parseRepeat()
        }
      parseRepeat()
      list
    }

    /** Parse a (potentially) repeating group, with variants treated separately. The repeating group is represented as a
      * list of maps, one for each occurrence of the group not recognized as a variant. Variants which can occur
      * multiple times are represented as separate lists of maps, those which can only occur once are represented as
      * simple maps. Key values for variants are derived by concatenating the variant field value to the base group key
      * separated by an underscore.
      */
    def parseRepeatingGroup(group: GroupComponent, values: ValueMap): Unit = {
      def verifyRepeats(key: String, max: Int) =
        if (getList(key, values).size >= max) segmentError(group.ident, Some(group.ident), ComponentErrors.TooManyLoops)
      def addInstance(key: String, data: ValueMap) =
        if (values.containsKey(key)) values get (key) match {
          case list: MapList => list add (data)
          case _ => segmentError(group.ident, Some(group.ident), ComponentErrors.TooManyLoops)
        }
        else values put (key, data)
      @tailrec
      def parseRepeat(): Unit = {
        logger.info(s"checking for loop start segment '${group.leadseg.ident}'': ${checkSegment(group.leadseg)}")
        if (checkSegment(group.leadseg)) {
          val leadmap = parseSegment(group.leadseg, Some(group.ident), group.items.head.asInstanceOf[ReferenceComponent].position)
          val varval = leadmap.get(group.varkey).asInstanceOf[String]
          if (group.varbyval.contains(varval)) {
            val variant = group.varbyval(varval)
            verifyRepeats(variant.key, variant.count)
            addInstance(variant.key, parseSection(variant.items.tail, Some(group.ident)))
          } else {
            verifyRepeats(group.key, group.count)
            addInstance(group.key, parseSection(group.items.tail, Some(group.ident)))
          }
          parseRepeat()
        }
      }

      parseRepeat()
    }

    /** Check if current segment is known. */
    def checkSegmentKnown(): Unit = lexer.currentType match {
      case SEGMENT =>
        if (!transaction.segmentIds.contains(lexer.token)) {
          val ident = lexer.token
          segmentError(ident, None, ComponentErrors.UnknownSegment)
          while (ident == lexer.token) discardSegment
        }
      case END =>
      case _ => throw new IllegalStateException("lexer in data when should be start of segment")
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
            logger.info(s"checking for segment '${segment.ident}'': ${checkSegment(segment)}")
            if (checkSegment(segment)) {
              if (ref.usage == UnusedUsage) segmentError(segment.ident, group, ComponentErrors.UnusedSegment)
              val data =
                if (ref.count == 1) parseSegment(segment, group, ref.position)
                else parseRepeatingSegment(segment, ref.count, group, ref.position)
              values put (segment.name, data)
              checkSegmentKnown()
            } else if (ref.usage == MandatoryUsage) segmentError(segment.ident, group, ComponentErrors.MissingRequired)
          }
          parseComponents(tail)
        }
        case (group: GroupComponent) :: tail => {
          parseRepeatingGroup(group, values)
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
    while (lexer.currentType() != SEGMENT && lexer.currentType() != END) lexer.advance()

  /** Discard input past end of current transaction. */
  def discardTransaction(): Unit

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment): Boolean

  /** Parse the input message. */
  def parse(): Try[ValueMap]
}