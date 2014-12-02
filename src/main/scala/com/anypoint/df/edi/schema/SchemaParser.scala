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

/** Transaction syntax error codes (X12 718 element codes). */
sealed abstract class TransactionSyntaxError(val code: Int)
case object NotSupportedTransaction extends TransactionSyntaxError(1)
case object MissingTrailerTransaction extends TransactionSyntaxError(2)
case object ControlNumberMismatch extends TransactionSyntaxError(3)
case object WrongSegmentCount extends TransactionSyntaxError(4)
case object SegmentsInError extends TransactionSyntaxError(5)
case object BadTransactionSetId extends TransactionSyntaxError(6)
case object BadTransactionSetControl extends TransactionSyntaxError(7)
case object AuthenticationKeyUnknown extends TransactionSyntaxError(8)
case object EncryptionKeyUnknown extends TransactionSyntaxError(9)
case object ServiceNotAvailable extends TransactionSyntaxError(10)
case object UnknownSecurityRecipient extends TransactionSyntaxError(11)
case object IncorrectMessageLength extends TransactionSyntaxError(12)
case object MessageAuthenticationFailed extends TransactionSyntaxError(13)
case object UnknownSecurityOriginator extends TransactionSyntaxError(15)
case object DecryptionSyntaxError extends TransactionSyntaxError(16)
case object SecurityNotSupported extends TransactionSyntaxError(17)
case object SetNotInGroup extends TransactionSyntaxError(18)
case object InvalidImplementationConvention extends TransactionSyntaxError(23)
case object MissingS3ESecurityEndSegment extends TransactionSyntaxError(24)
case object MissingS3ESecurityStartSegment extends TransactionSyntaxError(25)
case object MissingS4ESecurityEndSegment extends TransactionSyntaxError(26)
case object MissingS4ESecurityStartSegment extends TransactionSyntaxError(27)

/** Transaction set acknowledgment codes (X12 717 element codes). */
sealed abstract class TransactionAcknowledgmentCode(val code: String)
case object AcceptedTransaction extends TransactionAcknowledgmentCode("A")
case object AcceptedWithErrorsTransaction extends TransactionAcknowledgmentCode("E")
case object AuthenticationFailedTransaction extends TransactionAcknowledgmentCode("M")
case object RejectedTransaction extends TransactionAcknowledgmentCode("R")
case object ValidityFailedTransaction extends TransactionAcknowledgmentCode("W")
case object DecryptionBadTransaction extends TransactionAcknowledgmentCode("X")

case class TransactionAcknowledgment()

/** Segment syntax error codes (X12 720 element codes). */
sealed abstract class SegmentSyntaxError(val code: Int)
case object UnrecognizedSegment extends SegmentSyntaxError(1)
case object UnexpectedSegment extends SegmentSyntaxError(2)
case object MissingMandatorySegment extends SegmentSyntaxError(3)
case object TooManyLoops extends SegmentSyntaxError(4)
case object TooManyOccurs extends SegmentSyntaxError(5)
case object NotInTransactionSegment extends SegmentSyntaxError(6)
case object OutOfOrderSegment extends SegmentSyntaxError(7)
case object DataErrorsSegment extends SegmentSyntaxError(8)

/** Information for a segment error (used to generate X12 AK3 segment). */
case class SegmentError(val id: String, val position: Int, val loopId: Option[String],
  val error: Option[SegmentSyntaxError])

/** Data element syntax error codes (X12 723 element codes). */
sealed abstract class ElementSyntaxError(val code: Int, val text: String)
case object MissingRequiredElement extends ElementSyntaxError(1, "missing required element")
case object MissingConditionalElement extends ElementSyntaxError(2, "missing conditional element")
case object TooManyElements extends ElementSyntaxError(3, "too many elements")
case object DataTooShort extends ElementSyntaxError(4, "data value too short")
case object DataTooLong extends ElementSyntaxError(5, "data value too long")
case object InvalidCharacter extends ElementSyntaxError(6, "invalid character in data value")
case object InvalidCodeValue extends ElementSyntaxError(7, "invalid code value")
case object InvalidDate extends ElementSyntaxError(8, "invalid date")
case object InvalidTime extends ElementSyntaxError(9, "invalid time")
case object ExclusionConditionViolated extends ElementSyntaxError(10, "exclusion condition violated")
case object TooManyRepititions extends ElementSyntaxError(11, "too many repetitions")
case object TooManyComponents extends ElementSyntaxError(12, "too many components")

/** Information for an element error (used to generate X12 AK4 segment). */
case class ElementError(val elemPosition: Int, val compPosition: Option[Int], val repeatPosition: Option[Int],
  val elemReference: Option[Int], val error: ElementSyntaxError, val text: Option[String])

abstract class SchemaParser(val lexer: LexerBase, val schema: EdiSchema) extends ErrorHandler with SchemaJavaDefs {

  import SchemaJavaValues._
  
  val logger = Logger.getLogger(getClass.getName)

  /** Initialize parser and read header segments. */
  protected def init(): ValueMap

  /** Read interchange trailer segment(s) and finish with stream. */
  protected def term(props: ValueMap): Unit

  val dataErrors = Buffer[ElementError]()
  val segmentErrors = Buffer[SegmentError]()

  var inComposite: Option[CompositeComponent] = None
  var inElement: Option[ElementComponent] = None
  var inReference: Option[ReferenceComponent] = None
  var inGroup: Option[GroupComponent] = None
  var segmentErrorCount = 0
  var rejectTransaction = false
  
  /** Accumulate element error, failing transaction if severe. */
  def addElementError(error: ElementSyntaxError) = 
    if (segmentErrorCount > 4) throw new LexicalException("too many errors")
    else {
    val compPosition = if (inComposite.isDefined) Some(lexer.getComponentNumber) else None
    val repNumber = if (lexer.currentType == REPETITION) Some(lexer.getRepetitionNumber()) else None
    val elemRef = inElement.map(_.position)
    val text = error match {
      case DataTooShort | DataTooLong | InvalidCodeValue | InvalidDate | InvalidTime | ExclusionConditionViolated =>
        Some(lexer.token)
      case _ => None
    }
    dataErrors += ElementError(lexer.getElementNumber, compPosition, repNumber, elemRef, error, text)
    segmentErrorCount += 1
    val message = "element error in" +
     inGroup.map(group => " loop " + group.ident).getOrElse("") +
     inReference.map(refc => " segment " + refc.segment.name).getOrElse("") +
     inComposite.map(comp => s" composite ${comp.name} at position ${comp.position}").getOrElse("") +
     inElement.map(elem => s" element ${elem.element.ident} at position ${elem.position}").getOrElse("") +
     ": " + error.text 
    val fail = error match {
      case DataTooShort | DataTooLong => false
      case _ => true
    }
    if (fail) {
      rejectTransaction = true
      logger.error(message)
    } else logger.warn(message)
  }

  /** Error handler called by lexer for data error. */
  def error(lexer: LexerBase, typ: DataType, error: ErrorCondition, explain: java.lang.String): Unit = {
    addElementError(error match {
      case ErrorCondition.TOO_SHORT => DataTooShort
      case ErrorCondition.TOO_LONG => DataTooLong
      case ErrorCondition.INVALID_CHARACTER => InvalidCharacter
      case ErrorCondition.INVALID_CODE => InvalidCodeValue
      case ErrorCondition.INVALID_DATE => InvalidDate
      case ErrorCondition.INVALID_TIME => InvalidTime
    })
  }

  /** Parse a segment component, which is either an element or a composite. */
  def parseComponent(comp: SegmentComponent, map: ValueMap): Unit = {
    comp match {
      case elemComp: ElementComponent => {
        inElement = Some(elemComp)
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
        inElement = None
      }
      case compComp: CompositeComponent => {
        val prior = inComposite
        inComposite = Some(compComp)
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
        inComposite = prior
      }
    }
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], expect: ItemType, map: ValueMap) = {
    comps foreach { comp =>
      if (expect == lexer.currentType) {
        if (lexer.token.length > 0) parseComponent(comp, map)
        else if (comp.usage == MandatoryUsage) addElementError(MissingRequiredElement)
        else lexer.advance
      } else lexer.currentType match {
        case SEGMENT | END | DATA_ELEMENT => if (comp.usage == MandatoryUsage) addElementError(MissingRequiredElement)
        case QUALIFIER => addElementError(TooManyComponents)
        case REPETITION => addElementError(TooManyRepititions)
      }
    }
  }

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  protected def parseSegment(segment: Segment): ValueMap = {
    val map = new ValueMapImpl()
    lexer.advance
    parseCompList(segment.components, DATA_ELEMENT, map)
    lexer.currentType match {
      case SEGMENT | END =>
      case _ => addElementError(TooManyElements)
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
  def parse(): Try[ValueMap] = Try {
    
    import X12Acknowledgment._
    import X12SchemaValues._
    
    val map = new ValueMapImpl
    val interchange = init
    map put (interchangeProperties, interchange)
    val builder = new StringBuilder
    builder append(lexer.getDataSeparator)
    builder append(lexer.getComponentSeparator)
    builder append(if (lexer.getRepetitionSeparator < 0) 'U' else lexer.getRepetitionSeparator.asInstanceOf[Char])
    builder append(lexer.getSegmentTerminator)
    builder append(if (lexer.getReleaseIndicator < 0) 'U' else lexer.getReleaseIndicator.asInstanceOf[Char])
    map put (delimiterCharacters, builder.toString)
    val transLists = new ValueMapImpl().asInstanceOf[java.util.Map[String, MapList]]
    schema.transactions.keys foreach { key => transLists put (key, new MapListImpl) }
    map put (transactionsMap, transLists)
    val acksList = new MapListImpl
    map put (acknowledgments, acksList)
    var ackId = 1
    while (isGroupOpen) {
      val group = openGroup
      map put (groupProperties, group)
      lexer.countGroup()
      val ackroot = new ValueMapImpl
      ackroot put (transactionId, trans997 ident)
      ackroot put (transactionName, trans997 name)
      val ackhead = new ValueMapImpl
      ackroot put (transactionHeading, ackhead)
      ackroot put (transactionDetail, new ValueMapImpl)
      ackroot put (transactionSummary, new ValueMapImpl)
      val stdata = new ValueMapImpl
      ackhead put (segST name, stdata)
      stdata put (segST.components(0) key, trans997 ident)
      stdata put (segST.components(1) key, ackId toString)
      val ak1data = new ValueMapImpl
      ackhead put (segAK1 name, ak1data)
      ak1data put (segAK1.components(0) key, group get(functionalIdentifierKey))
      ak1data put (segAK1.components(1) key, group get(groupControlKey))
      ak1data put (segAK1.components(2) key, group get(versionIdentifierKey))
      val setacks = new MapListImpl
      ackhead put ("AK2", setacks)
      var setCount = 0
      var acceptCount = 0;
      while (!isGroupClose) {
        val (setid, setprops) = openSet
        setCount += 1
        map put (setIdentifier, setid)
        map put (setProperties, setprops)
        schema.transactions(setid) match {
          case t: Transaction => {
            val setack = new ValueMapImpl
            setacks add setack
            val ak2data = new ValueMapImpl
            setack put (segAK2 name, ak2data)
            ak2data put (segAK2.components(0) key, setprops get(transactionSetIdentifierKey))
            ak2data put (segAK2.components(1) key, setprops get(transactionSetControlKey))
            if (setprops containsKey implementationConventionKey) {
              ak2data put (segAK2.components(2) key, setprops get(implementationConventionKey))
            }
            rejectTransaction = false;
            val data = parseTransaction(t)
            val ak5data = new ValueMapImpl
            setack put (segAK5 name, ak5data)
            if (rejectTransaction) {
              // TODO
            } else {
              val list = transLists get setid
              list add(data)
              acceptCount += 1
              ak5data put (segAK5.components(0) key, AcceptedTransaction code)
            }
          }
          case _ => throw new IllegalStateException(s"unknown transaction type $setid")
        }
        closeSet(setprops)
      }
      closeGroup(group)
      val ak9data = new ValueMapImpl
      ackhead put (segAK9 name, ak9data)
      ak9data put (segAK9.components(0) key, AcceptedTransaction code)
      ak9data put (segAK9.components(1) key, Integer valueOf(setCount))
      ak9data put (segAK9.components(2) key, Integer valueOf(setCount))
      ak9data put (segAK9.components(3) key, Integer valueOf(acceptCount))
      val sedata = new ValueMapImpl
      ackhead put (segSE name, sedata)
      sedata put (segSE.components(0) key, Integer valueOf(setCount + 3))
      sedata put (segSE.components(1) key, ackId toString)
      acksList add(ackroot)
      ackId += 1
    }
    term(interchange)
    map
  }
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