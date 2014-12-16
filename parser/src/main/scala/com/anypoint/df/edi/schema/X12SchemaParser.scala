package com.anypoint.df.edi.schema

import java.io.InputStream
import java.io.IOException
import scala.collection.JavaConversions
import scala.collection.mutable.Buffer
import scala.util.Try
import scala.util.Success
import com.anypoint.df.edi.lexical.EdiConstants.ItemType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.LexerBase
import com.anypoint.df.edi.lexical.LexicalException
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.lexical.X12Lexer
import com.anypoint.df.edi.lexical.ErrorHandler
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition._
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition
import com.anypoint.df.edi.lexical.EdiConstants.DataType
import EdiSchema._
import SchemaJavaValues._
import X12SchemaValues._
import X12Acknowledgment._

/** Configuration parameters for X12 schema parser. */
case class X12ParserConfig(val lengthFail: Boolean, val asciiOnly: Boolean, val charFail: Boolean,
  val countFail: Boolean, val unknownFail: Boolean, val orderFail: Boolean, val unusedFail: Boolean,
  val occursFail: Boolean, val reportDataErrors: Boolean)

/** Parser for X12 EDI documents. */
case class X12SchemaParser(in: InputStream, sc: EdiSchema, config: X12ParserConfig)
  extends SchemaParser(new X12Lexer(in), sc.merge(X12Acknowledgment.trans997)) with X12SchemaDefs {

  /** Flag for transaction to be rejected because of errors. */
  var rejectTransaction = false

  /** Current segment reference, used in error reporting. */
  var currentSegment: Segment = null

  /** Accumulated data errors (as AK4 maps) from segment. */
  val dataErrors = Buffer[ValueMap]()

  /** Accumulated segment errors (as AK3 maps) from transaction. */
  val segmentErrors = Buffer[ValueMap]()

  /** Lexical error handler. */
  case object X12ErrorHandler extends ErrorHandler {
    def error(lexer: LexerBase, typ: DataType, error: ErrorCondition, explain: java.lang.String): Unit = {
      addElementError(error match {
        case TOO_SHORT => DataTooShort
        case TOO_LONG => DataTooLong
        case INVALID_CHARACTER => InvalidCharacter
        case INVALID_CODE => InvalidCodeValue
        case INVALID_DATE => InvalidDate
        case INVALID_TIME => InvalidTime
      })
    }
  }

  /** Check if an element syntax error condition is fatal for the containing transaction. */
  def checkFatal(error: ElementSyntaxError) = error match {
    case MissingConditionalElement => config unusedFail
    case TooManyElements => config countFail
    case DataTooShort => config lengthFail
    case DataTooLong => config lengthFail
    case InvalidCharacter => config charFail
    case TooManyRepititions => config countFail
    case TooManyComponents => config countFail
    case _ => true
  }

  /** Accumulate element error, failing transaction if severe. */
  def addElementError(error: ElementSyntaxError) = {
    if (checkFatal(error)) rejectTransaction = true
    if (config.reportDataErrors) {
      val comp = currentSegment.components(lexer.getElementNumber())
      val ak4 = new ValueMapImpl
      val compPos = new ValueMapImpl
      compPos put (compC030.components(0).key, lexer.getElementNumber() toString)
      comp match {
        case ElementComponent(elem, _, _, _, _) => ak4 put (segAK4.components(1).key, elem.ident)
        case _: CompositeComponent => compPos put (compC030.components(1).key, lexer.getComponentNumber() toString)
      }
      if (comp.count != 1) compPos put (compC030.components(2).key, lexer.getRepetitionNumber() toString)
      ak4 put (segAK4.components(1).key, compPos)
      ak4 put (segAK4.components(2).key, error.code toString)
      if (error != InvalidCharacter) ak4 put (segAK4.components(3).key, lexer.token)
      dataErrors += ak4
    }
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], expect: ItemType, map: ValueMap) =
    comps foreach {
      comp =>
        if (expect == lexer.currentType) {
          if (lexer.token.length > 0) parseComponent(comp, map)
          else if (comp.usage == MandatoryUsage) addElementError(MissingRequiredElement)
          else lexer.advance
        } else lexer.currentType match {
          case SEGMENT | END | DATA_ELEMENT =>
            if (comp.usage == MandatoryUsage) addElementError(MissingRequiredElement)
          case QUALIFIER => {
            addElementError(TooManyComponents)
            discardElement()
          }
          case REPETITION => {
            addElementError(TooManyRepititions)
            discardElement()
          }
        }
    }

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  def parseSegment(segment: Segment, group: Option[String]): ValueMap = {
    val map = new ValueMapImpl()
    dataErrors.clear
    currentSegment = segment
    lexer.advance
    parseCompList(segment.components, DATA_ELEMENT, map)
    lexer.currentType match {
      case SEGMENT | END =>
      case _ => {
        addElementError(TooManyElements)
        while (lexer.currentType != SEGMENT && lexer.currentType != END) discardElement
      }
    }
    if (!dataErrors.isEmpty && config.reportDataErrors) {
      val ak3 = new ValueMapImpl
      ak3 put (segAK3.components(0).key, segment.ident)
      ak3 put (segAK3.components(1).key, lexer.getSegmentNumber.toString)
      group.foreach(ident => ak3 put (segAK3.components(2).key, ident))
      ak3 put (segAK3.components(3).key, DataErrorsSegment.code.toString)
      ak3 put (segAK2.ident, JavaConversions.bufferAsJavaList(dataErrors.reverse))
      segmentErrors += ak3
    }
    map
  }

  /** Report segment error. */
  def segmentError(ident: String, group: Option[String], error: ComponentErrors.ComponentError) = {
    def addError(error: SegmentSyntaxError) =
      if (config.reportDataErrors) {
        val ak3 = new ValueMapImpl
        ak3 put (segAK3.components(0).key, ident)
        ak3 put (segAK3.components(1).key, lexer.getSegmentNumber.toString)
        group.foreach(ident => ak3 put (segAK3.components(2).key, ident))
        ak3 put (segAK3.components(3).key, error.code.toString)
        segmentErrors += ak3
      }
    error match {
      case ComponentErrors.TooManyLoops => {
        addError(TooManyLoops)
        if (config.occursFail) rejectTransaction = true
      }
      case ComponentErrors.TooManyRepetitions => {
        addError(TooManyOccurs)
        if (config.occursFail) rejectTransaction = true
      }
      case ComponentErrors.MissingRequired => {
        addError(MissingMandatorySegment)
        rejectTransaction = true;
      }
      case ComponentErrors.UnknownSegment => {
        addError(UnrecognizedSegment)
        if (config.unknownFail) rejectTransaction = true
      }
      case ComponentErrors.OutOfOrderSegment => {
        addError(OutOfOrderSegment)
        if (config.orderFail) rejectTransaction = true
      }
      case ComponentErrors.UnusedSegment =>
        if (config.unusedFail) {
          addError(UnexpectedSegment)
          rejectTransaction = true
        }
    }
  }

  def init() = {
    lexer.setHandler(X12ErrorHandler)
    lexer.init(new ValueMapImpl())
  }

  def term(props: ValueMap) = lexer.term(props)

  /** Check if at functional group open segment. */
  def isGroupOpen() = checkSegment(GSSegment)

  /** Parse start of a functional group. */
  def openGroup() =
    if (checkSegment(GSSegment)) parseSegment(GSSegment, None)
    else throw new IllegalStateException("missing required GS segment")

  /** Check if at functional group close segment. */
  def isGroupClose() = checkSegment(GESegment)

  /** Parse close of a functional group. */
  def closeGroup(props: ValueMap) = {
    if (checkSegment(GESegment)) {
      val endprops = parseSegment(GESegment, None)
      if (props.get(groupControlKey) != endprops.get(groupControlEndKey)) {
        throw new IllegalStateException("group control number in trailer does not match header")
      }
    } else throw new IllegalStateException("not positioned at GE segment")
  }

  /** Check if at transaction set open segment. */
  def isSetOpen() = checkSegment(STSegment)

  /** Parse start of a transaction set. */
  def openSet() =
    if (checkSegment(STSegment)) {
      val values = parseSegment(STSegment, None)
      (values.get(transactionSetIdentifierKey).asInstanceOf[String], values)
    } else throw new IllegalStateException("missing required ST segment")

  /** Check if at transaction set close segment. */
  def isSetClose() = checkSegment(SESegment)

  /** Parse close of a transaction set. */
  def closeSet(props: ValueMap) = {
    if (checkSegment(SESegment)) {
      val endprops = parseSegment(SESegment, None)
      if (props.get(transactionSetControlKey) != endprops.get(transactionSetControlEndKey)) {
        throw new IllegalStateException("transaction set control number in trailer does not match header")
      }
    } else throw new IllegalStateException("not positioned at SE segment")
  }

  /** Discard input past end of current transaction. */
  def discardTransaction() = {
    while (lexer.currentType == SEGMENT && lexer.token() != SESegment.ident) discardSegment
    discardSegment
  }

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == STSegment.ident || segment.ident == SESegment.ident

  /** Parse the input message. */
  def parse(): Try[ValueMap] = Try {

    import X12Acknowledgment._

    val map = new ValueMapImpl
    val interchange = init
    val builder = new StringBuilder
    builder append (lexer.getDataSeparator)
    builder append (lexer.getComponentSeparator)
    builder append (if (lexer.getRepetitionSeparator < 0) 'U' else lexer.getRepetitionSeparator.asInstanceOf[Char])
    builder append (lexer.getSegmentTerminator)
    builder append (if (lexer.getReleaseIndicator < 0) 'U' else lexer.getReleaseIndicator.asInstanceOf[Char])
    map put (delimiterCharacters, builder.toString)
    val transLists = new ValueMapImpl().asInstanceOf[java.util.Map[String, MapList]]
    schema.transactions.keys foreach { key => transLists put (key, new MapListImpl) }
    map put (transactionsMap, transLists)
    val acksList = new MapListImpl
    map put (acknowledgments, acksList)
    var ackId = 1
    while (isGroupOpen) {
      val group = openGroup
      group put (groupInterchange, interchange)
      lexer.countGroup()
      val ackroot = new ValueMapImpl
      ackroot put (transactionId, trans997 ident)
      ackroot put (transactionName, trans997 name)
      val ackhead = new ValueMapImpl
      ackroot put (transactionHeading, ackhead)
      ackroot put (transactionDetail, new ValueMapImpl)
      ackroot put (transactionSummary, new ValueMapImpl)
      ackroot put (transactionGroup, group)
      val stdata = new ValueMapImpl
      ackhead put (segST name, stdata)
      stdata put (segST.components(0) key, trans997 ident)
      stdata put (segST.components(1) key, ackId toString)
      val ak1data = new ValueMapImpl
      ackhead put (segAK1 name, ak1data)
      ak1data put (segAK1.components(0) key, group get (functionalIdentifierKey))
      ak1data put (segAK1.components(1) key, group get (groupControlKey))
      ak1data put (segAK1.components(2) key, group get (versionIdentifierKey))
      val setacks = new MapListImpl
      ackhead put ("AK2", setacks)
      var setCount = 0
      var acceptCount = 0;
      while (!isGroupClose) {
        val (setid, setprops) = openSet
        ackroot put (transactionSet, setprops)
        setCount += 1
        schema.transactions(setid) match {
          case t: Transaction => {
            val setack = new ValueMapImpl
            setacks add setack
            val ak2data = new ValueMapImpl
            setack put (segAK2 name, ak2data)
            ak2data put (segAK2.components(0) key, setprops get (transactionSetIdentifierKey))
            ak2data put (segAK2.components(1) key, setprops get (transactionSetControlKey))
            if (setprops containsKey implementationConventionKey) {
              ak2data put (segAK2.components(2) key, setprops get (implementationConventionKey))
            }
            rejectTransaction = false;
            val data = parseTransaction(t)
            data put (transactionGroup, group)
            data put (transactionSet, setprops)
            if (!segmentErrors.isEmpty) {
              val ak3s = JavaConversions.bufferAsJavaList(segmentErrors)
              setack put (segAK3.components(0).key, ak3s)
            }
            val ak5data = new ValueMapImpl
            setack put (segAK5 name, ak5data)
            if (rejectTransaction) {
              ak5data put (segAK5.components(0) key, RejectedTransaction code)
            } else {
              val list = transLists get setid
              list add (data)
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
      ak9data put (segAK9.components(1) key, Integer valueOf (setCount))
      ak9data put (segAK9.components(2) key, Integer valueOf (setCount))
      ak9data put (segAK9.components(3) key, Integer valueOf (acceptCount))
      val sedata = new ValueMapImpl
      ackhead put (segSE name, sedata)
      sedata put (segSE.components(0) key, Integer valueOf (setCount + 3))
      sedata put (segSE.components(1) key, ackId toString)
      acksList add (ackroot)
      ackId += 1
    }
    term(interchange)
    map
  }
}