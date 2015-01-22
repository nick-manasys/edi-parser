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

/** Entity identity information. Interchange qualifier and id are used for one direction of an interchange, while type
  * (if non-zero length) is matched against the interchange type in an incoming message. The application identifier is
  * used for the group header.
  */
case class IdentityInformation(interchangeQualifier: String, interchangeId: String, interchangeType: String)

/** Configuration parameters for X12 schema parser. If either receiver or sender identity information is it is verified
  * in processed messages.
  */
case class X12ParserConfig(val lengthFail: Boolean, val asciiOnly: Boolean, val charFail: Boolean,
  val countFail: Boolean, val unknownFail: Boolean, val orderFail: Boolean, val unusedFail: Boolean,
  val occursFail: Boolean, val reportDataErrors: Boolean, val generate997: Boolean, val generateTA1: Boolean,
  val receiverIds: Array[IdentityInformation], val senderIds: Array[IdentityInformation])

/** Parser for X12 EDI documents. */
case class X12SchemaParser(in: InputStream, sc: EdiSchema, config: X12ParserConfig)
  extends SchemaParser(new X12Lexer(in), sc.merge(X12Acknowledgment.trans997)) with X12SchemaDefs {

  /** Set of functional groups supported by schema. */
  val functionalGroups = schema.transactions.values.map { t => t.group } toSet

  /** Flag for currently in a transaction. */
  var inTransaction = false

  /** Flag for transaction to be rejected because of errors. */
  var rejectTransaction = false

  /** Current segment reference, used in error reporting. */
  var currentSegment: Segment = null

  /** Number of transaction sets seen in current group. */
  var groupTransactionCount = 0

  /** Number of transaction sets accepted in current group. */
  var groupAcceptCount = 0
  
  /** One or more segments of transaction in error flag. */
  var oneOrMoreSegmentsInError = false

  /** Accumulated data errors (as AK4 maps) from segment. */
  val dataErrors = Buffer[ValueMap]()

  /** Accumulated segment errors (as AK3 maps) from transaction. */
  val segmentErrors = Buffer[ValueMap]()

  /** Accumulated transaction errors. */
  val transactionErrors = Buffer[TransactionSyntaxError]()

  /** Accumulated group errors. */
  val groupErrors = Buffer[GroupSyntaxError]()

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
    if (inTransaction) {
      if (checkFatal(error)) rejectTransaction = true
      if (config.reportDataErrors) {
        val comp = currentSegment.components(lexer.getElementNumber)
        val ak4 = new ValueMapImpl
        ak4 put (segAK4compC030.components(0).key, Integer.valueOf(lexer.getElementNumber))
        comp match {
          case ElementComponent(elem, _, _, _, _, _) => ak4 put (segAK4.components(1).key, Integer.valueOf(elem.ident))
          case _: CompositeComponent => ak4 put (segAK4compC030.components(1).key, Integer.valueOf(lexer.getComponentNumber))
        }
        if (comp.count != 1) ak4 put (segAK4compC030.components(2).key, Integer.valueOf(lexer.getRepetitionNumber))
        ak4 put (segAK4.components(2).key, error.code toString)
        if (error != InvalidCharacter) ak4 put (segAK4.components(3).key, lexer.token)
        dataErrors += ak4
      }
    }
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap) = {
    def checkParse(comp: SegmentComponent, of: ItemType) =
      if (of == lexer.currentType) {
        if (lexer.token.length > 0) parseComponent(comp, map)
        else {
          if (comp.usage == MandatoryUsage) addElementError(MissingRequiredElement)
          lexer.advance
        }
      } else if (comp.usage == MandatoryUsage) addElementError(MissingRequiredElement)
    comps match {
      case h :: t => {
        checkParse(h, first)
        t.foreach(comp => checkParse(comp, rest))
      }
      case _ =>
    }
  }

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  def parseSegment(segment: Segment, group: Option[String], position: String): ValueMap = {
    if (logger.isDebugEnabled) logger.debug(s"parsing segment ${segment.ident} at position $position")
    val map = new ValueMapImpl()
    dataErrors.clear
    currentSegment = segment
    lexer.advance
    parseCompList(segment.components, DATA_ELEMENT, DATA_ELEMENT, map)
    lexer.currentType match {
      case SEGMENT | END =>
      case _ => {
        addElementError(TooManyElements)
        discardSegment
      }
    }
    if (!dataErrors.isEmpty && config.reportDataErrors) {
      logger.info(s"error(s) found in parsing segment")
      val ak3 = new ValueMapImpl
      val ak3data = new ValueMapImpl
      ak3data put (segAK3.components(0).key, segment.ident)
      ak3data put (segAK3.components(1).key, Integer.valueOf(lexer.getSegmentNumber))
      group.foreach(ident => ak3data put (segAK3.components(2).key, ident))
      ak3data put (segAK3.components(3).key, DataErrorsSegment.code.toString)
      ak3 put (segAK3.name, ak3data)
      ak3 put (segAK4.ident, JavaConversions.bufferAsJavaList(dataErrors.reverse))
      segmentErrors += ak3
      oneOrMoreSegmentsInError = true
    }
    if (logger.isDebugEnabled) logger.debug(s"now positioned at segment '${lexer.token}'")
    map
  }

  /** Report segment error. */
  def segmentError(ident: String, group: Option[String], error: ComponentErrors.ComponentError) = {
    def addError(error: SegmentSyntaxError) = {
      oneOrMoreSegmentsInError = true
      if (config.reportDataErrors) {
        val ak3 = new ValueMapImpl
        val ak3data = new ValueMapImpl
        ak3 put (segAK3.name, ak3data)
        ak3data put (segAK3.components(0).key, ident)
        ak3data put (segAK3.components(1).key, Integer.valueOf(lexer.getSegmentNumber))
        group.foreach(ident => ak3data put (segAK3.components(2).key, ident))
        ak3data put (segAK3.components(3).key, error.code.toString)
        segmentErrors += ak3
      }}
    if (inTransaction) error match {
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
    val map = lexer.init(new ValueMapImpl())
    lexer.setHandler(X12ErrorHandler)
    map
  }

  def term(props: ValueMap) = lexer.term(props)

  /** Check if at functional group open segment. */
  def isGroupOpen() = checkSegment(GSSegment)

  /** Parse start of a functional group. */
  def openGroup() =
    if (checkSegment(GSSegment)) {
      groupErrors.clear
      groupTransactionCount = 0
      parseSegment(GSSegment, None, "0000")
    } else throw new IllegalStateException("missing required GS segment")

  /** Check if at functional group close segment. */
  def isGroupClose() = checkSegment(GESegment)

  /** Parse close of a functional group. Returns number of transaction sets included in group. */
  def closeGroup(props: ValueMap) = {
    if (checkSegment(GESegment)) {
      val endprops = parseSegment(GESegment, None, "0000")
      if (props.get(groupControlKey) != endprops.get(groupControlEndKey)) groupErrors += GroupControlNumberMismatch
      if (endprops.get(numberOfSetsKey) != groupTransactionCount) groupErrors += GroupTransactionCountError
      endprops.get(numberOfSetsKey).asInstanceOf[Integer]
    } else {
      groupErrors += MissingGroupTrailer
      Integer valueOf (0)
    }
  }

  /** Check if at transaction set open segment. */
  def isSetOpen() = checkSegment(STSegment)

  /** Parse start of a transaction set. */
  def openSet() =
    if (checkSegment(STSegment)) {
      transactionErrors.clear
      oneOrMoreSegmentsInError = false
      lexer.resetSegmentNumber
      inTransaction = true
      val values = parseSegment(STSegment, None, "0000")
      groupTransactionCount += 1
      (values.get(transactionSetIdentifierKey).asInstanceOf[String], values)
    } else throw new IllegalStateException("missing required ST segment")

  /** Check if at transaction set close segment. */
  def isSetClose() = checkSegment(SESegment)

  /** Parse close of a transaction set. */
  def closeSet(props: ValueMap) = {
    if (checkSegment(SESegment)) {
      val endprops = parseSegment(SESegment, None, "0000")
      if (props.get(transactionSetControlKey) != endprops.get(transactionSetControlEndKey))
        transactionErrors += ControlNumberMismatch
      if (endprops.get(numberOfSegmentsKey) != lexer.getSegmentNumber) transactionErrors += WrongSegmentCount
      inTransaction = false
    } else transactionErrors += MissingTrailerTransaction
  }

  /** Discard input past end of current transaction. */
  def discardTransaction() = {
    while (lexer.currentType == SEGMENT && lexer.token != SESegment.ident) discardSegment
    discardSegment
  }

  /** Discard input to end of current group. */
  def discardToGroupEnd() =
    while (!isGroupClose)
      if (isSetOpen) {
        groupTransactionCount += 1
        discardTransaction
      } else discardSegment

  /** Parse transactions in group. */
  def parseGroup(interchange: ValueMap, group: ValueMap, groupCode: String, ackhead: ValueMap,
    transLists: java.util.Map[String, MapList]) = {
    val setacks = new MapListImpl
    while (!isGroupClose) {
      val (setid, setprops) = openSet
      val setack = new ValueMapImpl
      val ak2data = new ValueMapImpl
      setack put (segAK2 name, ak2data)
      ak2data put (segAK2.components(0) key, setprops get (transactionSetIdentifierKey))
      ak2data put (segAK2.components(1) key, setprops get (transactionSetControlKey))
      if (setprops containsKey implementationConventionKey)
        ak2data put (segAK2.components(2) key, setprops get (implementationConventionKey))
      val ak5data = new ValueMapImpl
      setack put (segAK5 name, ak5data)
      rejectTransaction = false
      var data: ValueMap = null
      schema.transactions(setid) match {
        case t: Transaction =>
          if (t.group == groupCode) {
            data = parseTransaction(t)
            data put (transactionGroup, group)
            data put (transactionSet, setprops)
            if (segmentErrors.nonEmpty) {
              val ak3s = JavaConversions.bufferAsJavaList(segmentErrors)
              setack put (segAK3 ident, ak3s)
            }
          } else transactionErrors += SetNotInGroup
        case _ => transactionErrors += NotSupportedTransaction
      }
      if (oneOrMoreSegmentsInError) transactionErrors += SegmentsInError
      if (transactionErrors.nonEmpty) rejectTransaction = true
      if (rejectTransaction) {
        ak5data put (segAK5.components(0) key, RejectedTransaction code)
        val limit = math.min(segAK5.components.length - 2, transactionErrors.length)
        (0 until limit) foreach (i => ak5data put (segAK5.components(i + 1) key, transactionErrors(i).code.toString))
      } else {
        val list = transLists get setid
        list add (data)
        groupAcceptCount += 1
        ak5data put (segAK5.components(0) key, AcceptedTransaction code)
      }
      if (rejectTransaction || segmentErrors.nonEmpty) setacks add setack
      closeSet(setprops)
    }
    if (setacks.size > 0) ackhead put (segAK2.ident, setacks)
  }

  /** Parse the input message. */
  def parse(): Try[ValueMap] = Try {

    import X12Acknowledgment._

    def matchIdentity(interQual: String, interId: String, usage: String, allowed: Array[IdentityInformation]) =
      allowed.filter { info =>
        info.interchangeQualifier == interQual && info.interchangeId == interId &&
          (info.interchangeType.length == 0 || info.interchangeType.indexOf(usage) >= 0)
      }

    def skipGroup(error: GroupSyntaxError) = {
      groupErrors += error
      discardToGroupEnd
    }

    val map = new ValueMapImpl
    val interchange = init
    val senders = matchIdentity(getRequiredString(SENDER_ID_QUALIFIER, interchange),
      getRequiredString(SENDER_ID, interchange), getRequiredString(TEST_INDICATOR, interchange), config.senderIds)
    if (senders.length == 0 && config.senderIds.length > 0)
      throw new LexicalException("Interchange sender infromation does not match configuration")
    val receivers = matchIdentity(getRequiredString(RECEIVER_ID_QUALIFIER, interchange),
      getRequiredString(RECEIVER_ID, interchange), getRequiredString(TEST_INDICATOR, interchange), config.receiverIds)
    if (receivers.length == 0 && config.receiverIds.length > 0)
      throw new LexicalException("Interchange receiver infromation does not match configuration")
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
      ackroot put (transactionInterSelfQualId, getRequiredValue(RECEIVER_ID_QUALIFIER, interchange))
      ackroot put (transactionInterSelfId, getRequiredValue(RECEIVER_ID, interchange))
      ackroot put (transactionInterPartnerQualId, getRequiredValue(SENDER_ID_QUALIFIER, interchange))
      ackroot put (transactionInterPartnerId, getRequiredValue(SENDER_ID, interchange))
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
      val code = getAsString(functionalIdentifierKey, "", group)
      if (functionalGroups.contains(code)) {
        val version = getAsString(versionIdentifierKey, "", group)
        if (version == schema.version) {
          parseGroup(interchange, group, code, ackhead, transLists)
        } else skipGroup(NotSupportedGroupVersion)
      } else skipGroup(NotSupportedGroup)
      val countPresent = closeGroup(group)
      val ak9data = new ValueMapImpl
      val error = ackhead.containsKey(segAK2.ident)
      ackhead put (segAK9 name, ak9data)
      val result =
        if (groupErrors.nonEmpty) RejectedGroup
        else if (groupTransactionCount == groupAcceptCount) if (error) AcceptedWithErrorsGroup else AcceptedGroup
        else if (groupAcceptCount > 0) PartiallyAcceptedGroup
        else RejectedGroup
      ak9data put (segAK9.components(0) key, result code)
      ak9data put (segAK9.components(1) key, Integer valueOf (countPresent))
      ak9data put (segAK9.components(2) key, Integer valueOf (groupTransactionCount))
      ak9data put (segAK9.components(3) key, Integer valueOf (groupAcceptCount))
      val limit = math.min(segAK9.components.length - 5, groupErrors.length)
      (0 until limit) foreach (i => ak9data put (segAK9.components(i + 4) key, groupErrors(i).code.toString))
      val sedata = new ValueMapImpl
      ackhead put (segSE name, sedata)
      sedata put (segSE.components(1) key, ackId toString)
      if (config.generate997) acksList add (ackroot)
      ackId += 1
    }
    term(interchange)
    map
  }
}