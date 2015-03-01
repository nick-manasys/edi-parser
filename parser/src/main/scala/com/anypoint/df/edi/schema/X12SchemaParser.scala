package com.anypoint.df.edi.schema

import java.io.{ InputStream, IOException }
import java.nio.charset.Charset
import java.util.{ Calendar, GregorianCalendar }
import scala.collection.JavaConversions
import scala.collection.mutable.Buffer
import scala.util.Try
import scala.util.Success
import com.anypoint.df.edi.lexical.{ ErrorHandler, LexerBase, LexicalException, X12Lexer }
import com.anypoint.df.edi.lexical.EdiConstants.DataType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition._
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.lexical.X12Lexer._
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
  val charSet: Charset, val receiverIds: Array[IdentityInformation], val senderIds: Array[IdentityInformation]) {
  if (receiverIds == null || senderIds == null) throw new IllegalArgumentException("receiver and sender id arrays cannot be null")
}

case class InterchangeException(note: InterchangeNoteCode, text: String) extends LexicalException(text)

/** Parser for X12 EDI documents. */
case class X12SchemaParser(in: InputStream, sc: EdiSchema, config: X12ParserConfig)
  extends SchemaParser(new X12Lexer(in, config charSet, -1, CharacterSet.EXTENDED),
    sc.merge(X12Acknowledgment.trans997)) with X12SchemaDefs {

  /** Set of functional groups supported by schema. */
  val functionalGroups = schema.transactions.values.map { t => t.group } toSet

  /** Flag for currently in a transaction. */
  var inTransaction = false

  /** Flag for transaction to be rejected because of errors. */
  var rejectTransaction = false

  /** Current segment reference, used in error reporting. */
  var currentSegment: Segment = null

  /** Control number for current interchange, used in error reporting. */
  var interchangeNumber = 0

  /** Segment number for interchange start. */
  var interchangeStartSegment = 0

  /** Flag for currently in a group. */
  var inGroup = false

  /** Control number for current group, used in error reporting. */
  var groupNumber = 0

  /** Segment number for group start. */
  var groupStartSegment = 0

  /** Number of transaction sets seen in current group. */
  var groupTransactionCount = 0

  /** Number of transaction sets accepted in current group. */
  var groupAcceptCount = 0

  /** Control number for current transaction, used in error reporting. */
  var transactionNumber = ""

  /** Starting segment number for transaction. */
  var transactionStartSegment = 0

  /** One or more segments of transaction in error flag. */
  var oneOrMoreSegmentsInError = false

  /** Interchange note code. */
  var interchangeNote: InterchangeNoteCode = null

  /** Interchange acknowledgment code. */
  var interchangeAck: InterchangeAcknowledgmentCode = null

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

  def positionInTransaction = s"segment ${lexer.getSegmentNumber - transactionStartSegment + 1} of transaction ${transactionNumber} in group $groupNumber of interchange $interchangeNumber"
  def positionInGroup = s"segment ${lexer.getSegmentNumber - groupStartSegment + 1} in group $groupNumber of interchange $interchangeNumber"
  def positionInInterchange = s"segment ${lexer.getSegmentNumber - interchangeStartSegment + 1} of interchange $interchangeNumber"
  def positionInMessage = s"segment ${lexer.getSegmentNumber}"

  def describeError(fatal: Boolean) = if (fatal) "fatal" else "recoverable"

  def describeComponent(incomp: Boolean) =
    if (incomp) s" for component '${currentSegment.components(lexer.getElementNumber - 1).name}'"
    else ""

  def logErrorInTransaction(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} transaction error '$text'${describeComponent(incomp)} at $positionInTransaction")

  def logTransactionEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} transaction error '$text'${describeComponent(incomp)} at $positionInGroup")

  def logGroupEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} group error '$text'${describeComponent(incomp)} at $positionInInterchange")

  def logInterchangeEnvelopeError(fatal: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} interchange error '$text'' at $positionInMessage")

  /** Accumulate element error, failing transaction if severe. */
  def addElementError(error: ElementSyntaxError) = {
    val fatal = checkFatal(error)
    if (inTransaction) {
      logErrorInTransaction(fatal, true, error.text)
      if (fatal) rejectTransaction = true
      val comp = currentSegment.components(lexer.getElementNumber)
      if (config.reportDataErrors) {
        val ak4 = new ValueMapImpl
        ak4 put (segAK4compC030.components(0).key, Integer.valueOf(lexer.getSegmentNumber - transactionStartSegment + 1))
        comp match {
          case ElementComponent(elem, _, _, _, _, _) => ak4 put (segAK4.components(1).key, Integer.valueOf(elem.ident))
          case _: CompositeComponent => ak4 put (segAK4compC030.components(1).key, Integer.valueOf(lexer.getComponentNumber))
        }
        if (comp.count != 1) ak4 put (segAK4compC030.components(2).key, Integer.valueOf(lexer.getRepetitionNumber))
        ak4 put (segAK4.components(2).key, error.code toString)
        if (error != InvalidCharacter) ak4 put (segAK4.components(3).key, lexer.token)
        dataErrors += ak4
      }
    } else if (inGroup) logTransactionEnvelopeError(true, true, error.text)
    else logGroupEnvelopeError(false, true, error.text)
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
  def parseSegment(segment: Segment, group: Option[GroupComponent], position: SegmentPosition): ValueMap = {
    if (logger.isTraceEnabled) logger.trace(s"parsing segment ${segment.ident} at position $position")
    val map = new ValueMapImpl
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
      ak3data put (segAK3.components(1).key, Integer.valueOf(lexer.getSegmentNumber - transactionStartSegment))
      group.foreach(ident => ak3data put (segAK3.components(2).key, ident))
      ak3data put (segAK3.components(3).key, DataErrorsSegment.code.toString)
      ak3 put (segAK3.name, ak3data)
      ak3 put (segAK4.ident, JavaConversions.bufferAsJavaList(dataErrors.reverse))
      segmentErrors += ak3
      oneOrMoreSegmentsInError = true
    }
    if (logger.isDebugEnabled) logger.trace(s"now positioned at segment '${lexer.token}'")
    map
  }

  /** Report segment error. */
  def segmentError(ident: String, group: Option[String], error: ComponentErrors.ComponentError) = {
    def addError(fatal: Boolean, error: SegmentSyntaxError) = {
      oneOrMoreSegmentsInError = true
      if (config.reportDataErrors) {
        val ak3 = new ValueMapImpl
        val ak3data = new ValueMapImpl
        ak3 put (segAK3.name, ak3data)
        ak3data put (segAK3.components(0).key, ident)
        ak3data put (segAK3.components(1).key, Integer.valueOf(lexer.getSegmentNumber - transactionStartSegment + 1))
        group.foreach(ident => ak3data put (segAK3.components(2).key, ident))
        ak3data put (segAK3.components(3).key, error.code.toString)
        segmentErrors += ak3
      }
      logErrorInTransaction(fatal, false, s"${error.text}: $ident")
      if (fatal) rejectTransaction = true
    }

    error match {
      case ComponentErrors.TooManyLoops => addError(config.occursFail, TooManyLoops)
      case ComponentErrors.TooManyRepetitions => addError(config.occursFail, TooManyOccurs)
      case ComponentErrors.MissingRequired => addError(true, MissingMandatorySegment)
      case ComponentErrors.UnknownSegment => addError(config.unknownFail, UnrecognizedSegment)
      case ComponentErrors.OutOfOrderSegment => addError(config.orderFail, OutOfOrderSegment)
      case ComponentErrors.UnusedSegment => if (config.unusedFail) addError(true, UnexpectedSegment)
    }
  }

  /** Check if at interchange envelope segment. */
  def isInterchangeEnvelope = lexer.currentType == SEGMENT &&
    (lexer.token == InterchangeStartSegment || lexer.token == InterchangeEndSegment)

  /** Check if at functional group envelope segment. */
  def isGroupEnvelope = checkSegment(GSSegment) || checkSegment(GESegment)

  /** Check if at functional group open segment. */
  def isGroupOpen = checkSegment(GSSegment)

  def groupError(error: GroupSyntaxError) = {
    groupErrors += error
    logGroupEnvelopeError(true, false, error.text)
  }

  /** Parse start of a functional group. */
  def openGroup =
    if (checkSegment(GSSegment)) {
      groupErrors.clear
      groupStartSegment = lexer.getSegmentNumber
      groupTransactionCount = 0
      val map = parseSegment(GSSegment, None, SegmentPosition(0, "0000"))
      inGroup = true
      map
    } else throw new IllegalStateException("missing required GS segment")

  /** Check if at functional group close segment. */
  def isGroupClose = checkSegment(GESegment)

  /** Parse close of a functional group. Returns number of transaction sets included in group. */
  def closeGroup(props: ValueMap) = {
    inGroup = false
    if (checkSegment(GESegment)) {
      val endprops = parseSegment(GESegment, None, SegmentPosition(0, "9999"))
      if (props.get(groupControlKey) != endprops.get(groupControlEndKey)) groupError(GroupControlNumberMismatch)
      if (endprops.get(numberOfSetsKey) != groupTransactionCount) groupError(GroupTransactionCountError)
      endprops.get(numberOfSetsKey).asInstanceOf[Integer]
    } else {
      groupError(MissingGroupTrailer)
      Integer valueOf (0)
    }
  }

  def transactionError(error: TransactionSyntaxError) = {
    transactionErrors += error
    logErrorInTransaction(true, false, error.text)
  }

  /** Check if at transaction set open segment. */
  def isSetOpen = checkSegment(STSegment)

  /** Parse start of a transaction set. */
  def openSet =
    if (checkSegment(STSegment)) {
      transactionErrors.clear
      oneOrMoreSegmentsInError = false
      inTransaction = true
      transactionStartSegment = lexer.getSegmentNumber
      val values = parseSegment(STSegment, None, SegmentPosition(0, "0000"))
      groupTransactionCount += 1
      (values.get(transactionSetIdentifierKey).asInstanceOf[String], values)
    } else throw new IllegalStateException("not positioned at ST segment")

  /** Check if at transaction set close segment. */
  def isSetClose = checkSegment(SESegment)

  /** Parse close of a transaction set. */
  def closeSet(props: ValueMap) = {
    if (checkSegment(SESegment)) {
      val endprops = parseSegment(SESegment, None, SegmentPosition(0, "9999"))
      if (props.get(transactionSetControlKey) != endprops.get(transactionSetControlEndKey)) {
        transactionError(ControlNumberMismatch)
      }
      val segcount = lexer.getSegmentNumber - transactionStartSegment
      if (endprops.get(numberOfSegmentsKey) != Integer.valueOf(segcount)) transactionError(WrongSegmentCount)
      inTransaction = false
    } else transactionError(MissingTrailerTransaction)
  }

  /** Convert loop start or end segment to identity form. If not at a loop segment, this just returns None. */
  def convertLoop = if (Set("LS", "LE").contains(lexer.token)) Some(lexer.token + lexer.peek) else None

  /** Discard input past end of current transaction. */
  def discardTransaction = {
    while (lexer.currentType != SEGMENT || lexer.token != SESegment.ident) discardSegment
    discardSegment
  }

  /** Discard input to end of current group. */
  def discardToGroupEnd =
    while (!isGroupClose)
      if (isSetOpen) {
        groupTransactionCount += 1
        discardTransaction
      } else discardSegment

  /** Discard input past end of current interchange. */
  def discardInterchange = {
    while (lexer.currentType != SEGMENT || lexer.token != "IEA") discardSegment
    discardSegment
  }

  /** Parse transactions in group. */
  def parseGroup(interchange: ValueMap, group: ValueMap, groupCode: String, ackhead: ValueMap,
    transLists: java.util.Map[String, MapList]) = {
    val setacks = new MapListImpl
    while (lexer.currentType != END && !isGroupEnvelope && !isInterchangeEnvelope) {
      if (isSetOpen) {
        val (setid, setprops) = openSet
        transactionNumber = getRequiredString(transactionSetControlKey, setprops)
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
            } else transactionError(SetNotInGroup)
          case _ => transactionError(NotSupportedTransaction)
        }
        while (lexer.currentType != END && !isEnvelopeSegment(lexer.token)) {
          logger.error(s"discarding $positionInTransaction (${lexer.token}) found when looking for transaction set end")
          discardSegment
        }
        if (isSetClose) closeSet(setprops)
        else transactionError(MissingTrailerTransaction)
        if (oneOrMoreSegmentsInError) transactionError(SegmentsInError)
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
      } else {
        logger.error(s"discarding $positionInGroup (${lexer.token}) found when looking for transaction set start")
        discardSegment
      }
    }
    if (setacks.size > 0) ackhead put (segAK2.ident, setacks)
  }

  def init(data: ValueMap): X12Lexer.InterchangeStartStatus = lexer.asInstanceOf[X12Lexer].init(data)

  def term(props: ValueMap): X12Lexer.InterchangeEndStatus = lexer.asInstanceOf[X12Lexer].term(props)

  /** Parse the input message. */
  def parse: Try[ValueMap] = Try {

    def matchIdentity(interQual: String, interId: String, usage: String, allowed: Array[IdentityInformation]) =
      allowed.filter { info =>
        info.interchangeQualifier == interQual && info.interchangeId == interId &&
          (info.interchangeType.length == 0 || info.interchangeType.indexOf(usage) >= 0)
      }

    def skipGroup(error: GroupSyntaxError) = {
      groupErrors += error
      discardToGroupEnd
    }

    def buildDelims = {
      val builder = new StringBuilder
      builder append (lexer.getDataSeparator)
      builder append (lexer.getComponentSeparator)
      builder append (if (lexer.getRepetitionSeparator < 0) 'U' else lexer.getRepetitionSeparator.asInstanceOf[Char])
      builder append (lexer.getSegmentTerminator)
      builder append (if (lexer.getReleaseIndicator < 0) 'U' else lexer.getReleaseIndicator.asInstanceOf[Char])
      builder toString
    }

    val map = new ValueMapImpl
    val interAckList = new MapListImpl
    map put (interchangeAcknowledgments, interAckList)
    val ta1map = new ValueMapImpl
    val funcAckList = new MapListImpl
    map put (functionalAcknowledgments, funcAckList)
    val transLists = new ValueMapImpl().asInstanceOf[java.util.Map[String, MapList]]
    schema.transactions.keys foreach { key => transLists put (key, new MapListImpl) }
    map put (transactionsMap, transLists)

    def buildTA1(ack: InterchangeAcknowledgmentCode, note: InterchangeNoteCode, interchange: ValueMap) = {
      ta1map put (segTA1.components(0) key, interchange get (INTER_CONTROL))
      val calendar = new GregorianCalendar
      ta1map put (segTA1.components(1) key, calendar)
      val milli = (calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000
      ta1map put (segTA1.components(2) key, Integer valueOf (milli))
      ta1map put (segTA1.components(3) key, ack code)
      ta1map put (segTA1.components(4) key, note code)
      interAckList add ta1map
    }

    def buildAckRoot(interchange: ValueMap) = {
      val ackroot = new ValueMapImpl
      ackroot put (transactionId, trans997 ident)
      ackroot put (transactionName, trans997 name)
      ackroot put (transactionInterSelfQualId, getRequiredValue(RECEIVER_ID_QUALIFIER, interchange))
      ackroot put (transactionInterSelfId, getRequiredValue(RECEIVER_ID, interchange))
      ackroot put (transactionInterPartnerQualId, getRequiredValue(SENDER_ID_QUALIFIER, interchange))
      ackroot put (transactionInterPartnerId, getRequiredValue(SENDER_ID, interchange))
      ackroot
    }

    def parseInterchange(interchange: ValueMap): InterchangeNoteCode = {
      lexer.setHandler(X12ErrorHandler)
      val senders = matchIdentity(getRequiredString(SENDER_ID_QUALIFIER, interchange),
        getRequiredString(SENDER_ID, interchange), getRequiredString(TEST_INDICATOR, interchange), config.senderIds)
      if (senders.length == 0 && config.senderIds.length > 0)
        throw new LexicalException("Interchange sender infromation does not match configuration")
      val receivers = matchIdentity(getRequiredString(RECEIVER_ID_QUALIFIER, interchange),
        getRequiredString(RECEIVER_ID, interchange), getRequiredString(TEST_INDICATOR, interchange), config.receiverIds)
      if (receivers.length == 0 && config.receiverIds.length > 0)
        throw new LexicalException("Interchange receiver information does not match configuration")
      map put (delimiterCharacters, buildDelims)
      map put (interchangeVersionId, getRequiredString(VERSION_ID, interchange))
      var ackId = 1
      var interNote: InterchangeNoteCode = InterchangeNoError
      while (lexer.currentType != END && !isInterchangeEnvelope) {
        if (isGroupOpen) {
          val group = openGroup
          groupStartSegment = lexer.getSegmentNumber - 2
          groupNumber = getRequiredInt(groupControlKey, group)
          group put (groupInterchange, interchange)
          lexer.countGroup
          val ackroot = buildAckRoot(interchange)
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
          val code = getAs(functionalIdentifierKey, "", group)
          if (functionalGroups.contains(code)) {
            if (group.get(responsibleAgencyKey) != "X" || group.get(versionIdentifierKey) == schema.version) {
              parseGroup(interchange, group, code, ackhead, transLists)
            } else skipGroup(NotSupportedGroupVersion)
          } else skipGroup(NotSupportedGroup)
          val countPresent = closeGroup(group)
          val ak9data = new ValueMapImpl
          val error = ackhead.containsKey(segAK2 ident)
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
          if (config.generate997) funcAckList add (ackroot)
          ackId += 1
        } else {
          logger.error(s"discarding $positionInInterchange (${lexer.token}) found when looking for group start")
          interNote = InterchangeInvalidContent
          discardSegment
        }
      }
      if (lexer.currentType == END) {
        logger.error(s"end of file with missing $InterchangeEndSegment")
        InterchangeEndOfFile
      } else if (lexer.token == InterchangeEndSegment) {
        LexerEndStatusInterchangeNote get term(interchange) foreach (code => {
            val text = s"Irrecoverable error in $InterchangeEndSegment at ${lexer.getSegmentNumber}" +
              (if (interchange.containsKey(INTER_CONTROL)) " with control number " + interchange.get(INTER_CONTROL)) +
              ": " + code.text
            logger error text
        })
        interNote
      } else {
        logInterchangeEnvelopeError(true, s"Missing $InterchangeEndSegment")
        InterchangeInvalidControlStructure
      }
    }

    var done = false
    while (!done) {
      val interchange = new ValueMapImpl
      try {
        lexer.setHandler(null)
        val result = init(interchange)
        if (result == InterchangeStartStatus.NO_DATA) done = true
        else {
          LexerStartStatusInterchangeNote get (result) foreach (code => {
            val text = s"Irrecoverable error in $InterchangeStartSegment at ${lexer.getSegmentNumber}" +
              (if (interchange.containsKey(INTER_CONTROL)) " with control number " + interchange.get(INTER_CONTROL)) +
              ": " + code
            logger error text
            throw InterchangeException(code, text)
          })
          interchangeStartSegment = lexer.getSegmentNumber - 1
          interchangeNumber = getRequiredInt(INTER_CONTROL, interchange)
          parseInterchange(interchange) match {
            case InterchangeNoError => buildTA1(AcknowledgedNoErrors, InterchangeNoError, interchange)
            case note => buildTA1(AcknowledgedWithErrors, note, interchange)
          }
        }
      } catch {
        case InterchangeException(note, text) => {
          buildTA1(AcknowledgedRejected, note, interchange)
          discardInterchange
        }
        case e: IOException => {
          buildTA1(AcknowledgedRejected, InterchangeEndOfFile, interchange)
          throw e
        }
      }
    }
    map
  }
}