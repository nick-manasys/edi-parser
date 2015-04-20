package com.anypoint.df.edi.schema

import java.io.{ InputStream, IOException }
import java.nio.charset.Charset
import java.util.{ Calendar, GregorianCalendar }
import scala.annotation.tailrec
import scala.collection.JavaConversions
import scala.collection.mutable.Buffer
import scala.util.Try
import scala.util.Success
import com.anypoint.df.edi.lexical.{ ErrorHandler, LexerBase, LexicalException, EdifactLexer }
import com.anypoint.df.edi.lexical.EdiConstants.DataType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition._
import com.anypoint.df.edi.lexical.EdifactConstants._
import com.anypoint.df.edi.lexical.EdifactLexer._
import EdiSchema._
import SchemaJavaValues._
import EdifactAcknowledgment._

/** Interchange identity information used by EDIFACT. All values present are matched against received interchanges.
  * @param identification required interchange identification
  * @param codeQualifier identification code qualifier (<code>null</code> if not used)
  * @param internalIdent internal identification (<code>null</code> if not used)
  * @param internalSubIdent internal sub-identification (<code>null</code> if not used)
  */
case class EdifactIdentityInformation(identification: String, codeQualifier: String, internalIdent: String, internalSubIdent: String) {
  if (identification == null) throw new IllegalArgumentException("identification value cannot be null")
}

/** Configuration parameters for EDIFACT schema parser. If either receiver or sender identity information is included it
  * is verified in processed messages.
  */
case class EdifactParserConfig(val lengthFail: Boolean, val charFail: Boolean, val countFail: Boolean,
  val unknownFail: Boolean, val orderFail: Boolean, val unusedFail: Boolean, val occursFail: Boolean,
  val substitutionChar: Int, val charSet: Charset, val receiverIds: Array[EdifactIdentityInformation],
  val senderIds: Array[EdifactIdentityInformation]) {
  if (receiverIds == null || senderIds == null) throw new IllegalArgumentException("receiver and sender id arrays cannot be null")
}

/** Validator called by parser to check that received interchange/group/message identifiers are not duplicates. */
trait EdifactNumberValidator {

  /** Generate unique context identifier for interchange sender-receiver pair. The returned identifier is saved and
    * passed to the other methods in order to identify the context of the interchange, so the form of the identifier is
    * entirely up to the implementation.
    * @param senderId interchange sender identification (required, non-<code>null</code>)
    * @param senderCode interchange sender code qualifier (<code>null</code> if unused)
    * @param senderInternal interchange sender internal identification or routing (<code>null</code> if unused)
    * @param senderInternalSub interchange sender internal sub-identification (<code>null</code> if unused)
    * @param receiverId interchange receiver identification (required, non-<code>null</code>)
    * @param receiverCode interchange receiver code qualifier (<code>null</code> if unused)
    * @param receiverInternal interchange receiver internal identification or routing (<code>null</code> if unused)
    * @param receiverInternalSub interchange receiver internal sub-identification (<code>null</code> if unused)
    */
  def contextToken(senderId: String, senderCode: String, senderInternal: String, senderInternalSub: String,
    receiverId: String, receiverCode: String, receiverInternal: String, receiverInternalSub: String): String

  /** Validate receive interchange identification.
    * @param controlRef interchange control reference
    * @param context interchange sender-receiver pair context token
    */
  def validateInterchange(controlRef: String, context: String): Boolean

  /** Validate receive group identification. The combination of all values should always be unique within an
    * interchange.
    * @param groupRef group reference number
    * @param senderId application sender identification (<code>null</code> if unused)
    * @param senderCode application sender code qualifier (<code>null</code> if unused)
    * @param receiverId application receiver identification (<code>null</code> if unused)
    * @param receiverCode application receiver code qualifier (<code>null</code> if unused)
    * @param context interchange sender-receiver pair context token
    */
  def validateGroup(groupRef: String, senderId: String, senderCode: String, receiverId: String, receiverCode: String,
    context: String): Boolean

  /** Validate received message identification.
    * @param msgRef message reference number
    * @param msgType message type (required, non-<code>null</code>)
    * @param msgVersion message version number (required, non-<code>null</code>)
    * @param msgRelease message release number (required, non-<code>null</code>)
    * @param agencyCode controlling agency code (required, non-<code>null</code>)
    * @param associationCode association assigned code (<code>null</code> if unused)
    * @param directoryVersion code list directory version number (<code>null</code> if unused)
    * @param subFunction message type sub-function identification (<code>null</code> if unused)
    * @param context interchange sender-receiver pair context token
    */
  def validateMessage(msgRef: String, msgType: String, msgVersion: String, msgRelease: String, agencyCode: String,
    associationCode: String, directoryVersion: String, subFunction: String, context: String): Boolean
}

/** Parser for EDIFACT EDI documents. */
case class EdifactSchemaParser(in: InputStream, sc: EdiSchema, numval: EdifactNumberValidator, config: EdifactParserConfig)
  extends SchemaParser(new EdifactLexer(in, config.charSet, config.substitutionChar), sc) with EdifactSchemaDefs {

  /** Actual version set after reading interchange headers. */
  var schemaDefs: EdifactVersionDefs = ControlV4Defs

  /** Flag for currently in a message. */
  var inMessage = false

  /** Flag for message to be rejected because of errors. */
  var rejectMessage = false

  /** Current segment reference, used in error reporting. */
  var currentSegment: Segment = null

  /** Flag for error reported in current segment. */
  var segmentError = false

  /** Control reference for current interchange, used in error reporting. */
  var interchangeReference = ""

  /** Segment number for interchange start. */
  var interchangeStartSegment = 0

  /** Flag for currently in a group. */
  var inGroup = false

  /** Group reference number for current group, used in error reporting. */
  var groupReference = ""

  /** Segment number for group start. */
  var groupStartSegment = 0

  /** Number of messages seen in current group. */
  var groupMessageCount = 0

  /** Message reference number for current message, used in error reporting. */
  var messageReference = ""

  /** Starting segment number for message. */
  var messageStartSegment = 0

  /** Data error details. This corresponds to an S011 composite structure, as used in CONTRL message UCI, UCF, UCM, and
    * UCD segments.
    */
  case class DataError(elementPosition: Int, componentPosition: Int, repeat: Int)

  /** Exception reporting problem in interchange. */
  case class InterchangeException(error: SyntaxError, text: String) extends RuntimeException(text)

  /** Accumulated data errors from segment. */
  val segmentErrors = Buffer[DataError]()

  /** Segment error details. This corresponds to a CONTRL message Group 2 or Group 5 structure. */
  case class SegmentErrorReport(segmentPosition: Int, errorCode: String, dataErrors: List[DataError])

  /** Accumulated segment errors for message. */
  val messageErrors = Buffer[SegmentErrorReport]()

  /** Lexical error handler. */
  case object EdifactErrorHandler extends ErrorHandler {
    def error(lexer: LexerBase, typ: DataType, error: ErrorCondition, explain: java.lang.String): Unit = {
      // TODO: expand errors reported by lexer to match EDIFACT list
      addElementError(error match {
        case TOO_SHORT => ElementTooShort
        case TOO_LONG => ElementTooLong
        case INVALID_CHARACTER => InvalidSyntaxCharacter
        case INVALID_CODE => ValuePositionNotSupported
        case INVALID_DATE => InvalidSimpleValue
        case INVALID_TIME => InvalidSimpleValue
      })
    }
  }

  /** Check if an element syntax error condition is fatal for the containing message. */
  def checkFatal(error: SyntaxError) = error match {
    case TooManyConstituents => config countFail
    case ElementTooShort => config lengthFail
    case ElementTooLong => config lengthFail
    case InvalidSyntaxCharacter => config charFail
    case TooManySegmentRepetitions => config countFail
    case TooManyGroupRepetitions => config countFail
    case _ => true
  }

  def positionGroupNumber = if (inGroup) s" in group $groupReference" else ""
  def positionInMessage = s"segment ${lexer.getSegmentNumber - messageStartSegment + 1} of message ${messageReference}${positionGroupNumber} of interchange $interchangeReference"
  def positionInGroup = s"segment ${lexer.getSegmentNumber - groupStartSegment + 1} in group $groupReference of interchange $interchangeReference"
  def positionInInterchange = s"segment ${lexer.getSegmentNumber - interchangeStartSegment + 1} of interchange $interchangeReference"
  def positionInText = s"segment ${lexer.getSegmentNumber}"

  def describeError(fatal: Boolean) = if (fatal) "fatal" else "recoverable"

  def describeComponent(incomp: Boolean) =
    if (incomp) {
      val index = 0 max (lexer.getElementNumber - 1)
      s" for component '${currentSegment.components(index).name}'"
    } else ""

  def logErrorInMessage(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} message error '$text'${describeComponent(incomp)} at $positionInMessage")

  def logMessageEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} message error '$text'${describeComponent(incomp)} at $positionInGroup")

  def logGroupEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} group error '$text'${describeComponent(incomp)} at $positionInInterchange")

  def logInterchangeEnvelopeError(fatal: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} interchange error '$text'' at $positionInMessage")

  // segment error handling:
  // can report multiple data errors per normal segment (though not for control segments)
  // each data error is an S011 composite structure (but keys differ from one segment to another, so use case class)
  // segment errors don't have to be fatal for the containing message, but no way to report if processing them

  /** Accumulate element error, failing message if severe. */
  def addElementError(error: SyntaxError) = {
    val fatal = checkFatal(error)
    if (inMessage) {
      logErrorInMessage(fatal, true, error.text)
      if (fatal) rejectMessage = true
      val comp = currentSegment.components(lexer.getElementNumber)
      //      if (config.reportDataErrors) {
      //        val ak4 = new ValueMapImpl
      //        ak4 put (segAK4compC030.components(0).key, Integer.valueOf(lexer.getSegmentNumber - messageStartSegment + 1))
      //        comp match {
      //          case ElementComponent(elem, _, _, _, _, _) => ak4 put (segAK4.components(1).key, Integer.valueOf(elem.ident))
      //          case _: CompositeComponent => ak4 put (segAK4compC030.components(1).key, Integer.valueOf(lexer.getComponentNumber))
      //        }
      //        if (comp.count != 1) ak4 put (segAK4compC030.components(2).key, Integer.valueOf(lexer.getRepetitionNumber))
      //        ak4 put (segAK4.components(2).key, error.code toString)
      //        if (error != InvalidCharacter) ak4 put (segAK4.components(3).key, lexer.token)
      //        dataErrors += ak4
      //      }
    } else if (inGroup) logMessageEnvelopeError(true, true, error.text)
    else logGroupEnvelopeError(false, true, error.text)
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap) = {
    def checkParse(comp: SegmentComponent, of: ItemType) =
      if (of == lexer.currentType) {
        if (lexer.token.length > 0) parseComponent(comp, map)
        else {
          if (comp.usage == MandatoryUsage) addElementError(MissingRequiredValue)
          lexer.advance
        }
      } else if (comp.usage == MandatoryUsage) addElementError(MissingRequiredValue)

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
    segmentError = false
    currentSegment = segment
    lexer.advance
    parseCompList(segment.components, DATA_ELEMENT, DATA_ELEMENT, map)
    lexer.currentType match {
      case SEGMENT | END =>
      case _ => {
        addElementError(TooManyConstituents)
        discardSegment
      }
    }
    if (logger.isDebugEnabled) logger.trace(s"now positioned at segment '${lexer.token}'")
    map
  }

  /** Report segment error. */
  def segmentError(ident: String, group: Option[String], error: ComponentErrors.ComponentError) = {
    def addError(fatal: Boolean, error: SyntaxError) = {
      segmentError = true
      logErrorInMessage(fatal, false, s"${error.text}: $ident")
      if (fatal) rejectMessage = true
    }

    error match {
      case ComponentErrors.TooManyLoops => addError(config.occursFail, TooManyGroupRepetitions)
      case ComponentErrors.TooManyRepetitions => addError(config.occursFail, TooManySegmentRepetitions)
      case ComponentErrors.MissingRequired => addError(true, MissingRequiredValue)
      case ComponentErrors.UnknownSegment => addError(config.unknownFail, InvalidOccurrence)
      case ComponentErrors.OutOfOrderSegment => addError(config.orderFail, NotSupportedInPosition)
      case ComponentErrors.UnusedSegment => if (config.unusedFail) addError(true, NotSupportedInPosition)
    }
  }

  /** Check if at interchange envelope segment. */
  def isInterchangeEnvelope = lexer.currentType == SEGMENT &&
    (lexer.token == schemaDefs.segUNB.ident || lexer.token == schemaDefs.segUNZ.ident)

  /** Check if at functional group envelope segment. */
  def isGroupEnvelope = checkSegment(schemaDefs.segUNG) || checkSegment(schemaDefs.segUNE)

  /** Check if at functional group open segment. */
  def isGroupOpen = checkSegment(schemaDefs.segUNG)

  def groupError(error: SyntaxError) = {
    logGroupEnvelopeError(true, false, error.text)
  }

  /** Parse start of a functional group. */
  def openGroup =
    if (checkSegment(schemaDefs.segUNG)) {
      groupStartSegment = lexer.getSegmentNumber
      groupMessageCount = 0
      val map = parseSegment(schemaDefs.segUNG, None, SegmentPosition(0, "0000"))
      inGroup = true
      map
    } else throw new IllegalStateException("not at UNG segment")

  /** Check if at functional group close segment. */
  def isGroupClose = checkSegment(schemaDefs.segUNE)

  /** Parse close of a functional group. Returns number of message sets included in group. */
  def closeGroup(props: ValueMap) = {
    inGroup = false
    if (checkSegment(schemaDefs.segUNE)) {
      val endprops = parseSegment(schemaDefs.segUNE, None, SegmentPosition(0, "9999"))
      if (props.get(groupHeadReferenceKey) != endprops.get(groupTrailReferenceKey)) groupError(ControlReferenceMismatch)
      if (endprops.get(groupTrailCountKey) != groupMessageCount) groupError(ControlCountMismatch)
      endprops.get(groupTrailCountKey).asInstanceOf[Integer]
    } else {
      groupError(MissingRequiredValue)
      Integer valueOf (0)
    }
  }

  def messageError(error: SyntaxError) = {
    logErrorInMessage(true, false, error.text)
  }

  /** Check if at message set open segment. */
  def isSetOpen = checkSegment(schemaDefs.segUNH)

  /** Parse start of a message set. */
  def openSet =
    if (checkSegment(schemaDefs.segUNH)) {
      messageErrors.clear
      inMessage = true
      messageStartSegment = lexer.getSegmentNumber
      val values = parseSegment(schemaDefs.segUNH, None, SegmentPosition(0, "0000"))
      groupMessageCount += 1
      (values.get(msgHeadMessageTypeKey).asInstanceOf[String], values)
    } else throw new IllegalStateException(s"not positioned at ${schemaDefs.segUNH.ident} segment")

  /** Check if at message set close segment. */
  def isSetClose = checkSegment(schemaDefs.segUNT)

  /** Parse close of a message set. */
  def closeSet(props: ValueMap) = {
    if (checkSegment(schemaDefs.segUNT)) {
      val endprops = parseSegment(schemaDefs.segUNT, None, SegmentPosition(0, "9999"))
      if (props.get(msgHeadReferenceKey) != endprops.get(msgTrailReferenceKey)) {
        messageError(ControlReferenceMismatch)
      }
      val segcount = lexer.getSegmentNumber - messageStartSegment
      if (endprops.get(msgTrailCountKey) != Integer.valueOf(segcount)) messageError(ControlCountMismatch)
      inMessage = false
    } else messageError(MissingRequiredValue)
  }

  /** Convert loop start or end segment to identity form. If not at a loop segment, this just returns None. */
  def convertLoop = if (Set("LS", "LE").contains(lexer.token)) Some(lexer.token + lexer.peek) else None

  /** Discard input past end of current message. */
  def discardTransaction = {
    while (lexer.currentType != SEGMENT || lexer.token != schemaDefs.segUNH.ident) discardSegment
    discardSegment
  }

  /** Discard input to end of current group. */
  def discardToGroupEnd =
    while (!isGroupClose)
      if (isSetOpen) {
        groupMessageCount += 1
        discardTransaction
      } else discardSegment

  /** Discard input past end of current interchange. */
  def discardInterchange = {
    while (lexer.currentType != SEGMENT || lexer.token != "IEA") discardSegment
    discardSegment
  }

  /** Parse messages in group. */
  def parseGroup(interchange: ValueMap, group: ValueMap, groupCode: String, ackhead: ValueMap,
    transLists: java.util.Map[String, MapList], providerId: String, groupSender: String, groupReceiver: String) = {
  }

  def init(data: ValueMap) = lexer.asInstanceOf[EdifactLexer].init(data)

  /** Parse the input message. */
  def parse: Try[ValueMap] = Try {

    def valueOrNull[T](index: Int, values: Array[T]): T = if (index < values.length) values(index) else null.asInstanceOf[T]

    def buildIdentity(values: Array[String]) = EdifactIdentityInformation(values(0), valueOrNull(1, values),
      valueOrNull(2, values), valueOrNull(3, values))

    def matchIdentity(identity: EdifactIdentityInformation, allowed: Array[EdifactIdentityInformation]) =
      allowed.filter { info =>
        identity.identification == info.identification &&
          (info.codeQualifier == null || info.codeQualifier == identity.codeQualifier) &&
          (info.internalIdent == null || info.internalIdent == identity.internalIdent) &&
          (info.internalSubIdent == null || info.internalSubIdent == identity.internalSubIdent)
      }

    def skipGroup(error: SyntaxError) = {
      //      groupErrors += error
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
    map put (interchangeAcksGenerated, interAckList)
    val funcAckList = new MapListImpl
    map put (functionalAcknowledgments, funcAckList)
    val transLists = new ValueMapImpl().asInstanceOf[java.util.Map[String, MapList]]
    //    schema.messages.keys foreach { key => transLists put (key, new MapListImpl) }
    //    map put (messagesMap, transLists)

    def buildCONTRL(interchange: ValueMap) = {
      val ta1map = new ValueMapImpl
      //      ta1map put (segTA1.components(0) key, interchange get (INTER_CONTROL))
      val calendar = new GregorianCalendar
      //      ta1map put (segTA1.components(1) key, calendar)
      //      val milli = (calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000
      //      ta1map put (segTA1.components(2) key, Integer valueOf (milli))
      //      ta1map put (segTA1.components(3) key, ack code)
      //      ta1map put (segTA1.components(4) key, note code)
      interAckList add ta1map
    }

    def buildAckRoot(interchange: ValueMap) = {
      val ackroot = new ValueMapImpl
      //      ackroot put (messageId, trans997 ident)
      //      ackroot put (messageName, trans997 name)
      //      ackroot put (messageInterSelfQualId, getRequiredValue(RECEIVER_ID_QUALIFIER, interchange))
      //      ackroot put (messageInterSelfId, getRequiredValue(RECEIVER_ID, interchange))
      //      ackroot put (messageInterPartnerQualId, getRequiredValue(SENDER_ID_QUALIFIER, interchange))
      //      ackroot put (messageInterPartnerId, getRequiredValue(SENDER_ID, interchange))
      ackroot
    }

    /** Builds array of string values matching the simple value components. */
    def getStrings(comps: List[SegmentComponent], data: ValueMap) = {
      @tailrec
      def getr(rem: List[SegmentComponent], acc: List[String]): List[String] = rem match {
        case (h: ElementComponent) :: t => getr(t, data.get(h.key).asInstanceOf[String] :: acc)
        case h :: t => getr(t, acc)
        case _ => acc.reverse
      }
      getr(comps, Nil).toArray
    }

    def parseTransaction = {
      val (setid, setprops) = openSet
    }

    var done = false
    while (!done) {

      // parse the interchange header segment(s)
      val interchange = new ValueMapImpl
      try {
        lexer.setHandler(null)
        val version = init(interchange)
        schemaDefs = versions(version)
        parseCompList(schemaDefs.segUNB.components.tail, ItemType.DATA_ELEMENT, ItemType.DATA_ELEMENT, interchange)
      } catch {
        case e: LexicalException => {
          logger.error(s"Unable to process message due to error in interchange header: ${e.getMessage}")
          throw e
        }
      }
      try {

        // initialize for interchange
        interchangeStartSegment = lexer.getSegmentNumber - 1
        interchangeReference = getRequiredString(interHeadReferenceKey, interchange)
        val sender = getStrings(schemaDefs.interHeadSender.components, interchange)
        val recipient = getStrings(schemaDefs.interHeadRecipient.components, interchange)
        val providerId = numval.contextToken(valueOrNull(0, sender), valueOrNull(1, sender), valueOrNull(2, sender),
          valueOrNull(3, sender), valueOrNull(0, recipient), valueOrNull(1, recipient), valueOrNull(2, recipient),
          valueOrNull(3, recipient))
        if (numval.validateInterchange(interchangeReference, providerId)) {
          val senders = matchIdentity(buildIdentity(sender), config.senderIds)
          if (senders.length == 0 && config.senderIds.length > 0)
            throw new InterchangeException(UnknownInterchangeSender, "Interchange sender infromation does not match configuration")
          val receivers = matchIdentity(buildIdentity(recipient), config.receiverIds)
          if (receivers.length == 0 && config.receiverIds.length > 0)
            throw InterchangeException(UnknownInterchangeRecipient, "Interchange recipient information does not match configuration")
          var ackId = 1
          while (lexer.currentType != END && !isInterchangeEnvelope) {
            if (isGroupEnvelope) {
              val groupmap = openGroup
              while (!isGroupClose) {
                parseTransaction
              }
            } else if (isSetOpen) {
              parseTransaction
            } else {
              val text = s"Unexpected segment ${lexer.token} at ${lexer.getSegmentNumber}"
              logger error text
              throw InterchangeException(InvalidOccurrence, text)
            }
          }
        } else {
          val text = s"Duplicate interchange control number $interchangeReference in UNB at ${lexer.getSegmentNumber}"
          logger error text
          throw InterchangeException(DuplicateDetected, text)
        }
      } catch {
        case InterchangeException(error, text) => {
          discardInterchange
        }
        case e: IOException => {
          throw e
        }
      }
    }
    map
  }
}