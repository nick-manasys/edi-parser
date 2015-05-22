package com.anypoint.df.edi.schema

import java.io.{ InputStream, IOException }
import java.nio.charset.Charset
import java.util.{ Calendar, GregorianCalendar }
import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import scala.util.Try
import scala.util.Success
import com.anypoint.df.edi.lexical.{ ErrorHandler, LexerBase, LexicalException, EdifactLexer }
import com.anypoint.df.edi.lexical.EdiConstants.{ DataType, ItemType }
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
  extends SchemaParser(new EdifactLexer(in, config.charSet, config.substitutionChar), sc) {

  import EdifactSchemaDefs._

  /** First interchange read. All interchanges sent in a block must use same delimiters, control and syntax versions. */
  var firstInterchange: ValueMap = null
  
  /** Syntax version in use. */
  var syntaxVersion: SyntaxVersion = null

  /** Flag for currently in a message. */
  var inMessage = false

  /** Flag for message to be rejected because of errors. */
  var rejectMessage = false

  /** Error code for current message. */
  var messageErrorCode: SyntaxError = null

  /** Current segment reference, used in error reporting. */
  var currentSegment: Segment = null

  /** Error code for current segment. */
  var segmentError: SyntaxError = null

  /** Control reference for current interchange, used in error reporting. */
  var interchangeReference = ""

  /** Segment number for interchange start. */
  var interchangeSegmentNumber = 0

  /** Number of groups in interchange. */
  var interchangeGroupCount = 0

  /** Number of messages in interchange. */
  var interchangeMessageCount = 0

  /** Message acknowledgment (UCM) under construction. */
  var currentUCM: ValueMap = null

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
  case class DataError(error: SyntaxError, elementPosition: Int, componentPosition: Int, repeat: Int)

  /** Exception reporting problem in interchange. */
  case class InterchangeException(error: SyntaxError, text: String) extends RuntimeException(text)

  /** Accumulated data errors from segment. */
  val segmentErrors = Buffer[DataError]()

  /** Segment error details. This corresponds to a CONTRL message Group 2 or Group 5 structure. */
  case class SegmentErrorReport(segmentPosition: Int, errorCode: String, dataErrors: List[DataError])

  /** Accumulated segment errors for message. */
  val messageErrors = Buffer[SegmentErrorReport]()

  /** Accumulated segment group 1 instances for CONTRL acknowledgment message. */
  val contrlGroup1s = Buffer[ValueMap]()

  /** Accumulated segment group 3 instances for CONTRL acknowledgment message. */
  val contrlGroup3s = Buffer[ValueMap]()

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

  def describeSegment = if (currentSegment == null) "" else s" (${currentSegment.ident})"

  def positionGroupNumber = if (inGroup) s" in group $groupReference" else ""
  def positionInMessage = s"segment ${lexer.getSegmentNumber - messageStartSegment + 1}$describeSegment of message ${messageReference}${positionGroupNumber} of interchange $interchangeReference"
  def positionInGroup = s"segment ${lexer.getSegmentNumber - groupStartSegment + 1}$describeSegment in group $groupReference of interchange $interchangeReference"
  def positionInInterchange = s"segment ${lexer.getSegmentNumber - interchangeSegmentNumber + 1}$describeSegment of interchange $interchangeReference"
  def positionInText = s"segment ${lexer.getSegmentNumber}$describeSegment"

  def describeError(fatal: Boolean) = if (fatal) "fatal" else "recoverable"

  def describeComponent(incomp: Boolean) =
    if (incomp) {
      val index = 0 max (lexer.getElementNumber - 1)
      if (index < currentSegment.components.size) s" for component '${currentSegment.components(index).name}'" else ""
    } else ""

  def logErrorInMessage(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} message error '$text'${describeComponent(incomp)} at $positionInMessage")

  def logMessageEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} message error '$text'${describeComponent(incomp)} at $positionInGroup")

  def logGroupEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} group error '$text'${describeComponent(incomp)} at $positionInInterchange")

  def logInterchangeEnvelopeError(fatal: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} interchange error '$text'' at $positionInMessage")

  /** Accumulate element error, failing message if severe. */
  def addElementError(error: SyntaxError) = {
    val fatal = checkFatal(error)
    if (inMessage) {
      logErrorInMessage(fatal, true, error.text)
      if (fatal) rejectMessage = true
      segmentErrors += DataError(error, lexer.getElementNumber, lexer.getComponentNumber, lexer.getRepetitionNumber)
    } else if (inGroup) logMessageEnvelopeError(true, true, error.text)
    else logGroupEnvelopeError(false, true, error.text)
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap) = {
    def isPresent(comp: SegmentComponent) = {
      lexer.token.length > 0 || (comp.isInstanceOf[CompositeComponent] && lexer.nextType == QUALIFIER)
    }
    def checkParse(comp: SegmentComponent, of: ItemType) =
      if (of == lexer.currentType) {
        if (isPresent(comp)) parseComponent(comp, map)
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
    segmentError = null
    segmentErrors.clear
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
    if (segmentError != null || !segmentErrors.isEmpty) {
      val code = if (segmentError == null) "" else segmentError.code
      messageErrors += SegmentErrorReport(lexer.getSegmentNumber - groupStartSegment, code, segmentErrors.toList)
    }
    if (logger.isDebugEnabled) logger.trace(s"now positioned at segment '${lexer.token}'")
    map
  }

  /** Report segment error. */
  def segmentError(ident: String, group: Option[String], error: ComponentErrors.ComponentError) = {
    def addError(fatal: Boolean, error: SyntaxError) = {
      logErrorInMessage(fatal, false, s"${error.text}: $ident")
      if (fatal) rejectMessage = true
      if (segmentError == null) {
        segmentError = error
      }
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
  
  /** Get the UNB segment definition for the syntax version. */
  def unbSegment(version: SyntaxVersion) = if (syntaxVersion == SyntaxVersion.VERSION4) segUNBv4 else segUNBv3

  /** Check if at interchange envelope segment. */
  def isInterchangeEnvelope = lexer.currentType == SEGMENT && (lexer.token == "UNB" || lexer.token == "UNZ")

  /** Check if at functional group envelope segment. */
  def isGroupEnvelope = checkSegment("UNG") || checkSegment("UNE")

  /** Check if at functional group open segment. */
  def isGroupOpen = checkSegment("UNG")

  def groupError(error: SyntaxError) = {
    logGroupEnvelopeError(true, false, error.text)
  }

  /** Parse start of a functional group. */
  def openGroup =
    if (checkSegment("UNG")) {
      groupStartSegment = lexer.getSegmentNumber
      groupMessageCount = 0
      val map = parseSegment(ungSegment(syntaxVersion), None, outsidePosition)
      inGroup = true
      map
    } else throw new IllegalStateException("not at UNG segment")

  /** Check if at functional group close segment. */
  def isGroupClose = checkSegment(segUNE)

  /** Parse close of a functional group. Returns number of message sets included in group. */
  def closeGroup(props: ValueMap) = {
    inGroup = false
    if (checkSegment(segUNE)) {
      val endprops = parseSegment(segUNE, None, SegmentPosition(0, "9999"))
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
    if (messageErrorCode != null) {
      messageErrorCode = error
    }
    rejectMessage = true
  }

  /** Check if at message set open segment. */
  def isSetOpen = checkSegment("UNH")

  /** Parse start of a message set. */
  def openSet =
    if (checkSegment("UNH")) {
      messageErrors.clear
      inMessage = true
      rejectMessage = false
      messageErrorCode = null
      currentUCM = new ValueMapImpl
      messageStartSegment = lexer.getSegmentNumber
      val unhSeg = unhSegment(syntaxVersion)
      val values = parseSegment(unhSeg, None, outsidePosition)
      messageReference = getAsString(unhSeg.components(0).key, values)
      val ucmSeg = ucmSegment(syntaxVersion)
      currentUCM put (ucmSeg.components(0).key, messageReference)
      copyComposite(unhSeg.components(1), values, ucmSeg.components(1), currentUCM)
      groupMessageCount += 1
      (values.get(msgHeadMessageTypeKey).asInstanceOf[String], values)
    } else throw new IllegalStateException(s"not positioned at UNH segment")

  /** Check if at message set close segment. */
  def isSetClose = checkSegment(segUNT)

  /** Parse close of a message set. */
  def closeSet(props: ValueMap) = {
    if (checkSegment(segUNT)) {
      val endprops = parseSegment(segUNT, None, SegmentPosition(0, "9999"))
      if (props.get(msgHeadReferenceKey) != endprops.get(msgTrailReferenceKey)) {
        messageError(ControlReferenceMismatch)
      }
      val segcount = lexer.getSegmentNumber - messageStartSegment
      if (getRequiredInt(msgTrailCountKey, endprops) != segcount) messageError(ControlCountMismatch)
    } else messageError(MissingRequiredValue)
    inMessage = false
    messageReference = null
    val ackcode =
      if (rejectMessage) AcknowledgedRejected
      else if (messageErrors.size == 0) AcknowledgedWithErrors
      else AcknowledgedAllLevels
    val ucmSeg = ucmSegment(syntaxVersion)
    currentUCM put (ucmSeg.components(2).key, ackcode.code)
    if (messageErrorCode != null) currentUCM put (ucmSeg.components(3).key, messageErrorCode.code)
    messageErrors.foreach (segerr => {
      val sg1 = new ValueMapImpl
      val ucs = new ValueMapImpl
      ucs put (segUCS.components(0).key, Integer.valueOf(segerr.segmentPosition))
      ucs put (segUCS.components(1).key, segerr.errorCode)
      val msg = contrlMsg(syntaxVersion)
      sg1 put (msg.heading(2).asInstanceOf[GroupComponent].items(0).key, ucs)
      if (!segerr.dataErrors.isEmpty) {
        val ucdlist = new MapListImpl
        sg1 put (CONTRLsg2.key, ucdlist)
        segerr.dataErrors.foreach (dataerr => {
          val ucd = new ValueMapImpl
          ucd put (segUCD.components(0).key, dataerr.error.code)
          def elems = segUCD.components(1).asInstanceOf[CompositeComponent].composite.components
          ucd put (elems(0).key, Integer.valueOf(dataerr.elementPosition))
          if (dataerr.componentPosition >= 0) {
            ucd put (elems(1).key, Integer.valueOf(dataerr.componentPosition))
            if (dataerr.repeat >= 0) {
              ucd put (elems(2).key, Integer.valueOf(dataerr.repeat))
            }
          }
          ucdlist add ucd
        })
      }
      contrlGroup1s += sg1
    })
  }

  /** Convert section control segment to next section number. If not at a section control, this just returns None. */
  def convertSectionControl =
    if (checkSegment(segUNS)) {
      val values = parseSegment(segUNS, None, outsidePosition)
      getRequiredString(sectionControlIdent, values) match {
        case "D" => Some(1)
        case "S" => Some(2)
        case _ => {
          messageError(NoAgreementForValue)
          None
        }
      }
    } else None

  /** Convert loop start or end segment to identity form. If not at a loop segment, this just returns None. */
  def convertLoop = if (Set("LS", "LE").contains(lexer.token)) Some(lexer.token + lexer.peek) else None

  /** Discard input past end of current message. */
  def discardTransaction = {
    while (lexer.currentType != SEGMENT || lexer.token != segUNT.ident) discardSegment
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
    while (lexer.currentType != END && (lexer.currentType != SEGMENT || lexer.token != "UNZ")) discardSegment
    discardSegment
  }

  /** Copy all values for a composite in one map to a composite in another map. */
  def copyComposite(fromcomp: Composite, frommap: ValueMap, tocomp: Composite, tomap: ValueMap): Unit = {
    @tailrec
    def copyr(from: List[SegmentComponent], to: List[SegmentComponent]): Unit = from match {
      case h :: t =>
        if (to.nonEmpty) {
          val key = from.head.key
          if (frommap.containsKey(key)) tomap put (to.head.key, frommap.get(key))
          copyr(t, to.tail)
        }
      case _ =>
    }
    copyr(fromcomp.components, tocomp.components)
  }
  def copyComposite(fromcomp: SegmentComponent, frommap: ValueMap, tocomp: SegmentComponent, tomap: ValueMap): Unit =
    copyComposite(fromcomp.asInstanceOf[CompositeComponent].composite, frommap,
      tocomp.asInstanceOf[CompositeComponent].composite, tomap)

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
      builder append (if (lexer.getRepetitionSeparator < 0) ' ' else lexer.getRepetitionSeparator.asInstanceOf[Char])
      builder append (lexer.getSegmentTerminator)
      builder append (if (lexer.getReleaseIndicator < 0) ' ' else lexer.getReleaseIndicator.asInstanceOf[Char])
      builder toString
    }

    val map = new ValueMapImpl
    val interAckList = new MapListImpl
    map put (interchangeAcksGenerated, interAckList)
    val funcAckList = new MapListImpl
    map put (functionalAcksGenerated, funcAckList)
    val transLists = new ValueMapImpl().asInstanceOf[java.util.Map[String, MapList]]
    schema.transactions.keys foreach { key => transLists put (key, new MapListImpl) }
    map put (messagesMap, transLists)

    /** Parse messages in group. */
    def parseGroup(group: ValueMap, groupCode: String, providerId: String, groupSender: String, groupReceiver: String) = {
      val (setid, setprops) = openSet
      schema.transactions.get(setid) match {
        case t: Transaction => transLists.get(setid).add(parseTransaction(t))
        case _ => messageError(NoAgreementForValue)
      }
    }

    def buildFuncCONTRL(interchange: ValueMap) = {
      val ctrlmap = new ValueMapImpl
      val msg = contrlMsg(syntaxVersion)
      ctrlmap put (transactionId, msg.ident)
      ctrlmap put (transactionName, msg.name)
      val intercopy = new ValueMapImpl(interchange)
      swap(interHeadSenderQualKey, interHeadRecipientQualKey, intercopy)
      swap(interHeadSenderIdentKey, interHeadRecipientIdentKey, intercopy)
      ctrlmap put (interchangeKey, intercopy)
      ctrlmap
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

    def openInterchange =
      if (firstInterchange == null) {
        try {
          firstInterchange = new ValueMapImpl()
          syntaxVersion = init(firstInterchange)
          map put (delimiterCharacters, buildDelims)
          parseCompList(unbSegment(syntaxVersion).components.tail, ItemType.DATA_ELEMENT, ItemType.DATA_ELEMENT,
            firstInterchange)
          firstInterchange
        } catch {
          case e: LexicalException => {
            logger.error(s"Unable to process message due to error in interchange header: ${e.getMessage}")
            throw e
          }
        }
      } else parseSegment(unbSegment(syntaxVersion), None, outsidePosition)

    while (lexer.nextType != END) {

      // parse the interchange header segment(s)
      val inter = openInterchange
      if (getRequiredString(SYNTAX_IDENTIFIER, firstInterchange) != getRequiredString(SYNTAX_IDENTIFIER, inter) ||
        getRequiredString(SYNTAX_VERSION_NUMBER, firstInterchange) != getRequiredString(SYNTAX_VERSION_NUMBER, inter)) {
        throw new InterchangeException(UnspecifiedError, "Multiple interchanges sent in a single transfer unit must use the same syntax and version")
      }
      interchangeSegmentNumber = lexer.getSegmentNumber
      interchangeGroupCount = 0
      interchangeMessageCount = 0
      map put (interchangeKey, inter)
      try {

        def parseMessage(context: String, group: Option[ValueMap]) = {
          val (setid, setprops) = openSet
          if (numval.validateMessage(getRequiredString(msgHeadReferenceKey, setprops),
            getRequiredString(msgHeadMessageTypeKey, setprops),
            getRequiredString(msgHeadMessageVersionKey, setprops),
            getRequiredString(msgHeadMessageReleaseKey, setprops),
            getRequiredString(msgHeadMessageAgencyKey, setprops),
            getAsString(msgHeadMessageAssignedKey, setprops),
            getAsString(msgHeadMessageDirectoryKey, setprops),
            getAsString(msgHeadMessageSubfunctionKey, setprops), context)) {
            schema.transactions(setid) match {
              case t: Transaction => {
                val data = parseTransaction(t)
                if (isSetClose) {
                  closeSet(setprops)
                  group.foreach { gmap => data.put(groupKey, gmap) }
                  data.put(interchangeKey, inter)
                  data.put(messageHeaderKey, setprops)
                  interchangeMessageCount = interchangeMessageCount + 1
                  transLists.get(setid).add(data)
                } else messageError(NotSupportedInPosition)
              }
              case _ => messageError(NoAgreementForValue)
            }
          } else messageError(DuplicateDetected)
        }

        // initialize for interchange
        interchangeSegmentNumber = lexer.getSegmentNumber - 1
        interchangeReference = getRequiredString(interHeadReferenceKey, inter)
        contrlGroup1s.clear
        contrlGroup3s.clear
        val ackroot = buildFuncCONTRL(inter)
        val ackhead = new ValueMapImpl
        ackroot put (transactionHeading, ackhead)
        ackroot put (transactionDetail, new ValueMapImpl)
        ackroot put (transactionSummary, new ValueMapImpl)
        funcAckList add (ackroot)
        val interack = new ValueMapImpl
        val segUCI = uciSegment(syntaxVersion)
        interack put (segUCI.components(0).key, interchangeReference)
        copyComposite(unbSender, inter, segUCI.components(1).asInstanceOf[CompositeComponent].composite, interack)
        copyComposite(unbRecipient, inter, segUCI.components(2).asInstanceOf[CompositeComponent].composite, interack)
        val contrl = contrlMsg(syntaxVersion)
        ackhead put (contrlMsg(syntaxVersion).heading(1).key, interack)

        def interchangeError(error: SyntaxError, text: String) = {
          logInterchangeEnvelopeError(true, error.text)
          interack put (segUCI.components(3).key, AcknowledgedRejected.code)
          interack put (segUCI.components(4).key, error.code)
          discardInterchange
          throw new InterchangeException(error, text)
        }

        val sender = getStrings(unbSender.components, inter)
        val recipient = getStrings(unbRecipient.components, inter)
        val context = numval.contextToken(valueOrNull(0, sender), valueOrNull(1, sender), valueOrNull(2, sender),
          valueOrNull(3, sender), valueOrNull(0, recipient), valueOrNull(1, recipient), valueOrNull(2, recipient),
          valueOrNull(3, recipient))
        if (numval.validateInterchange(interchangeReference, context)) {
          val receivers = matchIdentity(buildIdentity(recipient), config.receiverIds)
          if (receivers.length == 0 && config.receiverIds.length > 0)
            interchangeError(UnknownInterchangeRecipient, "Interchange recipient information does not match configuration")
          val senders = matchIdentity(buildIdentity(sender), config.senderIds)
          if (senders.length == 0 && config.senderIds.length > 0)
            interchangeError(UnknownInterchangeSender, "Interchange sender information does not match configuration")
          var ackId = 1
          while (lexer.currentType != END && !isInterchangeEnvelope) {
            if (isGroupEnvelope) {
              val groupmap = openGroup
              while (!isGroupClose) {
                if (isSetOpen) parseMessage(context, Some(groupmap))
                else groupError(InvalidOccurrence)
              }
            } else if (isSetOpen) {
              parseMessage(context, None)
            } else {
              val text = s"Unexpected segment ${lexer.token} at ${lexer.getSegmentNumber}"
              logger error text
              throw InterchangeException(InvalidOccurrence, text)
            }
          }
          val typ = lexer.currentType
          if (lexer.currentType == END) {
            interchangeError(InvalidOccurrence, s"end of file with missing $interchangeEndSegment")
          } else if (lexer.token == interchangeEndSegment) {
            val interend = parseSegment(segUNZ, None, outsidePosition)
            if (getRequiredInt(interTrailCountKey, interend) != interchangeMessageCount) interchangeError(ControlCountMismatch, ControlCountMismatch.text)
            if (getRequiredString(interTrailReferenceKey, interend) != interchangeReference) interchangeError(ControlReferenceMismatch, ControlReferenceMismatch.text)
            interack put (segUCI.components(3).key, AcknowledgedAllLevels.code)
          } else {
            interchangeError(InvalidOccurrence, s"expected $interchangeEndSegment, found ${lexer.token}")
          }
        } else {
          interchangeError(DuplicateDetected, s"Duplicate interchange control number $interchangeReference in UNB at ${lexer.getSegmentNumber}")
        }
        if (!contrlGroup1s.isEmpty) {
          val g1list = new MapListImpl
          contrlGroup1s.foreach(map => g1list.add(map))
          ackhead put (contrlMsg(syntaxVersion).detail(1).key, g1list)
        }
        if (!contrlGroup3s.isEmpty) {
          val g3list = new MapListImpl
          contrlGroup3s.foreach(map => g3list.add(map))
          ackhead put (contrlMsg(syntaxVersion).detail(2).key, g3list)
        }
      } catch {
        case e: InterchangeException => {
          discardInterchange
          throw e
        }
        case e: IOException => {
          throw e
        }
      }
    }
    map
  }
}