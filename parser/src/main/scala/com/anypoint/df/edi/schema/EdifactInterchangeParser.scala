package com.anypoint.df.edi.schema

import java.io.{ InputStream, IOException }
import java.nio.charset.Charset
import java.{ util => ju }
import java.util.{ Calendar, GregorianCalendar }

import scala.annotation.tailrec
import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer
import scala.util.{ Try, Success }

import org.apache.log4j.Logger

import com.anypoint.df.edi.lexical.{ EdifactLexer, ErrorHandler, LexerBase, LexicalException, TypeFormat }
import com.anypoint.df.edi.lexical.EdiConstants.ItemType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition._
import com.anypoint.df.edi.lexical.EdifactConstants._
import com.anypoint.df.edi.lexical.EdifactLexer._
import EdiSchema._
import EdifactAcknowledgment._
import EdifactSchemaDefs._
import SchemaJavaValues._
import com.mulesoft.ltmdata.StorageContext

/** Configuration parameters for EDIFACT schema parser.
  */
case class EdifactParserConfig(val lengthFail: Boolean, val charFail: Boolean, val countFail: Boolean,
  val unknownFail: Boolean, val orderFail: Boolean, val unusedFail: Boolean, val occursFail: Boolean,
  val enforceChars: Boolean, val substitutionChar: Int)

case class EdifactStructureConfig(structure: Structure, config: EdifactParserConfig)

case class EdifactHandlerError(val error: SyntaxError, val text: String)

/** Application callback to determine handling of envelope structures.
  */
trait EdifactEnvelopeHandler {

  /** Handle UNB segment data, returning either an EdifactHandlerError (if there's a problem that prevents processing of
    * the interchange) or the parser configuration to be used for the interchange (or <code>null</code> if default
    * parser configuration to be used).
    */
  def handleUnb(map: ju.Map[String, Object]): Object

  /** Handle UNG segment data, returning either an EdifactHandlerError (if there's a problem that prevents processing of
    * the group) or null.
    */
  def handleUng(map: ju.Map[String, Object]): EdifactHandlerError

  /** Handle UNH segment data, returning either an EdifactHandlerError (if there's a problem that prevents processing of
    * the message) or the structure configuration with message schema definition for parsing and validating the message
    * data and optional parser configuration.
    */
  def handleUnh(map: ju.Map[String, Object]): Object
}

/** Error information supplied to user. */
case class EdifactError(@BeanProperty val segment: Int, @BeanProperty val fatal: Boolean,
  @BeanProperty val errorCode: String, @BeanProperty val errorText: String) {
  def this() = this(0, false, "", "")
}

case class EdifactInterchangeParser(in: InputStream, defaultDelims: String, handler: EdifactEnvelopeHandler)
  extends SchemaParser(new EdifactLexer(in, defaultDelims), StorageContext.workingContext) {

  /** Exception reporting problem in interchange. */
  case class EdifactInterchangeException(error: SyntaxError, text: String, cause: Throwable = null)
    extends Exception(text, cause) {
    def this(err: SyntaxError, txt: String) = this(err, txt, null)
  }
  
  // keys in message map (excludes structure key)
  val structureDescriptor =
    storageContext.addDescriptor(Array(structureId, structureName, structureHeading, structureDetail, structureSummary))

  // value key used in multiple maps
  val errorListKey = "Errors"

  /** Typed lexer, for access to format-specific conversions and support. */
  val lexer = baseLexer.asInstanceOf[EdifactLexer]

  /** Current configuration in use (default value used for UNB, real configuration set from handler call.) */
  var config: EdifactParserConfig = null

  /** Syntax version in use. */
  var syntaxVersion: SyntaxVersion = null

  /** Result root map data. */
  var rootMap: ValueMap = null

  /** Flag for currently in a message. */
  var inMessage = false

  /** Flag for message to be rejected because of errors. */
  var rejectMessage = false

  /** Error code for current message. */
  var messageErrorCode: SyntaxError = null

  /** Current segment reference, used in error reporting. */
  var currentSegment: Segment = null

  /** Index within input of segment currently being parsed. This only updates when we start parsing the next segment. */
  var segmentIndex = 0

  /** Error code for current segment (not data error). */
  var segmentGeneralError: SyntaxError = null

  /** Interchange envelope data. */
  var interchangeMap: ValueMap = null

  /** Control reference for current interchange, used in error reporting. */
  var interchangeReference = ""

  /** Segment number for interchange start. */
  var interchangeSegmentNumber = 0

  /** Number of groups in interchange. */
  var interchangeGroupCount = 0

  /** Number of messages in interchange. */
  var interchangeMessageCount = 0

  /** Number of good messages in interchange. */
  var interchangeGoodCount = 0

  /** Flag for currently in an interchange. */
  var inInterchange = false;

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

  /** Current message data. */
  var messageMap: ValueMap = null

  /** Data error details. This corresponds to an S011 composite structure, as used in CONTRL message UCI, UCF, UCM, and
    * UCD segments.
    */
  case class DataError(error: SyntaxError, elementPosition: Int, componentPosition: Int, repeat: Int)

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
    def error(typ: TypeFormat, error: ErrorCondition, explain: java.lang.String): Unit = {
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

  def logErrorInMessage(fatal: Boolean, incomp: Boolean, segNum: Int, text: String) =
    logger.error(s"${describeError(fatal)} message error '$text'${describeComponent(incomp)} at $segNum")

  def logMessageEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} message envelope error '$text'${describeComponent(incomp)} at $positionInGroup")

  def logGroupEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} envelope error '$text'${describeComponent(incomp)} at $positionInInterchange")

  def logInterchangeEnvelopeError(fatal: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} interchange error '$text'' at $positionInText")

  /** Get segment number to report for an error. */
  def errorSegmentNumber =
    if (inMessage) lexer.getSegmentNumber - messageStartSegment + 1
    else if (inGroup) lexer.getSegmentNumber - groupStartSegment + 1
    else if (inInterchange) lexer.getSegmentNumber - interchangeSegmentNumber + 1
    else lexer.getSegmentNumber

  /** Accumulate element error, failing message if severe. */
  def addElementError(error: SyntaxError) = {
    val fatal = checkFatal(error)
    val report = EdifactError(segmentIndex, fatal, error.code, error.text)
    if (inMessage) {
      logErrorInMessage(fatal, true, errorSegmentNumber, error.text)
      addToList(errorListKey, report, messageMap)
      if (fatal) rejectMessage = true
    } else if (inGroup) {
      logMessageEnvelopeError(fatal, true, error.text)
      //      addToList(errorListKey, report, groupMap)
    } else if (inInterchange) {
      logGroupEnvelopeError(fatal, true, error.text)
      addToList(errorListKey, report, interchangeMap)
    } else {
      logInterchangeEnvelopeError(fatal, error.text)
      addToList(errorListKey, report, rootMap)
    }
    val elnum = lexer.getElementNumber + 1
    val compnum = if (lexer.getComponentNumber > 0 || lexer.nextType == COMPONENT) lexer.getComponentNumber + 1 else -1
    val repnum = if (lexer.getRepetitionNumber > 0) lexer.getRepetitionNumber + 1 else -1
    val item = DataError(error, elnum, compnum, repnum)
    if (segmentErrors.isEmpty || segmentErrors.last != item) segmentErrors += item
  }

  /** Report a repetition error on a composite component. */
  def repetitionError(comp: CompositeComponent) = addElementError(TooManyConstituents)

  /** Parse data element value. */
  def parseElement(elem: Element) = {
    val result = elem.typeFormat.parse(lexer)
    lexer.advance
    result
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap) = {
    def isPresent(comp: SegmentComponent) = {
      lexer.hasData || (comp.isInstanceOf[CompositeComponent] && lexer.nextType == COMPONENT)
    }
    def checkParse(comp: SegmentComponent, of: ItemType) =
      if (of == lexer.currentType) {
        if (isPresent(comp)) parseComponent(comp, of, rest.nextLevel, map)
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

  def handleSegmentErrors(segNum: Int) =
    if (segmentGeneralError != null || !segmentErrors.isEmpty) {
      val code = if (segmentGeneralError == null) null else segmentGeneralError.code
      messageErrors += SegmentErrorReport(segNum, code, segmentErrors.toList)
      segmentGeneralError = null
      segmentErrors.clear
    }

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  def parseSegment(segment: Segment, position: SegmentPosition): ValueMap = {
    if (logger.isTraceEnabled) logger.trace(s"parsing segment ${segment.ident} at position $position")
    segmentIndex = lexer.getSegmentNumber
    val map = storageContext.newMap(segment.keys)
    segmentErrors.clear
    currentSegment = segment
    val segNum = errorSegmentNumber
    lexer.advance
    parseCompList(segment.components, DATA_ELEMENT, DATA_ELEMENT, map)
    lexer.currentType match {
      case SEGMENT | END =>
      case _ => {
        addElementError(TooManyConstituents)
        lexer.discardSegment
      }
    }
    handleSegmentErrors(segNum)
    if (logger.isDebugEnabled) logger.trace(s"now positioned at segment '${lexer.segmentTag}'")
    map
  }

  def segmentNumber = errorSegmentNumber

  /** Report segment error. */
  def segmentError(ident: String, error: ComponentErrors.ComponentError, state: ErrorStates.ErrorState, num: Int) = {
    def addError(index: Int, fatal: Boolean, error: SyntaxError) = {
      logErrorInMessage(fatal, false, num, s"${error.text}: $ident")
      addToList(errorListKey, EdifactError(index, fatal, error.code, error.text), messageMap)
      if (fatal) rejectMessage = true
      if (segmentGeneralError == null) segmentGeneralError = error
    }

    error match {
      case ComponentErrors.TooManyLoops => addError(segmentIndex, config.occursFail, TooManyGroupRepetitions)
      case ComponentErrors.TooManyRepetitions => addError(segmentIndex, config.occursFail, TooManySegmentRepetitions)
      case ComponentErrors.MissingRequired => addError(lexer.getSegmentNumber, true, MissingRequiredValue)
      case ComponentErrors.UnknownSegment => addError(lexer.getSegmentNumber, config.unknownFail, InvalidOccurrence)
      case ComponentErrors.OutOfOrderSegment => addError(lexer.getSegmentNumber, config.orderFail, NotSupportedInPosition)
      case ComponentErrors.UnusedSegment => if (config.unusedFail) addError(lexer.getSegmentNumber, true, NotSupportedInPosition)
    }
    state match {
      case ErrorStates.ParseComplete => handleSegmentErrors(num)
      case ErrorStates.WontParse =>
        lexer.discardSegment
        handleSegmentErrors(num)
      case _ =>
    }
  }

  /** Get the UNB segment definition for the syntax version. */
  def unbSegment(version: SyntaxVersion) = if (syntaxVersion == SyntaxVersion.VERSION4) segUNBv4 else segUNBv3

  /** Check if at interchange envelope segment. */
  def isInterchangeEnvelope = lexer.currentType == SEGMENT && (lexer.segmentTag == "UNB" || lexer.segmentTag == "UNZ")

  /** Check if an envelope segment (handled directly, outside of structure). */
  def isEnvelopeSegment(ident: String) = EdiFact.isEnvelopeSegment(ident)

  /** Check if at functional group envelope segment. */
  def isGroupEnvelope = checkSegment("UNG") || checkSegment("UNE")

  /** Check if at functional group open segment. */
  def isGroupOpen = checkSegment("UNG")

  def groupError(error: SyntaxError, text: String) = {
    logGroupEnvelopeError(true, false, text)
    // TODO: discard to end of group
  }
  def groupError(error: SyntaxError): Unit = groupError(error, error.text)

  /** Parse start of a functional group. */
  def openGroup =
    if (checkSegment("UNG")) {
      groupStartSegment = lexer.getSegmentNumber
      groupMessageCount = 0
      val map = parseSegment(ungSegment(syntaxVersion), StartPosition)
      inGroup = true
      map
    } else throw new IllegalStateException("not at UNG segment")

  /** Check if at functional group close segment. */
  def isGroupClose = checkSegment(segUNE)

  /** Parse close of a functional group. Returns number of message sets included in group. */
  def closeGroup(props: ValueMap) = {
    inGroup = false
    if (checkSegment(segUNE)) {
      val endprops = parseSegment(segUNE, StartPosition)
      if (props.get(groupHeadReferenceKey) != endprops.get(groupTrailReferenceKey)) groupError(ControlReferenceMismatch)
      if (endprops.get(groupTrailCountKey) != groupMessageCount) groupError(ControlCountMismatch)
      endprops.get(groupTrailCountKey).asInstanceOf[Integer]
    } else {
      groupError(MissingRequiredValue)
      Integer valueOf (0)
    }
  }

  def messageError(error: SyntaxError) = {
    logErrorInMessage(true, false, errorSegmentNumber, error.text)
    addToList(errorListKey, EdifactError(lexer.getSegmentNumber, true, error.code, error.text), messageMap)
    if (messageErrorCode == null) messageErrorCode = error
    rejectMessage = true
    discardStructure
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
      currentUCM = storageContext.newMap(ucmSegment(syntaxVersion).keys)
      messageStartSegment = lexer.getSegmentNumber
      val unhSeg = unhSegment(syntaxVersion)
      val values = parseSegment(unhSeg, StartPosition)
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
    def closeError(error: SyntaxError) = {
      logErrorInMessage(true, false, errorSegmentNumber, error.text)
      if (messageErrorCode == null) messageErrorCode = error
      rejectMessage = true
    }
    if (checkSegment(segUNT)) {
      if (rejectMessage) lexer.discardSegment
      else {
        val endprops = parseSegment(segUNT, StartPosition)
        if (props.get(msgHeadReferenceKey) != endprops.get(msgTrailReferenceKey)) closeError(ControlReferenceMismatch)
        val segcount = lexer.getSegmentNumber - messageStartSegment
        if (getRequiredInt(msgTrailCountKey, endprops) != segcount) closeError(ControlCountMismatch)
      }
    } else closeError(MissingRequiredValue)
    inMessage = false
    messageReference = null
    val ackcode =
      if (rejectMessage) AcknowledgedRejected
      else AcknowledgedLevel
    if (ackcode != AcknowledgedLevel || !messageErrors.isEmpty) {

      // create segment group 1 instance for message with issues
      val ucmSeg = ucmSegment(syntaxVersion)
      currentUCM put (ucmSeg.components(2).key, ackcode.code)
      if (messageErrorCode != null) currentUCM put (ucmSeg.components(3).key, messageErrorCode.code)
      val sg1 = storageContext.newMap(contrlSg1(syntaxVersion).keys)
      val group1Comps = contrlSg1Comps(syntaxVersion)
      sg1 put (group1Comps(0).key, currentUCM)
      if (!messageErrors.isEmpty) {

        // create segment group 2 instance for each segment with error(s)
        val sg2list = storageContext.newMapSeq
        sg1 put (group1Comps(1).key, sg2list)
        messageErrors.foreach(segerr => {
          val sg2 = storageContext.newMap(CONTRLsg2.keys)
          sg2list add sg2
          val ucs = storageContext.newMap(segUCS.keys)
          ucs put (segUCS.components(0).key, Integer.valueOf(segerr.segmentPosition))
          if (segerr.errorCode != null) ucs put (segUCS.components(1).key, segerr.errorCode)
          sg2 put (contrlSg2Comps(0).key, ucs)
          if (!segerr.dataErrors.isEmpty) {

            // add UCD for each data error in segment
            val ucdlist = storageContext.newMapSeq
            sg2 put (contrlSg2Comps(1).key, ucdlist)
            segerr.dataErrors.foreach(dataerr => {
              val ucd = storageContext.newMap(segUCD.keys)
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
        })
      }
      contrlGroup1s += sg1
    }
  }

  /** Convert section control segment to next section number. If not at a section control, this just returns None. */
  def convertSectionControl =
    if (checkSegment(segUNS)) {
      val values = parseSegment(segUNS, StartPosition)
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
  def convertLoop = None

  /** Discard input past end of current message. */
  def discardStructure = {
    while (lexer.currentType != SEGMENT || lexer.segmentTag != segUNT.ident) lexer.discardSegment
    if (lexer.currentType == SEGMENT) closeSet(new ValueMapImpl)
  }

  /** Discard input to end of current group. */
  def discardToGroupEnd =
    while (!isGroupClose)
      if (isSetOpen) {
        groupMessageCount += 1
        lexer.discardSegment
        discardStructure
      } else lexer.discardSegment

  /** Discard input past end of current interchange. */
  def discardInterchange = {
    while (lexer.currentType != END && (lexer.currentType != SEGMENT || lexer.segmentTag != "UNZ")) lexer.discardSegment
    lexer.discardSegment
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

  def init(data: ValueMap) = lexer.init(data)

  /** Parse the input message. */
  def parse: Try[ValueMap] = Try(try {

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

    rootMap = new ValueMapImpl
    val ackGeneratedList = storageContext.newMapSeq
    rootMap put (functionalAcksGenerated, ackGeneratedList)
    val ackReceivedList = storageContext.newMapSeq
    rootMap put (functionalAcksReceived, ackReceivedList)
    val transLists = new ValueMapImpl
    rootMap put (messagesMap, transLists)
    val schemaVersionMessages = getOrSet(messagesMap, new ValueMapImpl, rootMap)
    rootMap put (errorListKey, new ju.ArrayList[EdifactError]())

    def buildFuncCONTRL(interchange: ValueMap) = {
      val msg = contrlMsg(syntaxVersion)
      val ctrlmap = storageContext.newMap(structureDescriptor)
      ctrlmap put (structureId, msg.ident)
      ctrlmap put (structureName, msg.name)
      ctrlmap put (structureSchema, msg)
      val intercopy = storageContext.newMemoryResidentMap(interchange)
      swap(interHeadSenderQualKey, interHeadRecipientQualKey, intercopy)
      swap(interHeadSenderIdentKey, interHeadRecipientIdentKey, intercopy)
      ctrlmap put (interchangeKey, intercopy)
      ctrlmap
    }

    def openInterchange =
      try {
        lexer.setHandler(null)
        segmentGeneralError = null
        segmentErrors.clear
        val inter = new ValueMapImpl
        syntaxVersion = init(inter)
        rootMap put (delimiterCharacters, buildDelims)
        parseCompList(unbSegment(syntaxVersion).components.tail, ItemType.DATA_ELEMENT, ItemType.DATA_ELEMENT, inter)
        inter
      } catch {
        case e: LexicalException =>
          logger.error(s"Unable to process message due to error in interchange header: ${e.getMessage}")
          if (segmentGeneralError != null) throw EdifactInterchangeException(segmentGeneralError, e.getMessage, e)
          else if (segmentErrors.nonEmpty) throw EdifactInterchangeException(segmentErrors(0).error, e.getMessage, e)
          else {
            addToList(errorListKey, EdifactError(lexer.getSegmentNumber, true, UnspecifiedError.code, e.getMessage), rootMap)
            throw EdifactInterchangeException(UnspecifiedError, e.getMessage, e)
          }
      }

    while (lexer.nextType != END) {
      try {

        // parse the interchange header segment(s)
        lexer.asInstanceOf[EdifactLexer].configure(-1, true)
        interchangeMap = openInterchange
        interchangeSegmentNumber = lexer.getSegmentNumber
        interchangeGroupCount = 0
        interchangeMessageCount = 0
        interchangeGoodCount = 0

        // initialize interchange and acknowledgment handling
        interchangeSegmentNumber = lexer.getSegmentNumber - 1
        interchangeReference = getRequiredString(interHeadReferenceKey, interchangeMap)
        contrlGroup1s.clear
        contrlGroup3s.clear
        val ackroot = buildFuncCONTRL(interchangeMap)
        val contrl = contrlMsg(syntaxVersion)
        val ackhead = storageContext.newMap(contrl.headingKeys)
        ackroot put (structureHeading, ackhead)
        ackroot put (structureDetail, storageContext.newMap(storageContext.emptyDescriptor))
        ackroot put (structureSummary, storageContext.newMap(storageContext.emptyDescriptor))
        ackGeneratedList add ackroot
        val segUCI = uciSegment(syntaxVersion)
        val interack = storageContext.newMap(segUCI.keys)
        interack put (segUCI.components(0).key, interchangeReference)
        copyComposite(unbSender, interchangeMap,
          segUCI.components(1).asInstanceOf[CompositeComponent].composite, interack)
        copyComposite(unbRecipient, interchangeMap,
          segUCI.components(2).asInstanceOf[CompositeComponent].composite, interack)
        ackhead put (contrl.heading.get.items(1).key, interack)

        def parseMessage(group: Option[ValueMap]) = {
          interchangeMessageCount = interchangeMessageCount + 1
          val (setid, setprops) = openSet
          if (setid == "CONTRL") ackGeneratedList remove ackroot
          handler.handleUnh(setprops) match {
            case s: SyntaxError => messageError(s)
            case EdifactStructureConfig(struct, cfg) => {
              if (cfg != null) {
                config = cfg
                lexer.asInstanceOf[EdifactLexer].configure(config.substitutionChar, config.enforceChars)
              }
              messageMap = storageContext.newMap(structureDescriptor)
              parseStructure(struct, false, messageMap)
              if (isSetClose) {
                closeSet(setprops)
                if (!rejectMessage) {
                  group.foreach { gmap => messageMap.put(groupKey, gmap) }
                  messageMap put (interchangeKey, interchangeMap)
                  messageMap put (messageHeaderKey, setprops)
                  messageMap put (structureSchema, struct)
                  val list =
                    if (setid == "CONTRL") ackReceivedList
                    else {
                      val version = getRequiredString(msgHeadMessageVersionKey, setprops) +
                        getRequiredString(msgHeadMessageReleaseKey, setprops)
                      val msgLists = getOrSet(version, new ValueMapImpl, schemaVersionMessages)
                      getOrSet(setid, storageContext.newMapSeq, msgLists)
                    }
                  list add (messageMap)
                  interchangeGoodCount = interchangeGoodCount + 1
                }
              } else messageError(NotSupportedInPosition)
              if (rejectMessage) mergeToList(errorListKey, messageMap, interchangeMap)
              messageMap = null
            }
          }
        }

        def interchangeError(error: SyntaxError, text: String) = {
          logInterchangeEnvelopeError(true, text)
          interack put (segUCI.components(3).key, AcknowledgedRejected.code)
          interack put (segUCI.components(4).key, error.code)
          addToList(errorListKey, EdifactError(lexer.getSegmentNumber, true, error.code, text), rootMap)
          throw new EdifactInterchangeException(error, text)
        }

        def processInterchange(config: EdifactParserConfig) {
          lexer.asInstanceOf[EdifactLexer].configure(config.substitutionChar, config.enforceChars)
          var ackId = 1
          lexer.setHandler(EdifactErrorHandler)
          while (lexer.currentType != END && !isInterchangeEnvelope) {
            if (isGroupEnvelope) {
              val groupmap = openGroup
              handler.handleUng(groupmap) match {
                case EdifactHandlerError(error, text) => groupError(error, text)
                case null =>
                  while (!isGroupClose) {
                    if (isSetOpen) parseMessage(Some(groupmap))
                    else groupError(InvalidOccurrence)
                  }
              }
            } else if (isSetOpen) {
              parseMessage(None)
            } else {
              val text = s"Unexpected segment ${lexer.segmentTag} at ${lexer.getSegmentNumber}"
              interchangeError(InvalidOccurrence, text)
            }
          }
          val typ = lexer.currentType
          if (lexer.currentType == END) {
            interchangeError(InvalidOccurrence, s"end of file with missing $interchangeEndSegment")
          } else if (lexer.segmentTag == interchangeEndSegment) {
            val interend = parseSegment(segUNZ, StartPosition)
            if (getRequiredInt(interTrailCountKey, interend) != interchangeMessageCount) interchangeError(ControlCountMismatch, ControlCountMismatch.text)
            if (getRequiredString(interTrailReferenceKey, interend) != interchangeReference) interchangeError(ControlReferenceMismatch, ControlReferenceMismatch.text)
            interack put (segUCI.components(3).key, AcknowledgedLevel.code)
          } else {
            interchangeError(InvalidOccurrence, s"expected $interchangeEndSegment, found ${lexer.segmentTag}")
          }
        }

        try {
          handler.handleUnb(interchangeMap) match {
            case EdifactHandlerError(error, text) => interchangeError(error, text)
            case c: EdifactParserConfig => processInterchange(c)
            case null => processInterchange(EdifactParserConfig(true, true, true, true, true, true, true, true, -1))
            case _ => throw new IllegalArgumentException("invalid return from call")
          }
        } finally {
          if (!contrlGroup1s.isEmpty) {
            val g1list = storageContext.newMapSeq
            contrlGroup1s.foreach(map => g1list.add(map))
            ackhead put (contrl.heading.get.items(2).key, g1list)
          }
          if (!contrlGroup3s.isEmpty) {
            val g3list = storageContext.newMapSeq
            contrlGroup3s.foreach(map => g3list.add(map))
            ackhead put (contrl.heading.get.items(3).key, g3list)
          }
          if (interchangeGoodCount == 0) mergeToList(errorListKey, interchangeMap, rootMap)
        }
      } catch {
        case e: EdifactInterchangeException => {
          discardInterchange
        }
      }
    }
    rootMap
  } finally {
    try { lexer close } catch { case e: Throwable => }
  })
}