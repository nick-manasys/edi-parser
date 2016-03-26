package com.anypoint.df.edi.schema

import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import scala.util.{ Success, Try }

import java.io.{ InputStream, IOException }
import java.nio.charset.Charset
import java.util.{ Calendar, GregorianCalendar }

import com.anypoint.df.edi.lexical.{ ErrorHandler, HL7Lexer, LexerBase, LexicalException, TypeFormat }
import com.anypoint.df.edi.lexical.EdiConstants.ItemType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition._
import EdiSchema._
import HL7Identity._
import HL7SchemaDefs._
import SchemaJavaValues._
import com.mulesoft.ltmdata.StorageContext

/** Configuration parameters for HL7 schema parser. If either receiver or sender identity information is included it
  * is verified in processed messages.
  */
case class HL7ParserConfig(val lengthFail: Boolean, val charFail: Boolean, val countFail: Boolean,
    val unknownFail: Boolean, val orderFail: Boolean, val unusedFail: Boolean, val occursFail: Boolean,
    val substitutionChar: Int, val receiverIds: Array[HL7IdentityInformation],
    val senderIds: Array[HL7IdentityInformation]) {
  if (receiverIds == null || senderIds == null) throw new IllegalArgumentException("receiver and sender id arrays cannot be null")
}

/** Handler called by parser to validate received message header and return structure schema. */
trait HL7EnvelopeHandler {

  /** Handle received message header.
    * @param data
    */
  @throws(classOf[LexicalException])
  def handleMsh(data: ValueMap): Structure
}

/** Parser for HL7 EDI documents. */
case class HL7SchemaParser(in: InputStream, evnhand: HL7EnvelopeHandler, config: HL7ParserConfig)
    extends SchemaParser(new HL7Lexer(in, config.substitutionChar), StorageContext.workingContext) {

  import HL7SchemaDefs._
  import HL7Acknowledgment._

  /** Typed lexer, for access to format-specific conversions and support. */
  val lexer = baseLexer.asInstanceOf[HL7Lexer]

  /** Current segment reference, used in error reporting. */
  var currentSegment: Segment = null

  /** Message control ID. */
  var messageControl: String = null

  /** Flag for message to be rejected because of errors. */
  var acknowledgmentCode: AcknowledgmentCode = AcknowledgedApplicationAccept

  /** Accumulated segment errors for message. */
  val messageErrors = Buffer[ValueMap]()

  /** Check if an envelope segment (handled directly, outside of structure). */
  def isEnvelopeSegment(ident: String) = HL7.isEnvelopeSegment(ident)

  /** Lexical error handler. */
  case object HL7ErrorHandler extends ErrorHandler {
    def error(typ: TypeFormat, error: ErrorCondition, explain: java.lang.String): Unit = error match {
      case TOO_SHORT => addElementError(ErrorDataType, false, "element too short")
      case TOO_LONG => addElementError(ErrorDataType, false, "element too long")
      case INVALID_CHARACTER => addElementError(ErrorDataType, false, "invalid character")
      case INVALID_CODE => addElementError(ErrorTableValue, false, "invalid code value")
      case _ =>
    }
  }

  def describeSegment = if (currentSegment == null) "" else s" (${currentSegment.ident})"

  def describeError(fatal: Boolean) = if (fatal) "fatal" else "recoverable"

  def positionInMessage = s"segment ${lexer.getSegmentNumber + 1}$describeSegment of message $messageControl"

  def describeComponent(incomp: Boolean) =
    if (incomp) {
      val index = 0 max (lexer.getElementNumber - 1)
      if (index < currentSegment.components.size) {
        val comp = currentSegment.components(index)
        s" for component ${comp.key}: '${comp.name}'"
      } else ""
    } else ""

  def logErrorInMessage(fatal: Boolean, incomp: Boolean, text: String) =
    logger.error(s"${describeError(fatal)} message error: $text${describeComponent(incomp)} at $positionInMessage")

  /** Accumulate element error, failing message if severe. */
  def addElementError(error: ErrorCode, fatal: Boolean, text: String) = {
    logErrorInMessage(fatal, true, text)
    if (fatal) acknowledgmentCode = AcknowledgedApplicationReject
    else if (acknowledgmentCode == AcknowledgedApplicationAccept) acknowledgmentCode = AcknowledgedApplicationError
    val errmap = storageContext.newMap(segERR.keys)
    // TODO: generate the actual ERR segment values
    val elnum = lexer.getElementNumber + 1
    val compnum = if (lexer.getComponentNumber > 0 || lexer.nextType == COMPONENT) lexer.getComponentNumber + 1 else -1
    val repnum = if (lexer.getRepetitionNumber > 0) lexer.getRepetitionNumber + 1 else -1
    messageErrors += errmap
  }

  /** Report a repetition error on a composite component. */
  def repetitionError(comp: CompositeComponent) = addElementError(ErrorDataType, false, "too many repetitions")

  /** Parse data element value. */
  def parseElement(elem: Element) = {
    val result = elem.valueType.parse(lexer)
    lexer.advance
    result
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap) = {
    def isPresent(comp: SegmentComponent) = {
      lexer.hasData || (comp.isInstanceOf[CompositeComponent] && lexer.nextType == rest.nextLevel)
    }
    def checkParse(comp: SegmentComponent, of: ItemType) = {
      if (isPresent(comp)) parseComponent(comp, of, rest.nextLevel, map)
      else {
        if (comp.usage == MandatoryUsage) addElementError(ErrorRequiredFieldMissing, false, "missing required field")
        lexer.advance
      }
    }
    @tailrec
    def parseRest(remain: List[SegmentComponent]): Unit = {
      if (rest == lexer.currentType) remain match {
        case h :: t =>
          checkParse(h, rest)
          parseRest(t)
        case _ =>
          logger.warn(s"${describeError(false)} message error: too many values present${describeComponent(true)} at $positionInMessage")
          while (rest == lexer.currentType) lexer.advance
      } else remain match {
        case h :: t =>
          if (h.usage == MandatoryUsage) addElementError(ErrorRequiredFieldMissing, false, "missing required field")
          else parseRest(t)
        case _ =>
      }
    }

    comps match {
      case h :: t if (first == lexer.currentType) => {
        checkParse(h, first)
        parseRest(t)
      }
      case _ =>
    }
  }

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  def parseSegment(segment: Segment, position: SegmentPosition): ValueMap = {
    if (logger.isTraceEnabled) logger.trace(s"parsing segment ${segment.ident} at position $position")
    val map = storageContext.newMap(segment.keys)
    currentSegment = segment
    lexer.advance
    parseCompList(segment.components, DATA_ELEMENT, DATA_ELEMENT, map)
    lexer.currentType match {
      case SEGMENT | END =>
      case _ => {
        addElementError(ErrorDataType, false, "too many values present")
        lexer.discardSegment
      }
    }
    // TODO: handleSegmentErrors
    if (logger.isDebugEnabled) logger.trace(s"now positioned at segment '${lexer.segmentTag}'")
    map
  }

  def segmentNumber = lexer.getSegmentNumber + 1

  /** Report segment error. */
  def segmentError(ident: String, error: ComponentErrors.ComponentError, state: ErrorStates.ErrorState, num: Int) = {
    def addError(fatal: Boolean, error: ErrorCode, text: String) = {
      logErrorInMessage(fatal, false, s"$text: $ident")
      if (fatal) acknowledgmentCode = AcknowledgedApplicationReject
      else if (acknowledgmentCode == AcknowledgedApplicationAccept) acknowledgmentCode = AcknowledgedApplicationError
      val errmap = storageContext.newMap(segERR.keys)
      // TODO: if (segmentGeneralError == null) segmentGeneralError = error
    }

    error match {
      case ComponentErrors.MissingRequired => addError(true, ErrorSegmentSequence, "required segment missing")
      case ComponentErrors.UnknownSegment => addError(config.unknownFail, ErrorSegmentSequence, "unknown segment")
      case ComponentErrors.OutOfOrderSegment => addError(config.orderFail, ErrorSegmentSequence, "segment out of order")
      case ComponentErrors.UnusedSegment => if (config.unusedFail) addError(true, ErrorSegmentSequence, "unused segment present")
      case _ =>
    }
    state match {
      case ErrorStates.WontParse => lexer.discardSegment
      case _ =>
    }
  }

  /** Report message error. */
  def messageError(error: ErrorCode) = {
    logErrorInMessage(true, false, error.text)
    acknowledgmentCode = AcknowledgedApplicationReject
    discardStructure
  }

  def convertSectionControl = None

  def convertLoop = None

  /** Discard input past end of current message. */
  def discardStructure = while (lexer.currentType != END) lexer.discardSegment

  def init(data: ValueMap) = {
    val delims = lexer.init(data)
    currentSegment = segMSH
    parseCompList(segMSH.components.drop(1), ItemType.DATA_ELEMENT, ItemType.DATA_ELEMENT, data)
    delims
  }

  /** Parse the input message. */
  def parse: Try[ValueMap] = Try(try {
    val map = new ValueMapImpl
    val mshmap = storageContext.newMap(segMSH.keys)
    map put (mshKey, mshmap)
    val delims = init(mshmap)
    map put (delimiterCharacters, delims)
    lexer.setHandler(HL7ErrorHandler)
    messageControl = getAs(mshControlKey, "", mshmap)
    val struct = evnhand.handleMsh(mshmap)
    map put (structureId, struct.ident)
    map put (structureName, struct.name)
    map put (dataKey, parseStructure(struct, true, storageContext.newMap(struct.headingKeys)))
    map
  } catch {
    case t: Throwable =>
      t.printStackTrace
      new ValueMapImpl
  } finally {
    try { lexer close } catch { case e: Throwable => }
  })
}