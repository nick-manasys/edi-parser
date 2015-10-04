package com.anypoint.df.edi.schema

import java.io.{ InputStream, IOException }
import java.nio.charset.Charset
import java.{ util => ju }
import java.util.{ Calendar, GregorianCalendar }

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer
import scala.util.{ Try, Success }

import org.apache.log4j.Logger

import com.anypoint.df.edi.lexical.{ ErrorHandler, LexerBase, LexicalException, X12Lexer }
import com.anypoint.df.edi.lexical.EdiConstants.{ DataType, ItemType }
import com.anypoint.df.edi.lexical.EdiConstants.DataType._
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition._
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.lexical.X12Lexer._

import EdiSchema._
import SchemaJavaValues._
import X12Acknowledgment._
import X12SchemaDefs._

/** Configuration parameters for X12 schema parser.
  */
case class X12ParserConfig(val lengthFail: Boolean, val charFail: Boolean, val countFail: Boolean,
  val unknownFail: Boolean, val orderFail: Boolean, val unusedFail: Boolean, val occursFail: Boolean,
  val reportDataErrors: Boolean, val generate999: Boolean, val substitutionChar: Int, val strChar: CharacterRestriction)

/** Application callback to determine handling of envelope structures.
  */
trait X12EnvelopeHandler {

  /** Handle ISA segment data, returning either an InterchangeNoteCode (if there's a problem that prevents processing of
    * the interchange) or the parser configuration to be used for reading the interchange (or <code>null</code> if
    * default parser configuration to be used).
    */
  @throws(classOf[LexicalException])
  def handleIsa(map: ju.Map[String, Object]): Object

  /** Handle GS segment data, returning either an GroupSyntaxError (if there's a problem that prevents processing of
    * the group) or the parser configuration to be used for reading the interchange (or <code>null</code> if to use the
    * parser configuration previously set).
    */
  @throws(classOf[LexicalException])
  def handleGs(map: ju.Map[String, Object]): Object

  /** Handle ST segment data, returning either a StructureSyntaxError (if there's a problem that prevents processing of
    * the transaction set) or the transaction schema definition for parsing and validating the transaction set data.
    */
  def handleSt(map: ju.Map[String, Object]): Object
}

/** Exception reporting problem in interchange. */
case class X12InterchangeException(note: InterchangeNoteCode, text: String, cause: Throwable = null) extends RuntimeException(text, cause)

class X12InterchangeParser(in: InputStream, charSet: Charset, handler: X12EnvelopeHandler) extends SchemaJavaDefs {

  val logger = Logger.getLogger(getClass.getName)

  val lexer = new X12Lexer(in, charSet)

  /** Parser for X12 EDI documents. A separate parser instance is created for each interchange. */
  private class X12SchemaParser(inter: ValueMap, root: ValueMap) extends SchemaParser(lexer) {

    /** Configuration in use. */
    private var config: X12ParserConfig = null

    /** Structure code for generated acknowledgments. */
    private var ackTransCode: String = null

    /** Generated functional acknowledgments list. */
    val funcAckList = getRequiredMapList(functionalAcksGenerated, root)

    /** Map of schema version maps for transaction set data. */
    val schemaVersionTransactions = getOrSet(transactionsMap, new ValueMapImpl, root)

    /** Flag for currently in a transaction. */
    var inStructure = false

    /** Flag for group or transaction to be rejected because of errors. */
    var rejectStructure = false

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
    var groupStructureCount = 0

    /** Number of transaction sets accepted in current group. */
    var groupAcceptCount = 0

    /** Control number for current transaction, used in error reporting. */
    var transactionNumber = ""

    /** Starting segment number for transaction. */
    var transactionStartSegment = 0

    /** One or more segments of transaction in error flag. */
    var oneOrMoreSegmentsInError = false

    /** Accumulated data errors (as AK4/Group IK4 maps) from segment. */
    val dataErrors = Buffer[ValueMap]()

    /** Accumulated segment errors (as AK3/IK3 maps) from transaction. */
    val segmentErrors = Buffer[ValueMap]()

    /** Accumulated transaction errors. */
    val transactionErrors = Buffer[TransactionSyntaxError]()

    /** Accumulated group errors. */
    val groupErrors = Buffer[GroupSyntaxError]()

    /** Set the configuration. This must be called at least once prior to using the parser. */
    def setConfig(cfg: X12ParserConfig) = {
      config = cfg
      lexer.asInstanceOf[X12Lexer].configure(config.substitutionChar, config.strChar)
      ackTransCode = if (config generate999) "999" else "997"
    }

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

    def positionInStructure = s"segment ${lexer.getSegmentNumber - transactionStartSegment + 1} of transaction ${transactionNumber} in group $groupNumber of interchange $interchangeNumber"
    def positionInGroup = s"segment ${lexer.getSegmentNumber - groupStartSegment + 1} in group $groupNumber of interchange $interchangeNumber"
    def positionInInterchange = s"segment ${lexer.getSegmentNumber - interchangeStartSegment + 1} of interchange $interchangeNumber"
    def positionInMessage = s"segment ${lexer.getSegmentNumber}"

    def describeError(fatal: Boolean) = if (fatal) "fatal" else "recoverable"

    def describeComponent(incomp: Boolean) =
      if (incomp) {
        val index = 0 max (lexer.getElementNumber - 1)
        s" for component '${currentSegment.components(index).name}'"
      } else ""

    def logErrorInStructure(fatal: Boolean, incomp: Boolean, text: String) =
      logger.error(s"${describeError(fatal)} transaction error '$text'${describeComponent(incomp)} at $positionInStructure")

    def logStructureEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
      logger.error(s"${describeError(fatal)} transaction error '$text'${describeComponent(incomp)} at $positionInGroup")

    def logGroupEnvelopeError(fatal: Boolean, incomp: Boolean, text: String) =
      logger.error(s"${describeError(fatal)} group error '$text'${describeComponent(incomp)} at $positionInInterchange")

    def logInterchangeEnvelopeError(fatal: Boolean, text: String) =
      logger.error(s"${describeError(fatal)} interchange error '$text'' at $positionInMessage")

    /** Accumulate element error, failing transaction if severe. */
    def addElementError(error: ElementSyntaxError) = {
      if (inStructure) {
        val fatal = checkFatal(error)
        logErrorInStructure(fatal, true, error.text)
        if (fatal) rejectStructure = true
        val comp = currentSegment.components(lexer.getElementNumber)
        if (config.reportDataErrors) {
          val xk4 = new ValueMapImpl
          val xk4Comps = segXK4Comps(config generate999)
          val compC030Comps = xk4Comps(0).asInstanceOf[CompositeComponent].composite.components
          xk4 put (compC030Comps(0).key, Integer.valueOf(lexer.getSegmentNumber - transactionStartSegment + 1))
          comp match {
            case ElementComponent(elem, _, _, _, _, _) => xk4 put (xk4Comps(1).key, Integer.valueOf(elem.ident))
            case _: CompositeComponent => xk4 put (compC030Comps(1).key, Integer.valueOf(lexer.getComponentNumber))
          }
          if (comp.count != 1) xk4 put (compC030Comps(2).key, Integer.valueOf(lexer.getRepetitionNumber))
          xk4 put (xk4Comps(2).key, error.code toString)
          if (error != InvalidCharacter) xk4 put (xk4Comps(3).key, lexer.token())
          if (config generate999) {
            val ik4Group = new ValueMapImpl
            ik4Group put (groupIK4.seq.items.head.key, xk4)
            dataErrors += ik4Group
          } else dataErrors += xk4
        }
      } else {
        if (error == MissingRequiredElement) rejectStructure = true
        if (inGroup) logStructureEnvelopeError(true, true, error.text)
        else logGroupEnvelopeError(false, true, error.text)
      }
    }

    /** Report a repetition error on a composite component. */
    def repetitionError(comp: CompositeComponent) = addElementError(TooManyRepititions)

    /** Parse data element value. */
    def parseElement(elem: Element) = {
      val result = elem.dataType match {
        case ALPHA => lexer.parseAlpha(elem.minLength, elem.maxLength)
        case ALPHANUMERIC => lexer.parseAlphaNumeric(elem.minLength, elem.maxLength)
        case BINARY => throw new IOException("Handling not implemented for binary values")
        case DATE => lexer.parseDate(elem.minLength, elem.maxLength)
        case ID => lexer.parseAlphaNumeric(elem.minLength, elem.maxLength)
        case INTEGER => lexer.parseInteger(elem.minLength, elem.maxLength)
        case NUMBER => lexer.parseBigInteger(elem.minLength, elem.maxLength)
        case REAL => lexer.parseBigDecimal(elem.minLength, elem.maxLength)
        case TIME => Integer.valueOf(lexer.parseTime(elem.minLength, elem.maxLength))
        case typ: DataType if (typ.isDecimal) =>
          lexer.parseImpliedDecimalNumber(typ.decimalPlaces, elem.minLength, elem.maxLength)
        case typ: DataType => throw new IllegalArgumentException(s"Data type $typ is not supported in X12")
      }
      lexer.advance
      result
    }

    /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
    def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap) = {
      def checkParse(comp: SegmentComponent, of: ItemType) =
        if (of == lexer.currentType) {
          if (lexer.hasData) parseComponent(comp, of, rest.nextLevel, map)
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
    def parseSegment(segment: Segment, position: SegmentPosition): ValueMap = {
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
          lexer.discardSegment
        }
      }
      if (!dataErrors.isEmpty && config.reportDataErrors) {
        logger.info(s"error(s) found in parsing segment")
        val xk3 = new ValueMapImpl
        val xk3data = new ValueMapImpl
        val xk3Comps = segXK3Comps(config generate999)
        xk3data put (xk3Comps(0) key, segment.ident)
        xk3data put (xk3Comps(1) key, Integer.valueOf(lexer.getSegmentNumber - transactionStartSegment))
        if (loopStack.nonEmpty) xk3data put (xk3Comps(2) key, loopStack.top.ident)
        xk3data put (xk3Comps(3) key, DataErrorsSegment.code.toString)
        val xk3Keys = groupXK3Keys(config generate999)
        xk3 put (xk3Keys(0), xk3data)
        xk3 put (xk3Keys(1), dataErrors.reverse.asJava)
        segmentErrors += xk3
        oneOrMoreSegmentsInError = true
      }
      if (logger.isDebugEnabled) logger.trace(s"now positioned at segment '${lexer.segmentTag}'")
      map
    }

    def segmentNumber = lexer.getSegmentNumber - transactionStartSegment + 1

    /** Report segment error. */
    def segmentError(ident: String, error: ComponentErrors.ComponentError, state: ErrorStates.ErrorState, num: Int) = {
      def addError(fatal: Boolean, error: SegmentSyntaxError) = {
        oneOrMoreSegmentsInError = true
        if (config.reportDataErrors) {
          val xk3 = new ValueMapImpl
          val xk3data = new ValueMapImpl
          val xk3Keys = groupXK3Keys(config generate999)
          xk3 put (xk3Keys(0), xk3data)
          val xk3Comps = segXK3Comps(config generate999)
          xk3data put (xk3Comps(0) key, ident)
          xk3data put (xk3Comps(1) key, Integer.valueOf(num))
          if (loopStack.nonEmpty) xk3data put (xk3Comps(2) key, loopStack.top.ident)
          xk3data put (xk3Comps(3) key, error.code.toString)
          segmentErrors += xk3
        }
        logErrorInStructure(fatal, false, s"${error.text}: $ident")
        if (fatal) rejectStructure = true
      }

      error match {
        case ComponentErrors.TooManyLoops => addError(config.occursFail, TooManyLoops)
        case ComponentErrors.TooManyRepetitions => addError(config.occursFail, TooManyOccurs)
        case ComponentErrors.MissingRequired => addError(true, MissingMandatorySegment)
        case ComponentErrors.UnknownSegment => addError(config.unknownFail, UnrecognizedSegment)
        case ComponentErrors.OutOfOrderSegment => addError(config.orderFail, OutOfOrderSegment)
        case ComponentErrors.UnusedSegment => if (config.unusedFail) addError(true, UnexpectedSegment)
      }
      state match {
        case ErrorStates.WontParse => lexer.discardSegment
        case _ =>
      }
    }

    /** Check if at interchange envelope segment. */
    def isInterchangeEnvelope = lexer.currentType == SEGMENT &&
      (lexer.segmentTag == InterchangeStartSegment || lexer.segmentTag == InterchangeEndSegment)

    /** Check if an envelope segment (handled directly, outside of structure). */
    def isEnvelopeSegment(ident: String) = X12.isEnvelopeSegment(ident)

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
        groupStructureCount = 0
        groupAcceptCount = 0
        val map = parseSegment(GSSegment, outsidePosition)
        inGroup = true
        map
      } else throw new IllegalStateException("missing required GS segment")

    /** Check if at functional group close segment. */
    def isGroupClose = checkSegment(GESegment)

    /** Parse close of a functional group. Returns number of transaction sets included in group. */
    def closeGroup(props: ValueMap) = {
      inGroup = false
      if (checkSegment(GESegment)) {
        val endprops = parseSegment(GESegment, SegmentPosition(0, "9999"))
        if (props.get(groupControlNumberHeaderKey) != endprops.get(groupControlNumberTrailerKey)) groupError(GroupControlNumberMismatch)
        if (endprops.get(groupNumberSetsIncludedKey) != groupStructureCount) groupError(GroupTransactionCountError)
        endprops.get(groupNumberSetsIncludedKey).asInstanceOf[Integer]
      } else {
        groupError(MissingGroupTrailer)
        Integer valueOf (0)
      }
    }

    def transactionError(error: TransactionSyntaxError) = {
      transactionErrors += error
      logErrorInStructure(true, false, error.text)
    }

    /** Check if at transaction set open segment. */
    def isSetOpen = checkSegment(STSegment)

    /** Parse start of a transaction set. */
    def openSet =
      if (checkSegment(STSegment)) {
        transactionErrors.clear
        oneOrMoreSegmentsInError = false
        inStructure = true
        transactionStartSegment = lexer.getSegmentNumber
        val values = parseSegment(STSegment, outsidePosition)
        groupStructureCount += 1
        (values.get(setIdentifierCodeKey).asInstanceOf[String], values)
      } else throw new IllegalStateException("not positioned at ST segment")

    /** Check if at transaction set close segment. */
    def isSetClose = checkSegment(SESegment)

    /** Parse close of a transaction set. */
    def closeSet(props: ValueMap) = {
      if (checkSegment(SESegment)) {
        val endprops = parseSegment(SESegment, SegmentPosition(0, "9999"))
        if (props.get(setControlNumberHeaderKey) != endprops.get(setControlNumberTrailerKey)) {
          transactionError(ControlNumberMismatch)
        }
        val segcount = lexer.getSegmentNumber - transactionStartSegment
        if (endprops.get(setNumberSegmentsIncludedKey) != Integer.valueOf(segcount)) transactionError(WrongSegmentCount)
        inStructure = false
      } else transactionError(MissingTrailerTransaction)
    }

    /** Convert section control segment to next section number. If not at a section control, this just returns None. */
    def convertSectionControl = None

    /** Convert loop start or end segment to identity form. If not at a loop segment, this just returns None. */
    def convertLoop =
      if (lexer.segmentTag == "LS" || lexer.segmentTag == "LE") Some(lexer.segmentTag + lexer.peekToken)
      else None

    /** Discard input past end of current transaction. */
    def discardStructure = {
      while (lexer.currentType != SEGMENT || lexer.segmentTag != SESegment.ident) lexer.discardSegment
      lexer.discardSegment
    }

    /** Discard input to end of current group. */
    def discardToGroupEnd =
      while (!isGroupClose)
        if (isSetOpen) {
          groupStructureCount += 1
          discardStructure
        } else lexer.discardSegment

    /** Parse transactions in group. */
    def parseGroup(group: ValueMap, version: String, ackhead: ValueMap) = {

      def handleStructure(t: Structure, setprops: ValueMap, setack: ValueMap): ValueMap = {
        val data = parseStructure(t, false)
        data put (interchangeKey, inter)
        data put (groupKey, group)
        data put (setKey, setprops)
        val key = if (config generate999) groupIK3.key else groupAK3.key
        if (segmentErrors.nonEmpty) setack put (key, segmentErrors.asJava)
        data
      }

      val setacks = new MapListImpl
      val transLists = getOrSet("v" + version.take(6), new ValueMapImpl, schemaVersionTransactions)
      while (lexer.currentType != END && !isGroupEnvelope && !isInterchangeEnvelope) {
        if (isSetOpen) {
          val (setid, setprops) = openSet
          transactionNumber = getRequiredString(setControlNumberHeaderKey, setprops)
          val setack = new ValueMapImpl
          val ak2data = new ValueMapImpl
          val groupKeys = groupXK2Keys(config.generate999)
          setack put (groupKeys(0), ak2data)
          ak2data put (segAK2.components(0) key, setprops get (setIdentifierCodeKey))
          ak2data put (segAK2.components(1) key, transactionNumber)
          if (version.startsWith("005010") && setprops.containsKey(setImplementationConventionKey)) {
            ak2data put (segAK2.components(2) key, setprops get (setImplementationConventionKey))
          }
          val xk5data = new ValueMapImpl
          setack put (groupKeys(2), xk5data)
          rejectStructure = false
          var data: ValueMap = null
          val result = handler.handleSt(setprops)
          result match {
            case s: Structure => {
              data = handleStructure(s, setprops, setack)
              data put (structureSchema, s)
            }
            case e: TransactionSyntaxError => transactionError(e)
          }
          while (lexer.currentType != END && !isEnvelopeSegment(lexer.segmentTag)) {
            logger.error(s"discarding $positionInStructure (${lexer.segmentTag}) found when looking for transaction set end")
            lexer.discardSegment
          }
          if (isSetClose) closeSet(setprops)
          else transactionError(MissingTrailerTransaction)
          if (oneOrMoreSegmentsInError) transactionError(SegmentsInError)
          if (transactionErrors.nonEmpty) rejectStructure = true
          val xk5Comps = segXK5Comps(config.generate999)
          if (rejectStructure) {
            xk5data put (xk5Comps(0) key, RejectedTransaction code)
            val limit = math.min(xk5Comps.length - 2, transactionErrors.length)
            (0 until limit) foreach (i => xk5data put (xk5Comps(i + 1) key, transactionErrors(i).code.toString))
          } else {
            val list = getOrSet(setid, new MapListImpl, transLists)
            list add (data)
            groupAcceptCount += 1
            xk5data put (xk5Comps(0) key, AcceptedTransaction code)
          }
          if (rejectStructure || segmentErrors.nonEmpty) setacks add setack
        } else {
          logger.error(s"discarding $positionInGroup (${lexer.segmentTag}) found when looking for transaction set start")
          lexer.discardSegment
        }
      }
      if (setacks.size > 0) ackhead put (ackTransKeys(config generate999)(2), setacks)
    }

    def buildAckRoot(interchange: ValueMap) = {
      val ackroot = new ValueMapImpl
      ackroot put (structureId, ackTransCode)
      ackroot put (structureName, (if (config generate999) trans999 else trans997) name)
      val intercopy = new ValueMapImpl(interchange)
      ackroot put (interchangeKey, intercopy)
      swap(SENDER_ID_QUALIFIER, RECEIVER_ID_QUALIFIER, intercopy)
      swap(SENDER_ID, RECEIVER_ID, intercopy)
      ackroot
    }

    def parseInterchangeGroups = {
      lexer.setHandler(X12ErrorHandler)
      rejectStructure = false
      while (isGroupOpen) {
        val group = openGroup
        if (rejectStructure) throw new X12InterchangeException(InterchangeInvalidContent, "invalid GH segment")
        else {
          val groupErr = handler.handleGs(group) match {
            case e: GroupSyntaxError => e
            case x =>
              if (x.isInstanceOf[X12ParserConfig]) setConfig(x.asInstanceOf[X12ParserConfig])
              null
          }
          groupStartSegment = lexer.getSegmentNumber - 2
          groupNumber = getRequiredInt(groupControlNumberHeaderKey, group)
          val version = getRequiredString(groupVersionReleaseIndustryKey, group)
          lexer.countGroup
          val ackroot = buildAckRoot(inter)
          val groupcopy = new ValueMapImpl(group)
          swap(groupApplicationSenderKey, groupApplicationReceiverKey, groupcopy)
          ackroot put (groupKey, groupcopy)
          val ackhead = new ValueMapImpl
          ackroot put (structureHeading, ackhead)
          ackroot put (structureDetail, new ValueMapImpl)
          ackroot put (structureSummary, new ValueMapImpl)
          ackroot put (structureSchema, if (config generate999) trans999 else trans997)
          val ak1data = new ValueMapImpl
          ackhead put (ackTransKeys(config generate999)(1), ak1data)
          ak1data put (segAK1Comps(0) key, group get (groupFunctionalIdentifierKey))
          ak1data put (segAK1Comps(1) key, group get (groupControlNumberHeaderKey))
          if (version.startsWith("005")) ak1data put (segAK1Comps(2) key, version)
          var countPresent = 0
          if (groupErr == null) {
            parseGroup(group, version, ackhead)
            countPresent = closeGroup(group)
          } else {
            groupError(groupErr)
            discardToGroupEnd
            lexer.discardSegment
          }
          val ak9data = new ValueMapImpl
          val error = ackhead.containsKey(segAK2 ident)
          ackhead put (ackTransKeys(config generate999)(3), ak9data)
          val result =
            if (groupErrors.nonEmpty) RejectedGroup
            else if (groupStructureCount == groupAcceptCount) if (error) AcceptedWithErrorsGroup else AcceptedGroup
            else if (groupAcceptCount > 0) PartiallyAcceptedGroup
            else RejectedGroup
          ak9data put (segAK9.components(0) key, result code)
          ak9data put (segAK9.components(1) key, Integer valueOf (countPresent))
          ak9data put (segAK9.components(2) key, Integer valueOf (groupStructureCount))
          ak9data put (segAK9.components(3) key, Integer valueOf (groupAcceptCount))
          val limit = math.min(segAK9.components.length - 5, groupErrors.length)
          (0 until limit) foreach (i => ak9data put (segAK9.components(i + 4) key, groupErrors(i).code.toString))
          if (getRequiredString(groupFunctionalIdentifierKey, group) != "FA") funcAckList add (ackroot)
        }
      }
    }

    /** Parse all content of an interchange.
      */
    def parseInterchange = {
      interchangeStartSegment = lexer.getSegmentNumber - 1
      interchangeNumber = getRequiredInt(INTER_CONTROL, inter)
      root put (interchangeKey, inter)
      if (checkSegment("ISB")) lexer.discardSegment
      if (checkSegment("ISE")) lexer.discardSegment
      if (checkSegment("TA3")) lexer.discardSegment
      if (checkSegment("TA1")) {
        val receiveTA1s =
          if (root.containsKey(interchangeAcksReceived)) getAs[MapList](interchangeAcksReceived, root)
          else new MapListImpl
        while (lexer.segmentTag == "TA1") receiveTA1s add parseSegment(segTA1, SegmentPosition(0, ""))
        root put (interchangeAcksReceived, receiveTA1s)
      }
    }
  }

  def init(data: ValueMap): X12Lexer.InterchangeStartStatus = lexer.init(data)

  /** Check if at segment start. */
  def checkSegment(ident: String) = lexer.currentType == SEGMENT && lexer.segmentTag == ident

  /** Discard input past end of current interchange. */
  def discardInterchange = {
    while (lexer.currentType != END && lexer.segmentTag != "IEA") lexer.discardSegment
    while (lexer.nextType != SEGMENT && lexer.currentType != END) lexer.advance
  }

  def term(props: ValueMap): X12Lexer.InterchangeEndStatus = lexer.term(props)

  /** Parse the entire input. */
  def parse: Try[ValueMap] = Try(try {

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
    map put (functionalAcksGenerated, funcAckList)
    val transLists = new ValueMapImpl().asInstanceOf[ju.Map[String, MapList]]
    map put (transactionsMap, transLists)

    def interchangeError(interchange: ValueMap, code: InterchangeNoteCode) = {
      val text = s"Irrecoverable error in $InterchangeStartSegment at ${lexer.getSegmentNumber - 1}" +
        (if (interchange.containsKey(INTER_CONTROL)) " with control number " + interchange.get(INTER_CONTROL)) +
        ": " + code
      logger error text
      throw X12InterchangeException(code, text)
    }

    def buildTA1(ack: InterchangeAcknowledgmentCode, note: InterchangeNoteCode, inter: ValueMap) = {
      val ta1map = new ValueMapImpl
      ta1map put (segTA1.components(0) key, inter get (INTER_CONTROL))
      ta1map put (segTA1.components(1) key, inter get (INTERCHANGE_DATE))
      ta1map put (segTA1.components(2) key, inter get (INTERCHANGE_TIME))
      ta1map put (segTA1.components(3) key, ack code)
      ta1map put (segTA1.components(4) key, note code)
      interAckList add ta1map
    }

    var done = false
    while (!done) {
      val inter = new ValueMapImpl
      var interchangeAck: InterchangeAcknowledgmentCode = AcknowledgedRejected
      try {
        lexer.setHandler(null)
        init(inter) match {
          case InterchangeStartStatus.NO_DATA => done = true
          case InterchangeStartStatus.VALID => handler.handleIsa(inter) match {
            case code: InterchangeNoteCode => interchangeError(inter, code)
            case x => {
              map put (delimiterCharacters, buildDelims)
              val parser = new X12SchemaParser(inter, map)
              val config = if (x.isInstanceOf[X12ParserConfig]) x.asInstanceOf[X12ParserConfig]
              else X12ParserConfig(true, true, true, true, true, true, true, true, false, -1, CharacterRestriction.BASIC)
              parser.setConfig(config)
              parser.parseInterchange
              interchangeAck = AcknowledgedWithErrors
              if (parser.isGroupOpen) parser.parseInterchangeGroups
              if (lexer.currentType == END) {
                logger.error("end of file with missing IEA")
                throw X12InterchangeException(InterchangeEndOfFile, "end of file with missing IEA")
              }
              if (checkSegment("IEA")) {
                LexerEndStatusInterchangeNote get term(inter) match {
                  case Some(code) => {
                    val text = s"Irrecoverable error in IEA at ${lexer.getSegmentNumber} with control number ${inter.get(INTER_CONTROL)}: ${code.text}"
                    logger error text
                    buildTA1(interchangeAck, code, inter)
                  }
                  case None => buildTA1(AcknowledgedNoErrors, InterchangeNoError, inter)
                }
              } else throw X12InterchangeException(InterchangeInvalidControlStructure, s"Unknown or unexpected control segment ${lexer.segmentTag}")
            }
          }
          case status => LexerStartStatusInterchangeNote get (status) match {
            case Some(code) => interchangeError(inter, code)
            case None => throw new IllegalStateException(s"No handling defined for lexer start status $status")
          }
        }
      } catch {
        case e: X12InterchangeException => buildTA1(interchangeAck, e.note, inter)
        case e: IOException => {
          buildTA1(AcknowledgedRejected, InterchangeEndOfFile, inter)
          throw e
        }
      }
      discardInterchange
    }
    map
  } finally {
    try { lexer close } catch { case e: Throwable => }
  })
}
