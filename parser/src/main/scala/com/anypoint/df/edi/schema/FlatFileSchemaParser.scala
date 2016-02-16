package com.anypoint.df.edi.schema

import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import scala.util.{ Success, Try }

import java.io.{ InputStream, IOException }
import java.nio.charset.Charset
import java.util.{ Calendar, GregorianCalendar }

import com.anypoint.df.edi.lexical.{ ErrorHandler, LexerBase, LexicalException, FlatFileLexer }
import com.anypoint.df.edi.lexical.EdiConstants.{ DataType, ItemType }
import com.anypoint.df.edi.lexical.EdiConstants.DataType._
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition._
import EdiSchema._
import HL7Identity._
import HL7SchemaDefs._
import SchemaJavaValues._
import com.mulesoft.ltmdata.StorageContext

/** Parser for flat file documents. */
case class FlatFileSchemaParser(in: InputStream, struct: Structure)
extends SchemaParser(new FlatFileLexer(in), StorageContext.workingContext) {

  /** Typed lexer, for access to format-specific conversions and support. */
  val lexer = baseLexer.asInstanceOf[FlatFileLexer]

  /** Current segment reference, used in error reporting. */
  var currentSegment: Segment = null

  def describeSegment = if (currentSegment == null) "" else s" ('${currentSegment.ident}')"

  def describeError(fatal: Boolean) = if (fatal) "fatal" else "recoverable"

  def positionInMessage = s"line ${lexer.getSegmentNumber}$describeSegment"

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

  /** Report a repetition error on a composite component. */
  def repetitionError(comp: CompositeComponent) = {}
  
  def segmentError(fatal: Boolean, text: String) = {
    logErrorInMessage(fatal, false, text)
  }
  
  def isEnvelopeSegment(ident: String) = false

  /** Parse data element value. */
  def parseElement(elem: Element) = {
    val result = elem.dataType match {
      case ALPHANUMERIC => lexer.parseAlphaNumeric(elem.minLength, elem.maxLength)
      case DATE => lexer.parseDate(elem.minLength, elem.maxLength)
      case INTEGER => lexer.parseInteger(elem.minLength, elem.maxLength)
      case NUMERIC => lexer.parseUnscaledNumber(elem.minLength, elem.maxLength)
      case TIME => Integer.valueOf(lexer.parseTime(elem.minLength, elem.maxLength))
      case typ: DataType => throw new IllegalArgumentException(s"Data type $typ is not supported in flat files")
    }
    result
  }

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap) = {
    @tailrec
    def parserr(remain: List[SegmentComponent]): Unit = remain match {
      case h :: t => {
        parseComponent(h, ItemType.DATA_ELEMENT, rest.nextLevel, map)
        parserr(t)
      }
      case _ =>
    }

    parserr(comps)
  }

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  def parseSegment(segment: Segment, position: SegmentPosition): ValueMap = {
    if (logger.isTraceEnabled) logger.trace(s"parsing segment ${segment.ident} at position $position")
    val map = storageContext.newMap(segment.keys)
    currentSegment = segment
    parseCompList(segment.components, DATA_ELEMENT, DATA_ELEMENT, map)
    currentSegment = null
    if (lexer.nextLine && logger.isDebugEnabled) logger.trace(s"now positioned at segment '${lexer.segmentTag}'")
    map
  }

  def segmentNumber = lexer.getSegmentNumber + 1

  /** Report segment error. */
  def segmentError(ident: String, error: ComponentErrors.ComponentError, state: ErrorStates.ErrorState, num: Int) = {
    error match {
      case ComponentErrors.TooManyLoops => segmentError(true, s"too many loop instances $ident")
      case ComponentErrors.TooManyRepetitions => segmentError(true, s"too many segment repetitions $ident")
      case ComponentErrors.MissingRequired => segmentError(true, s"missing required segment $ident")
      case ComponentErrors.UnknownSegment => segmentError(false, s"unknown segment $ident")
      case ComponentErrors.OutOfOrderSegment => segmentError(true, s"out of order segment $ident")
      case ComponentErrors.UnusedSegment => segmentError(false, s"unused segment $ident")
    }
    lexer.nextLine
  }

  def convertSectionControl = None

  def convertLoop = None

  /** Discard input past end of current message. */
  def discardStructure = while (lexer.currentType != END) lexer.discardSegment

  /** Parse the input message. */
  def parse: Try[ValueMap] = Try(try {
    val map = new ValueMapImpl
    lexer.setTagField(struct.tagStart.get, struct.tagLength.get)
    lexer.init
    map put (structureId, struct.ident)
    map put (structureName, struct.name)
    map put (struct.ident, parseStructure(struct, true, new ValueMapImpl))
    map
  } catch {
    case t: Throwable =>
      t.printStackTrace
      throw t
  } finally {
    try { lexer close } catch { case e: Throwable => }
  })
}