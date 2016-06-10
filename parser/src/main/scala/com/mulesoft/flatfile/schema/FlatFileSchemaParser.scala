package com.mulesoft.flatfile.schema

import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import scala.util.{ Success, Try }

import java.io.{ InputStream, IOException }
import java.nio.charset.Charset
import java.{ util => ju }

import com.mulesoft.flatfile.lexical.{ EdiConstants, ErrorHandler, LexerBase, LexicalException, FlatFileLexer }
import com.mulesoft.flatfile.lexical.EdiConstants.ItemType
import com.mulesoft.flatfile.lexical.EdiConstants.ItemType._
import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition
import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition._
import EdiSchema._
import SchemaJavaValues._
import com.mulesoft.ltmdata.StorageContext

/** Base parser for flat file documents. */
abstract class FlatFileParserBase(in: InputStream, charSet: Charset, structOpt: Option[Structure])
extends SchemaParser(new FlatFileLexer(in, charSet, true), StorageContext.workingContext) {

  /** Typed lexer, for access to format-specific conversions and support. */
  val lexer = baseLexer.asInstanceOf[FlatFileLexer]

  /** Current segment reference, used in error reporting. */
  var currentSegment: Segment = null
  
  @tailrec
  final def lookupSegment(target: TagTarget): Option[Segment] = {
    target match {
      case TagSegment(segment) => Some(segment)
      case TagNext(offset, length, targets) =>
        val tag = lexer.loadTagField(offset, length)
        targets.get(tag) match {
          case Some(t) => lookupSegment(t)
          case _ => None
        }
    }
  }
  
  override def segmentIdent = {
    if (currentSegment == null) throw new IllegalStateException("Segment not defined")
    else if (currentSegment.ident.nonEmpty) currentSegment.ident
    else currentSegment.name
  }
  
  override def findSegment = {
    structOpt match {
      case Some(struct) =>
        lookupSegment(struct.tagLookup) match {
          case Some(s) => currentSegment = s
          case _ =>
        }
      case _ => throw new IllegalStateException("Not in a structure")
    }
  }

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
    logger.error(s"${describeError(fatal)} message error: $text${describeComponent(incomp)}")

  /** Report a repetition error on a composite component. */
  override def repetitionError(comp: CompositeComponent) = {}
  
  def segmentError(fatal: Boolean, text: String) = {
    logErrorInMessage(fatal, false, text)
  }
  
  override def isEnvelopeSegment(ident: String) = false

  /** Parse data element value. */
  override def parseElement(elem: Element) = {
    lexer.load(elem.typeFormat.maxLength)
    elem.typeFormat.parse(lexer)
  }

  override def parseComponent(comp: SegmentComponent, first: ItemType, rest: ItemType, map: ValueMap): Unit = {
    def storeValue(value: Object) = {
      if (userValue(comp.usage)) map put (comp.key, value)
    }
    comp match {
      case elemComp: ElementComponent =>
        val elem = elemComp.element
        if (comp.count != 1) {
          val complist = storageContext.newValueSeq
          (1 to comp.count).foreach(_ => complist.add(parseElement(elem)))
          storeValue(complist)
        } else storeValue(parseElement(elem))
      case compComp: CompositeComponent => {
        val composite = compComp.composite
        val descript = storageContext.addDescriptor(composite.keys)
        if (comp.count != 1) {
          val complist = storageContext.newMapSeq
          (1 to comp.count).foreach { _ => 
            val compmap: ju.Map[String, Object] = storageContext.newMap(descript)
            parseCompList(composite.components, first, rest, compmap)
            complist.add(compmap)
          }
          storeValue(complist)
        } else {
          val compmap: ju.Map[String, Object] = storageContext.newMap(descript)
          parseCompList(composite.components, first, rest, compmap)
          storeValue(compmap)
        }
      }
    }
  }
  
  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  override def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap) = {
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

  /** Parse a segment to a map of values. The base parser must be positioned at the start of the segment when this is
   * called. */
  override def parseSegment(segment: Segment, position: SegmentPosition): ValueMap = {
    if (logger.isTraceEnabled && position.defined) logger.trace(s"parsing segment ${segment.ident} at position $position")
    val map = storageContext.newMap(segment.keys)
    currentSegment = segment
    parseCompList(segment.components, DATA_ELEMENT, DATA_ELEMENT, map)
    currentSegment = null
    lexer.nextLine
    map
  }

  def segmentNumber = lexer.getSegmentNumber + 1

  /** Report segment error. */
  override def segmentError(ident: String, error: ComponentErrors.ComponentError, state: ErrorStates.ErrorState, num: Int) = {
    error match {
      case ComponentErrors.TooManyLoops => segmentError(true, s"too many loop instances $ident")
      case ComponentErrors.TooManyRepetitions => segmentError(true, s"too many segment repetitions $ident")
      case ComponentErrors.MissingRequired => segmentError(true, s"missing required segment $ident")
      case ComponentErrors.UnknownSegment => segmentError(false, s"unknown segment $ident")
      case ComponentErrors.OutOfOrderSegment => segmentError(true, s"out of order segment $ident")
      case ComponentErrors.UnusedSegment => segmentError(false, s"unused segment $ident")
    }
    lexer.discardSegment
  }

  override def convertSectionControl = None

  override def convertLoop = None

  /** Discard input past end of current message. */
  override def discardStructure = while (lexer.currentType != END) lexer.discardSegment 
  
  /** Parse the input message. */
  def parse: Try[ValueMap]
}

/** Parser for structured flat file documents. */
class FlatFileStructureParser(in: InputStream, cs: Charset, struct: Structure)
extends FlatFileParserBase(in, cs, Some(struct)) {

  /** Parse the input message. */
  override def parse: Try[ValueMap] = Try(try {
    val map = new ValueMapImpl
    lexer.init
    map put (structureId, struct.ident)
    map put (structureName, struct.name)
    map put (dataKey, parseStructure(struct, true, new ValueMapImpl))
    map
  } catch {
    case t: Throwable =>
      t.printStackTrace
      throw t
  } finally {
    try { lexer close } catch { case e: Throwable => }
  })
}

/** Parser for single repeated segment documents. */
class FlatFileSegmentParser(in: InputStream, cs: Charset, segment: Segment) extends FlatFileParserBase(in, cs, None) {

  /** Parse the input message. */
  override def parse: Try[ValueMap] = Try(try {
    val map = new ValueMapImpl
    lexer.init
    val data = new MapListImpl
    map put (dataKey, data)
    while (lexer.currentType != END) data.add(parseSegment(segment, StartPosition))
    map
  } catch {
    case t: Throwable =>
      t.printStackTrace
      throw t
  } finally {
    try { lexer close } catch { case e: Throwable => }
  })
}


/** Parser for documents containing any defined segments in any order. */
class FlatFileUnorderedParser(in: InputStream, cs: Charset, schema: EdiSchema)
extends FlatFileParserBase(in, cs, None) {

  /** Parse the input message. */
  override def parse: Try[ValueMap] = Try(try {
    val map = new ValueMapImpl
    lexer.init
    val data = new ValueMapImpl
    map put (dataKey, data)
    while (lexer.currentType != END) {
      lookupSegment(schema.tagLookup) match {
        case Some(s) =>
          val segdata = parseSegment(s, StartPosition)
          getOrSet(s.ident, new MapListImpl, data).add(segdata)
        case _ =>
          segmentError(true, "Unrecognized segment")
          lexer.discardSegment
      }
    }
    map
  } catch {
    case t: Throwable =>
      t.printStackTrace
      throw t
  } finally {
    try { lexer close } catch { case e: Throwable => }
  })
}