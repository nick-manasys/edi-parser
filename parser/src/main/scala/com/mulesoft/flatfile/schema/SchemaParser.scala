package com.mulesoft.flatfile.schema

import scala.annotation.tailrec
import scala.collection.mutable.{ Buffer, Stack }
import scala.util.Try

import java.io.{ IOException, InputStream }
import java.{ util => ju }

import org.apache.log4j.Logger

import com.mulesoft.flatfile.lexical.{ DelimiterLexer, ErrorHandler, LexerBase, LexicalException }
import com.mulesoft.flatfile.lexical.EdiConstants._
import com.mulesoft.flatfile.lexical.EdiConstants.ItemType._
import com.mulesoft.flatfile.lexical.ErrorHandler._
import com.mulesoft.flatfile.schema.EdiSchema._

import com.mulesoft.ltmdata.StorageContext

/** Parse EDI document based on schema. */
abstract class SchemaParser(val baseLexer: LexerBase, val storageContext: StorageContext) extends SchemaJavaDefs {

  import SchemaJavaValues._

  val logger = Logger.getLogger(getClass.getName)

  /** Stack of wrapped loop nestings currently active in parse. */
  val loopStack = Stack[LoopWrapperComponent]()
  
  def userValue(usage: Usage) = usage match {
    case UnusedUsage | IgnoredUsage => false
    case _ => true
  }

  /** Report a repetition error on a composite component. This can probably be generalized in the future. */
  def repetitionError(comp: CompositeComponent): Unit

  /** Parse a segment component, which is either an element or a composite. */
  def parseComponent(comp: SegmentComponent, first: ItemType, rest: ItemType, map: ValueMap): Unit

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap): Unit
  
  /** Identify current segment for processing (sets up the segment identifier, if necessary). */
  def findSegment: Unit
  
  /** Get segment identifier in preparation for parsing a segment within structure. */
  def segmentIdent: String

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  def parseSegment(segment: Segment, position: SegmentPosition): ValueMap

  /** Check if at segment start. */
  def checkSegment(segment: Segment) = baseLexer.currentType == SEGMENT && segmentIdent == segment.ident

  /** Check if at segment start. */
  def checkSegment(ident: String) = baseLexer.currentType == SEGMENT && segmentIdent == ident

  object ComponentErrors {
    sealed trait ComponentError
    case object TooManyLoops extends ComponentError
    case object TooManyRepetitions extends ComponentError
    case object MissingRequired extends ComponentError
    case object UnknownSegment extends ComponentError
    case object OutOfOrderSegment extends ComponentError
    case object UnusedSegment extends ComponentError
  }

  object ErrorStates {
    sealed trait ErrorState
    case object BeforeParse extends ErrorState
    case object ParseComplete extends ErrorState
    case object WontParse extends ErrorState
  }

  /** Get current segment number for error reporting. */
  def segmentNumber: Int

  /**
   * Report segment error.
   * @param ident segment ident
   * @param error
   * @param state current state, used for handling (segment should be discarded if WontParse)
   * @param number segment number
   */
  def segmentError(ident: String, error: ComponentErrors.ComponentError, state: ErrorStates.ErrorState, number: Int): Unit

  /**
   * Parse a complete structure body (not including any envelope segments). If the call specifies onepart, a single
   * child map is added for the structure data; otherwise, separate child maps are used for each of the three standard
   * sections (heading, detail, and summary). Each child map uses the component position combined with the segment or
   * group name as key. For a segment or group with no repeats allowed the value is the map of the values in the
   * segment or components in the group. For a segment or group with repeats allowed the value is a list of maps, one
   * for each occurrence..
   */
  def parseStructure(structure: Structure, onepart: Boolean, topMap: ValueMap) = {

    import ComponentErrors._
    import ErrorStates._

    /** Get list of maps for key. If the list is not already set, this creates and returns a new one. */
    def getList(key: String, values: ValueMap): ju.List[ju.Map[String, Object]] =
      if (values.containsKey(key)) values.get(key).asInstanceOf[ju.List[ju.Map[String, Object]]]
      else {
        val list = storageContext.newMapSeq
        values put (key, list)
        list
      }

    /**
     * Parse a structure data table. As a convenience, this returns the passed-in table data map.
     * @param table index
     * @param optseq table structure sequence option
     * @param terms terminations from next table
     * @param map table data map
     */
    def parseTable(table: Int, optseq: Option[StructureSequence], terms: Terminations, map: ValueMap) = {

      def parseStructureSequence(seq: StructureSequence, terms: List[Terminations], values: ValueMap): Unit = {

        /**
         * Parse subsequence components, matching input against included segments. Each segment is at a unique position
         * in the subsequence, so it's easy to tell when segments are out of order by tracking the current position.
         * Input segments not included in the subsequence are first checked against the terminations set for this
         * subsequence, then against the termination sets for containing loops (at least up to the first required
         * termination), and if found in either of these the subsequence is ended and this returns.
         * @param subsq
         * @return true if exiting structure sequence, false if just moving to next subsequence
         */
        def parseSubsequence(subsq: StructureSubsequence): Boolean = {

          def checkTerm(ident: String) = {
            @tailrec
            def checkr(rem: List[Terminations]): Boolean = rem match {
              case h :: t =>
                if (h.idents.contains(ident)) true
                else if (h.required > 1) false
                else checkr(t)
              case _ => false
            }

            checkr(terms)
          }

          @tailrec
          def parseComponent(position: String): Boolean = {

            def parseLoop(loop: GroupComponent, groupTerm: Terminations): Unit = {
              val ident = segmentIdent
              val data = storageContext.newMap(loop.keys)
              val number = segmentNumber
              if (loop.usage == UnusedUsage) segmentError(ident, UnusedSegment, ParseComplete, number)
              // need to add handling of variant loops back in
              parseStructureSequence(loop.seq, groupTerm :: terms, data)
              if (loop.usage != UnusedUsage) {
                val count = loop.count
                val key = loop.key
                if (count == 1) {
                  if (values.containsKey(key)) segmentError(ident, TooManyLoops, ParseComplete, number)
                  else values put (key, data)
                } else {
                  val list = getList(key, values)
                  if (count > 0 && count <= list.size) segmentError(ident, TooManyLoops, ParseComplete, number)
                  list add data
                }
              }
            }

            /**
             * Parse a wrapped loop, handling wrap open and close segments directly.
             * @param wrap
             */
            def parseWrappedLoop(wrap: LoopWrapperComponent): Unit = {
              @tailrec
              def parser: String = {
                loopStack.push(wrap)
                parseLoop(wrap.wrapped, wrap.groupTerms)
                loopStack.pop
                val ident = convertLoop.getOrElse(segmentIdent)
                if (ident == wrap.wrapped.leadSegmentRef.segment.ident) parser
                else ident
              }
              
              if (wrap.usage == UnusedUsage) segmentError(wrap.open.ident, UnusedSegment, ParseComplete, segmentNumber)
              baseLexer.discardSegment
              if (parser == wrap.endCode) baseLexer.discardSegment
              else segmentError(wrap.close.ident, MissingRequired, ParseComplete, segmentNumber)
            }

            findSegment
            val ident = convertLoop.getOrElse(segmentIdent)
            if (baseLexer.currentType == ItemType.SEGMENT && !isEnvelopeSegment(ident)) {

              def adjustPosition(nextpos: String) = {
                if (nextpos >= position) nextpos
                else {
                  segmentError(ident, OutOfOrderSegment, BeforeParse, segmentNumber)
                  position
                }
              }

              val compStruct = subsq.comps get (ident)
              compStruct match {
                case Some(ref: ReferenceComponent) =>
                  val segment = ref.segment
                  val key = ref.key
                  val nextpos = adjustPosition(ref.position.position)
                  val number = segmentNumber
                  if (ref.usage == UnusedUsage) segmentError(ident, UnusedSegment, ParseComplete, number)
                  val data = parseSegment(segment, ref.position)
                  if (ref.usage != UnusedUsage) {
                    val count = ref.count
                    if (count == 1) {
                      if (values.containsKey(key)) segmentError(ident, TooManyRepetitions, ParseComplete, number)
                      else values put (key, data)
                    } else {
                      val list = getList(key, values)
                      if (count > 0 && count <= list.size)
                        segmentError(segment.ident, TooManyRepetitions, ParseComplete, number)
                      list add data
                    }
                  }
                  if (subsq.terms.idents.contains(ident)) false
                  else parseComponent(nextpos)
                case Some(loop: GroupComponent) =>
                  val nextpos = adjustPosition(loop.position.position)
                  parseLoop(loop, subsq.groupTerms(loop))
                  if (subsq.terms.idents.contains(ident)) false
                  else parseComponent(nextpos)
                case Some(wrap: LoopWrapperComponent) =>
                  val nextpos = adjustPosition(wrap.wrapped.position.position)
                  parseWrappedLoop(wrap)
                  parseComponent(nextpos)
                case Some(x) =>
                  val text = s"Illegal structure of type ${x.getClass.getName} at position $position (ident $ident)"
                  throw new IllegalStateException(text)
                case _ =>
                  // it's got to be a None, but this way it'll be handled even when None changes (see B2B-80)
                  if (subsq.terms.idents.contains(ident)) false
                  else if (checkTerm(ident)) true
                  else {
                    if (structure.segmentIds.contains(segmentIdent)) segmentError(segmentIdent, OutOfOrderSegment,
                      WontParse, segmentNumber)
                    else segmentError(segmentIdent, UnknownSegment, WontParse, segmentNumber)
                    parseComponent(position)
                  }
              }
            } else true
          }

          parseComponent(subsq.startPos)
        }

        @tailrec
        def parser(rem: List[StructureSubsequence]): Unit = rem match {
          case h :: t => if (!parseSubsequence(h)) parser(t)
          case _ =>
        }

        parser(seq.subSequences)
        seq.requiredComps.foreach { comp =>
          if (!values.containsKey(comp.key)) {
            comp match {
              case ref: ReferenceComponent => if (!isEnvelopeSegment(ref.segment.ident))
                segmentError(ref.segment.ident, MissingRequired, ParseComplete, segmentNumber)
              case wrap: LoopWrapperComponent =>
                segmentError(wrap.open.ident, MissingRequired, ParseComplete, segmentNumber)
              case grp: GroupComponent =>
                segmentError(grp.leadSegmentRef.segment.ident, MissingRequired, ParseComplete, segmentNumber)
            }
          }
        }
      }

      optseq.foreach { seq => parseStructureSequence(seq, List(terms), map) }
      map
    }

    if (onepart) parseTable(0, structure.heading, EdiSchema.emptyTerminations, topMap)
    else {
      topMap put (structureId, structure.ident)
      topMap put (structureName, structure.name)
      topMap put (structureHeading, parseTable(0, structure.heading, structure.detailTerms, new ValueMapImpl))
      convertSectionControl match {
        case Some(1) | None => {
          topMap put (structureDetail, parseTable(1, structure.detail, structure.summaryTerms, new ValueMapImpl))
        }
        case _ =>
      }
      convertSectionControl match {
        case Some(1) => segmentError(segmentIdent, OutOfOrderSegment, ParseComplete, segmentNumber - 1)
        case _ =>
      }
      topMap put (structureSummary, parseTable(2, structure.summary, EdiSchema.emptyTerminations, new ValueMapImpl))
    }
  }

  /** Discard input past end of current structure. */
  def discardStructure: Unit

  /** Convert section control segment to next section number. If not at a section control, this just returns None. */
  def convertSectionControl: Option[Int]

  /** Convert loop start or end segment to identity form. If not at a loop segment, this just returns None. */
  def convertLoop: Option[String]

  /** Check if an envelope segment (handled directly, outside of structure). */
  def isEnvelopeSegment(ident: String): Boolean
}

abstract class DelimiterSchemaParser(val delimLexer: DelimiterLexer, sc: StorageContext)
extends SchemaParser(delimLexer, sc) {

  /** Parse data element value (and if appropriate, advance to the next element). */
  def parseElement(elem: Element): Object
  
  override def findSegment = Unit
  
  override def segmentIdent = delimLexer.segmentTag

  override def parseComponent(comp: SegmentComponent, first: ItemType, rest: ItemType, map: ValueMap): Unit = {
    def storeValue(value: Object) = {
      if (userValue(comp.usage)) map put (comp.key, value)
    }
    comp match {
      case elemComp: ElementComponent =>
        val elem = elemComp.element
        if (comp.count != 1) {
          val complist = storageContext.newValueSeq
          complist add parseElement(elem)
          while (baseLexer.currentType == REPETITION) complist add parseElement(elem)
          storeValue(complist)
        } else storeValue(parseElement(elem))
      case compComp: CompositeComponent => {
        val composite = compComp.composite
        if (comp.count != 1) {
          val complist = storageContext.newMapSeq
          // TODO: is this check necessary? should never get here if not
          if (baseLexer.currentType == first) {
            val descript = storageContext.addDescriptor(composite.keys)
            val compmap: ju.Map[String, Object] = storageContext.newMap(descript)
            parseCompList(composite.components, first, rest, compmap)
            complist.add(compmap)
            while (baseLexer.currentType == REPETITION) {
              val repmap = storageContext.newMap(descript)
              parseCompList(composite.components, REPETITION, rest, repmap)
              complist add repmap
            }
            if (comp.count > 0 && complist.size > comp.count) {
              repetitionError(compComp)
              while (complist.size > comp.count) complist.remove(comp.count)
            }
          } else throw new IllegalStateException("internal error - unexpected state")
          storeValue(complist)
        } else parseCompList(composite.components, first, rest, map)
      }
    }
  }
}