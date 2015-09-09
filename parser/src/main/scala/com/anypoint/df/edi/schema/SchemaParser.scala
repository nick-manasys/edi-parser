package com.anypoint.df.edi.schema

import scala.annotation.tailrec
import scala.collection.mutable.{ Buffer, Stack }
import scala.util.Try

import java.io.IOException
import java.io.InputStream

import org.apache.log4j.Logger

import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.EdiConstants.DataType._
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.LexerBase
import com.anypoint.df.edi.lexical.LexicalException
import com.anypoint.df.edi.lexical.ErrorHandler
import com.anypoint.df.edi.lexical.ErrorHandler._
import com.anypoint.df.edi.schema.EdiSchema._

/** Parse EDI document based on schema. */
abstract class SchemaParser(val lexer: LexerBase) extends SchemaJavaDefs {

  import SchemaJavaValues._

  val logger = Logger.getLogger(getClass.getName)

  val outsidePosition = SegmentPosition(0, "0000")

  /** Stack of loop nestings currently active in parse. */
  val loopStack = Stack[GroupComponent]()

  /** Discard current element. */
  def discardElement = {
    lexer.advance
    while (lexer.currentType == COMPONENT || lexer.currentType == REPETITION) lexer.advance
  }

  /** Parse a segment component, which is either an element or a composite. */
  def parseComponent(comp: SegmentComponent, first: ItemType, rest: ItemType, map: ValueMap): Unit = {
    comp match {
      case elemComp: ElementComponent => {
        val elem = elemComp.element
        val value = elem.dataType match {
          case ALPHA => lexer.parseAlpha(elem.minLength, elem.maxLength)
          case ALPHANUMERIC | DATETIME | STRINGDATA | VARIES => lexer.parseAlphaNumeric(elem.minLength, elem.maxLength)
          case BINARY => throw new IOException("Handling not implemented for binary values")
          case DATE => lexer.parseDate(elem.minLength, elem.maxLength)
          case ID => lexer.parseAlphaNumeric(elem.minLength, elem.maxLength)
          case INTEGER => lexer.parseInteger(elem.minLength, elem.maxLength)
          case NUMBER | REAL => lexer.parseNumber(elem.minLength, elem.maxLength)
          case NUMERIC => lexer.parseNumeric(elem.minLength, elem.maxLength)
          case SEQID => lexer.parseSeqId
          case TIME => Integer.valueOf(lexer.parseTime(elem.minLength, elem.maxLength))
          case typ: DataType if (typ.isDecimal) =>
            lexer.parseImpliedDecimalNumber(typ.decimalPlaces, elem.minLength, elem.maxLength)
        }
        map put (comp.key, value)
      }
      case compComp: CompositeComponent => {
        val composite = compComp.composite
        if (comp.count > 1) {
          val complist = new MapListImpl
          map put (comp.key, complist)
          // TODO: is this check necessary? should never get here if not
          if (lexer.currentType == first) {
            val compmap = new ValueMapImpl
            parseCompList(composite.components, first, rest, compmap)
            complist add compmap
            while (lexer.currentType == REPETITION) {
              val repmap = new ValueMapImpl
              parseCompList(composite.components, REPETITION, rest, repmap)
              complist add repmap
            }
            if (complist.size > comp.count) {
              repetitionError(compComp)
              while (complist.size > comp.count) complist.remove(comp.count)
            }
          }
        } else parseCompList(composite.components, first, rest, map)
      }
    }
  }

  /** Report a repetition error on a composite component. This can probably be generalized in the future. */
  def repetitionError(comp: CompositeComponent): Unit

  /** Parse a list of components (which may be the segment itself, a repeated set of values, or a composite). */
  def parseCompList(comps: List[SegmentComponent], first: ItemType, rest: ItemType, map: ValueMap): Unit

  /** Parse a segment to a map of values. The base parser must be positioned at the segment tag when this is called. */
  def parseSegment(segment: Segment, position: SegmentPosition): ValueMap

  /** Check if at segment start. */
  def checkSegment(segment: Segment) = lexer.currentType == SEGMENT && lexer.token == segment.ident

  /** Check if at segment start. */
  def checkSegment(ident: String) = lexer.currentType == SEGMENT && lexer.token == ident

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

  /** Get current segment number for error reporting.
    */
  def segmentNumber: Int

  /** Report segment error.
    * @param ident segment ident
    * @param error
    * @param state current state, used for handling (segment should be discarded if WontParse)
    * @param number segment number
    */
  def segmentError(ident: String, error: ComponentErrors.ComponentError, state: ErrorStates.ErrorState, number: Int): Unit

  /** Parse a complete structure body (not including any envelope segments). The returned map has separate child maps
    * for each of the three sections of a structure (heading, detail, and summary). Each child map uses the component
    * position combined with the segment or group name as key. For a segment or group with no repeats allowed the
    * value is the map of the values in the segment or components in the group. For a segment or group with repeats
    * allowed the value is a list of maps, one for each occurrence..
    */
  def parseStructure(structure: Structure, onepart: Boolean) = {

    import ComponentErrors._
    import ErrorStates._

    /** Get list of maps for key. If the list is not already set, this creates and returns a new one. */
    def getList(key: String, values: ValueMap) =
      if (values.containsKey(key)) values.get(key).asInstanceOf[MapList]
      else {
        val list = new MapListImpl
        values put (key, list)
        list
      }

    /** Parse a structure data table.
      * @param table index
      * @param optseq table structure sequence option
      * @param terms terminations from next table
      */
    def parseTable(table: Int, optseq: Option[StructureSequence], terms: Terminations) = {

      def parseStructureSequence(seq: StructureSequence, terms: List[Terminations], values: ValueMap): Unit = {

        /** Parse subsequence components, matching input against included segments. Each segment is at a unique position
          * in the subsequence, so it's easy to tell when segments are out of order by tracking the current position.
          * Input segments not included in the subsequence are first checked against the terminations set for this
          * subsequence, then against the termination sets for containing loops (at least up to the first required
          * termination), and if found in either of these the subsequence is ended and this returns.
          * @param subsq
          * @returns true if exiting structure sequence, false if just moving to next subsequence
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
              loopStack.push(loop)
              val ident = lexer.token
              val data = new ValueMapImpl
              val number = segmentNumber
              // need to add handling of variant loops back in
              parseStructureSequence(loop.seq, groupTerm :: terms, data)
              if (loop.usage == UnusedUsage) segmentError(ident, UnusedSegment, ParseComplete, number)
              else {
                val count = loop.count
                val key = loop.key
                if (count == 1) {
                  if (values.containsKey(key)) segmentError(ident, TooManyRepetitions, ParseComplete, number)
                  else values put (key, data)
                } else {
                  val list = getList(key, values)
                  if (count > 0 && count <= list.size) segmentError(ident, TooManyRepetitions, ParseComplete, number)
                  list add data
                }
              }
              loopStack.pop
            }

            /** Parse a wrapped loop, handling wrap open and close segments directly.
              * @param wrap
              */
            def parseWrappedLoop(wrap: LoopWrapperComponent): Unit = {
              if (wrap.usage == UnusedUsage)
                segmentError(wrap.open.ident, UnusedSegment, ParseComplete, segmentNumber)
              discardSegment
              parseLoop(wrap.wrapped, wrap.groupTerms)
              convertLoop match {
                case Some(wrap.endCode) => discardSegment
                case _ => segmentError(wrap.close.ident, MissingRequired, ParseComplete, segmentNumber)
              }
            }

            val ident = convertLoop.getOrElse(lexer.token)
            if (!isEnvelopeSegment(ident)) {

              def adjustPosition(nextpos: String) = {
                if (nextpos >= position) nextpos
                else {
                  segmentError(ident, OutOfOrderSegment, BeforeParse, segmentNumber)
                  position
                }
              }

              subsq.comps get (ident) match {
                case Some(ref: ReferenceComponent) =>
                  val segment = ref.segment
                  val key = ref.key
                  val nextpos = adjustPosition(ref.position.position)
                  val number = segmentNumber
                  val data = parseSegment(segment, ref.position)
                  if (ref.usage == UnusedUsage) segmentError(ident, UnusedSegment, ParseComplete, number)
                  else {
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
                case None =>
                  if (subsq.terms.idents.contains(ident)) false
                  else if (checkTerm(ident)) true
                  else {
                    if (structure.segmentIds.contains(lexer.token)) segmentError(lexer.token, OutOfOrderSegment,
                      WontParse, segmentNumber)
                    else segmentError(lexer.token, UnknownSegment, WontParse, segmentNumber)
                    parseComponent(position)
                  }
                case _ => throw new IllegalStateException(s"Illegal structure at position $position")
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

      val map = new ValueMapImpl
      optseq.foreach { seq => parseStructureSequence(seq, List(terms), map) }
      map
    }

    if (onepart) parseTable(0, structure.heading, EdiSchema.emptyTerminations)
    else {
      val topMap: ValueMap = new ValueMapImpl
      topMap put (structureId, structure.ident)
      topMap put (structureName, structure.name)
      topMap put (structureHeading, parseTable(0, structure.heading, structure.detailTerms))
      convertSectionControl match {
        case Some(1) | None => {
          topMap put (structureDetail, parseTable(1, structure.detail, structure.summaryTerms))
        }
        case _ =>
      }
      convertSectionControl match {
        case Some(1) => segmentError(lexer.token, OutOfOrderSegment, ParseComplete, segmentNumber - 1)
        case _ =>
      }
      topMap put (structureSummary, parseTable(2, structure.summary, EdiSchema.emptyTerminations))
      topMap
    }
  }

  /** Discard input past end of current segment. */
  def discardSegment = {
    if (lexer.currentType == SEGMENT) lexer.advance
    while (lexer.currentType != SEGMENT && lexer.currentType != END) lexer.advance
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