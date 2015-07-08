package com.anypoint.df.edi.schema

import java.io.IOException
import java.io.InputStream
import org.apache.log4j.Logger
import scala.annotation.tailrec
import scala.util.Try
import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.EdiConstants.DataType._
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.LexerBase
import com.anypoint.df.edi.lexical.LexicalException
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.lexical.ErrorHandler
import com.anypoint.df.edi.lexical.ErrorHandler._
import scala.collection.mutable.Buffer

/** Parse EDI document based on schema. */
abstract class SchemaParser(val lexer: LexerBase, val schema: EdiSchema) extends SchemaJavaDefs {

  import SchemaJavaValues._

  val logger = Logger.getLogger(getClass.getName)

  val outsidePosition = SegmentPosition(0, "0000")

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
  def parseSegment(segment: Segment, group: Option[GroupComponent], position: SegmentPosition): ValueMap

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

  /** Report segment error.
    * @param ident segment ident
    * @param group containing group
    * @param error
    * @param flush report error immediately (segment already parsed or will not be parsed)
    */
  def segmentError(ident: String, group: Option[String], error: ComponentErrors.ComponentError, flush: Boolean): Unit

  /** Containing scope within a parse. This gives a way of looking into scopes back up the tree of nested scopes for
    * the current point in the parse, to 1) tell whether a segment that doesn't fit in the current scope is for some
    * containing scope and 2) whether the segment should have preceded the current parse position (in which case it's
    * considered out of order), is at the current parse position (for a loop, which terminates all nested scopes up to
    * the one where it's defined), or follows the current parse position (which terminates to that level).
    * @param comps id (or id+loop identifier code, in the case of an LS/LE segment) to component
    * @param lead segment starting the scope (allowed to go backward in position, since it'll be starting another loop)
    * @param current position string for current place at level
    */
  case class ContainingScope(comps: Map[String, TransactionComponent], lead: Option[Segment], current: SegmentPosition)

  /** Parse a complete transaction body (not including any envelope segments). The returned map has a maximum of five
    * values: the transaction id and name, and separate child maps for each of the three sections of a transaction
    * (heading, detail, and summary). Each child map is keyed by segment name (with the ID suffixed in parenthesis) or
    * group id. For a segment with no repeats allowed the associated value is the map of the values in the segment. For
    * a segment with repeats allowed the value is a list of maps, one for each occurrence of the segment. For a group
    * the value is also a list of maps, with each map of the same form as the child maps of the top-level result (so
    * keys are segment or nested group names, values are maps or lists).
    */
  def parseTransaction(transaction: Transaction, onepart: Boolean) = {

    /** Get list of maps for key. If the list is not already set, this creates and returns a new one. */
    def getList(key: String, values: ValueMap) =
      if (values.containsKey(key)) values.get(key).asInstanceOf[MapList]
      else {
        val list = new MapListImpl
        values put (key, list)
        list
      }

    /** Parse a (potentially) repeating segment into a list of maps. */
    def parseRepeatingSegment(segment: Segment, limit: Int, group: Option[GroupComponent], position: SegmentPosition): MapList = {
      val list: MapList = new MapListImpl
      @tailrec
      def parseRepeat: Unit =
        if (checkSegment(segment)) {
          if (limit > 0 && limit <= list.size) {
            segmentError(segment.ident, group.map { _.ident }, ComponentErrors.TooManyRepetitions, false)
          }
          list add (parseSegment(segment, group, position))
          parseRepeat
        }
      parseRepeat
      list
    }

    /** Parse a transaction data table.
      * @param table index
      * @param comps segment id to component map
      */
    def parseTable(table: Int, comps: Map[String, TransactionComponent]) = {

      /** Parse a (potentially) repeating group, with variants treated separately. The repeating group is represented as a
        * list of maps, one for each occurrence of the group not recognized as a variant. Variants which can occur
        * multiple times are represented as separate lists of maps, those which can only occur once are represented as
        * simple maps. Key values for variants are derived by concatenating the variant field value to the base group key
        * separated by an underscore.
        */
      def parseRepeatingGroup(group: GroupComponent, values: ValueMap, scopes: List[ContainingScope]): Unit = {
        def verifyRepeats(key: String, max: Int) =
          if (max != 1) {
            // make sure list is set in map
            val list = getList(key, values)
            if (max > 1 && list.size == max) {
              segmentError(group.ident, Some(group.ident), ComponentErrors.TooManyLoops, true)
            }
          }
        def addInstance(key: String, data: ValueMap) =
          if (values.containsKey(key)) {
            values get (key) match {
              case list: MapList => list add (data)
              case _ => segmentError(group.ident, Some(group.ident), ComponentErrors.TooManyLoops, true)
            }
          } else values put (key, data)
        @tailrec
        def parseRepeat: Unit = {
          val leadref = group.leadSegmentRef
          val leadseg = leadref.segment
          if (logger.isTraceEnabled) logger.trace(s"checking for loop start segment '${leadseg.ident}'': ${checkSegment(leadseg)}")
          if (checkSegment(leadseg)) {
            if (group.usage == UnusedUsage) {
              segmentError(leadseg.ident, Some(group.ident), ComponentErrors.UnusedSegment, false)
            }
            val parse = new ValueMapImpl
            if (leadref.count == 1) {
              val leadmap = parseSegment(leadseg, Some(group), leadref.position)
              val varval = if (group.varkey.isDefined) getAsString(group.varkey.get, leadmap) else null
              if (group.varbyval.contains(varval)) {
                val variant = group.varbyval(varval)
                verifyRepeats(variant.key, variant.count)
                parse put (leadref.key, leadmap)
                parseSection(variant.compsById, scopes, Some(group), parse)
                if (group.usage != UnusedUsage) addInstance(variant.key, parse)
              } else {
                verifyRepeats(group.key, group.count)
                parse put (leadref.key, leadmap)
                parseSection(group.compsById, scopes, Some(group), parse)
                if (group.usage != UnusedUsage) addInstance(group.key, parse)
              }
            } else {
              verifyRepeats(group.key, group.count)
              val leadlist = parseRepeatingSegment(leadref.segment, leadref.count, Some(group), leadref.position)
              parse put (leadref.key, leadlist)
              parseSection(group.compsById, scopes, Some(group), parse)
              if (group.usage != UnusedUsage) addInstance(group.key, parse)
            }
            parseRepeat
          }
        }

        parseRepeat
      }

      /** Check if current segment is known. */
      def checkSegmentKnown: Unit = lexer.currentType match {
        case SEGMENT =>
          if (!transaction.segmentIds.contains(lexer.token)) {
            val ident = lexer.token
            segmentError(ident, None, ComponentErrors.UnknownSegment, true)
            while (ident == lexer.token) discardSegment
          }
        case END =>
        case _ => throw new IllegalStateException("lexer in data when should be start of segment")
      }

      /** Parse a portion of transaction data represented by a map of components (which may be segment references or
        * loops) into a map.
        * @param comps segment id to component map
        * @param scopes containing scope information for checking segment ordering and section termination (non-empty)
        * @param group containing group identifier
        * @param values result data map
        */
      def parseSection(comps: Map[String, TransactionComponent], scopes: List[ContainingScope],
        group: Option[GroupComponent], values: ValueMap) = {
        val startPos = scopes.head.current.position
        val grpid = group.map { _.ident }
        def checkTerminate(compid: String, ident: String, position: SegmentPosition): Boolean = {
          def verifyPosition(maxpos: SegmentPosition): Boolean =
            if (maxpos.isBefore(position)) {
              segmentError(ident, grpid, ComponentErrors.OutOfOrderSegment, true)
              discardSegment
              false
            } else true
          @tailrec
          def checkr(remain: List[ContainingScope]): Boolean = remain match {
            case h :: t => h.comps.get(compid) match {
              case Some(ref: ReferenceComponent) if (h.lead.nonEmpty && h.lead.get == ref.segment) => true
              case Some(comp) => verifyPosition(comp.position)
              case None => checkr(t)
            }
            case _ => {
              // segment not in any scope, report as out of order
              val error =
                if (transaction.segmentIds.contains(compid)) ComponentErrors.OutOfOrderSegment
                else ComponentErrors.UnknownSegment
              segmentError(ident, None, error, true)
              discardSegment
              false
            }
          }
          if (lexer.currentType == ItemType.END) true
          else checkr(scopes)
        }

        /** Parse a wrapped loop, handling wrap open and close segments directly.
          * @param wrap
          */
        def parseWrappedLoop(wrap: LoopWrapperComponent) = {
          if (wrap.usage == UnusedUsage) {
            segmentError(wrap.open.ident, Some(wrap.ident), ComponentErrors.UnusedSegment, true)
          }
          discardSegment
          val group = wrap.loopGroup
          parseRepeatingGroup(group, values,
            ContainingScope(wrap.compsById, Some(group.leadSegmentRef.segment), wrap.position) :: scopes)
          convertLoop.map { endid =>
            wrap.compsById.get(endid) match {
              case Some(ref: ReferenceComponent) => if (ref.segment == wrap.close) {
                discardSegment
                parseComponents(wrap.endPosition.position)
              }
              case _ =>
            }
          }
        }

        /** Parse section components, checking order by position and reporting errors but still handling all segments
          * defined in the section. This needs to handle the case of a repeating loop as a special case, since the lead
          * segment will repeat, potentially after other segments are seen (making it appear out-of-order), and when
          * this happens the loop needs to be restarted.
          * @param position
          */
        @tailrec
        def parseComponents(position: String): Unit = {
          val ident = lexer.token
          if (logger.isTraceEnabled) logger.trace(s"parsing segment $ident: ${comps.contains(ident)}")
          if (!isEnvelopeSegment(ident)) {
            comps get (ident) match {
              case Some(ref: ReferenceComponent) => {
                // check for repeat of lead segment in loop
                val segment = ref.segment
                if (ref.position.position != startPos || !values.containsKey(ref.key)) {
                  if (ref.usage == UnusedUsage) segmentError(ident, grpid, ComponentErrors.UnusedSegment, false)
                  val nextpos =
                    if (ref.position.position >= position) ref.position.position
                    else {
                      segmentError(ident, grpid, ComponentErrors.OutOfOrderSegment, false)
                      position
                    }
                  val data =
                    if (ref.count == 1) parseSegment(segment, group, ref.position)
                    else parseRepeatingSegment(segment, ref.count, group, ref.position)
                  if (ref.usage != UnusedUsage) {
                    if (values.containsKey(ref.key)) segmentError(ident, grpid, ComponentErrors.TooManyRepetitions, true)
                    else values put (ref.key, data)
                  }
                  parseComponents(nextpos)
                }
              }
              case Some(grp: GroupComponent) => {
                if (grp.endPosition.position < position) {
                  segmentError(ident, grpid, ComponentErrors.OutOfOrderSegment, true)
                } else if (grp.position.position >= position) {
                  parseRepeatingGroup(grp, values,
                    ContainingScope(comps, group.map { _.leadSegmentRef.segment }, grp.position) :: scopes)
                  parseComponents(grp.endPosition.position)
                }
              }
              case None => convertLoop match {
                case Some(loopid) => comps get (loopid) match {
                  case Some(wrap: LoopWrapperComponent) => parseWrappedLoop(wrap)
                  case _ => if (!checkTerminate(loopid, ident, SegmentPosition(table, position))) parseComponents(position)
                }
                case None =>
                  if (!checkTerminate(ident, ident, SegmentPosition(table, position))) parseComponents(position)
              }
              case _ => throw new IllegalStateException("Illegal structure for group $group")
            }
          }
        }

        parseComponents(startPos)
        comps.values.foreach { comp =>
          if (comp.usage == MandatoryUsage && !values.containsKey(comp.key))
            comp match {
              case ref: ReferenceComponent =>
                if (!isEnvelopeSegment(ref.segment.ident)) {
                  segmentError(ref.segment.ident, grpid, ComponentErrors.MissingRequired, true)
                }
              case loop: LoopWrapperComponent =>
                segmentError(loop.ident, grpid, ComponentErrors.MissingRequired, true)
              case grp: GroupComponent => segmentError(grp.ident, grpid, ComponentErrors.MissingRequired, true)
            }
        }
      }

      val context = List(ContainingScope(transaction.compsById, None, SegmentPosition(table, "0000")))
      val parse = new ValueMapImpl
      parseSection(comps, context, None, parse)
      parse
    }

    if (onepart) parseTable(0, transaction.headingById)
    else {
      val topMap: ValueMap = new ValueMapImpl
      topMap put (transactionId, transaction.ident)
      topMap put (transactionName, transaction.name)
      topMap put (transactionHeading, parseTable(0, transaction.headingById))
      convertSectionControl match {
        case Some(1) | None => {
          topMap put (transactionDetail, parseTable(1, transaction.detailById))
        }
        case _ =>
      }
      convertSectionControl match {
        case Some(1) => segmentError(lexer.token, None, ComponentErrors.OutOfOrderSegment, true)
        case _ =>
      }
      topMap put (transactionSummary, parseTable(2, transaction.summaryById))
      topMap
    }
  }

  /** Discard input past end of current segment. */
  def discardSegment = {
    if (lexer.currentType == SEGMENT) lexer.advance
    while (lexer.currentType != SEGMENT && lexer.currentType != END) lexer.advance
  }

  /** Discard input past end of current transaction. */
  def discardTransaction: Unit

  /** Convert section control segment to next section number. If not at a section control, this just returns None. */
  def convertSectionControl: Option[Int]

  /** Convert loop start or end segment to identity form. If not at a loop segment, this just returns None. */
  def convertLoop: Option[String]

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(ident: String) = schema.ediForm.isEnvelopeSegment(ident)

  /** Parse the input message. */
  def parse: Try[ValueMap]
}