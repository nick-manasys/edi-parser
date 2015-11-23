package com.anypoint.df.edi.schema

import scala.annotation.tailrec
import collection.{ mutable => sm }

import java.{ util => ju }

import com.anypoint.df.edi.lexical.EdiConstants.DataType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType

/** EDI schema representation.
  *
  * @author MuleSoft, Inc.
  */
object EdiSchema {

  // usage codes
  sealed abstract class Usage(val code: String)
  case object MandatoryUsage extends Usage("M")
  case object OptionalUsage extends Usage("O")
  case object ConditionalUsage extends Usage("C")
  case object UnusedUsage extends Usage("U")
  def convertUsage(value: String) = value match {
    case MandatoryUsage.code => MandatoryUsage
    case OptionalUsage.code => OptionalUsage
    case ConditionalUsage.code => ConditionalUsage
    case UnusedUsage.code => UnusedUsage
    case _ => throw new IllegalArgumentException("'" + value + "' is not an allowed usage code")
  }

  // dependency notes - note syntax rules are different for X12, using only single character
  sealed abstract class DependencyType(val code: Char)
  case object ExactlyOneDependency extends DependencyType('1')
  case object AllOrNoneDependency extends DependencyType('2')
  case object OneOrMoreDependency extends DependencyType('3')
  case object OneOrNoneDependency extends DependencyType('4')
  case object IfFirstAllDependency extends DependencyType('5')
  case object IfFirstAtLeastOneDependency extends DependencyType('6')
  case object IfFirstNoneDependency extends DependencyType('7')
  case class DependencyNote(val kind: DependencyType, val items: Seq[Int])
  // TODO: add dependency rules to schema representation

  /** Component base definition.
    * @param ident unique identifier
    * @param name readable name
    */
  sealed abstract class ComponentBase(val ident: String, val name: String)

  /** Element definition.
    * @param id unique identifier
    * @param nm readable name
    * @param dataType type
    * @param minLength minimum value length
    * @param maxLength maximum value length
    */
  case class Element(id: String, nm: String, val dataType: DataType, val minLength: Int, val maxLength: Int)
    extends ComponentBase(id, nm)

  /** Segment (or composite) component, either an element or a composite reference.
    * @param name readable name
    * @param data value key
    * @param position numeric position
    * @param usage
    * @param count maximum repetition count
    */
  sealed abstract class SegmentComponent(val name: String, val key: String, val position: Int, val usage: Usage,
    val count: Int)

  /** Element segment (or composite) component.
    * @param element
    * @param nm optional readable name (default is from element)
    * @param ky data value key
    * @param pos numeric position
    * @param use
    * @param cnt maximum repetition count
    */
  case class ElementComponent(val element: Element, nm: Option[String], ky: String, pos: Int, use: Usage, cnt: Int)
    extends SegmentComponent(nm.getOrElse(element.name), ky, pos, use, cnt)

  /** Composite segment component.
    * @param composite
    * @param nm optional readable name (default is from composite)
    * @param ky data value key
    * @param pos numeric position
    * @param use
    * @param cnt maximum repetition count
    */
  case class CompositeComponent(val composite: Composite, nm: Option[String], ky: String, pos: Int, use: Usage,
    cnt: Int) extends SegmentComponent(nm.getOrElse(composite.name), ky, pos, use, cnt) {
    val itemType = if (composite.isSimple) ItemType.DATA_ELEMENT else ItemType.SUB_COMPONENT
  }

  /** Occurrence rule definition. Subclasses define the actual rule checking.
    * @param code
    * @param components
    */
  sealed abstract class OccurrenceRule(val code: String, val components: List[SegmentComponent]) {
    if (components isEmpty) throw new IllegalArgumentException("components list must not be empty")
    def hasHead(map: ju.Map[String, Object]) = map containsKey (components.head key)
    def verify(map: ju.Map[String, Object]): Boolean
  }
  // X12 Required, EDIFACT One or more
  val OneOrMoreCode = "R"
  case class OneOrMore(comps: List[SegmentComponent]) extends OccurrenceRule(OneOrMoreCode, comps) {
    def verify(map: ju.Map[String, Object]) = !comps.find(comp => map containsKey (comp key)).isEmpty
  }
  // X12 Conditional, EDIFACT If first, then all
  val IfFirstThenAllCode = "C"
  case class IfFirstThenAll(comps: List[SegmentComponent]) extends OccurrenceRule(IfFirstThenAllCode, comps) {
    def verify(map: ju.Map[String, Object]) =
      if (hasHead(map)) {
        comps.find(comp => !(map containsKey (comp key))).isEmpty
      } else true
  }
  // X12 Exclusion, EDIFACT One or none
  val OneOrNoneCode = "E"
  case class OneOrNone(comps: List[SegmentComponent]) extends OccurrenceRule(OneOrNoneCode, comps) {
    def verify(map: ju.Map[String, Object]) =
      comps.count(comp => map containsKey (comp key)) <= 1
  }
  // X12 List Conditional, EDIFACT If first, then at least one
  val IfFirstThenMoreCode = "L"
  case class IfFirstThenMore(comps: List[SegmentComponent]) extends OccurrenceRule(IfFirstThenMoreCode, comps) {
    def verify(map: ju.Map[String, Object]) =
      if (hasHead(map)) {
        !comps.tail.find(comp => map containsKey (comp key)).isEmpty
      } else true
  }
  // X12 Paired, EDIFACT All or none
  val AllOrNoneCode = "P"
  case class AllOrNone(comps: List[SegmentComponent]) extends OccurrenceRule(AllOrNoneCode, comps) {
    def verify(map: ju.Map[String, Object]) = {
      val first = hasHead(map)
      comps.tail.find(comp => (map containsKey (comp key)) != first).isEmpty
    }
  }
  // EDIFACT One and only one
  val OneAndOnlyOneCode = "O"
  case class OneAndOnlyOne(comps: List[SegmentComponent]) extends OccurrenceRule(OneAndOnlyOneCode, comps) {
    def verify(map: ju.Map[String, Object]) =
      comps.count(comp => map containsKey (comp key)) == 1
  }
  // EDIFACT If first, then none
  val IfFirstThenNoneCode = "X"
  case class IfFirstThenNone(comps: List[SegmentComponent]) extends OccurrenceRule(IfFirstThenNoneCode, comps) {
    def verify(map: ju.Map[String, Object]) =
      if (hasHead(map)) {
        comps.find(comp => map containsKey (comp key)).isEmpty
      } else true
  }

  /** Composite definition.
    * @param id unique identifier
    * @param nm readable name
    * @param components
    * @param rules
    * @param maxLength maximum length for entire value (0 if no limit)
    */
  case class Composite(id: String, nm: String, val components: List[SegmentComponent], val rules: List[OccurrenceRule],
    val maxLength: Int) extends ComponentBase(id, nm) {
    def this(id: String, nm: String, comps: List[SegmentComponent], rules: List[OccurrenceRule]) =
      this(id, nm, comps, rules, 0)
    def rewrite(prefix: String, form: EdiForm): Composite =
      Composite(ident, name,
        components.map { scomp =>
          val rekey = form.keyName(prefix, scomp.position)
          scomp match {
            case ec: ElementComponent =>
              ElementComponent(ec.element, Some(ec.name), rekey, ec.position, ec.usage, ec.count)
            case cc: CompositeComponent =>
              val comp = if (cc.count == 1) cc.composite.rewrite(rekey, form) else cc.composite
              CompositeComponent(comp, Some(cc.name), rekey, cc.position, cc.usage, cc.count)
          }
        },
        rules, maxLength)
    val isSimple = components.forall { comp => comp.isInstanceOf[ElementComponent] }
  }

  /** Segment definition.
    * @param ident identifier (used for key, also as id/idRef in YAML)
    * @param tag value in data (used to identify segments in data, generally same as ident but flat file may differ)
    * @param name human readable name
    * @param components
    * @param rules
    */
  case class Segment(val ident: String, val tag: String, val name: String, val components: List[SegmentComponent],
    val rules: List[OccurrenceRule]) {
    def this(id: String, nm: String, comps: List[SegmentComponent], rls: List[OccurrenceRule]) =
      this (id, id, nm, comps, rls)
  }

  /** Segment position within a structure. Position numbers are reused across different tables of a structure
    * definition, so this gives a unique value for every segment.
    * @param table
    * @param position
    */
  case class SegmentPosition(table: Int, position: String) {
    def isBefore(other: SegmentPosition) =
      table < other.table || (table == other.table && position < other.position)
  }
  
  /** Base for all structure components.
    * @param key map key for data
    * @param position segment (or starting segment) position
    * @param usage
    * @param count maximum repetition count (0 for unlimited)
    */
  sealed abstract class StructureComponent(val key: String, val position: SegmentPosition, val usage: Usage,
    val count: Int)

  private def ident(comp: StructureComponent) = comp match {
    case r: ReferenceComponent => r.segment.ident
    case g: GroupBase => g.leadSegmentRef.segment.ident
    case w: LoopWrapperComponent => w.startCode
  }

  /** Subsequence of structure components with no segments reused. The segments are mapped so they can be processed in
    * any order.
    */
  case class StructureSubsequence(val startPos: String, val comps: Map[String, StructureComponent],
    val terms: Terminations, val groupTerms: Map[GroupComponent, Terminations]) {
//    println(s"subsequence from $startPos: ${comps.values.foldLeft("")((txt, comp) => txt + " " + comp.key)} (terms [${terms.required} required]: ${terms.idents.foldLeft("")((txt, id) => txt + " " + id)})")
  }

  /** Termination segment identifiers for group.
    * @param required number of required segments
    * @param idents segment identifiers for terminating level
    */
  case class Terminations(val required: Int, val idents: Set[String])
    
  val emptyTerminations = Terminations(0, Set())

  /** Sequence of structure components (segments and groups). This supports both loops and structure sections, adding
    * supporting information to the basic sequence for use in parsing and writing.
    * @param loop forces a separate subsequence for the first segment in a loop
    * @param items structure components
    */
  case class StructureSequence(private val loop: Boolean, val items: List[StructureComponent]) {

    /** Start position. */
    val startPos =  items.head.position

    /** All required components in sequence. */
    val requiredComps = items.filter { _.usage == MandatoryUsage }
    
    private val lastRequired = if (requiredComps.nonEmpty) requiredComps.last else null

    /** Idents of all segments used at this level, and of all segments reused at level. */
    val (allAtLevel, reusedSegments) =
      items.filterNot { comp => comp.isInstanceOf[WildcardComponent] || comp.isInstanceOf[LoopWrapperComponent] }.map { ident(_) }.
      foldLeft((Set[String](), Set[String]())) {
        case ((segs, reps), ident) =>
          if (segs.contains(ident)) (segs, reps + ident) else (segs + ident, reps)
      }
    
//    println(s"handling structure from $startPos: ${items.foldLeft("")((txt, comp) => txt + " " + comp.key)})")
//    println(s"found ${reusedSegments.size} reused segments: ${reusedSegments.foldLeft("")((txt, seg) => txt + " " + seg)})")

    //
    // Splitting sequences into subsequences:
    //
    // First split subsequences based on wildcard segments. Each wildcard segment is its own separate
    // StructureSubsequence, with everything in the termination set (so that after processing the wildcard the
    // subsequence will be done). A required segment preceding a wildcard segment is also its own separate
    // StructureSubsequence and has that segment as its termination set, so when the segment is seen the next segment
    // will be interpreted as the wildcard (the required segment preceding a wildcard appears to always be a singleton,
    // which makes sense to prevent ambiguity).
    //
    // The other issue is segments reuse. Reused segments need to be separated by a required segment, so make the
    // required segment preceding a reuse also a separate StructureSubsequence. Unlike the ones for required segments
    // preceding a wildcard, these may be repeated so the segment itself cannot be included in the termination set, but
    // all segments in the following subsequence can be used in the termination set.
    //
    // But since the schema is constructed bottom-up, there's no way to know if a segment is going to be part of the
    // termination set for the sequence (as it would be if it's reused at the containing level - the EDIFACT BAPLIE
    // structure is a good example of this, with LOC used in three places, included as the trigger segment for a group
    // following the group with its first use). So as a special case, when there are optional segments at the end of
    // a sequence split the sequence with the last required segment. That way out-of-order optional segments can be
    // processed, but if there's a reused segment from earlier in the sequence included in the termination set it'll be
    // handled as a termination.
    //

    private type CompList = List[StructureComponent]

    private def buildSubSequences = {
      val seqs = sm.Buffer[StructureSubsequence]()

      def addNormal(incl: CompList, terms: Terminations) = {
        val comps = incl.map { c => (ident(c), c) }.toMap
        // build terminations for all groups included in subsequence
        val (_, _, grps) = incl.reverse.foldLeft((terms.required, terms.idents, Map[GroupComponent, Terminations]())) {
          case ((cnt, ids, acc), comp) =>
            val ncnt = if (comp.usage == MandatoryUsage) cnt + 1 else cnt
            val nids = ids + ident(comp)
            comp match {
              case g: GroupComponent => (ncnt, nids, acc + (g -> Terminations(cnt, ids + ident(g.leadSegmentRef))))
              case _ => (ncnt, nids, acc)
            }
        }
//        grps.foreach { case (grp, terms) => println(s"group ${grp.ident} has terms (${terms.required} required): ${terms.idents.foldLeft("")((txt, id) => txt + " " + id)})") }
        seqs += StructureSubsequence(incl.head.position.position, comps, terms, grps)
      }
      def addWild(wild: WildcardComponent) = {
        val comps = wild.possibles.map { s => (s.ident, ReferenceComponent(s, wild.position, OptionalUsage, 1)) }.toMap
        seqs += StructureSubsequence(wild.position.position, comps, Terminations(1, comps.keySet), Map())
      }
      def addRequired(req: StructureComponent) = {
        val id = ident(req)
        seqs += StructureSubsequence(req.position.position, Map(id -> req), Terminations(1, Set(id)), Map())
      }

      def reqCount(comps: CompList) = comps.foldLeft(0)((acc, c) => if (c.usage == MandatoryUsage) acc + 1 else acc)

      /** Recursively split sequence into subsequences based on reused segments. This is only called after wildcards are
        * removed from the components list, so there's no need to handle those.
        */
      def splitReuses(comps: CompList, terms: Terminations): Unit = {
        @tailrec
        def findreq(rem: CompList, acc: CompList): (CompList, CompList) = rem match {
          case h :: t => if (h.usage == MandatoryUsage) (acc reverse, rem) else findreq(t, h :: acc)
          case _ => (acc reverse, Nil)
        }
        def addNonEmpty(acc: CompList, rest: CompList) = {
          if (acc.nonEmpty) {
            val termIds = rest.map { ident(_) }.toSet
            val inclIds = acc.map { ident(_) }.toSet
            addNormal(acc, Terminations(reqCount(rest), termIds -- inclIds))
          }
          splitr(rest, Nil)
        }
        @tailrec
        def splitr(rem: CompList, acc: CompList): Unit = rem match {
          case h :: t =>
            val id = ident(h)
            if ((reusedSegments.contains(id) && t.exists { ident(_) == id })) {
              val (seq, rest) = findreq(t, h :: acc)
              if (rest.isEmpty) splitHead(h, t)
              else addNonEmpty(seq, rest)
            } else if (h == lastRequired && acc.nonEmpty) addNonEmpty(acc.reverse, rem)
            else splitr(t, h :: acc)
          case _ => if (!acc.isEmpty) addNormal(acc.reverse, terms)
        }
        def splitHead(head: StructureComponent, rest: CompList) = {
          val restIds = rest.map { ident(_) }.toSet - ident(head)
          val termIds = if (head.count == 1) restIds + ident(head) else restIds
          addNormal(List(head), Terminations(reqCount(rest), termIds))
          splitr(rest, Nil)
        }

        comps match {
          case h :: t =>
            // force singleton subsequence for first component in loop (to allow for repeated loops)
            if (loop && comps.head == h) splitHead(h, t)
            else splitr(comps, Nil)
          case _ =>
        }
      }

      /** Recursively split sequence into subsequences based on wild card segment references. Subsequences with no wild
        * cards are further split to handle reused segments.
        */
      @tailrec
      def splitWilds(rem: CompList): Unit =
        rem span (!_.isInstanceOf[WildcardComponent]) match {
          case (lead, wild :: rest) =>
            lead match {
              case split :+ last =>
                if (!last.isInstanceOf[ReferenceComponent] || last.usage != MandatoryUsage || last.count != 1) throw new IllegalStateException("Structure uses wildcard segment without preceding required singleton segment reference")
                splitReuses(split, Terminations(1, Set(ident(last))))
                addRequired(last)
              case _ =>
            }
            addWild(wild.asInstanceOf[WildcardComponent])
            splitWilds(rest)
          case (lead, _) => splitReuses(lead, Terminations(0, Set()))
        }

      splitWilds(items)
      // TODO: check if any problems here
//      val count = seqs.foldLeft(0)((sum, subsq) => sum + subsq.comps.size)
//      if (count != items.size) {
//        println(s"count $count != ${items.size}")
//      }
      seqs.toList
    }

    /** Ordered list of subsequences in sequence. */
    val subSequences = buildSubSequences

    /** End position. */
    val endPosition: SegmentPosition = items.last match {
      case ref: ReferenceComponent => ref.position
      case wrap: LoopWrapperComponent => wrap.endPosition
      case grp: GroupComponent => grp.seq.endPosition
      case _ => throw new IllegalStateException(s"last item in sequence is not a segment reference or group")
    }
  }

  /** Key for a structure component. */
  def componentKey(ident: String, pos: SegmentPosition) = pos.position + "_" + ident.replace(' ', '_')

  /** Segment reference.
    * @param segment
    * @param position
    * @param use
    * @param cnt
    */
  case class ReferenceComponent(val segment: Segment, pos: SegmentPosition, use: Usage, cnt: Int)
    extends StructureComponent(componentKey(segment.ident, pos), pos, use, cnt)

  /** Any segment reference.
    * @param ident
    * @param position
    * @param use
    * @param cnt
    * @param possibles segments usable with wildcard
    */
  case class WildcardComponent(val ident: String, pos: SegmentPosition, use: Usage, cnt: Int,
    val possibles: List[Segment]) extends StructureComponent(componentKey(ident, pos), pos, use, cnt)

  /** Loop wrapper component.
    * @param open
    * @param close
    * @param start
    * @param endPosition
    * @param use
    * @param ident
    * @param grp
    */
  case class LoopWrapperComponent(val open: Segment, val close: Segment, start: SegmentPosition,
    val endPosition: SegmentPosition, use: Usage, val groupId: String, val wrapped: GroupComponent)
    extends StructureComponent(wrapped.key, start, use, 1) {
    val startCode = open.ident + groupId
    val endCode = close.ident + groupId
    val groupTerms = Terminations(1, Set(endCode))
  }

  /** Base for group components.
    * @param ky
    * @param use
    * @param cnt
    * @param coice
    * @param seq
    */
  sealed abstract class GroupBase(ky: String, pos: SegmentPosition, use: Usage, cnt: Int, val choice: Boolean,
    val seq: StructureSequence) extends StructureComponent(ky, pos, use, cnt) {
    val leadSegmentRef: ReferenceComponent = seq.items match {
      case (r: ReferenceComponent) :: t => r
      case (g: GroupBase) :: t => g.leadSegmentRef
      case _ => throw new IllegalStateException(s"first item in sequence is not a segment reference")
    }
  }

  /** Variant of a group component. The items in a variant must be a subset of the items in the group.
    * @param baseid base identifier for all variants of the group
    * @param use
    * @param cnt
    * @param itms
    */
  case class VariantGroup(val baseid: String, val elemval: String, pos: SegmentPosition, use: Usage, cnt: Int,
    ssq: StructureSequence)
    extends GroupBase(componentKey(s"$baseid[$elemval]", pos), pos, use, cnt, false, ssq)

  /** Group component consisting of one or more nested components.
    * @param ident group identifier
    * @param use
    * @param cnt
    * @param ssq 
    * @param varkey key for field controlling variant selection
    * @param variants group variant forms
    * @param ky explicit key value (ident used by default)
    * @param ch choice group flag
    * @param tagStart starting column of tag field (ignored unless flat file)
    * @param tagLength length of tag field (ignored unless flat file)
    */
  case class GroupComponent(val ident: String, use: Usage, cnt: Int, ssq: StructureSequence, val varkey: Option[String],
    val variants: List[VariantGroup], ky: Option[String] = None, pos: Option[SegmentPosition] = None,
    ch: Boolean = false, val tagStart: Option[Int] = None, val tagLength: Option[Int] = None)
    extends GroupBase(ky.getOrElse(componentKey(ident, pos.getOrElse(ssq.startPos))),
      pos.getOrElse(ssq.startPos), use, cnt, ch, ssq) {

    /** Group variants by key value. */
    val varbyval = Map(variants map { v => (v.elemval, v) }: _*)
  }

  /** Structure definition.
    * @param ident
    * @param name
    * @param group
    * @param heading
    * @param detail
    * @param summary
    * @param version
    * @param tagStart starting column of tag field (ignored unless flat file)
    * @param tagLength length of tag field (ignored unless flat file)
    */
  case class Structure(val ident: String, val name: String, val group: Option[String],
    val heading: Option[StructureSequence], val detail: Option[StructureSequence],
    val summary: Option[StructureSequence], val version: EdiSchemaVersion, val tagStart: Option[Int] = None,
    val tagLength: Option[Int] = None) {

    /** Segments used in structure. */
    val segmentsUsed = {
      def referencer(seq: StructureSequence, segments: Set[Segment]): Set[Segment] =
        seq.items.foldLeft(segments)((acc, comp) => comp match {
          case ref: ReferenceComponent => acc + ref.segment
          case wrap: LoopWrapperComponent => referencer(wrap.wrapped.seq, acc + wrap.open + wrap.close)
          case group: GroupComponent => referencer(group.seq, acc)
          case _ => acc
        })
      def tableRefs(seqopt: Option[StructureSequence], segments: Set[Segment]): Set[Segment] = seqopt match {
        case Some(seq) => referencer(seq, segments)
        case None => segments
      }

      tableRefs(summary, tableRefs(detail, tableRefs(heading, Set[Segment]())))
    }

    /** Composites used in structure. */
    val compositesUsed = {
      def referencer(comps: List[SegmentComponent], composites: Set[Composite]): Set[Composite] =
        comps.foldLeft(composites)((acc, comp) => comp match {
          case CompositeComponent(composite, _, _, _, _, _) => referencer(composite.components, acc + composite)
          case _ => acc
        })
      segmentsUsed.foldLeft(Set[Composite]())((acc, seg) => referencer(seg.components, acc))
    }

    /** Elements used in structure. */
    val elementsUsed = {
      def referencer(comps: List[SegmentComponent], elements: Set[Element]): Set[Element] =
        comps.foldLeft(elements)((acc, comp) => comp match {
          case CompositeComponent(composite, _, _, _, _, _) => referencer(composite.components, acc)
          case ElementComponent(element, _, _, _, _, _) => acc + element
        })
      segmentsUsed.foldLeft(Set[Element]())((acc, seg) => referencer(seg.components, acc))
    }

    /** Ids of all segments used in structure at any level. */
    val segmentIds = segmentsUsed.map(segment => segment.ident)
    
    private def terms(section: Option[StructureSequence]): Set[String] = section match {
      case Some(seq) => {
        @tailrec
        def termr(rem: List[StructureComponent], acc: Set[String]): Set[String] = rem match {
          case h :: t =>
            val nacc = acc + EdiSchema.ident(h)
            if (h.usage == MandatoryUsage) nacc else termr(t, nacc)
          case _ => acc
        }
        termr(seq.items, Set())
      }
      case None => Set()
    }
    
    private val sumterms = terms(summary)
    val summaryTerms = Terminations(0, sumterms)
    private val detterms = terms(detail)
    val detailTerms = Terminations(0, if (detail.isEmpty || detail.get.requiredComps.isEmpty) detterms ++ sumterms else detterms)
  }

  type StructureMap = Map[String, Structure]

  sealed abstract class EdiForm(val text: String, val sectioned: Boolean) {
    def isEnvelopeSegment(ident: String): Boolean
    val loopWrapperStart: String
    val loopWrapperEnd: String

    /** Construct data value key name from parent identifier and position value. */
    def keyName(parentId: String, position: Int): String
    
    /** Create key for version in data maps. */
    def versionKey(version: String): String
  }
  case object EdiFact extends EdiForm("EDIFACT", true) {
    val envelopeSegs = Set("UNB", "UNZ", "UNG", "UNE", "UNH", "UNT", "UNS")
    def isEnvelopeSegment(ident: String) = envelopeSegs contains ident
    val loopWrapperStart = "UGH"
    val loopWrapperEnd = "UGT"
    def keyName(parentId: String, position: Int) = {
      val scaled = if (position % 10 == 0) position / 10 else position
      if (position < 100) parentId + "0" + scaled.toString
      else parentId + scaled.toString
    }
    def versionKey(version: String) = version.toUpperCase
  }
  case object X12 extends EdiForm("X12", true) {
    val envelopeSegs = Set("ISA", "IEA", "GS", "GE", "ST", "SE")
    def isEnvelopeSegment(ident: String) = envelopeSegs contains ident
    val loopWrapperStart = "LS"
    val loopWrapperEnd = "LE"
    def keyName(parentId: String, position: Int) = parentId + (if (position < 10) "0" + position else position)
    def versionKey(version: String) = "v" + version
  }
  case object HL7 extends EdiForm("HL7", false) {
    def isEnvelopeSegment(ident: String) = "MSH" == ident || "" == ident
    val loopWrapperStart = ""
    val loopWrapperEnd = ""
    def keyName(parentId: String, position: Int) = parentId + "-" + (if (position < 10) "0" + position else position)
    def versionKey(version: String) = "v" + version.filterNot { _ == '.' }
  }
  case object FlatFile extends EdiForm("FIXEDWIDTH", false) {
    def isEnvelopeSegment(ident: String) = false
    val loopWrapperStart = ""
    val loopWrapperEnd = ""
    def keyName(parentId: String, position: Int) = parentId + (if (position < 10) "0" + position else position)
    def versionKey(version: String) = version
  }
  def convertEdiForm(value: String) = value match {
    case EdiFact.text => EdiFact
    case X12.text => X12
    case HL7.text => HL7
    case FlatFile.text => FlatFile
    case _ => throw new IllegalArgumentException(s"Unknown EDI form $value")
  }
}

case class EdiSchemaVersion(val ediForm: EdiSchema.EdiForm, val version: String) {
  def versionKey = ediForm.versionKey(version)
}

case class EdiSchema(val ediVersion: EdiSchemaVersion, val elements: Map[String, EdiSchema.Element],
  val composites: Map[String, EdiSchema.Composite], val segments: Map[String, EdiSchema.Segment],
  val structures: EdiSchema.StructureMap) {
  def this(ver: EdiSchemaVersion) = this(ver, Map[String, EdiSchema.Element](),
    Map[String, EdiSchema.Composite](), Map[String, EdiSchema.Segment](), Map[String, EdiSchema.Structure]())
  def merge(other: EdiSchema) = EdiSchema(ediVersion, elements ++ other.elements, composites ++ other.composites,
    segments ++ other.segments, structures ++ other.structures)
  def merge(transact: EdiSchema.Structure) = {
    val elemMap = elements ++ (transact.elementsUsed.foldLeft(Map[String, EdiSchema.Element]())((map, elem) => map + (elem.ident -> elem)))
    val compMap = composites ++ (transact.compositesUsed.foldLeft(Map[String, EdiSchema.Composite]())((map, comp) => map + (comp.ident -> comp)))
    val segMap = segments ++ (transact.segmentsUsed.foldLeft(Map[String, EdiSchema.Segment]())((map, seg) => map + (seg.ident -> seg)))
    val transMap = structures + (transact.ident -> transact)
    EdiSchema(ediVersion, elemMap, compMap, segMap, transMap)
  }
}