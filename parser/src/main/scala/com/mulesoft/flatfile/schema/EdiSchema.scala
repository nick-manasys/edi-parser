package com.mulesoft.flatfile.schema

import scala.annotation.tailrec
import collection.{ mutable => sm }

import java.{ util => ju }

import com.mulesoft.flatfile.lexical.EdiConstants.ItemType
import com.mulesoft.flatfile.lexical.TypeFormat
import com.mulesoft.ltmdata.MemoryResident

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
  case object IgnoredUsage extends Usage("I")
  def convertUsage(value: String) = value match {
    case MandatoryUsage.code   => MandatoryUsage
    case OptionalUsage.code    => OptionalUsage
    case ConditionalUsage.code => ConditionalUsage
    case UnusedUsage.code      => UnusedUsage
    case _                     => throw new IllegalArgumentException("'" + value + "' is not an allowed usage code")
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
    * @param ident unique identifier (empty string if inlined)
    * @param name readable name
    */
  sealed abstract class ComponentBase(val ident: String, val name: String)

  /** Element definition.
    * @param id unique identifier (empty string if inlined)
    * @param nm readable name
    * @param typeFormat format
    */
  case class Element(id: String, nm: String, val typeFormat: TypeFormat)
    extends ComponentBase(id, nm)

  /** Segment (or composite) component, either an element or a composite reference.
    * @param name readable name
    * @param data value key
    * @param position numeric position
    * @param usage
    * @param count maximum repetition count
    */
  sealed abstract class SegmentComponent(val name: String, val key: String, val position: Int, val usage: Usage,
    val count: Int) {
    def ident: String
  }

  /** Element segment (or composite) component.
    * @param element
    * @param nm optional readable name (default is from element)
    * @param ky data value key
    * @param pos numeric position
    * @param use
    * @param cnt maximum repetition count
    * @param tagPart element used as (part of) segment tag flag
    * @param value fixed text value for unused (checked on input, written on output) and ignored (written on output)
    * usage types
    */
  case class ElementComponent(val element: Element, nm: Option[String], ky: String, pos: Int, use: Usage, cnt: Int,
    val tagPart: Boolean = false, val value: Option[String] = None)
    extends SegmentComponent(nm.getOrElse(element.name), ky, pos, use, cnt) {
    def ident = element.ident
  }

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
    def ident = composite.ident
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

  private def collectKeys(comps: List[SegmentComponent], prior: List[String]): Array[String] =
    comps.foldLeft(prior) { (acc, comp) =>
      if (comp.count == 1) comp match {
        case ec: ElementComponent   => ec.key :: acc
        case cc: CompositeComponent => cc.composite.keys.toList ::: acc
      }
      else comp.key :: acc
    }.reverse.toArray

  private def collectTypeCodes(comps: List[SegmentComponent], prior: Set[String]): Set[String] =
    comps.foldLeft(prior) { (acc, comp) =>
      comp match {
        case ec: ElementComponent   => acc + ec.element.typeFormat.typeCode
        case cc: CompositeComponent => collectTypeCodes(cc.composite.components, acc)
      }
    }

  /** Composite definition.
    * @param id unique identifier (empty string if inlined)
    * @param nm readable name
    * @param components
    * @param rules
    * @param maxLength maximum length for entire value (0 if no limit)
    */
  case class Composite(id: String, nm: String, val components: List[SegmentComponent], val rules: List[OccurrenceRule],
    val maxLength: Int) extends ComponentBase(id, nm) {
    def this(id: String, nm: String, comps: List[SegmentComponent], rules: List[OccurrenceRule]) =
      this(id, nm, comps, rules, 0)
    def rewrite(prefix: String, form: EdiForm): Composite = {
      if (form.inlineComposites) {
        val comps = components.map { scomp =>
          val rekey = form.keyName(prefix, "", "", scomp.position)
          scomp match {
            case ec: ElementComponent =>
              ElementComponent(ec.element, Some(ec.name), rekey, ec.position, ec.usage, ec.count, ec.tagPart, ec.value)
            case cc: CompositeComponent =>
              val comp = if (cc.count == 1) cc.composite.rewrite(rekey, form) else cc.composite
              CompositeComponent(comp, Some(cc.name), rekey, cc.position, cc.usage, cc.count)
          }
        }
        Composite(ident, name, comps, rules, maxLength)
      } else this
    }
    val isSimple = components.forall { comp => comp.isInstanceOf[ElementComponent] }
    val keys = collectKeys(components, Nil)
  }

  /** Segment definition.
    * @param ident identifier (used for key in multisegment schemas, also as id/idRef in YAML if non in-lined; ignored
    * in single segment schemas)
    * @param name human readable name
    * @param components
    * @param rules
    */
  case class Segment(val ident: String, val name: String, val components: List[SegmentComponent],
    val rules: List[OccurrenceRule]) {
    val keys = collectKeys(components, Nil)
    val typeCodes = collectTypeCodes(components, Set[String]())
  }

  /** Segment position within a structure. Position numbers are reused across different tables of a structure
    * definition, so this gives a unique value for every segment.
    * @param table
    * @param position
    */
  sealed abstract class SegmentPosition(val table: Int, val position: String) {
    def before(other: SegmentPosition) =
      table < other.table || (table == other.table && position < other.position)
    def defined: Boolean
  }
  class DefinedPosition(table: Int, position: String) extends SegmentPosition(table, position) {
    def defined = true
  }
  object StartPosition extends SegmentPosition(-1, "") {
    def defined = false
  }

  /** Base for all structure components.
    * @param key map key for data
    * @param position segment (or starting segment) position
    * @param usage
    * @param count maximum repetition count (0 for unlimited)
    */
  sealed abstract class StructureComponent(val key: String, val position: SegmentPosition, val usage: Usage,
    val count: Int) {
    val ident: String
  }

  private def segmentIdent(seg: Segment) =
    if (seg.ident.nonEmpty) seg.ident
    else seg.name

  private def compIdent(comp: StructureComponent) = comp match {
    case r: ReferenceComponent   => segmentIdent(r.segment)
    case g: GroupBase            => segmentIdent(g.leadSegmentRef.segment)
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
  case class StructureSequence(val loop: Boolean, val items: List[StructureComponent]) {

    /** Start position. */
    val startPos = items.head.position

    /** All required components in sequence. */
    val requiredComps = items.filter { _.usage == MandatoryUsage }

    private val lastRequired = if (requiredComps.nonEmpty) requiredComps.last else null

    /** Tags of all segments used at this level, and of all segments reused at level. */
    val (allAtLevel, reusedSegments) =
      items.filterNot {
        comp => comp.isInstanceOf[WildcardComponent] || comp.isInstanceOf[LoopWrapperComponent]
      }.map { compIdent(_) }.foldLeft((Set[String](), Set[String]())) {
        case ((segs, reps), tag) =>
          if (segs.contains(tag)) (segs, reps + tag) else (segs + tag, reps)
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
        val comps = incl.map { c => (compIdent(c), c) }.toMap
        // build terminations for all groups included in subsequence
        val (_, _, grps) = incl.reverse.foldLeft((terms.required, terms.idents, Map[GroupComponent, Terminations]())) {
          case ((cnt, ids, acc), comp) =>
            val ncnt = if (comp.usage == MandatoryUsage) cnt + 1 else cnt
            val nids = ids + compIdent(comp)
            comp match {
              case g: GroupComponent => (ncnt, nids, acc + (g -> Terminations(cnt, ids + compIdent(g.leadSegmentRef))))
              case _                 => (ncnt, nids, acc)
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
        val id = compIdent(req)
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
          case _      => (acc reverse, Nil)
        }
        def addNonEmpty(acc: CompList, rest: CompList) = {
          if (acc.nonEmpty) {
            val termIds = rest.map { compIdent(_) }.toSet
            val inclIds = acc.map { compIdent(_) }.toSet
            addNormal(acc, Terminations(reqCount(rest), termIds -- inclIds))
          }
          splitr(rest, Nil)
        }
        @tailrec
        def splitr(rem: CompList, acc: CompList): Unit = rem match {
          case h :: t =>
            val id = compIdent(h)
            if ((reusedSegments.contains(id) && t.exists { compIdent(_) == id })) {
              val (seq, rest) = findreq(t, h :: acc)
              if (rest.isEmpty) splitHead(h, t)
              else addNonEmpty(seq, rest)
            } else if (h == lastRequired && acc.nonEmpty) addNonEmpty(acc.reverse, rem)
            else splitr(t, h :: acc)
          case _ => if (!acc.isEmpty) addNormal(acc.reverse, terms)
        }
        def splitHead(head: StructureComponent, rest: CompList) = {
          val restIds = rest.map { compIdent(_) }.toSet - compIdent(head)
          val termIds = if (head.count == 1) restIds + compIdent(head) else restIds
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
                splitReuses(split, Terminations(1, Set(compIdent(last))))
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
      case ref: ReferenceComponent    => ref.position
      case wrap: LoopWrapperComponent => wrap.endPosition
      case grp: GroupComponent        => grp.seq.endPosition
      case _                          => throw new IllegalStateException(s"last item in sequence is not a segment reference or group")
    }
  }

  /** Key for a structure component. */
  def componentKey(ident: String, pos: SegmentPosition) =
    if (pos.defined) pos.position + "_" + ident.replace(' ', '_')
    else ident.replace(' ', '_')

  /** Identifier used in key for segment. */
  def segmentId(segment: Segment) = if (segment.ident.nonEmpty) segment.ident else segment.name

  /** Segment reference.
    * @param segment
    * @param position
    * @param use
    * @param cnt
    */
  case class ReferenceComponent(val segment: Segment, pos: SegmentPosition, use: Usage, cnt: Int)
    extends StructureComponent(componentKey(segmentId(segment), pos), pos, use, cnt) {
    override val ident = segmentId(segment)
  }

  /** Any segment reference.
    * @param ident
    * @param position
    * @param use
    * @param cnt
    * @param possibles segments usable with wildcard
    */
  case class WildcardComponent(override val ident: String, pos: SegmentPosition, use: Usage, cnt: Int,
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
    val groupTerms = Terminations(1, Set(endCode, wrapped.leadSegmentRef.segment.ident))
    override val ident = groupId
  }

  private def collectKeys(seq: StructureSequence): Array[String] = {
    seq.items.foldLeft(List[String]()) { (acc, comp) =>
      if (comp.count == 1) comp match {
        case group: GroupBase => group.keys.toList ::: acc
        case _                => comp.key :: acc
      }
      else comp.key :: acc
    }.reverse.toArray
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
      case (g: GroupBase) :: t          => g.leadSegmentRef
      case _                            => throw new IllegalStateException(s"first item in sequence is not a segment reference")
    }
    val keys = collectKeys(seq)
  }

  /** Key for a group component. X12 loop idents are sometimes based on numbers, while their positions match the first
    * component of the loop. This leads to strange keys like "0130_2300_Loop", so this key derivation checks if the base
    * ident starts with a numeric value and if so uses it as-is.
    */
  def componentGroupKey(ident: String, pos: SegmentPosition) =
    if (ident.isEmpty) throw new IllegalArgumentException(s"ident required for component at postion $pos")
    else if (!ident.head.isDigit && pos.defined) pos.position + "_" + ident.replace(' ', '_')
    else ident.replace(' ', '_')

  /** Variant of a group component. The items in a variant must be a subset of the items in the group.
    * @param baseid base identifier for all variants of the group
    * @param use
    * @param cnt
    * @param itms
    */
  case class VariantGroup(val baseid: String, val elemval: String, pos: SegmentPosition, use: Usage, cnt: Int,
    ssq: StructureSequence) extends GroupBase(componentGroupKey(s"$baseid[$elemval]", pos), pos, use, cnt, false, ssq) {
    override val ident = baseid + ":" + elemval
  }

  /** Group component consisting of one or more nested components.
    * @param ident group identifier
    * @param use
    * @param cnt
    * @param ssq
    * @param varkey key for field controlling variant selection
    * @param variants group variant forms
    * @param ky explicit key value (ident used by default)
    * @param ch choice group flag
    */
  case class GroupComponent(override val ident: String, use: Usage, cnt: Int, ssq: StructureSequence,
    val varkey: Option[String], val variants: List[VariantGroup], ky: Option[String] = None,
    pos: Option[SegmentPosition] = None, ch: Boolean = false)
    extends GroupBase(ky.getOrElse(componentGroupKey(ident, pos.getOrElse(ssq.startPos))),
      pos.getOrElse(ssq.startPos), use, cnt, ch, ssq) {

    /** Group variants by key value. */
    val varbyval = Map(variants map { v => (v.elemval, v) }: _*)
  }

  /** Base class for target associated with a fixed width segment tag component. */
  sealed abstract class TagTarget {
    val size: Int
  }

  /** Segment identified by fixed width segment tag component. */
  case class TagSegment(val segment: Segment) extends TagTarget {
    override val size = 1
    override def toString: String = s"TagSegment(${segment.ident})"
  }

  /** Next fixed width segment tag component. */
  case class TagNext(val offset: Int, val length: Int, val targets: Map[String, TagTarget]) extends TagTarget {
    override val size = targets.foldLeft(0) { case (acc, (k, v)) => acc + v.size }
    override def toString: String = {
      val builder = new StringBuilder
      builder.append(s"TagNext($offset, $length):")
      targets.foreach { case (k, v) => builder.append(s" $k -> $v") }
      builder.toString
    }
  }

  /** Choice between sets of segment tags at different positions in lines. */
  case class TagChoice(val left: TagTarget, right: TagTarget) extends TagTarget {
    override val size = left.size + right.size
    override def toString: String = s"TagChoice($left, $right)"
  }

  /** Build tag structure for a set of segments. */
  def buildTagTarget(segments: Set[Segment]): TagTarget = {

    case class TagField(val start: Int, val length: Int, val value: String)

    def sameField(f0: TagField, f1: TagField) = {
      f0.start == f1.start && f0.length == f1.length
    }

    /** Build list of tag values for segment (start offset, length, value). */
    def segmentTags(s: Segment): List[TagField] = {
      val buffer = sm.Buffer[TagField]()
      def compr(offset: Int, allowTag: Boolean, comp: CompositeComponent): Int = {
        val count = comp.count
        if (count == 1) tagr(offset, allowTag, comp.composite.components)
        else {
          val length = tagr(0, false, comp.composite.components)
          offset + length * count
        }
      }

      @tailrec
      def tagr(offset: Int, allowTag: Boolean, comps: List[SegmentComponent]): Int = {
        comps match {
          case h :: t =>
            h match {
              case e: ElementComponent =>
                val length = e.element.typeFormat.maxLength
                if (e.tagPart) {
                  if (e.count != 1) throw new IllegalArgumentException(s"Repeated element ${e.name} cannot be part of tag")
                  else if (e.value.isEmpty) throw new IllegalArgumentException(s"Tag value not defined for element ${e.name}")
                  else {
                    val value = e.value.get
                    if (value.size > length) throw new IllegalArgumentException(s"Tag value too long for element ${e.name}")
                    else buffer += TagField(offset, length, value)
                  }
                }
                tagr(offset + length * e.count, true, t)
              case c: CompositeComponent =>
                val next = compr(offset, allowTag, c)
                tagr(next, allowTag, t)
            }
          case _ => offset
        }
      }

      tagr(0, true, s.components)
      buffer.toList
    }

    /** List of segments paired with the ordered list of tag fields. */
    type SegTags = List[(Segment, List[TagField])]

    /** Build target information for a list of segment details ordered by increasing tag list size. */
    def buildTarget(segTags: SegTags): TagTarget = {

      def buildSegmentTarget(seg: Segment, tags: List[TagField]): TagTarget = {
        @tailrec
        def buildr(rem: List[TagField], acc: TagTarget): TagTarget = {
          rem match {
            case Nil                                 => acc
            case TagField(start, length, value) :: t => buildr(t, TagNext(start, length, Map(value -> acc)))
          }
        }
        buildr(tags.reverse, TagSegment(seg))
      }

      segTags match {
        case (seg, tags) :: Nil              => buildSegmentTarget(seg, tags)
        case (seg0, Nil) :: ((seg1, _) :: _) => throw new IllegalArgumentException(s"Segment ${seg0.ident} conflicts with ${seg1.ident}")
        case (seg0, tags) :: _ =>
          val tag0 = tags.head
          // (alternatives to this segment using same tag field, segments not using this tag field)
          val (alts, others) = segTags.partition { case (_, (f0 :: _)) => sameField(tag0, f0) }
          if (alts.isEmpty) TagChoice(buildSegmentTarget(seg0, tags), buildTarget(others))
          else {
            val valueMap = alts.groupBy {
              case (_, tagHead :: _) => tagHead.value
            }.map {
              case (key, list) =>
                val pruned = list.map { case (s, l) => (s, l.tail) }
                (key, buildTarget(pruned))
            }
            val fanOut = TagNext(tag0.start, tag0.length, valueMap)
            if (others.isEmpty) fanOut
            else TagChoice(fanOut, buildTarget(others))
          }
        case Nil => TagNext(0, 0, Map())
      }
    }

    val segTags = segments.map { s => (s, segmentTags(s)) }.filter { case (s, l) => l.nonEmpty }.toList
    val sortedTags = segTags.sortBy { case (s, l) => l.size }
    val target = buildTarget(sortedTags)
    val count = target.size
    if (count > 0 && count < segments.size) throw new IllegalArgumentException(s"All segments must define tags if any do (found $count of ${segments.size} tags)")
    target
  }

  /** Structure definition.
    * @param ident
    * @param name
    * @param group
    * @param heading
    * @param detail
    * @param summary
    * @param version
    */
  case class Structure(val ident: String, val name: String, val group: Option[String],
    val heading: Option[StructureSequence], val detail: Option[StructureSequence],
    val summary: Option[StructureSequence], val version: EdiSchemaVersion) extends MemoryResident {

    def memoryId = ident

    /** Segments used in structure. */
    val segmentsUsed = {
      def referencer(seq: StructureSequence, segments: Set[Segment]): Set[Segment] =
        seq.items.foldLeft(segments)((acc, comp) => comp match {
          case ref: ReferenceComponent    => acc + ref.segment
          case wrap: LoopWrapperComponent => referencer(wrap.wrapped.seq, acc + wrap.open + wrap.close)
          case group: GroupComponent      => referencer(group.seq, acc)
          case _                          => acc
        })
      def tableRefs(seqopt: Option[StructureSequence], segments: Set[Segment]): Set[Segment] = seqopt match {
        case Some(seq) => referencer(seq, segments)
        case None      => segments
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
          case CompositeComponent(composite, _, _, _, _, _)   => referencer(composite.components, acc)
          case ElementComponent(element, _, _, _, _, _, _, _) => acc + element
        })
      segmentsUsed.foldLeft(Set[Element]())((acc, seg) => referencer(seg.components, acc))
    }
    
    val typeCodes = segmentsUsed.foldLeft(Set[String]())((acc, s) => acc ++ s.typeCodes)

    /** Ids of all segments used in structure at any level. */
    val segmentIds = segmentsUsed.map(segment => segment.ident)

    private def terms(section: Option[StructureSequence]): Set[String] = section match {
      case Some(seq) => {
        @tailrec
        def termr(rem: List[StructureComponent], acc: Set[String]): Set[String] = rem match {
          case h :: t =>
            val nacc = acc + compIdent(h)
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

    private def optionKeys(oseq: Option[StructureSequence]) = oseq match {
      case Some(seq) => collectKeys(seq)
      case None      => Array[String]()
    }
    val headingKeys = optionKeys(heading)
    val detailKeys = optionKeys(detail)
    val summaryKeys = optionKeys(summary)

    // generate tag lookups for fixed width structures
    val tagLookup: TagTarget =
      if (version.ediForm.multiPartTags) buildTagTarget(segmentsUsed)
      else TagNext(0, 0, Map())
  }

  type StructureMap = Map[String, Structure]

  sealed abstract class SchemaLayout(val structures: Boolean, val sectioned: Boolean)
  case object MultiTableStructure extends SchemaLayout(true, true)
  case object SingleTableStructure extends SchemaLayout(true, false)
  case object SegmentsOnly extends SchemaLayout(false, false)

  private def defaultKey(parentId: String, position: Int) = parentId + (if (position < 10) "0" + position else position)

  sealed abstract class EdiForm(val text: String, val layout: SchemaLayout, val fixed: Boolean) extends SchemaJavaDefs {
    def isEnvelopeSegment(ident: String): Boolean
    val loopWrapperStart: String
    val loopWrapperEnd: String

    /** Construct data value key name from parent identifier, id, name, position value. */
    def keyName(parentId: String, id: String, name: String, position: Int): String

    /** Create key for version in data maps. */
    def versionKey(version: String): String

    /** Format uses segment and component positions flag. */
    val usePositions: Boolean

    /** Inline single-instance composites flag. */
    val inlineComposites: Boolean

    /** Multi-part segment tags flag. */
    val multiPartTags: Boolean

    // keys used in YAML
    val lengthKey = "length"
    val minLengthKey = "minLength"
    val maxLengthKey = "maxLength"
    val formatKey = "format"

    /** Build type format definition from YAML map data. */
    def readFormat(code: String, map: ValueMap): TypeFormat

    type PairWriter = (String, Object) => Unit

    /** Persist type format definition as YAML map data. */
    def writeFormat(format: TypeFormat, writer: PairWriter): Unit
  }
  sealed abstract class DelimitedEdiBase(text: String, layout: SchemaLayout) extends EdiForm(text, layout, false) {
    override val usePositions = true
    override val inlineComposites = true
    override val multiPartTags = false
    def formatBuilder: (String, Int, Int) => TypeFormat
    def convertMinMaxLength(map: ValueMap) = {
      if (map.containsKey(lengthKey)) {
        val length = getRequiredInt(lengthKey, map)
        (length, length)
      } else (getRequiredInt(minLengthKey, map), getRequiredInt(maxLengthKey, map))
    }
    override def readFormat(code: String, map: ValueMap): TypeFormat = {
      val (min, max) = convertMinMaxLength(map)
      formatBuilder(code, min, max)
    }
    override def writeFormat(format: TypeFormat, writer: PairWriter): Unit = {
      if (format.minLength == format.maxLength) writer(lengthKey, Integer.valueOf(format.minLength))
      else {
        writer(minLengthKey, Integer.valueOf(format.minLength))
        writer(maxLengthKey, Integer.valueOf(format.maxLength))
      }
    }
  }
  case object EdiFact extends DelimitedEdiBase("EDIFACT", MultiTableStructure) {
    import com.mulesoft.flatfile.lexical.EdifactConstants

    private val envelopeSegs = Set("UNB", "UNZ", "UNG", "UNE", "UNH", "UNT", "UNS")
    override def isEnvelopeSegment(ident: String) = envelopeSegs contains ident
    override val loopWrapperStart = "UGH"
    override val loopWrapperEnd = "UGT"
    override def keyName(parentId: String, id: String, name: String, position: Int) = {
      val scaled = if (position % 10 == 0) position / 10 else position
      if (position < 100) parentId + "0" + scaled.toString
      else parentId + scaled.toString
    }
    override def versionKey(version: String) = version.toUpperCase
    override def formatBuilder = EdifactConstants.buildType
  }
  case object X12 extends DelimitedEdiBase("X12", MultiTableStructure) {
    import com.mulesoft.flatfile.lexical.X12Constants

    private val envelopeSegs = Set("ISA", "IEA", "GS", "GE", "ST", "SE")
    override def isEnvelopeSegment(ident: String) = envelopeSegs contains ident
    override val loopWrapperStart = "LS"
    override val loopWrapperEnd = "LE"
    override def keyName(parentId: String, id: String, name: String, position: Int) = defaultKey(parentId, position)
    override def versionKey(version: String) = "v" + version
    override def formatBuilder = X12Constants.buildType
  }
  case object HL7 extends DelimitedEdiBase("HL7", SingleTableStructure) {
    import com.mulesoft.flatfile.lexical.HL7Support

    override def isEnvelopeSegment(ident: String) = "MSH" == ident || "" == ident
    override val loopWrapperStart = ""
    override val loopWrapperEnd = ""
    override def keyName(parentId: String, id: String, name: String, position: Int) =
      defaultKey(parentId + "-", position)
    override def versionKey(version: String) = "v" + version.filterNot { _ == '.' }
    override def formatBuilder = HL7Support.buildType
  }
  sealed abstract class FixedEdiBase(text: String, layout: SchemaLayout) extends EdiForm(text, layout, true) {
    import com.mulesoft.flatfile.schema.fftypes.FlatFileFormat

    override def isEnvelopeSegment(ident: String) = false
    override val loopWrapperStart = ""
    override val loopWrapperEnd = ""
    override def versionKey(version: String) = version
    override def keyName(parentId: String, id: String, name: String, position: Int) =
      if (id.nonEmpty) id
      else if (name.nonEmpty) name
      else throw new IllegalArgumentException("Fixed width schemas need id or name definition for every value")

    override val usePositions = false
    override val inlineComposites = false
    override val multiPartTags = true

    def loadFormat(code: String, length: Int, map: ValueMap): TypeFormat

    override def readFormat(code: String, map: ValueMap): TypeFormat = {
      val length = getRequiredInt(lengthKey, map)
      val format = getAsMap(formatKey, map)
      loadFormat(code, length, format)
    }
    override def writeFormat(format: TypeFormat, writer: PairWriter): Unit = {
      writer(lengthKey, Integer.valueOf(format.maxLength))
      format match {
        case f: FlatFileFormat => {
          val map = new ValueMapImpl
          f.writeOptions((key: String, value: Object) => map.put(key, value))
          if (!map.isEmpty) writer(formatKey, map)
        }
        case _ => throw new IllegalArgumentException(s"Invalid flat file format '${format.typeCode}'")
      }
    }
  }
  case object FlatFile extends FixedEdiBase("FLATFILE", SingleTableStructure) {
    import com.mulesoft.flatfile.schema.fftypes.{ CopybookFormats, FixedWidthFormats }

    override def loadFormat(code: String, length: Int, map: ValueMap): TypeFormat = {
      FixedWidthFormats.fixedFactories.get(code) match {
        case Some(f) => f.readFormat(length, map)
        case _ => CopybookFormats.copybookFactories.get(code) match {
          case Some(f) => f.readFormat(length, map)
          case _       => throw new IllegalArgumentException(s"Unknown format '$code'")
        }
      }
    }
  }
  case object FixedWidth extends FixedEdiBase("FIXEDWIDTH", SegmentsOnly) {
    import com.mulesoft.flatfile.schema.fftypes.FixedWidthFormats

    override def loadFormat(code: String, length: Int, map: ValueMap): TypeFormat = {
      FixedWidthFormats.fixedFactories.get(code) match {
        case Some(f) => f.readFormat(length, map)
        case _       => throw new IllegalArgumentException(s"Unknown format '$code'")
      }
    }
  }
  case object Copybook extends FixedEdiBase("COPYBOOK", SegmentsOnly) {
    import com.mulesoft.flatfile.schema.fftypes.CopybookFormats

    override def loadFormat(code: String, length: Int, map: ValueMap): TypeFormat = {
      CopybookFormats.copybookFactories.get(code) match {
        case Some(f) => f.readFormat(length, map)
        case _       => throw new IllegalArgumentException(s"Unknown format '$code'")
      }
    }
  }
  def convertEdiForm(value: String) = value match {
    case EdiFact.text    => EdiFact
    case X12.text        => X12
    case HL7.text        => HL7
    case FlatFile.text   => FlatFile
    case FixedWidth.text => FixedWidth
    case Copybook.text   => Copybook
    case _               => throw new IllegalArgumentException(s"Unknown EDI form $value")
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
    val elemMap = elements ++ (transact.elementsUsed.foldLeft(Map[String, EdiSchema.Element]())((map, elem) =>
      elem.ident match {
        case "" => map
        case id => map + (id -> elem)
      }))
    val compMap = composites ++ (transact.compositesUsed.foldLeft(Map[String, EdiSchema.Composite]())((map, comp) =>
      comp.ident match {
        case "" => map
        case id => map + (id -> comp)
      }))
    val segMap = segments ++ (transact.segmentsUsed.foldLeft(Map[String, EdiSchema.Segment]())((map, seg) => map + (seg.ident -> seg)))
    val transMap = structures + (transact.ident -> transact)
    EdiSchema(ediVersion, elemMap, compMap, segMap, transMap)
  }

  // generate tag lookups for fixed width schema with no structures
  val tagLookup: EdiSchema.TagTarget =
    if (structures.isEmpty && segments.nonEmpty && ediVersion.ediForm.multiPartTags) {
      EdiSchema.buildTagTarget(segments.values.toSet)
    } else EdiSchema.TagNext(0, 0, Map())
}