package com.anypoint.df.edi.schema

import scala.annotation.tailrec

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
    * @param ident
    * @param name
    * @param components
    * @param rules
    */
  case class Segment(val ident: String, val name: String, val components: List[SegmentComponent],
    val rules: List[OccurrenceRule])

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

  /** Build map from segment id to structure component at level.
    * @param comps
    */
  def componentsById(comps: List[StructureComponent]) =
    comps.foldLeft(Map[String, StructureComponent]())((acc, comp) => comp match {
      case ref: ReferenceComponent => acc + (ref.segment.ident -> ref)
      case wrap: LoopWrapperComponent => acc + (wrap.open.ident + wrap.ident -> wrap)
      case grp: GroupComponent => acc + (grp.leadSegmentRef.segment.ident -> grp)
      case _ => acc
    })

  /** Key for a structure component. */
  def componentKey(ident: String, pos: SegmentPosition) = pos.position + " " + ident

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
    */
  case class WildcardComponent(val ident: String, pos: SegmentPosition, use: Usage, cnt: Int)
    extends StructureComponent(componentKey(ident, pos), pos, use, cnt)

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
    val endPosition: SegmentPosition, use: Usage, val ident: String, grp: GroupComponent)
      extends StructureComponent(componentKey(open.ident, start), start, use, 1) {
    val loopGroup = GroupComponent(grp.ident, grp.usage, grp.count, grp.items, grp.varkey, grp.variants, Some(key))
    val compsById = Map((open.ident + ident) -> ReferenceComponent(open, start, MandatoryUsage, 1),
      (close.ident + ident) -> ReferenceComponent(close, endPosition, MandatoryUsage, 1))
  }

  /** Base for group components.
    * @param ky
    * @param use
    * @param cnt
    * @param items
    */
  sealed abstract class GroupBase(ky: String, pos: SegmentPosition, use: Usage, cnt: Int, val choice: Boolean,
      val items: List[StructureComponent]) extends StructureComponent(ky, pos, use, cnt) {

    /** Components in group by segment identifier. */
    val compsById = componentsById(items)
  }

  /** Variant of a group component. The items in a variant must be a subset of the items in the group.
    * @param baseid base identifier for all variants of the group
    * @param use
    * @param cnt
    * @param itms
    */
  case class VariantGroup(val baseid: String, val elemval: String, pos: SegmentPosition, use: Usage, cnt: Int,
    itms: List[StructureComponent])
      extends GroupBase(componentKey(s"$baseid[$elemval]", pos), pos, use, cnt, false, itms)

  /** Get lead reference from list of structure components. If the first component is not a reference this recursively
    * descends until a first component reference is found.
    * @param ident
    * @param comps
    */
  def leadReference(ident: String, comps: List[StructureComponent]): ReferenceComponent = comps match {
    case (ref: ReferenceComponent) :: t => ref
    case (grp: GroupBase) :: t => leadReference(ident, grp.items)
    case _ => throw new IllegalStateException(s"first item in group $ident is not a segment reference")
  }

  /** Group component consisting of one or more nested components.
    * @param ident group identifier
    * @param use
    * @param cnt
    * @param itms
    * @param varkey key for field controlling variant selection
    * @param variants group variant forms
    * @param ky explicit key value (ident used by default)
    */
  case class GroupComponent(val ident: String, use: Usage, cnt: Int, itms: List[StructureComponent],
    val varkey: Option[String], val variants: List[VariantGroup], ky: Option[String] = None, ch: Boolean = false)
      extends GroupBase(ky.getOrElse(componentKey(ident, leadReference(ident, itms).position)),
        leadReference(ident, itms).position, use, cnt, ch, itms) {

    /** Group head segment reference. */
    val leadSegmentRef = leadReference(ident, items)

    /** End position. */
    val endPosition: SegmentPosition = itms.last match {
      case ref: ReferenceComponent => ref.position
      case wrap: LoopWrapperComponent => wrap.endPosition
      case grp: GroupComponent => grp.endPosition
      case _ => throw new IllegalStateException(s"last item in group $ident is not a segment reference or group")
    }

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
    */
  case class Structure(val ident: String, val name: String, val group: Option[String],
      val heading: List[StructureComponent], val detail: List[StructureComponent],
      val summary: List[StructureComponent], val version: EdiSchemaVersion) {

    /** Segments used in structure. */
    val segmentsUsed = {
      def referencer(comps: List[StructureComponent], segments: Set[Segment]): Set[Segment] =
        comps.foldLeft(segments)((acc, comp) => comp match {
          case ref: ReferenceComponent => acc + ref.segment
          case wrap: LoopWrapperComponent => referencer(wrap.loopGroup.items, acc) + wrap.open + wrap.close
          case group: GroupComponent => referencer(group.items, acc)
        })
      referencer(summary, referencer(detail, referencer(heading, Set[Segment]())))
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

    /** Top-level components in heading by segment id. */
    val headingById = componentsById(heading)

    /** Top-level components in detail by segment id. */
    val detailById = componentsById(detail)

    /** Top-level components in summary by segment id. */
    val summaryById = componentsById(summary)

    /** All top-level components in structure. */
    val compsById = headingById ++ detailById ++ summaryById
  }

  type StructureMap = Map[String, Structure]

  sealed abstract class EdiForm(val text: String) {
    def isEnvelopeSegment(ident: String): Boolean
    val loopWrapperStart: String
    val loopWrapperEnd: String

    /** Construct data value key name from parent identifier and position value. */
    def keyName(parentId: String, position: Int): String
  }
  case object EdiFact extends EdiForm("EDIFACT") {
    def isEnvelopeSegment(ident: String) = Set("UNB", "UNZ", "UNG", "UNE", "UNH", "UNT", "UNS") contains ident
    val loopWrapperStart = "UGH"
    val loopWrapperEnd = "UGT"
    def keyName(parentId: String, position: Int) = {
      val scaled = if (position % 10 == 0) position / 10 else position
      if (position < 100) parentId + "0" + scaled.toString
      else parentId + scaled.toString
    }
  }
  case object X12 extends EdiForm("X12") {
    def isEnvelopeSegment(ident: String) = Set("ISA", "IEA", "GS", "GE", "ST", "SE") contains ident
    val loopWrapperStart = "LS"
    val loopWrapperEnd = "LE"
    def keyName(parentId: String, position: Int) = parentId + (if (position < 10) "0" + position else position)
  }
  case object HL7 extends EdiForm("HL7") {
    def isEnvelopeSegment(ident: String) = "MSH" == ident || "" == ident
    val loopWrapperStart = ""
    val loopWrapperEnd = ""
    def keyName(parentId: String, position: Int) = parentId + "-" + (if (position < 10) "0" + position else position)
  }
  def convertEdiForm(value: String) = value match {
    case EdiFact.text => EdiFact
    case X12.text => X12
    case HL7.text => HL7
    case _ => throw new IllegalArgumentException(s"Unknown EDI form $value")
  }
}

case class EdiSchemaVersion(val ediForm: EdiSchema.EdiForm, val version: String)

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