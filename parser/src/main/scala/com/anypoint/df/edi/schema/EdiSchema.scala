package com.anypoint.df.edi.schema

import org.yaml.snakeyaml.Yaml
import scala.beans.BeanProperty
import com.anypoint.df.edi.lexical.EdiConstants.DataType
import scala.annotation.tailrec

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

  /** Element definition.
    * @param ident unique identifier (actually a number)
    * @param name readable name
    * @param dataType type
    * @param minLength minimum value length
    * @param maxLength maximum value length
    */
  case class Element(val ident: String, val name: String, val dataType: DataType, val minLength: Int,
    val maxLength: Int)

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
  case class CompositeComponent(val composite: Composite, nm: Option[String], ky: String, pos: Int, use: Usage, cnt: Int)
    extends SegmentComponent(nm.getOrElse(composite.name), ky, pos, use, cnt)

  /** Occurrence rule definition. Subclasses define the actual rule checking.
    * @param code
    * @param components
    */
  sealed abstract class OccurrenceRule(val code: String, val components: List[SegmentComponent]) {
    if (components isEmpty) throw new IllegalArgumentException("components list must not be empty")
    def hasHead(map: java.util.Map[String, Object]) = map containsKey (components.head key)
    def verify(map: java.util.Map[String, Object]): Boolean
  }
  // X12 Required, EDIFACT One or more
  val OneOrMoreCode = "R"
  case class OneOrMore(comps: List[SegmentComponent]) extends OccurrenceRule(OneOrMoreCode, comps) {
    def verify(map: java.util.Map[String, Object]) = !comps.find(comp => map containsKey (comp key)).isEmpty
  }
  // X12 Conditional, EDIFACT If first, then all
  val IfFirstThenAllCode = "C"
  case class IfFirstThenAll(comps: List[SegmentComponent]) extends OccurrenceRule(IfFirstThenAllCode, comps) {
    def verify(map: java.util.Map[String, Object]) =
      if (hasHead(map)) {
        comps.find(comp => !(map containsKey (comp key))).isEmpty
      } else true
  }
  // X12 Exclusion, EDIFACT One or none
  val OneOrNoneCode = "E"
  case class OneOrNone(comps: List[SegmentComponent]) extends OccurrenceRule(OneOrNoneCode, comps) {
    def verify(map: java.util.Map[String, Object]) =
      comps.count(comp => map containsKey (comp key)) <= 1
  }
  // X12 List Conditional, EDIFACT If first, then at least one
  val IfFirstThenMoreCode = "L"
  case class IfFirstThenMore(comps: List[SegmentComponent]) extends OccurrenceRule(IfFirstThenMoreCode, comps) {
    def verify(map: java.util.Map[String, Object]) =
      if (hasHead(map)) {
        !comps.tail.find(comp => map containsKey (comp key)).isEmpty
      } else true
  }
  // X12 Paired, EDIFACT All or none
  val AllOrNoneCode = "P"
  case class AllOrNone(comps: List[SegmentComponent]) extends OccurrenceRule(AllOrNoneCode, comps) {
    def verify(map: java.util.Map[String, Object]) = {
      val first = hasHead(map)
      comps.tail.find(comp => (map containsKey (comp key)) != first).isEmpty
    }
  }
  // EDIFACT One and only one
  val OneAndOnlyOneCode = "O"
  case class OneAndOnlyOne(comps: List[SegmentComponent]) extends OccurrenceRule(OneAndOnlyOneCode, comps) {
    def verify(map: java.util.Map[String, Object]) =
      comps.count(comp => map containsKey (comp key)) == 1
  }
  // EDIFACT If first, then none
  val IfFirstThenNoneCode = "X"
  case class IfFirstThenNone(comps: List[SegmentComponent]) extends OccurrenceRule(IfFirstThenNoneCode, comps) {
    def verify(map: java.util.Map[String, Object]) =
      if (hasHead(map)) {
        comps.find(comp => map containsKey (comp key)).isEmpty
      } else true
  }

  /** Composite definition.
    * @param ident
    * @param name
    * @param components
    * @param rules
    */
  case class Composite(val ident: String, val name: String, val components: List[SegmentComponent],
    val rules: List[OccurrenceRule]) {
    def rewrite(prefix: String, form: EdiForm) = Composite(ident, name,
      components.map { comp =>
        val rekey = form.keyName(prefix, comp.position)
        comp match {
          case ec: ElementComponent =>
            ElementComponent(ec.element, Some(ec.name), rekey, ec.position, ec.usage, ec.count)
          case cc: CompositeComponent =>
            CompositeComponent(cc.composite, Some(cc.name), rekey, cc.position, cc.usage, cc.count)
        }
      },
      rules)
  }

  /** Segment definition.
    * @param ident
    * @param name
    * @param components
    * @param rules
    */
  case class Segment(val ident: String, val name: String, val components: List[SegmentComponent],
    val rules: List[OccurrenceRule])

  /** Segment position within a transaction. Position numbers are reused across different tables of a transaction
    * definition, so this gives a unique value for every segment.
    * @param table
    * @param position
    */
  case class SegmentPosition(table: Int, position: String) {
    def isBefore(other: SegmentPosition) =
      table < other.table || (table == other.table && position < other.position)
  }

  /** Base for all transaction components.
    * @param key map key for data
    * @param position segment (or starting segment) position
    * @param usage
    * @param count maximum repetition count (0 for unlimited)
    */
  sealed abstract class TransactionComponent(val key: String, val position: SegmentPosition, val usage: Usage,
    val count: Int)

  /** Build map from segment id to transaction component at level.
    * @param comps
    */
  def componentsById(comps: List[TransactionComponent]) =
    comps.foldLeft(Map[String, TransactionComponent]())((acc, comp) => comp match {
      case ref: ReferenceComponent => acc + (ref.segment.ident -> ref)
      case wrap: LoopWrapperComponent => acc + (wrap.open.ident + wrap.ident -> wrap)
      case grp: GroupComponent => acc + (grp.leadSegment.ident -> grp)
      case _ => acc
    })

  /** Segment reference.
    * @param segment
    * @param position
    * @param use
    * @param cnt
    */
  case class ReferenceComponent(val segment: Segment, pos: SegmentPosition, use: Usage, cnt: Int)
    extends TransactionComponent(segment.ident, pos, use, cnt)

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
    extends TransactionComponent(open.ident + ident, start, use, 1) {
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
  sealed abstract class GroupBase(ky: String, pos: SegmentPosition, use: Usage, cnt: Int,
    val items: List[TransactionComponent]) extends TransactionComponent(ky, pos, use, cnt) {

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
    itms: List[TransactionComponent]) extends GroupBase(s"$baseid[$elemval]", pos, use, cnt, itms)

  /** Get lead reference from list of transaction components. If the first component is not a reference this throws an
    * exception.
    * @param ident
    * @param comps
    */
  def leadReference(ident: String, comps: List[TransactionComponent]) = comps match {
    case (ref: ReferenceComponent) :: t => ref
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
  case class GroupComponent(val ident: String, use: Usage, cnt: Int, itms: List[TransactionComponent],
    val varkey: Option[String], val variants: List[VariantGroup], ky: Option[String] = None)
    extends GroupBase(ky.getOrElse(ident), leadReference(ident, itms).position, use, cnt, itms) {

    /** Group start position and lead segment. */
    val leadSegment = leadReference(ident, items).segment

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

  /** Transaction set definition.
    * @param ident
    * @param name
    * @param group
    * @param heading
    * @param detail
    * @param summary
    */
  case class Transaction(val ident: String, val name: String, val group: Option[String],
    val heading: List[TransactionComponent], val detail: List[TransactionComponent],
    val summary: List[TransactionComponent]) {

    /** Segments used in transaction set. */
    val segmentsUsed = {
      def referencer(comps: List[TransactionComponent], segments: Set[Segment]): Set[Segment] =
        comps.foldLeft(segments)((acc, comp) => comp match {
          case ref: ReferenceComponent => acc + ref.segment
          case wrap: LoopWrapperComponent => referencer(wrap.loopGroup.items, acc) + wrap.open + wrap.close
          case group: GroupComponent => referencer(group.items, acc)
        })
      referencer(summary, referencer(detail, referencer(heading, Set[Segment]())))
    }

    /** Composites used in transaction set. */
    val compositesUsed = {
      def referencer(comps: List[SegmentComponent], composites: Set[Composite]): Set[Composite] =
        comps.foldLeft(composites)((acc, comp) => comp match {
          case CompositeComponent(composite, _, _, _, _, _) => referencer(composite.components, acc + composite)
          case _ => acc
        })
      segmentsUsed.foldLeft(Set[Composite]())((acc, seg) => referencer(seg.components, acc))
    }

    /** Elements used in transaction set. */
    val elementsUsed = {
      def referencer(comps: List[SegmentComponent], elements: Set[Element]): Set[Element] =
        comps.foldLeft(elements)((acc, comp) => comp match {
          case CompositeComponent(composite, _, _, _, _, _) => referencer(composite.components, acc)
          case ElementComponent(element, _, _, _, _, _) => acc + element
        })
      segmentsUsed.foldLeft(Set[Element]())((acc, seg) => referencer(seg.components, acc))
    }

    /** Ids of all segments used in transaction set at any level. */
    val segmentIds = segmentsUsed.map(segment => segment.ident)

    /** Top-level components in heading by segment id. */
    val headingById = componentsById(heading)

    /** Top-level components in detail by segment id. */
    val detailById = componentsById(detail)

    /** Top-level components in summary by segment id. */
    val summaryById = componentsById(summary)

    /** All top-level components in transaction. */
    val compsById = headingById ++ detailById ++ summaryById
  }

  type TransactionMap = Map[String, Transaction]

  sealed abstract class EdiForm(val text: String) {
    def isEnvelopeSegment(ident: String): Boolean
    val loopWrapperStart: String
    val loopWrapperEnd: String

    /** Construct data value key name from parent identifier and position value. */
    def keyName(parentId: String, position: Int): String
  }
  case object EdiFact extends EdiForm("EDIFACT") {
    def isEnvelopeSegment(ident: String) = Set("UNH", "UNT") contains ident
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
  def convertEdiForm(value: String) = value match {
    case EdiFact.text => EdiFact
    case X12.text => X12
  }
}

case class EdiSchema(val ediForm: EdiSchema.EdiForm, val version: String, val elements: Map[String, EdiSchema.Element],
  val composites: Map[String, EdiSchema.Composite], val segments: Map[String, EdiSchema.Segment],
  val transactions: EdiSchema.TransactionMap) {
  def this(ver: String) = this(EdiSchema.X12, ver, Map[String, EdiSchema.Element](), Map[String, EdiSchema.Composite](),
    Map[String, EdiSchema.Segment](), Map[String, EdiSchema.Transaction]())
  def merge(other: EdiSchema) = EdiSchema(ediForm, version, elements ++ other.elements, composites ++ other.composites,
    segments ++ other.segments, transactions ++ other.transactions)
  def merge(transact: EdiSchema.Transaction) = {
    val elemMap = elements ++ (transact.elementsUsed.foldLeft(Map[String, EdiSchema.Element]())((map, elem) => map + (elem.ident -> elem)))
    val compMap = composites ++ (transact.compositesUsed.foldLeft(Map[String, EdiSchema.Composite]())((map, comp) => map + (comp.ident -> comp)))
    val segMap = segments ++ (transact.segmentsUsed.foldLeft(Map[String, EdiSchema.Segment]())((map, seg) => map + (seg.ident -> seg)))
    val transMap = transactions + (transact.ident -> transact)
    EdiSchema(ediForm, version, elemMap, compMap, segMap, transMap)
  }
}