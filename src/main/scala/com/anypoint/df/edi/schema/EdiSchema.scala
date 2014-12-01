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

  case class Element(val ident: String, val dataType: DataType, val minLength: Int, val maxLength: Int)

  abstract class SegmentComponent(val name: String, val position: Int, val usage: Usage, val count: Int) {
    val key = name + " (" + (if (position >= 10) position.toString else "0" + position.toString) + ")"

  }
  case class ElementComponent(val element: Element, nm: String, pos: Int, use: Usage, cnt: Int) extends SegmentComponent(nm, pos, use, cnt)
  case class CompositeComponent(val composite: Composite, nm: String, pos: Int, use: Usage, cnt: Int) extends SegmentComponent(nm, pos, use, cnt)

  case class Composite(val ident: String, val name: String, val components: List[SegmentComponent])

  case class Segment(val ident: String, val name: String, val components: List[SegmentComponent])

  sealed abstract class TransactionComponent(val usage: Usage, val count: Int)
  case class ReferenceComponent(val segment: Segment, use: Usage, cnt: Int) extends TransactionComponent(use, cnt)
  case class GroupComponent(val ident: String, use: Usage, cnt: Int, val items: List[TransactionComponent]) extends TransactionComponent(use, cnt)

  case class Transaction(val ident: String, val name: String, val group: String,
    val heading: List[TransactionComponent], val detail: List[TransactionComponent],
    val summary: List[TransactionComponent]) {
    lazy val segmentsUsed = {
      def referencer(comps: List[TransactionComponent], segments: Set[Segment]): Set[Segment] =
        comps.foldLeft(segments)((segs, comp) => comp match {
          case ref: ReferenceComponent => segs + ref.segment
          case group: GroupComponent => referencer(group.items, segs)
        })
      referencer(summary, referencer(detail, referencer(heading, Set[Segment]())))
    }
    lazy val compositesUsed = {
      def referencer(comps: List[SegmentComponent], composites: Set[Composite]): Set[Composite] =
        comps.foldLeft(composites)((acc, comp) => comp match {
          case CompositeComponent(composite, _, _, _, _) => referencer(composite.components, acc + composite)
          case _ => acc
        })
      segmentsUsed.foldLeft(Set[Composite]())((acc, seg) => referencer(seg.components, acc))
    }
    lazy val elementsUsed = {
      def referencer(comps: List[SegmentComponent], elements: Set[Element]): Set[Element] =
        comps.foldLeft(elements)((acc, comp) => comp match {
          case CompositeComponent(composite, _, _, _, _) => referencer(composite.components, acc)
          case ElementComponent(element, _, _, _, _) => acc + element
        })
      segmentsUsed.foldLeft(Set[Element]())((acc, seg) => referencer(seg.components, acc))
    }
  }

  type TransactionMap = Map[String, Transaction]

  sealed abstract class EdiForm(val text: String)
  case object EdiFact extends EdiForm("EDIFACT")
  case object X12 extends EdiForm("X12")
  def convertEdiForm(value: String) = value match {
    case EdiFact.text => EdiFact
    case X12.text => X12
  }
}

case class EdiSchema(val ediForm: EdiSchema.EdiForm, val elements: Map[String, EdiSchema.Element],
  val composites: Map[String, EdiSchema.Composite], val segments: Map[String, EdiSchema.Segment],
  val transactions: Map[String, EdiSchema.Transaction]) {
  def merge(other: EdiSchema) = EdiSchema(ediForm, elements ++ other.elements, composites ++ other.composites,
    segments ++ other.segments, transactions ++ other.transactions)
  def merge(transact: EdiSchema.Transaction) = {
    val elemMap = elements ++ (transact.elementsUsed.foldLeft(Map[String, EdiSchema.Element]())((map, elem) => map + (elem.ident -> elem)))
    val compMap = composites ++ (transact.compositesUsed.foldLeft(Map[String, EdiSchema.Composite]())((map, comp) => map + (comp.ident -> comp)))
    val segMap = segments ++ (transact.segmentsUsed.foldLeft(Map[String, EdiSchema.Segment]())((map, seg) => map + (seg.ident -> seg)))
    val transMap = transactions + (transact.ident -> transact)
    EdiSchema(ediForm, elemMap, compMap, segMap, transMap)
  }
}