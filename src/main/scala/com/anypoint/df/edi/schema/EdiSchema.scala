package com.anypoint.df.edi.schema

import org.yaml.snakeyaml.Yaml
import scala.beans.BeanProperty

/**
 * EDI schema representation.
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

  // definitions for data types
  sealed abstract class DataType(val code: String)
  case object RealType extends DataType("R") // digits with explicit decimal point (if anywhere except rightmost)
  case object IdType extends DataType("ID") // unique value from predefined list
  case object AlphaNumericType extends DataType("AN") // alphanumerics and spaces, leading spaces preserved
  case object AlphaType extends DataType("A") // alphas and spaces, leading spaces preserved
  case object DateType extends DataType("DT") // date in YYMMDD form
  case object TimeType extends DataType("TM") // time in HHMMSSd..d form (seconds and decimal seconds optional)
  case object BinaryType extends DataType("B") // binary octets
  case object NumberType extends DataType("N") // digits, optional decimal, optional minus (decimal requires leading/training digit(s))
  case object IntegerType extends DataType("N0") // integer digits, optional minus
  abstract class DecimalType(val places: Int) extends DataType("N" + ('0' + places).toChar) // digits with implied decimal point, optional minus
  case object Decimal1Type extends DecimalType(1)
  case object Decimal2Type extends DecimalType(2)
  case object Decimal3Type extends DecimalType(3)
  case object Decimal4Type extends DecimalType(4)
  case object Decimal5Type extends DecimalType(5)
  case object Decimal6Type extends DecimalType(6)
  case object Decimal7Type extends DecimalType(7)
  case object Decimal8Type extends DecimalType(8)
  case object Decimal9Type extends DecimalType(9)
  def convertDataType(value: String) = value match {
    case RealType.code => RealType
    case IdType.code => IdType
    case AlphaNumericType.code => AlphaNumericType
    case AlphaType.code => AlphaType
    case DateType.code => DateType
    case TimeType.code => TimeType
    case BinaryType.code => BinaryType
    case NumberType.code => NumberType
    case IntegerType.code => IntegerType
    case _ if (value.size == 2 && value(0) == 'N') => value(1) match {
      case '1' => Decimal1Type
      case '2' => Decimal2Type
      case '3' => Decimal3Type
      case '4' => Decimal4Type
      case '5' => Decimal5Type
      case '6' => Decimal6Type
      case '7' => Decimal7Type
      case '8' => Decimal8Type
      case '9' => Decimal9Type
      case _ => throw new IllegalArgumentException(s"'$value' is not an allowed data type code")
    }
    case _ => throw new IllegalArgumentException(s"'$value' is not an allowed data type code")
  }
  // allow Java code access to instances (Eclipse doesn't like doing it directly)
  val RealDataTypes = List[DataType](RealType, NumberType, Decimal1Type, Decimal2Type, Decimal3Type, Decimal4Type, Decimal5Type, Decimal6Type, Decimal7Type, Decimal8Type, Decimal9Type)
  val IntegerDataTypes = List[DataType](IntegerType, TimeType)
  val DateDataTypes = List[DataType](DateType)
  val StringDataTypes = List[DataType](IdType, AlphaNumericType, AlphaType)

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

  case class Transaction(val ident: String, val name: String, val group: String, val heading: List[TransactionComponent], val detail: List[TransactionComponent], val summary: List[TransactionComponent])

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
}