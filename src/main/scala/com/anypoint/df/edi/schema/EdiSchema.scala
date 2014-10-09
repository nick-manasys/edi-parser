package com.anypoint.df.edi.schema

import org.yaml.snakeyaml.Yaml
import scala.beans.BeanProperty

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
  abstract class DataType(val code: String)
  case object RealType extends DataType("R") // digits with explicit decimal point (if anywhere except rightmost)
  case object IdType extends DataType("ID") // unique value from predefined list (X12 only?)
  case object AlphaNumericType extends DataType("AN") // alphanumerics and spaces, leading spaces preserved
  case object AlphaType extends DataType("A") // alphas and spaces, leading spaces preserved
  case object DateType extends DataType("DT") // date in YYMMDD form
  case object TimeType extends DataType("TM") // time in HHMMSSd..d form (seconds and decimal seconds optional)
  case object BinaryType extends DataType("B") // binary octets
  case object NumberType extends DataType("N") // digits, optional decimal, optional minus (decimal requires leading/training digit(s))
  case object IntegerType extends DataType("N0") // integer digits, optional minus
  case class DecimalType(val places: Int) extends DataType("N" + ('0' + places)) // digits with implied decimal point, optional minus
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
    case _ =>
      if (value.size == 2 && value(0) == 'N' && value(1) >= '1' && value(1) <= '9') DecimalType(value(1) - '0')
      else throw new IllegalArgumentException("'" + value + "' is not an allowed data type code")
  }

  case class Element(val ident: String, val dataType: DataType, val minLength: Int, val maxLength: Int)

  abstract class SegmentComponent(val ident: String, val name: String, val usage: Usage, val count: Int)
  case class ElementComponent(id: String, nm: String, use: Usage, cnt: Int) extends SegmentComponent(id, nm, use, cnt)
  case class CompositeComponent(id: String, nm: String, use: Usage, cnt: Int) extends SegmentComponent(id, nm, use, cnt)

  case class Composite(val ident: String, val name: String, val components: List[SegmentComponent])

  case class Segment(val ident: String, val name: String, val components: List[SegmentComponent])

  sealed abstract class TransactionComponent(val ident: String, val usage: Usage, val count: Int)
  case class ReferenceComponent(id: String, use: Usage, cnt: Int) extends TransactionComponent(id, use, cnt)
  case class GroupComponent(id: String, use: Usage, cnt: Int, val items: List[TransactionComponent]) extends TransactionComponent(id, use, cnt)

  case class Transaction(val ident: String, val heading: List[TransactionComponent], val detail: List[TransactionComponent], val summary: List[TransactionComponent])

  type TransactionMap = Map[String, Transaction]

  case class Schema(val elements: List[Element], val composites: List[Composite], val segments: List[Segment], val transactions: List[Transaction])
}