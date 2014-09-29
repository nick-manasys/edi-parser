package com.anypoint.df.edi.schema

object EdiSchema {

  sealed trait MaximumUsage
  case class FiniteUsage(val number: Int) extends MaximumUsage
  case object UnlimitedUsage extends MaximumUsage
  val UsageDefault = FiniteUsage(1)
  
  // dependency notes - note syntax rules are different for X12, using only single character
  abstract class DependencyType(code: String)
  case object ExactlyOneDependency extends DependencyType("D1")
  case object AllOrNoneDependency extends DependencyType("D2")
  case object OneOrMoreDependency extends DependencyType("D3")
  case object OneOrNoneDependency extends DependencyType("D4")
  case object IfFirstAllDependency extends DependencyType("D5")
  case object IfFirstAtLeastOneDependency extends DependencyType("D6")
  case object IfFirstNoneDependency extends DependencyType("D7")
  case class DependencyNote(kind: DependencyType, items: Seq[Int])
  // TODO: add dependency rules to schema representation

  // definitions for element types
  abstract class ElementType(code: String)
  case object RealType extends ElementType("R") // digits with explicit decimal point (if anywhere except rightmost)
  case object IdType extends ElementType("ID")  // unique value from predefined list (X12 only?)
  case object AlphaNumericType extends ElementType("AN") // alphanumerics and spaces, leading spaces preserved
  case object AlphaType extends ElementType("A")    // alphas and spaces, leading spaces preserved
  case object DateType extends ElementType("DT")    // date in YYMMDD form
  case object TimeType extends ElementType("TM")    // time in HHMMSSd..d form (seconds and decimal seconds optional)
  case object BinaryType extends ElementType("B")   // binary octets
  case object NumberType extends ElementType("N")   // digits, optional decimal, optional minus (decimal requires leading/training digit(s))
  case object IntegerType extends ElementType("N0") // integer digits, optional minus
  case class DecimalType(places: Int) extends ElementType("N") // digits with implied decimal point, optional minus
  
  abstract class ValueBase(val ident: String, val fullName: String, val minOccurs: Int, val maxOccurs: Int)
  case class SimpleValue(id: String, name: String, val dataType: ElementType, val minLength: Int, val maxLength: Int, min: Int, max: Int) extends ValueBase(id, name, min, max)
  case class CompositeValue(id: String, name: String, min: Int, max: Int, val values: Seq[ValueBase]) extends ValueBase(id, name, min, max)
  
  case class Segment(val ident: String, val usage: MaximumUsage, val values: Seq[ValueBase])
  
  case class Transaction(val ident: String, val segments: Seq[Segment])
  
  type TransactionSet = Map[String, Transaction]
}