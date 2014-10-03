package com.anypoint.df.edi.sef

import scala.collection._
import scala.io.Source
import java.io.InputStream
import java.io.BufferedReader
import java.io.Reader
import scala.annotation.tailrec
import scala.collection.BufferedIterator
import scala.util.parsing.combinator._

object MessageParser extends RegexParsers {
  
  import com.anypoint.df.edi.schema.EdiSchema._
  
  private val eoi = """\z""".r
  private val eol = sys.props("line.separator").r
  private val separator = eoi | eol
  override val skipWhitespace = false
  
  sealed abstract class SegmentRequirement(val code: Char)
  case object MandatoryRequirement extends SegmentRequirement('M')
  case object FunctionalRequirement extends SegmentRequirement('F')
  case object ConditionalRequirement extends SegmentRequirement('C')
  val RequirementDefault = ConditionalRequirement

  sealed abstract class UsageNotationMark(val code: Char)
  case object NotUsedMark extends UsageNotationMark('.')
  case object MustUseMark extends UsageNotationMark('!')
  case object RecommendedMark extends UsageNotationMark('$')
  case object NotRecommendMark extends UsageNotationMark('-')
  case object DependentMark extends UsageNotationMark('&')

  /** Every kind of item in a transaction definition. */
  sealed trait TransactionItem
  /** Position increment change. */
  case class PositionIncrement(val increment: Int) extends TransactionItem
  /**
   * Actual component of a transaction definition, either a segment reference or a loop/group definition.
   *  @param ident segment or loop/group identifier
   *  @param mark usage notation (optional, overrides requirement)
   *  @param require segment requirement (optional, defaults to C)
   *  @param ordinal optional override of sequencing
   *  @param usage maximum usage (optional, defaults to 1)
   */
  abstract class TransactionComponent(val ident: String, val mark: Option[UsageNotationMark], val ordinal: Option[Int], val require: SegmentRequirement) extends TransactionItem
  case class SegmentReference(id: String, mrk: Option[UsageNotationMark], ord: Option[Int], val mask: Int, req: SegmentRequirement, val usage: MaximumUsage) extends TransactionComponent(id, mrk, ord, req)
  case class Group(id: String, mrk: Option[UsageNotationMark], ord: Option[Int], req: SegmentRequirement, val repeat: Int, val items: Seq[TransactionItem]) extends TransactionComponent(id, mrk, ord, req)
  
  case class TransactionTable(val comps: Seq[TransactionItem])
  
  case class TransactionSet(val ident: String, val tables: Seq[TransactionTable])

  // Grammar for .SETS section
  //
  // setsSection := ".SETS\n" transSet*
  // transSet := id "=" table* "\n"
  // table := "^"+ transItem*
  // transItem := segRef | group | increment
  // segRef := "[" useMark? id maskNum? ordNum? ("," stdReq? maxUse?)? "]"
  // useMark := "." | "!" | "$" | "-" | "&"
  // maskNum := "*" posInt
  // ordNum := "@" posInt
  // stdReq := "M" | "F" | "C"
  // maxUse := "," (">1" | posInt)
  // posInt := [0-9]*
  // position := [0-9]*
  // id := [0-9,A-Z]*
  // increment := ("+" | "-") posInt
  // group := ordNum? "{" groupLead? transItem* "}"
  // groupLead := id? ":" posInt

  // simple base parsers
  val posInt: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  val position: Parser[Int] = """\d+""".r ^^ { _.toInt }
  val id: Parser[String] = """\w+""".r
  val maskNum: Parser[Int] = "*" ~> posInt
  val ordNum: Parser[Int] = "@" ~> posInt

  // parser for usage marker character preceding segment id
  val useMark = new Parser[UsageNotationMark] {
    def apply(input: Input): ParseResult[UsageNotationMark] = {
      if (input.atEnd) Failure("End of input", input)
      else input.first match {
        case NotUsedMark.code => Success(NotUsedMark, input.rest)
        case MustUseMark.code => Success(MustUseMark, input.rest)
        case RecommendedMark.code => Success(RecommendedMark, input.rest)
        case NotRecommendMark.code => Success(NotRecommendMark, input.rest)
        case DependentMark.code => Success(DependentMark, input.rest)
        case _ => Failure("No match", input)
      }
    }
  }

  // parser for requirement according to standard
  val stdReq = new Parser[SegmentRequirement] {
    def apply(input: Input): ParseResult[SegmentRequirement] = {
      if (input.atEnd) Failure("End of input", input)
      else input.first match {
        case MandatoryRequirement.code => Success(MandatoryRequirement, input.rest)
        case FunctionalRequirement.code => Success(FunctionalRequirement, input.rest)
        case ConditionalRequirement.code => Success(ConditionalRequirement, input.rest)
        case _ => Failure("No match", input)
      }
    }
  }

  // parser for maximum count = "," (">1" | posInt)
  val maxUse: Parser[MaximumUsage] = "," ~> (">1" | posInt) ^^ {
    case count: Int => FiniteUsage(count)
    case s: String => UnlimitedUsage
  }

  // parser for group lead = position? ":" posint
  val groupLead: Parser[(Option[String], Int)] = (id.? ~ (":" ~> posInt.?)) ^^ {
    case id ~ oprep => (id, oprep getOrElse 1)
  }

  // parser for segment reference = "[" useMark? id maskNum? ordNum? ("," segReq? ("," maxUse)?)? "]"
  val segRef: Parser[SegmentReference] = "[" ~> useMark.? ~ id ~ maskNum.? ~ ordNum.? ~ ("," ~> stdReq.? ~ maxUse.?).? <~ "]" ^^ {
    case opmark ~ id ~ opmask ~ opord ~ (Some(opreq ~ opuse)) => SegmentReference(id, opmark, opord, opmask getOrElse 0, opreq getOrElse RequirementDefault, opuse getOrElse UsageDefault)
    case opmark ~ id ~ opmask ~ opord ~ None => SegmentReference(id, opmark, opord, opmask getOrElse 0, RequirementDefault, UsageDefault)
  }

  // parser for group definition = ordNum? "{" groupLead? transItem* "}"
  @tailrec
  def firstSegment(items: Seq[TransactionItem]): SegmentReference = items match {
    case (segref: SegmentReference) :: _ => segref
    case (group: Group) :: _ => firstSegment(group.items)
    case _ :: tail => firstSegment(tail)
    case Nil => throw new IllegalStateException("No segment definition in item list")
  }
  val group: Parser[Group] = ordNum.? ~ ("{" ~> groupLead.? ~ transItem.+ <~ "}") ^^ {
    case opord ~ (Some((opid, rep)) ~ items) => {
      val first = firstSegment(items)
      Group(opid getOrElse first.id, first.mark, None, first.require, rep, items)
    }
    case opord ~ (None ~ items) => {
      val first = firstSegment(items)
      Group(first.id, first.mark, None, first.require, 1, items)
    }
  }

  // parser for position increment = ("+" | "-") posInt
  val increment: Parser[PositionIncrement] = """\+|\-""".r ~ posInt ^^ {
    case "+" ~ num => PositionIncrement(num)
    case "-" ~ num => PositionIncrement(-num)
  }

  // parser for transaction item = segRef | group | increment
  val transItem: Parser[TransactionItem] = segRef | group | increment
  
  // parser for table = "^"+ transItem*
  val table: Parser[TransactionTable] = ("""\^""".r+) ~> transItem.+ ^^ {
    case list => TransactionTable(list)
  }
  
  // parser for transaction set = id "=" table* "\n"
  val transSet: Parser[TransactionSet] = (id <~ "=") ~ table.+ <~ separator ^^ {
    case id ~ tables => TransactionSet(id, tables)
  }
  
  // parser for .SETS section = ".SETS\n" transSet*
  val setsSection: Parser[Seq[TransactionSet]] = ".SETS" ~> separator ~> transSet.*
  
  // definitions for .SEGS and COMS
  sealed trait ValueListItem
  case class GroupValue(val repeatCount: Int, val items: Seq[ValueListItem]) extends ValueListItem
  case class BaseValue(val ident: String, val mark: Option[UsageNotationMark], val ordinal: Option[Int], val lengths: MinMaxPair, val usage: SegmentRequirement, val repeat: Int) extends ValueListItem
  case class SegmentDef(val ident: String, val values: Seq[ValueListItem], val rules: Seq[DependencyNote], val masks: Seq[Mask])
  case class CompositeDef(val ident: String, val values: Seq[ValueListItem], val rules: Seq[DependencyNote])
  
  sealed abstract class UsageNotationMask(val code: Char)
  case object InheritMask extends UsageNotationMask('.')
  case object NotUsedMask extends UsageNotationMask('#')
  case object MustUseMask extends UsageNotationMask('@')
  case object RecommendedMask extends UsageNotationMask('$')
  case object NotRecommendMask extends UsageNotationMask('-')
  case object DependentMask extends UsageNotationMask('&')
  case object UsedMask extends UsageNotationMask('+')
  
  case class MaskItem(val usage: UsageNotationMask, val modify: MaskItemModification)
  sealed trait MaskItemModification
  case object NoModification extends MaskItemModification
  case class UseMaskModification(val maskIndex: Int) extends MaskItemModification
  case class PropertiesMaskModification(val segReq: Option[SegmentRequirement], val lengths: MinMaxPair) extends MaskItemModification
  case class Mask(val items: Seq[MaskItem])
  
  case class MinMaxPair(val min: Option[Int], val max: Option[Int])
  val DefaultMinMaxPair = MinMaxPair(None, None)
  
  // Grammar for .SEGS section
  //
  // segsSection := ".SEGS\n" segDef*
  // segDef := id "=" valueItem+ ("+" depNote+)? ("," segMask)*"\n"
  // valueItem := simpleValue | repGroup
  // simpleValue := "[" useMark? id ordNum? (";" minMax) ("," stdReq? posInt?)? "]"
  // minMax := posInt? (":" posInt?)?
  // repGroup := "{" posInt valueItem* "}"
  // depNote := "D" depType depList
  // depList := "(" posInt ("," posInt)* ")"
  // depType := "1" | "2" | "3" | "4" | "5" | "6" | "7"
  // segMaskItem := (useMask maskMod?)*
  // segMask := segMaskItem*
  // maskMod := maskNum | (stdReq? ("[" minMax "]")?)
  // useMask := "." | "#" | "@" | "$" | "-" | "&" | "+"

  // parser for usage marker character in mask
  val useMask = new Parser[UsageNotationMask] {
    def apply(input: Input): ParseResult[UsageNotationMask] = {
      if (input.atEnd) Failure("End of input", input)
      else input.first match {
        case InheritMask.code => Success(InheritMask, input.rest)
        case NotUsedMask.code => Success(NotUsedMask, input.rest)
        case MustUseMask.code => Success(MustUseMask, input.rest)
        case RecommendedMask.code => Success(RecommendedMask, input.rest)
        case NotRecommendMask.code => Success(NotRecommendMask, input.rest)
        case DependentMask.code => Success(DependentMask, input.rest)
        case UsedMask.code => Success(UsedMask, input.rest)
        case _ => Failure("No match", input)
      }
    }
  }
  
  // parser for minimum/maximum length pair = posInt? (":" posInt?)?
  val minMax: Parser[MinMaxPair] = posInt.? ~ (":" ~> posInt?).? ^^ {
    case None ~ None => DefaultMinMaxPair
    case opmin ~ opopmax => MinMaxPair(opmin, opopmax getOrElse None)
  }
  
  // parser for segment modification in mask = maskNum | (stdReq? ("[" minMax "]")?)
  val maskMod: Parser[MaskItemModification] = maskNum.? ~ stdReq.? ~ ("[" ~> minMax <~ "]").? ^^ {
    case Some(mnum) ~ None ~ None => UseMaskModification(mnum)
    case None ~ None ~ None => NoModification
    case None ~ opreq ~ opmm => PropertiesMaskModification(opreq, opmm getOrElse DefaultMinMaxPair)
    // how to handle the error case where both mask and modification used?
  }
  
  // parser for segment mask item = useMask maskMod?
  val segMaskItem: Parser[MaskItem] = useMask ~ maskMod ^^ {
    case use ~ mod => MaskItem(use, mod)
  }
  
  // parser for segment mask = segMaskItem*
  val segMask: Parser[Mask] = segMaskItem.* ^^ {
    case items => Mask(items)
  }
  
  // parser for dependency type
  val depType = new Parser[DependencyType] {
    def apply(input: Input): ParseResult[DependencyType] = {
      if (input.atEnd) Failure("End of input", input)
      else input.first match {
        case ExactlyOneDependency.code => Success(ExactlyOneDependency, input.rest)
        case AllOrNoneDependency.code => Success(AllOrNoneDependency, input.rest)
        case OneOrMoreDependency.code => Success(OneOrMoreDependency, input.rest)
        case OneOrNoneDependency.code => Success(OneOrNoneDependency, input.rest)
        case IfFirstAllDependency.code => Success(IfFirstAllDependency, input.rest)
        case IfFirstAtLeastOneDependency.code => Success(IfFirstAtLeastOneDependency, input.rest)
        case IfFirstNoneDependency.code => Success(IfFirstNoneDependency, input.rest)
        case _ => Failure("No match", input)
      }
    }
  }
  
  // parser for dependency note item list = "(" id ("," id)* ")"
  val depList: Parser[Seq[Int]] = "(" ~> rep1sep(position, ",") <~ ")"
  
  // parser for dependency note = "D" depType depList
  val depNote: Parser[DependencyNote] = "D" ~> depType ~ depList ^^ {
    case typ ~ list => DependencyNote(typ, list)
  }
  
  // parser for simple value = "[" useMark? id ordNum? (";" minMax) ("," stdReq? posInt?)? "]"
  val simpleValue: Parser[BaseValue] = "[" ~> useMark.? ~ id ~ ordNum.? ~ (";" ~> minMax).? ~ ("," ~> stdReq.? ~ ("," ~> posInt.?).?).? <~ "]" ^^ {
    case opuse ~ id ~ opord ~ opmm ~ Some(opreq ~ Some(opmax)) => BaseValue(id, opuse, opord, opmm getOrElse DefaultMinMaxPair, opreq getOrElse RequirementDefault, opmax getOrElse 1)
    case opuse ~ id ~ opord ~ opmm ~ Some(opreq ~ None) => BaseValue(id, opuse, opord, opmm getOrElse DefaultMinMaxPair, opreq getOrElse RequirementDefault, 1)
    case opuse ~ id ~ opord ~ opmm ~ None => BaseValue(id, opuse, opord, opmm getOrElse DefaultMinMaxPair, RequirementDefault, 1)
  }
  
  // parser for repeated group = "{" posInt valueItem* "}"
  val repGroup: Parser[GroupValue] = "{" ~> posInt ~ valueItem.* <~ "}" ^^ {
    case rep ~ items => GroupValue(rep, items)
  }
  
  // parser for either simple value or repeated group = simpleValue | repGroup
  val valueItem: Parser[ValueListItem] = simpleValue | repGroup
  
  // parser for segment definition = id "=" valueItem+ ("+" depNote+)? ("," segMask)*"\n"
  val segDef: Parser[SegmentDef] = (id <~ "=") ~ valueItem.+ ~ ("""\+""".r ~> depNote.+).? ~ ("," ~> segMask).* <~ separator ^^ {
    case id ~ items ~ opdeps ~ masks => SegmentDef(id, items, opdeps getOrElse Nil, masks)
  }
  
  // parser for .SEGS section segsSection := ".SEGS\n" segDef*
  val segsSection: Parser[Seq[SegmentDef]] = ".SEGS" ~> separator ~> segDef.*
  
  // definitions for composites

  case class CompositeSubElement(id: Int, ordinal: Int, mark: UsageNotationMark, minUsage: Int, maxUsage: Int, require: SegmentRequirement)

  // segments and composites are lists of values
  // values are either elements or composites
  // each value has ordinal, usage, repetition count, min and max length values
  // do we want to preserve the derivation of the values, or just work with the values?

  // at least for now, ignore masks with the intention of implementing them as separate versions of the segment or composite
  // make the segment or composite identifier a test string so that a suffix can be used for masks
}

// syntax rules at end of segment
// X12 form:
// A4=[329][206,M][207,M][128][127][186][373][86]+P0405
// paired syntax rule with P - if either A404 ([128]) or A405 ([127]) is present, then the other must be also
// R0708R0910 At least one of 07 and 08 is required, and at least one of 09 and 10 is required.
// C0102C030405 If 01 is present, then 02 is required. If 03 is present, then both 04 and 05 are required.
// E060810 Only one of 06, 08, or 10 may be present.
// L060810 If 06 is present, then at least one of 08 or 10 is required.
// P060708 If any of 06, 07, or 08 are present, then all are required.
// rules use position, not ordinal
// EDIFACT form:
// D1(010,020,050) Exactly one of the items at position 01, 02, and 05 must be present.
// D2(010,020) The items at 01 and 02 are both included or both excluded.
// D3(010,030,040) At least one of the items at 01, 03, and 04 must be included.
// D4(030,040,050) At most, one of the items at 03, 04, and 05 can be included, or all may be excluded.
// D5(030,040,050) If the first item (at position 03) is present, then all of the others (at 04 and 05) must be present.
//  However, if items at 04 and 05 are present, the first need not be present.
// D6(030,040,050) If the first item (at position 03) is present, then at least one of the others (at 04 and 05) must be
//  present. However, if the item at 04 and/or 05 is present, the first need not be present.
// D7(030,040,050) If the first item (at position 03) is present, then the items at 04 and 05 cannot be included.
// can combine rules:
// D3(010,020)D7(030,040,050)combines a D3 “one or more” with a D7 “if first, then none.”

// masks are referenced from the .SETS section definitions, but defined in the .SEGS section (and .COMS section)
// the definitions follow the syntax rules/notes in the segment definition, each mask started with a comma
// '.' character used for unchanged components, # for not used, @ for must be used
// $ recommended
// - not recommended
// & dependent
// + used
// *n composite mask
// elements omitted at the end of the mask are unused

// TODO: definitions for codes

// TODO: definitions for value refs and value lists

// TODO: definitions for object variables

// TODO: definitions for semantic rules