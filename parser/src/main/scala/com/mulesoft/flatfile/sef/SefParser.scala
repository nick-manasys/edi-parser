package com.mulesoft.flatfile.sef

import scala.annotation.tailrec
import scala.util._
import scala.util.parsing.combinator._
import scala.util.parsing.input.Reader

object MessageParser extends RegexParsers {

  import com.mulesoft.flatfile.schema.EdiSchema._

  sealed trait Section

  private val separator = """\z|\n""".r
  override val skipWhitespace = false

  sealed trait MaximumUsage
  case class FiniteUsage(val number: Int) extends MaximumUsage
  case object UnlimitedUsage extends MaximumUsage
  val UsageDefault = FiniteUsage(1)

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

  // definitions for element types (replaced in EdiSchema with JavaBean-style)
  abstract class ElementType(val lead: Char, val rest: Int)
  case object RealType extends ElementType('R', -1) // digits with explicit decimal point (if anywhere except rightmost)
  case object IdType extends ElementType('I', 'D')  // unique value from predefined list (X12 only?)
  case object AlphaNumericType extends ElementType('A', 'N') // alphanumerics and spaces, leading spaces preserved
  case object AlphaType extends ElementType('A', -1)    // alphas and spaces, leading spaces preserved
  case object DateType extends ElementType('D', 'T')    // date in YYMMDD form
  case object TimeType extends ElementType('T', 'M')    // time in HHMMSSd..d form (seconds and decimal seconds optional)
  case object BinaryType extends ElementType('B', -1)   // binary octets
  case object NumberType extends ElementType('N', -1)   // digits, optional decimal, optional minus (decimal requires leading/training digit(s))
  case object IntegerType extends ElementType('N', '0') // integer digits, optional minus
  case class DecimalType(places: Int) extends ElementType('N', 0) // digits with implied decimal point, optional minus

  /** Every kind of item in a structure definition. */
  sealed trait StructureItem
  /** Position increment change. */
  case class PositionIncrement(val increment: Int) extends StructureItem
  /**
   * Actual component of a structure definition, either a segment reference or a loop/group definition.
   *  @param ident segment or loop/group identifier
   *  @param mark usage notation (optional, overrides requirement)
   *  @param require segment requirement (optional, defaults to C)
   *  @param ordinal optional override of sequencing
   *  @param usage maximum usage (optional, defaults to 1)
   */
  abstract class StructureComponent(val ident: String, val mark: Option[UsageNotationMark], val ordinal: Option[Int], val require: SegmentRequirement) extends StructureItem
  case class SegmentReference(id: String, mrk: Option[UsageNotationMark], ord: Option[Int], val mask: Int, req: SegmentRequirement, val usage: MaximumUsage) extends StructureComponent(id, mrk, ord, req)
  case class TransGroup(id: String, mrk: Option[UsageNotationMark], ord: Option[Int], req: SegmentRequirement, val repeat: Int, val items: Seq[StructureItem]) extends StructureComponent(id, mrk, ord, req)

  case class StructureTable(val comps: Seq[StructureItem])

  case class StructureSet(val ident: String, val tables: Seq[StructureTable])

  case class StructuresSection(val defs: Seq[StructureSet]) extends Section

  // Grammar for .SETS section
  //
  // setsSection := ".SETS\n" transSet*
  // transSet := id "=" table* "\n"
  // table := "^"+ transItem*
  // transItem := segRef | transGroup | increment
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
  // transGroup := ordNum? "{" groupLead? transItem* "}"
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
  def firstSegment(items: Seq[StructureItem]): SegmentReference = items match {
    case (segref: SegmentReference) :: _ => segref
    case (group: TransGroup) :: _ => firstSegment(group.items)
    case _ :: tail => firstSegment(tail)
    case Nil => throw new IllegalStateException("No segment definition in item list")
  }
  val transGroup: Parser[TransGroup] = ordNum.? ~ ("{" ~> groupLead.? ~ transItem.+ <~ "}") ^^ {
    case opord ~ (Some((opid, rep)) ~ items) => {
      val first = firstSegment(items)
      TransGroup(opid getOrElse first.id, first.mark, None, first.require, rep, items)
    }
    case opord ~ (None ~ items) => {
      val first = firstSegment(items)
      TransGroup(first.id, first.mark, None, first.require, 1, items)
    }
  }

  // parser for position increment = ("+" | "-") posInt
  val increment: Parser[PositionIncrement] = """\+|\-""".r ~ posInt ^^ {
    case "+" ~ num => PositionIncrement(num)
    case "-" ~ num => PositionIncrement(-num)
  }

  // parser for structure item = segRef | transGroup | increment
  val transItem: Parser[StructureItem] = segRef | transGroup | increment

  // parser for table = "^"+ transItem*
  val table: Parser[StructureTable] = ("""\^""".r+) ~> transItem.+ ^^ {
    case list => StructureTable(list)
  }

  // parser for structure set = id "=" table* "\n"
  val transSet: Parser[StructureSet] = (id <~ "=") ~ table.+ <~ separator ^^ {
    case id ~ tables => StructureSet(id, tables)
  }

  // parser for .SETS section = ".SETS\n" transSet*
  val setsSection: Parser[StructuresSection] = ".SETS" ~> separator ~> transSet.* ^^ {
    case sets => StructuresSection(sets)
  }

  //
  // Definitions for .SEGS and .COMS sections
  sealed trait ValueListItem
  case class GroupValue(val repeatCount: Int, val items: Seq[ValueListItem]) extends ValueListItem
  case class BaseValue(val ident: String, val mark: Option[UsageNotationMark], val ordinal: Option[Int], val lengths: MinMaxPair, val usage: SegmentRequirement, val repeat: Int) extends ValueListItem

  case class SegmentDef(val ident: String, val values: Seq[ValueListItem], val rules: Seq[DependencyNote], val masks: Seq[Mask])
  case class CompositeDef(val ident: String, val values: Seq[ValueListItem], val rules: Seq[DependencyNote], val masks: Seq[Mask])

  case class SegmentsSection(val defs: Seq[SegmentDef]) extends Section
  case class CompositesSection(val defs: Seq[CompositeDef]) extends Section

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
  // segDef := id "=" segItem+ ("+" depNote+)? ("," maskDef)*"\n"
  // segItem := segSimple | segGroup
  // segSimple := "[" useMark? id ordNum? (";" minMax) ("," stdReq? posInt?)? "]"
  // minMax := posInt? (":" posInt?)?
  // segGroup := "{" posInt segItem* "}"
  // depNote := "D" depType depList
  // depList := "(" posInt ("," posInt)* ")"
  // depType := "1" | "2" | "3" | "4" | "5" | "6" | "7"
  // maskItem := (useMask maskMod?)*
  // maskDef := maskItem*
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
  val maskItem: Parser[MaskItem] = useMask ~ maskMod ^^ {
    case use ~ mod => MaskItem(use, mod)
  }

  // parser for segment mask = maskItem*
  val maskDef: Parser[Mask] = maskItem.* ^^ {
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

  // parser for simple value = "[" useMark? id ordNum? (";" minMax) ("," stdReq? ("," posInt?)?)? "]"
  val segSimple: Parser[BaseValue] = "[" ~> useMark.? ~ id ~ ordNum.? ~ (";" ~> minMax).? ~ ("," ~> stdReq.? ~ ("," ~> posInt.?).?).? <~ "]" ^^ {
    case opuse ~ id ~ opord ~ opmm ~ Some(opreq ~ Some(opmax)) => BaseValue(id, opuse, opord, opmm getOrElse DefaultMinMaxPair, opreq getOrElse RequirementDefault, opmax getOrElse 1)
    case opuse ~ id ~ opord ~ opmm ~ Some(opreq ~ None) => BaseValue(id, opuse, opord, opmm getOrElse DefaultMinMaxPair, opreq getOrElse RequirementDefault, 1)
    case opuse ~ id ~ opord ~ opmm ~ None => BaseValue(id, opuse, opord, opmm getOrElse DefaultMinMaxPair, RequirementDefault, 1)
  }

  // parser for repeated group = "{" posInt segItem* "}"
  val segGroup: Parser[GroupValue] = "{" ~> posInt ~ segItem.* <~ "}" ^^ {
    case rep ~ items => GroupValue(rep, items)
  }

  // parser for either simple value or repeated group = segSimple | segGroup
  val segItem: Parser[ValueListItem] = segSimple | segGroup

  // parser for segment definition = id "=" segItem+ ("+" depNote+)? ("," maskDef)*"\n"
  val segDef: Parser[SegmentDef] = (id <~ "=") ~ segItem.+ ~ ("""\+""".r ~> depNote.+).? ~ ("," ~> maskDef).* <~ separator ^^ {
    case id ~ items ~ opdeps ~ masks => SegmentDef(id, items, opdeps getOrElse Nil, masks)
  }

  // parser for .SEGS section = ".SEGS\n" segDef*
  val segsSection: Parser[SegmentsSection] = ".SEGS" ~> separator ~> segDef.* ^^ {
    case defs => SegmentsSection(defs)
  }

  // Grammar for .COMS section
  //
  // Note that the only difference between composite definitions and segment definitions is that the former do not
  // include element repeat counts.
  //
  // comsSection := ".COMS\n" comDef*
  // comDef := id "=" compItem+ ("+" depNote+)? ("," maskDef)*"\n"
  // comItem := compSimple | comGroup
  // comSimple := "[" useMark? id ordNum? (";" minMax) ("," stdReq?)? "]"
  // comGroup := "{" posInt comItem* "}"

  // parser for simple value = "[" useMark? id ordNum? (";" minMax) ("," stdReq?)? "]"
  val comSimple: Parser[BaseValue] = "[" ~> useMark.? ~ id ~ ordNum.? ~ (";" ~> minMax).? ~ ("," ~> stdReq.?).? <~ "]" ^^ {
    case opuse ~ id ~ opord ~ opmm ~ Some(opreq) => BaseValue(id, opuse, opord, opmm getOrElse DefaultMinMaxPair, opreq getOrElse RequirementDefault, 1)
    case opuse ~ id ~ opord ~ opmm ~ None => BaseValue(id, opuse, opord, opmm getOrElse DefaultMinMaxPair, RequirementDefault, 1)
  }

  // parser for repeated group = "{" posInt comItem* "}"
  val comGroup: Parser[GroupValue] = "{" ~> posInt ~ comItem.* <~ "}" ^^ {
    case rep ~ items => GroupValue(rep, items)
  }

  // parser for either simple value or repeated group = comSimple | comGroup
  val comItem: Parser[ValueListItem] = comSimple | comGroup

  // parser for composite definition = id "=" comItem+ ("+" depNote+)? ("," maskDef)*"\n"
  val comDef: Parser[CompositeDef] = (id <~ "=") ~ comItem.+ ~ ("""\+""".r ~> depNote.+).? ~ ("," ~> maskDef).* <~ separator ^^ {
    case id ~ items ~ opdeps ~ masks => CompositeDef(id, items, opdeps getOrElse Nil, masks)
  }

  // parser for .COMS section = ".COMS\n" comDef*
  val comsSection: Parser[CompositesSection] = ".COMS" ~> separator ~> comDef.* ^^ {
    case defs => CompositesSection(defs)
  }

  //
  // Definitions for .ELMS section
  case class ElementDef(val id: String, val dataType: ElementType, val minLength: Int, val maxLength: Int)

  case class ElementsSection(val elements: Seq[ElementDef]) extends Section

  // Grammar for .ELMS section
  //
  // Note that the only difference between composite definitions and segment definitions is that the former do not
  // include element repeat counts.
  //
  // elmsSection := ".ELMS\n" elmDef*
  // elmDef := id "=" elmType "," posInt "," posInt "\n"
  // elmType := "R" | "ID" | "AN" | "A" | "DT" | "TM" | "B" | "N" | "N0" | "N[1-9]"

  // parser for element type = "R" | "ID" | "AN" | "A" | "DT" | "TM" | "B" | "N" | "N0" | "N[1-9]"
  val elmType = new Parser[ElementType] {
    def apply(input: Input): ParseResult[ElementType] = {
      def matchChars(lead: Char, rest: Int, remain: Input): ParseResult[ElementType] = (lead, rest) match {
        case (RealType.lead, RealType.rest) => Success(RealType, remain)
        case (IdType.lead, IdType.rest) => Success(IdType, remain)
        case (AlphaNumericType.lead, AlphaNumericType.rest) => Success(AlphaNumericType, remain)
        case (AlphaType.lead, AlphaType.rest) => Success(AlphaType, remain)
        case (DateType.lead, DateType.rest) => Success(DateType, remain)
        case (TimeType.lead, TimeType.rest) => Success(TimeType, remain)
        case (BinaryType.lead, BinaryType.rest) => Success(BinaryType, remain)
        case (NumberType.lead, NumberType.rest) => Success(NumberType, remain)
        case (IntegerType.lead, IntegerType.rest) => Success(IntegerType, remain)
        case ('N', digit) if (digit >= '1' && digit <= '9') => Success(DecimalType(digit - '0'), remain)
        case _ => Failure("No match", input)
      }
      def tryOne() = matchChars(input.first, -1, input.rest)
      def tryTwo() = {
        val two = matchChars(input.first, input.rest.first, input.rest.rest)
        if (two.successful) two
        else tryOne
      }
      if (input.atEnd) Failure("End of input", input)
      else if (!input.rest.atEnd) tryTwo
      else tryOne
    }
  }

  // parser for element definition = id "=" elmType "," posInt "," posInt "\n"
  val elmDef: Parser[ElementDef] = (id <~ "=") ~ (elmType <~ ",") ~ (posInt <~ ",") ~ (posInt <~ separator) ^^ {
    case id ~ typ ~ min ~ max => ElementDef(id, typ, min, max)
  }

  // parser for .ELMS section = ".ELMS\n" elmDef*
  val elmsSection: Parser[ElementsSection] = ".ELMS" ~> separator ~> elmDef.* ^^ {
    case defs => ElementsSection(defs)
  }

  //
  // Parsers for all the ignored sections
  case class IgnoredSection() extends Section

  val discardLine: Parser[Option[Any]] = """.*""" ~> separator ^^ {
    case _ => println("matched"); None
  }

  val ignoredSection: Parser[IgnoredSection] = """\.""" ~> discardLine.* ^^ {
    case _ => IgnoredSection()
  }

  //
  // Full SEF parser
  // TODO: look into optimizing parse so it commits when it matches a section and won't try backtracking
  val sefParser: Parser[Seq[Section]] = (setsSection | segsSection | comsSection | elmsSection | ignoredSection).*

  //
  // Convert parsed SEF data into EDI schema
  // work in progress, suspended for now
//  def parseSef(reader: Reader[Char]): Try[StructureMap] = {
//    def convert(sections: Seq[Section]): StructureMap = {
//
//      // first build maps of elements and composites
//      val elementMap = sections.foldLeft(Map[String, ElementDef]())((map, section) => section match {
//        case ElementsSection(elements) => elements.foldLeft(map)((map, element) => map + (element.id -> element))
//        case _ => map
//      })
//      val compositeMap = sections.foldLeft(Map[String, CompositeDef]())((map, section) => section match {
//        case CompositesSection(composites) => composites.foldLeft(map)((map, composite) => map + (composite.ident -> composite))
//        case _ => map
//      })
//      
//      case class CompositeKey(id: String, mask: Int)
//      
//      /***
//       * Accumulate composite schema definitions in map. Value items include both composites and repeated value lists,
//       * so this needs to recurse any time one of these is found. In the case of a composite, it also needs to add the
//       * actual composite definition to the map.
//       */
//      @tailrec
//      def accumulateComposites(items: Seq[ValueListItem], defs: Map[CompositeKey, CompositeValue]): Map[CompositeKey, CompositeValue] = {
//        items.foldLeft(defs)((defs, item) => item match {
//          case BaseValue(ident, mark, ordinal, lengths, usage, repeat) => {
//            if (elementMap.contains(ident) || defs.contains(CompositeKey(ident, 0))) defs
//            else {
//              
//              // must be a composite which hasn't been handled yet, first make sure all dependencies are defined
//              val comp = compositeMap(ident)
//              val newdefs = accumulateComposites(comp.values, defs)
//              
//              newdefs
//            }
//          }
//          case GroupValue(count, items) => accumulateComposites(items, defs)
//        })
//        
//        // 
//        defs.get(key) match {
//          case Some(value) => defs
//          case None => {
//            val compdef = compositeMap(key.id)
//            val newdefs = compdef.values.foldLeft(defs)((defs, value) => value match {
//              case BaseValue(ident, mark, ordinal, lengths, usage, repeat) => {
//                if (elementMap.contains(key.id) || defs.contains(key)) defs
//                else addSchema(key, defs)
//              }
//              case GroupValue(count, items) => items.foldLeft(defs)((defs, item) => item match {
//              }
//                  )
//            })
//          }
//        }
//
//      }
//
//      // convert composites to schema form
//      // note that this needs to handle maps, too, so each individual composite may result in several additions
//      // 
//      @tailrec
//      def addSchema(key: CompositeKey, defs: Map[CompositeKey, CompositeValue]): Map[CompositeKey, CompositeValue] = {
//        defs.get(key) match {
//          case Some(value) => defs
//          case None => {
//            val compdef = compositeMap(key.id)
//            val newdefs = compdef.values.foldLeft(defs)((defs, value) => value match {
//              case BaseValue(ident, mark, ordinal, lengths, usage, repeat) => {
//                if (elementMap.contains(key.id) || defs.contains(key)) defs
//                else addSchema(key, defs)
//              }
//              case GroupValue(count, items) => items.foldLeft(defs)((defs, item) => item match {
//              }
//                  )
//            })
//          }
//        }
//
//      }
//      compositeMap.keySet.foldLeft(Map[String, CompositeValue]())((map, comp) => comp match {
//        case BaseValue(ident, mark, ordinal, lengths, usage, repeat) =>
//        case GroupValue(count, items) =>
//      })
//
//      // directly convert segments to schema form
//      val segmentsMap = sections.foldLeft(Map[String, SegmentDef]())((map, section) => section match {
//        case SegmentsSection(segments) => segments.foldLeft(map)((map, segment) => map + (segment.id -> segment))
//        case _ => map
//      })
//
//      // convert composites
//      Map[String, Structure]()
//    }
//    val phraseParser = phrase(sefParser)
//    phraseParser(reader) match {
//      case Success(sections, _) => scala.util.Success(convert(sections))
//      case NoSuccess(msg, _) => scala.util.Failure(msg)
//    }
//  }
}