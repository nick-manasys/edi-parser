package com.mulesoft.flatfile.schema.tools

import com.mulesoft.flatfile.schema.{ EdiSchema, EdiSchemaVersion, YamlWriter }
import com.mulesoft.flatfile.schema.EdiSchema._
import com.mulesoft.flatfile.schema.fftypes._
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import java.io.{ BufferedReader, File, FileInputStream, FileWriter, InputStreamReader, InputStream }
import scala.annotation.tailrec
import scala.collection.{ immutable => sci, mutable => scm }
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{ Position, Reader }

case class CopybookImportError(error: Boolean, line: Int, message: String, fullText: String)

object DataDescriptionParser extends Parsers {

  type Elem = String

  type StringMap = scm.Map[String, String]

  var formatDetails: StringMap = null

  def matchAnyParser = new Parser[Elem] {
    def apply(in: Input) = {
      if (in.atEnd) Failure("No input available", in)
      else Success(in.first, in.rest)
    }
  }

  def propertyParser(key: String, base: Parser[Elem]): Parser[Elem] = new Parser[Elem] {
    def apply(in: Input) = {
      val result = base(in)
      result match {
        case Success(value, _) => {
          if (formatDetails.isDefinedAt(key)) Error(s"Duplicate $key clause", in)
          else {
            formatDetails += key -> value
            result
          }
        }
        case x => x
      }
    }
  }

  // keys for properties in map
  val blankKey = "BLANK"
  val dateKey = "DATE"
  val justifiedKey = "JUSTIFIED"
  val nameKey = "NAME"
  val occursKey = "OCCURS"
  val occursToKey = "OCCURS-TO"
  val occursDependKey = "OCCURS-DEPEND"
  val pictureKey = "PICTURE"
  val redefinesKey = "REDEFINES"
  val separateKey = "SEPARATE"
  val signKey = "SIGN"
  val usageKey = "USAGE"
  val valueKey = "VALUE"

  // level number is always the first component of a data description, and followed by either a data-name, FILLER, or
  //  other clauses; if no data-name is present FILLER is implicit (so first match productions, then if failure assume
  //  we have a data-name)
  // level 66 is always a RENAMES clause and must immediately follow the last data description in that record
  // level 77 is working storage, so should not be present
  // level 88 is always a condition
  val blankBaseParser = "BLANK" <~ "WHEN".? <~ ("ZERO" | "ZEROS" | "ZEROES")
  val blankParser = propertyParser(blankKey, blankBaseParser)
  val dateBaseParser = "DATE" ~> "FORMAT" ~> "IS".? ~> matchAnyParser
  val dateParser = propertyParser(dateKey, dateBaseParser)
  val externalParser = propertyParser("EXTERNAL", "EXTERNAL")
  val globalParser = propertyParser("GLOBAL", "GLOBAL")
  val justifyBaseParser = ("JUSTIFIED" | "JUST") <~ "RIGHT".?
  val justifyParser = propertyParser(justifiedKey, justifyBaseParser)
  val occursDependsParser = propertyParser(occursDependKey, "DEPENDING" ~> "ON".? ~> matchAnyParser)
  val occursParser = propertyParser(occursKey, "OCCURS" ~> matchAnyParser <~ "TIMES".? <~ occursDependsParser.?) ~>
    propertyParser(occursToKey, "TO" ~> matchAnyParser ~> "TIMES".? ~> occursDependsParser).? ~>
    (("ASCENDING" | "DESCENDING") ~> "KEY".? ~> "IS".? ~> matchAnyParser).? ~>
    ("INDEXED" ~> "BY".? ~> matchAnyParser).?
  val pictureBaseParser = ("PIC" | "PICTURE") ~> "IS".? ~> matchAnyParser
  val pictureParser = propertyParser(pictureKey, pictureBaseParser)
  val signStartParser = ("SIGN" ~> "IS".?).? ~> ("LEADING" | "TRAILING")
  val signEndParser = "SEPARATE" <~ "CHARACTER".?
  val signParser = propertyParser(signKey, signStartParser) ~! propertyParser(separateKey, signEndParser).?
  val usageBaseParser = "USAGE".? ~> "IS".? ~>
    (("BINARY" | "COMP-1" | "COMPUTATIONAL-1" | "COMP-2" | "COMPUTATIONAL-2" | "COMP-4" | "COMPUTATIONAL-4" |
      "DISPLAY" | "DISPLAY-1") <~ "NATIVE".? | "COMP" | "COMPUTATIONAL" | "COMP-3" | "COMPUTATIONAL-3" | "INDEX" |
      "PACKED-DECIMAL")
  val usageParser = propertyParser(usageKey, usageBaseParser)
  val valueParser = propertyParser(valueKey, ("VALUE" ~> "IS".? ~> matchAnyParser) |
    ("VALUES" ~> "ARE".? ~> matchAnyParser <~ (("THROUGH" | "THRU") ~> matchAnyParser).?))
  val clauseParser = blankParser | dateParser | globalParser | justifyParser | occursParser | pictureParser | signParser |
    usageParser | valueParser
  val redefinesParser = propertyParser(redefinesKey, "REDEFINES" ~> matchAnyParser)
  val descriptionParser = matchAnyParser ~> (("FILLER" ~> (redefinesParser).? | clauseParser) | clauseParser |
    propertyParser(nameKey, matchAnyParser <~ redefinesParser) | propertyParser(nameKey, matchAnyParser)) ~
    clauseParser.*

  class WordReader(lineNumber: Int, wordNumber: Int, words: Array[String]) extends Reader[String] with Position {
    def atEnd: Boolean = wordNumber > words.length
    def first: String = words(wordNumber - 1)
    def pos: Position = this
    def column = wordNumber
    def line = lineNumber
    def lineContents = {
      words.foldLeft(new StringBuilder)((acc, w) => acc.append(w).append(' ')).toString
    }
    override def longString = {
      val position = words.take(wordNumber - 1).foldLeft(-1)((acc, w) => acc + w.length + 1)
      val marker = new java.lang.StringBuilder
      (0 to position).foreach { _ => marker.append(' ') }
      lineContents + '\n' + marker.append('^').toString
    }
    def rest: Reader[String] = new WordReader(lineNumber, wordNumber + 1, words)
  }

  private def describe(input: Input) = {
    if (input.atEnd) "empty"
    else input.first
  }

  def parse(input: WordReader, init: StringMap): (List[CopybookImportError], StringMap) = {
    formatDetails = init
    descriptionParser(input) match {
      case Success(value, remain) =>
        if (remain.atEnd) (Nil, formatDetails)
        else (List(new CopybookImportError(true, input.line, "Unknown input", remain.pos.longString)), formatDetails)
      case Error(msg, remain)   => (List(new CopybookImportError(true, input.line, msg, remain.pos.longString)), formatDetails)
      case Failure(msg, remain) => (List(new CopybookImportError(true, input.line, msg, remain.pos.longString)), formatDetails)
    }
  }

  def apply(line: Int, words: Array[String], init: StringMap): (List[CopybookImportError], StringMap) = {
    parse(new WordReader(line, 1, words), init)
  }

  def apply(line: Int, words: Array[String]): (List[CopybookImportError], StringMap) = {
    apply(line, words, scm.Map[String, String]())
  }

  //  def test(words: Array[String]): Unit = {
  //    println(s"\nParsing ${words.toList}")
  //    parse(new WordReader(1, 1, words)) match {
  //      case (Nil, map) => println("Successful parse")
  //      case (l, _) => l.foreach { e => println(s"Parse problem (${e.error}) at line ${e.line}: ${e.message}\n${e.fullText}") }
  //    }
  //    println(s"Map is $formatDetails")
  //  }
  //
  //  // simple base parsers
  //  def main(args: Array[String]): Unit = {
  //    test(Array("JUST", "RIGHT", "GLOBAL", "BLANK", "ZERO", "END"))
  //    test(Array("FILLER", "JUST", "RIGHT", "GLOBAL", "BLANK", "ZERO"))
  //    test(Array("xyz", "JUST", "RIGHT", "GLOBAL", "BLANK", "ZERO"))
  //    test(Array("xyz", "JUST", "abc", "RIGHT", "GLOBAL", "BLANK", "ZERO"))
  //    test(Array("xyz", "JUST", "RIGHT", "abc", "GLOBAL", "BLANK", "ZERO"))
  //    test(Array("abc", "REDEFINES", "xyz", "JUST", "RIGHT", "GLOBAL", "BLANK", "ZERO"))
  //    test(Array("abc", "JUST", "RIGHT", "OCCURS", "25", "TIMES", "BLANK", "ZERO", "USAGE", "IS", "COMP-3"))
  //    test(Array("abc", "JUST", "RIGHT", "BLANK", "ZERO", "USAGE", "IS", "COMP"))
  //    test(Array("abc", "JUST", "RIGHT", "OCCURS", "25", "USAGE", "IS", "COMP"))
  //  }
}

sealed abstract class UsageFormat
object DisplayUsage extends UsageFormat
object PackedDecimalUsage extends UsageFormat
object BinaryUsage extends UsageFormat

class CopybookImport(in: InputStream, enc: String) {

  import DataDescriptionParser.StringMap

  /** Iterator access to lines of input as lists of whitespace-delimited uppercase tokens. */
  class LineIterator(is: InputStream, enc: String) extends Iterator[Array[String]] {
    private val buffer = new BufferedReader(new InputStreamReader(is, enc))

    private val warnings: scm.Buffer[CopybookImportError] = new scm.ArrayBuffer
    private var lineCount = 0
    private var peekLine = 0
    private var startLine = 0
    private var nextTokens: Array[String] = Array()

    @tailrec
    private def accumr(lead: String): Array[String] = {
      val line = buffer.readLine
      if (line == null) Array()
      else {
        lineCount += 1
        if (line.size < 7 || line(6) == '*') accumr(lead)
        else {
          val usetext = line.slice(7, 72).trim
          if (usetext.length > 0) {
            if (lead.isEmpty) peekLine = lineCount
            val fulltext = lead + line.slice(7, 72).trim
            val split = fulltext.indexOf('.')
            if (split >= 0) fulltext.substring(0, split).replace(',', ' ').replace(';', ' ').toUpperCase.split(" +")
            else accumr(fulltext + ' ')
          } else accumr(lead)
        }
      }
    }

    @tailrec
    private def filteredDef: Array[String] = {
      val tokens = accumr("")
      if (tokens.size > 0) {
        val level = tokens(0)
        if (level != "66" && level != "88") tokens
        else {
          println(s"Warning: ignoring unsupported level at $lineCount")
          filteredDef
        }
      } else tokens
    }

    /** Get the next item definition as an array of tokens. This discards leading and trailing texn each line of input,
      * and also discards any unused definitions (levels 66 and 88).
      */
    def next: Array[String] = {
      if (nextTokens.isEmpty) throw new IllegalStateException("Past end of input")
      else {
        val result = nextTokens
        startLine = peekLine
        nextTokens = filteredDef
        result
      }
    }

    def hasNext = nextTokens.nonEmpty

    def peek = nextTokens

    def lineNumber = startLine

    def lineWarnings = warnings.toList

    nextTokens = filteredDef
  }

  val input = new LineIterator(in, enc)

  val compositeNameCounts = new scm.HashMap[String, Int]
  val segmentNameCounts = new scm.HashMap[String, Int]

  val segments = scm.Buffer[Segment]()
  val problems = scm.Buffer[CopybookImportError]()

  var elements = new scm.HashMap[String, Set[Element]]

  private def dataError(msg: String) = {
    problems += CopybookImportError(true, input.lineNumber, msg, "")
    throw new IllegalArgumentException(msg)
  }

  def abbreviateName(name: String) = {
    val builder = new StringBuilder
    @tailrec
    def abbrevr(chars: String): Unit = chars.indexOf('-') match {
      case -1 =>
      case n =>
        if (chars.length > n + 1) {
          val char = chars(n + 1)
          if (char.isLetterOrDigit) {
            builder += char
            abbrevr(chars.substring(n + 2))
          } else dataError("Hyphen character in name must be followed by an alphanumeric")
        } else dataError("Hyphen character in name must be followed by an alphanumeric")
    }
    if (name.isEmpty) dataError("Empty name string")
    else if (!name(0).isLetterOrDigit) dataError("Invalid start of name")
    else {
      builder += name.head
      abbrevr(name.tail)
      builder.toString
    }
  }

  def generateIdent(prefix: String, counts: scm.Map[String, Int]) = {
    val prior = counts.getOrElse(prefix, 0)
    counts.put(prefix, prior + 1)
    prefix + prior
  }

  def convertReps(picchar: Char, pattern: Seq[Char], count: Int): (Int, Seq[Char]) = {
    @tailrec
    def convrunr(rem: Seq[Char], count: Int): (Int, Seq[Char]) =
      if (rem.isEmpty) (count, rem)
      else if (rem.head == picchar) convrunr(rem.tail, count + 1)
      else if (rem.head == '(') {
        val (reps, rest) = rem.tail.span { _ != ')' }
        if (rest.isEmpty) dataError("Invalid expression in PIC clause")
        convrunr(rest.tail, count + reps.mkString.toInt - 1)
      } else (count, rem)

    convrunr(pattern, count)
  }

  def fillMode(map: StringMap): FillMode = {
    map.get(DataDescriptionParser.justifiedKey) match {
      case Some(_) => FillMode.RIGHT
      case None    => FillMode.LEFT
    }
  }

  def numberSign(signed: Boolean, map: StringMap): NumberSign = {
    if (signed) map.get(DataDescriptionParser.signKey) match {
      case Some("LEADING") => NumberSign.ALWAYS_LEFT
      case _               => NumberSign.ALWAYS_RIGHT
    }
    else NumberSign.UNSIGNED
  }

  def convertPic(name: String, pic: String, form: UsageFormat, signed: Boolean, map: StringMap): Element = {

    val mode = fillMode(map)
    val zoned = !(map.contains(DataDescriptionParser.separateKey))

    def convertImplicitDisplay(pic: Seq[Char], lead: Int, sign: NumberSign): Element = {
      val (fract, rem) = convertReps('9', pic, 0)
      val signSize = if (zoned) 0 else 1
      val length = lead + fract + signSize
      if (rem.nonEmpty) dataError("Invalid expression in PIC clause")
      else {
        val format =
          if (fract == 0) IntegerFormat(length, sign, mode, zoned && signed)
          else DecimalFormat(length, sign, fract, mode, zoned && signed)
        Element("", name, format)
      }
    }

    def convertImplicitPacked(pic: Seq[Char], lead: Int): Element = {
      val (fract, rem) = convertReps('9', pic, 0)
      val length = lead + fract
      if (rem.isEmpty) Element("", name, PackedDecimalFormat(length, fract, signed))
      else dataError("Invalid expression in PIC clause")
    }

    def convertImplicitBinary(pic: Seq[Char], lead: Int): Element = {
      val (fract, rem) = convertReps('9', pic, 0)
      val digits = lead + fract
      val width =
        if (digits < 5) 2
        else if (digits < 10) 4
        else if (digits < 19) 8
        else dataError("Too many digits for BINARY usage")
      if (rem.isEmpty) Element("", name, BinaryFormat(width, digits, fract, signed))
      else dataError("Invalid expression in PIC clause")
    }

    pic.head match {
      case 'X' =>
        val (lead, rem) = convertReps('X', pic.tail, 1)
        val format = StringFormat(lead, mode)
        Element("", name, format)
      case 'S' =>
        convertPic(name, pic.tail, form, true, map)
      case '9' =>
        val (lead, rem) = convertReps('9', pic.tail, 1)
        val sign = numberSign(signed, map)
        rem.toList match {
          case 'V' :: t =>
            form match {
              case DisplayUsage       => convertImplicitDisplay(t, lead, sign)
              case PackedDecimalUsage => convertImplicitPacked(t, lead)
              case BinaryUsage        => convertImplicitBinary(t, lead)
            }
          case Nil =>
            form match {
              case DisplayUsage       => convertImplicitDisplay(Nil, lead, sign)
              case PackedDecimalUsage => convertImplicitPacked(Nil, lead)
              case BinaryUsage        => convertImplicitBinary(Nil, lead)
            }
          case _ =>
            dataError("Invalid expression in PIC clause")
        }
      case 'V' =>
        val tail = pic.tail
        form match {
          case DisplayUsage       => convertImplicitDisplay(tail, 0, numberSign(signed, map))
          case PackedDecimalUsage => convertImplicitPacked(tail, 0)
          case BinaryUsage        => convertImplicitBinary(tail, 0)
        }
      case _ => dataError("Invalid expression in PIC clause")
    }
  }

  def buildSegment: Option[Segment] = {

    var fillCount = 0

    /** Build composite from nested field definitions at level. */
    def buildCompositeComp(name: String, count: Int, level: String): CompositeComponent = {
      val nested = buildNested(level)
      val comp = Composite("", name, nested, Nil, 0)
      CompositeComponent(comp, None, name, -1, OptionalUsage, count)
    }

    /** Build list of components from nested field definitions at level. */
    def buildNested(level: String): List[SegmentComponent] = {

      /** Recursively build definitions at level, generating the list of segment components for a composite or segment. */
      def buildr(acc: List[SegmentComponent]): List[SegmentComponent] = {
        def count(map: StringMap) = {
          map.get(DataDescriptionParser.occursToKey) match {
            case Some(v) => v.toInt
            case _ => map.get(DataDescriptionParser.occursKey) match {
              case Some(v) => v.toInt
              case _       => 1
            }
          }
        }

        if (input.hasNext) {
          val nextLevel = input.peek(0)
          if (nextLevel <= level) acc.reverse
          else {
            try {
              val definition = input.next
              val (problist, map) = DataDescriptionParser(input.lineNumber, definition)
              problems ++= problist
              if (problist.forall { !_.error }) {
                if (definition.size == 2) {
                  val nested = buildCompositeComp(definition(1), count(map), definition(0))
                  buildr(nested :: acc)
                } else if (map.isDefinedAt(DataDescriptionParser.redefinesKey)) {
                  problems += CopybookImportError(false, input.lineNumber, "Ignoring unsupported REDEFINE", "")
                  buildNested(definition(0))
                  buildr(acc)
                } else {
                  val (name, usage) =
                    if (map.contains(DataDescriptionParser.nameKey)) {
                      (map(DataDescriptionParser.nameKey), OptionalUsage)
                    } else {
                      fillCount += 1
                      ("FILLER" + fillCount, UnusedUsage)
                    }
                  map.get(DataDescriptionParser.pictureKey) match {
                    case Some(picture) =>

                      def buildElementComp(form: UsageFormat) = {
                        val element = convertPic(name, picture, form, false, map)
                        val value = map.get(DataDescriptionParser.valueKey) map { v =>
                          val first = v(0)
                          val end = v.length - 1
                          if (end > 0 && first == v(end) && (first == '\'' || first == '"')) v.substring(1, end)
                          else v
                      }
                        ElementComponent(element, None, name, -1, usage, count(map), false, value)
                      }

                      map.get(DataDescriptionParser.usageKey) match {
                        case Some("DISPLAY") | None =>
                          buildr(buildElementComp(DisplayUsage) :: acc)
                        case Some("COMP-3") | Some("COMPUTATIONAL-3") | Some("PACKED-DECIMAL") =>
                          buildr(buildElementComp(PackedDecimalUsage) :: acc)
                        case Some("COMP") | Some("COMPUTATIONAL") | Some("BINARY") =>
                          buildr(buildElementComp(BinaryUsage) :: acc)
                        case Some(other) =>
                          problems += new CopybookImportError(true, input.lineNumber, s"Unsupported USAGE $other", definitionText(definition))
                          buildr(acc)
                      }
                    case _ =>
                      val nested = buildCompositeComp(definition(1), count(map), definition(0))
                      buildr(nested :: acc)
                  }
                }
              } else buildr(acc)
            } catch {
              case e: IllegalArgumentException => buildr(acc)
            }
          }
        } else acc.reverse
      }

      buildr(Nil)
    }

    /** Get definition line text as a single line (comments and such stripped). */
    def definitionText(words: Array[String]) = {
      val builder = new StringBuilder
      words.foreach { w =>
        if (builder.length > 0) builder.append(' ')
        builder.append(w)
      }
      builder.toString
    }

    var position = 0
    val recdef = input.next
    if (recdef.size != 2 || recdef.head != "01") {
      problems += CopybookImportError(true, input.lineNumber, "Missing expected record definition line", "")
    }
    val ident = recdef(1)
    val comps = buildNested("01")
    if (comps.isEmpty) None
    else Some(Segment(ident, "", comps, Nil))
  }

  def buildSchema: (Option[EdiSchema], List[CopybookImportError]) = {
    while (input.hasNext) {
      buildSegment.foreach { segments += _ }
    }
    if (segments.isEmpty) (None, problems.toList)
    else {
      val segs = segments.foldLeft(sci.ListMap[String, Segment]()) { (acc, s) => acc + (s.ident -> s) }
      val schema = new EdiSchema(EdiSchemaVersion(Copybook, ""), Map.empty, Map.empty, segs, Map.empty)
      (Some(schema), problems.toList)
    }
  }
}

object CopybookImport {

  /** Reads copybook and schema paths, then generates the schema from the copybook and writes it.
    */
  def main(args: Array[String]): Unit = {
    val infile = new File(args(0))
    val (optschema, problems) = new CopybookImport(new FileInputStream(infile), "UTF-8").buildSchema
    problems.foreach { x => println(s"${x.message} for definition starting at line ${x.line}") }
    optschema match {
      case Some(s) =>
        val writer = new FileWriter(args(1))
        YamlWriter.write(s, Array[String](), writer)
        writer.close
        println("wrote output schema")
      case None =>
        println("failed to generate schema")
    }
  }
}