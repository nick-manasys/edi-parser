package com.anypoint.df.edi.schema.tools

import com.anypoint.df.edi.schema.{ EdiSchema, EdiSchemaVersion, YamlWriter }
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.schema.fftypes._
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import java.io.{ BufferedReader, File, FileInputStream, FileWriter, InputStreamReader, InputStream }
import scala.annotation.tailrec
import scala.collection.{ mutable => scm }
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{ Position, Reader }

case class CopybookImportError(error: Boolean, line: Int, message: String, fullText: String)

object DataDescriptionParser extends Parsers {

  type Elem = String

  var formatDetails: scm.Map[String, String] = null

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
  val descriptionParser = matchAnyParser ~> (("FILLER" ~> (redefinesParser) | clauseParser) | clauseParser |
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

  def parse(input: WordReader): (List[CopybookImportError], scm.Map[String, String]) = {
    formatDetails = scm.Map[String, String]()
    descriptionParser(input) match {
      case Success(value, remain) =>
        if (remain.atEnd) (Nil, formatDetails)
        else (List(new CopybookImportError(true, input.line, "Unknown input", remain.pos.longString)), formatDetails)
      case Error(msg, remain) => (List(new CopybookImportError(true, input.line, msg, remain.pos.longString)), formatDetails)
      case Failure(msg, remain) => (List(new CopybookImportError(true, input.line, msg, remain.pos.longString)), formatDetails)
    }
  }

  def apply(line: Int, words: Array[String]): (List[CopybookImportError], scm.Map[String, String]) = {
    parse(new WordReader(line, 1, words))
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

class CopybookImport(in: InputStream, enc: String) {

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
        convrunr(rest.tail, count + reps.toString.toInt - 1)
      } else (count, rem)

    convrunr(pattern, count)
  }

  def genKey(base: String, position: Int) =
    if (position < 10) base + "0" + position
    else base + position

  def fillMode(map: scm.Map[String, String]): FillMode = {
    map.get(DataDescriptionParser.justifiedKey) match {
      case Some(_) => FillMode.RIGHT
      case None => FillMode.LEFT
    }
  }

  def numberSign(map: scm.Map[String, String]): NumberSign = {
    map.get(DataDescriptionParser.signKey) match {
      case Some("LEADING") => NumberSign.NEGATIVE_ONLY
      case _ => NumberSign.ALWAYS_RIGHT
    }
  }

  def convertPic(name: String, pic: String, packed: Boolean, signed: Boolean, map: scm.Map[String, String]): Element = {

    val mode = fillMode(map)
    val zoned = map.contains(DataDescriptionParser.separateKey)

    def convertImplicitDisplay(name: String, pic: Seq[Char], lead: Int, sign: NumberSign): Element = {
      val (fract, rem) = convertReps('9', pic, 0)
      val length = lead + fract
      if (rem.isEmpty) Element("", name, DecimalFormat(length, sign, fract, mode, zoned))
      else dataError("Invalid expression in PIC clause")
    }

    def convertImplicitPacked(name: String, pic: Seq[Char], lead: Int): Element = {
      val (fract, rem) = convertReps('9', pic, 0)
      val length = lead + fract
      if (rem.isEmpty) Element("", name, PackedDecimalFormat(length, fract, signed))
      else dataError("Invalid expression in PIC clause")
    }
    pic.head match {
      case 'X' =>
        val (lead, rem) = convertReps('X', pic.tail, 1)
        val format = StringFormat(lead, mode)
        Element("", name, format)
      case 'S' =>
        convertPic(name, pic.tail, packed, true, map)
      case '9' =>
        val (lead, rem) = convertReps('9', pic.tail, 1)
        val sign = numberSign(map)
        if (rem.isEmpty) {
          val format = IntegerFormat(lead, sign, mode, zoned)
          Element("", name, format)
        } else if (rem.head == 'V') {
          if (packed) convertImplicitPacked(name, rem.tail, lead)
          else convertImplicitDisplay(name, rem.tail, lead, sign)
        } else dataError("Invalid expression in PIC clause")
      case 'V' =>
        if (packed) convertImplicitPacked(name, pic.tail, 0)
        else convertImplicitDisplay(name, pic.tail, 0, numberSign(map))
      case _ => dataError("Invalid expression in PIC clause")
    }
  }

  def buildSegment: Option[Segment] = {

    def buildNested(name: String, rootKey: String, position: Int, count: Int, level: String) = {
      val nested =
        if (count == 1) buildr(level, rootKey, position + 1, Nil)
        else buildr(level, name, 1, Nil)
      val comp = Composite("", name, nested, Nil, 0)
      CompositeComponent(comp, Some(name), genKey(rootKey, position), position, MandatoryUsage, count)
    }

    def definitionText(words: Array[String]) = {
      val builder = new StringBuilder
      words.foreach { w =>
        if (builder.length > 0) builder.append(' ')
        builder.append(w)
      }
      builder.toString
    }

    def buildr(level: String, key: String, position: Int, acc: List[SegmentComponent]): List[SegmentComponent] = {
      def count(map: scm.Map[String, String]) = {
        map.get(DataDescriptionParser.occursToKey) match {
          case Some(v) => v.toInt
          case _ => map.get(DataDescriptionParser.occursKey) match {
            case Some(v) => v.toInt
            case _ => 1
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
                val nested = buildNested(definition(1), key, position, count(map), definition(0))
                buildr(level, key, position + 1, nested :: acc)
              } else if (map.isDefinedAt(DataDescriptionParser.redefinesKey)) {
                problems += CopybookImportError(false, input.lineNumber, "Ignoring unsupported REDEFINE", "")
                buildr(level, key, position, Nil)
                buildr(level, key, position, acc)
              } else {
                map.get(DataDescriptionParser.nameKey) match {
                  case Some(name) =>
                    map.get(DataDescriptionParser.pictureKey) match {
                      case Some(picture) =>
                        map.get(DataDescriptionParser.usageKey) match {
                          case Some("DISPLAY") | None =>
                            val element = convertPic(name, picture, false, false, map)
                            val comp = ElementComponent(element, Some(name), genKey(key, position), position, MandatoryUsage, 1)
                            buildr(level, key, position + 1, comp :: acc)
                          case Some("COMP-3") | Some("COMPUTATIONAL-3") | Some("PACKED-DECIMAL") =>
                            val element = convertPic(name, picture, true, false, map)
                            val comp = ElementComponent(element, Some(name), genKey(key, position), position, MandatoryUsage, 1)
                            buildr(level, key, position + 1, comp :: acc)
                          case Some(other) =>
                            problems += new CopybookImportError(true, input.lineNumber, s"Unsupported USAGE $other", definitionText(definition))
                            buildr(level, key, position + 1, acc)
                        }
                      case _ =>
                        val nested = buildNested(definition(1), key, position, count(map), definition(0))
                        buildr(level, key, position + 1, nested :: acc)
                    }
                  case None =>
                    // TODO: add unused definition
                    buildr(level, key, position + 1, acc)
                }
              }
            } else buildr(level, key, position + 1, acc)
          } catch {
            case e: IllegalArgumentException => buildr(level, key, position, acc)
          }
        }
      } else acc.reverse
    }

    var position = 0
    val recdef = input.next
    if (recdef.size != 2 || recdef.head != "01") {
      problems += CopybookImportError(true, input.lineNumber, "Missing expected record definition line", "")
    }
    val segname = recdef(1)
    val abbrev = abbreviateName(segname)
    val ident = generateIdent(abbrev, segmentNameCounts)
    val comps = buildr("01", abbrev, 1, Nil)
    if (comps.isEmpty) None
    else Some(Segment(ident, ident, segname, comps, Nil))
  }

  def buildSchema: (Option[EdiSchema], List[CopybookImportError]) = {
    while (input.hasNext) {
      buildSegment.foreach { segments += _ }
    }
    if (segments.isEmpty) (None, problems.toList)
    else {
      val segs = segments.map { s => (s.ident, s) }.toMap
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