package com.anypoint.df.edi.schema.tools

import java.io.{ BufferedReader, File, FileInputStream, FileWriter, InputStreamReader, InputStream }
import scala.annotation.tailrec
import scala.collection.{ mutable => scm }
import com.anypoint.df.edi.schema.{ EdiSchema, EdiSchemaVersion, YamlWriter }
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.lexical.EdiConstants
import com.anypoint.df.edi.lexical.EdiConstants.DataType

class CopybookImport(in: InputStream, enc: String) {

  val input = new LineIterator(in, enc)

  val compositeNameCounts = new scm.HashMap[String, Int]
  val segmentNameCounts = new scm.HashMap[String, Int]

  var elements = new scm.HashMap[String, Set[Element]]

  /** Iterator access to lines of input as lists of whitespace-delimited uppercase tokens. */
  class LineIterator(is: InputStream, enc: String) extends Iterator[Array[String]] {
    private val buffer = new BufferedReader(new InputStreamReader(is, enc))

    private var lineCount = 0
    private var nextTokens: Array[String] = Array()

    @tailrec
    private def accumr(lead: String): List[String] = {
      val line = buffer.readLine
      if (line == null) Nil
      else {
        lineCount += 1
        if (line.size < 7 || line(6) == '*') accumr(lead)
        else {
          val fulltext = lead + line.slice(7, 72).trim
          val split = fulltext.indexOf('.')
          if (split >= 0) fulltext.substring(0, split).toUpperCase.split(" +").toList
          else accumr(fulltext)
        }
      }
    }

    @tailrec
    private def filteredDef: Array[String] = {
      val tokens = accumr("")
      tokens match {
        case ("66" | "88") :: _ =>
          println(s"Warning: ignoring unsupported level ${tokens.head} at $lineCount")
          filteredDef
        case _ => tokens.toArray
      }
    }

    /** Get the next item definition as a list of tokens. This discards leading and trailing texn each line of input,
      * and also discards any unused definitions (levels 66 and 88).
      */
    def next: Array[String] = {
      if (nextTokens.isEmpty) throw new IllegalStateException("Past end of input")
      else {
        val result = nextTokens
        nextTokens = filteredDef
        result
      }
    }

    def hasNext = nextTokens.nonEmpty

    def peek = nextTokens

    def lineNumber = lineCount

    nextTokens = filteredDef
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

  private def dataError(msg: String) = throw new IllegalStateException(s"$msg at ${input.lineNumber}")

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

  def convertImplicitDecimal(name: String, pic: Seq[Char], lead: Int): Element = {
    val (fract, rem) = convertReps('9', pic, 0)
    val length = lead + fract
    if (rem.isEmpty) Element("", name, EdiConstants.toUnrestrictedType("N" + fract), length, length)
    else dataError("Invalid expression in PIC clause")
  }

  def convertPic(name: String, pic: String): Element = pic.head match {
    case 'X' =>
      val (lead, rem) = convertReps('X', pic.tail, 1)
      Element("", name, DataType.ALPHANUMERIC, lead, lead)
    case '9' =>
      val (lead, rem) = convertReps('9', pic.tail, 1)
      if (rem.isEmpty) Element("", name, DataType.NUMERIC, lead, lead)
      else if (rem.head == 'V') convertImplicitDecimal(name, rem.tail, lead)
      else dataError("Invalid expression in PIC clause")
    case 'V' => convertImplicitDecimal(name, pic.tail, 0)
    case _   => dataError("Invalid expression in PIC clause")
  }

  def buildSegment: Segment = {

    def buildNested(name: String, rootKey: String, position: Int, count: Int, level: String) = {
      val nested =
        if (count == 1) buildr(level, rootKey, position + 1, Nil)
        else buildr(level, name, 1, Nil)
      val comp = Composite("", name, nested, Nil, 0)
      CompositeComponent(comp, Some(name), genKey(rootKey, position), position, MandatoryUsage, 1)
    }

    def buildr(level: String, key: String, position: Int, acc: List[SegmentComponent]): List[SegmentComponent] =
      if (input.hasNext) {
        val nextLevel = input.peek(0)
        if (nextLevel <= level) acc.reverse
        else {
          val compdef = input.next
          if (compdef.size < 2) dataError("Unknown definition format")
          val (count, define) = if (compdef.size >= 5 && compdef(2) == "OCCURS") {
            if (compdef(4) != "TIMES") dataError("Unsupported OCCURS clause form")
            (compdef(3).toInt, compdef.take(2) ++ compdef.drop(5))
          } else (1, compdef)
          if (define.size == 2) {
            val nested = buildNested(define(1), key, position, count, define(0))
            buildr(level, key, position + 1, nested :: acc)
          } else {
            define(2) match {
              case "REDEFINES" =>
                buildr(level, key, position, Nil)
                buildr(level, key, position, acc)
              case "PIC" | "PICTURE" =>
                val element = convertPic(define(1), define(3))
                val comp = ElementComponent(element, Some(define(1)), genKey(key, position), position, MandatoryUsage, 1)
                buildr(level, key, position + 1, comp :: acc)
            }
          }
        }
      } else acc.reverse

    var position = 0
    val recdef = input.next
    if (recdef.size != 2 || recdef.head != "01") dataError("Missing expected record definition line")
    val segname = recdef(1)
    val abbrev = abbreviateName(segname)
    val ident = generateIdent(abbrev, segmentNameCounts)
    val comps = buildr("01", abbrev, 1, Nil)
    Segment(ident, ident, segname, comps, Nil)
  }

  def buildSchema: EdiSchema = {
    val buffer = scm.Buffer[Segment]()
    while (input.hasNext) buffer += buildSegment
    val segs = buffer.map { s => (s.ident, s) }.toMap
    new EdiSchema(EdiSchemaVersion(Copybook, ""), Map.empty, Map.empty, segs, Map.empty)
  }
}

object CopybookImport {

  /** Reads copybook and schema paths, then generates the schema from the copybook and writes it.
    */
  def main(args: Array[String]): Unit = {
    val infile = new File(args(0))
    val schema = new CopybookImport(new FileInputStream(infile), "UTF-8").buildSchema
    val writer = new FileWriter(args(1))
    YamlWriter.write(schema, Array[String](), writer)
    writer.close
    println("wrote output schema")
  }
}