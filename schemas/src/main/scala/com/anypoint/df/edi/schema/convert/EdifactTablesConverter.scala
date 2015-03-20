package com.anypoint.df.edi.schema.convert

import scala.io.Source
import java.io.File
import java.io.InputStream
import scala.annotation.tailrec
import com.anypoint.df.edi.schema.EdiSchema._
import java.io.FileInputStream
import com.anypoint.df.edi.schema.EdiSchema
import java.io.FileOutputStream
import com.anypoint.df.edi.schema.YamlWriter
import java.io.OutputStreamWriter
import java.io.InputStreamReader
import com.anypoint.df.edi.schema.YamlReader
import com.anypoint.df.edi.lexical.EdiConstants
import com.anypoint.df.edi.lexical.EdiConstants._
import java.io.BufferedReader

/** Application to generate EDIFACT transaction schemas from table data.
  */
object EdifactTablesConverter {

  // YAML file extension
  val yamlExtension = ".esl"

  // file names
  val codeListsName = "UNCL"
  val elementDefsName = "EDED"
  val compositeDefsName = "EDCD"
  val segmentDefsName = "EDSD"
  val messagesDirName = "messages"

  /** Simple iterator-style access to lines of input. */
  case class LineIterator(is: InputStream, enc: String) {
    val buffer = new BufferedReader(new InputStreamReader(is, enc))

    var lastLine = ""
    var nextLine = ""

    def hasNext = nextLine != null

    def peek = nextLine

    def next =
      if (nextLine == null) throw new IllegalStateException("past end of input")
      else {
        lastLine = nextLine
        nextLine = buffer.readLine
        lastLine
      }

    next
  }

  /** Get input stream for file in directory. */
  def fileInput(dir: File, name: String) = new FileInputStream(new File(dir, name))

  /** Split character sequence into two string at first space, trimming leading and trailing spaces from both
    * strings.
    */
  def splitLead(text: Seq[Char]) = {
    val split = text.span(c => c != ' ')
    (split._1.toString trim, split._2.toString trim)
  }

  /** Check if a break line (one starting with hyphens or special break characters). */
  def isBreak(line: String) = line.startsWith("---") || line.startsWith("ÄÄÄ")

  /** Skip lines until break line is skipped, or end of input. */
  def skipBreakLine(lines: LineIterator) = {
    while (lines.hasNext && !isBreak(lines next)) {}
    lines hasNext
  }

  /** Skip the next line, which must be blankk. */
  def skipBlankLine(lines: LineIterator) = {
    val skip = lines.next.trim
    if (skip.length > 0) throw new IllegalStateException(s"Expected empty line, found: '$skip'")
  }

  /** Skip lines until a blank line or end of input is found, returning with the iterator positioned after the blank
    * line or at end.
    */
  def skipPastBlankLine(lines: LineIterator) = while (lines.hasNext && lines.next.trim.length > 0) Unit

  /** Read input file to build all element schemas.
    */
  def readElements(in: InputStream) = {
    val lines = LineIterator(in, "ISO-8859-1")
    @tailrec
    def merger(acc: String): String = {
      val line = lines.next.trim
      if (line.length > 0) merger(acc + " " + line)
      else acc
    }
    def nextBlob: String = merger(lines.next.trim)
    @tailrec
    def buildr(acc: List[Element]): List[Element] =
      if (skipBreakLine(lines)) {
        skipBlankLine(lines)
        val nametext = splitLead(nextBlob)
        val desctext = splitLead(nextBlob)
        val reprtext = splitLead(nextBlob)
        if (desctext._1 != "Desc:" || reprtext._1 != "Repr:") throw new IllegalStateException("Error parsing element definition")
        if (!nametext._2.endsWith("[I]")) {
          val typsplit = reprtext._2 span (c => c.isLetter)
          val typ = EdiConstants.toEdifactType(typsplit._1.toString.toUpperCase)
          val sizesplit = typsplit._2 span (c => c == '.')
          val maxsize = sizesplit._2.toString.toInt
          val minsize = if (sizesplit._1.isEmpty) maxsize else 0
          buildr(Element(nametext._1, nametext._2, typ, minsize, maxsize) :: acc)
        } else buildr(acc)
      } else acc

    buildr(Nil) reverse
  }

  /** Trim trailing spaces from supplied line of text. */
  def trimTrailing(line: String) = {
    @tailrec
    def trimr(index: Int): String =
      if (index >= 0 && line.charAt(index) == ' ') trimr(index - 1)
      else line.substring(0, index + 1)
    trimr(line.length - 1)
  }

  /** Extract trimmed substring from supplied line of text, where the start of the substring must be before the end of
    * the line but the expected end of the substring may be beyond the end.
    */
  def safeSubstring(text: String, from: Int, to: Int) = {
    println(s"extracting $from to $to from '$text'")
    if (to <= text.length) text.substring(from, to) trim
    else if (from >= text.length) throw new IllegalArgumentException(s"tried substring starting at $from with length ${text.length}")
    else text.substring(from) trim
  }
  /** Build a list of asterisk positions in the supplied line of text. This is used to define the field start positions
    * within field-structured files. At least two asterisks must be present in the line.
    */
  def buildTemplate(line: String): List[Int] = {
    @tailrec
    def parser(last: Int, acc: List[Int]): List[Int] = {
      val next = line.indexOf('*', last + 1)
      if (next >= 0) parser(next, next :: acc)
      else acc
    }
    val templ = parser(-1, Nil).reverse
    if (templ.length > 1) templ
    else throw new IllegalArgumentException("invalid template (less than two asterisks)")
  }

  /** Parse a line based on template. If trailing positions in the template have no text the last field present is
    * continued on the next line of input. To handle special cases (such as notes included in the definition) the
    * <code>require</code> flag can be used to abort the processing of lines which do not have a value present in the
    * first template field.
    *
    * @param starts field start positions (at least two values, last value is past end of last field)
    * @param require value required for first field
    * @param lines input iterator
    */
  def parseTemplate(starts: List[Int], require: Boolean, lines: LineIterator): Array[String] = {
    /** Parse a single field, continuing partial fields to next line. */
    @tailrec
    def parser(line: String, partial: Option[String], start: Int, rem: List[Int], acc: List[String]): List[String] =
      rem match {
        case h :: t => {
          val field = partial match {
            case Some(s) => s + ' ' + safeSubstring(line, start, h)
            case _ => safeSubstring(line, start, h)
          }
          if (t.isEmpty) {
            if (field.length > 0) field :: acc
            else acc
          } else if (line.length >= h) parser(line, None, h, t, field :: acc)
          else if (lines hasNext) parser(lines.next, Some(field), start, rem, acc)
          else acc
        }
        case _ => acc
      }

    if (lines.hasNext) {
      val line = lines.next
      println(s"parsing line '$line'")
      if (line.length == 0) Array()
      else {
        val (h :: t) = starts
        val field = safeSubstring(line, h, t.head)
        if (!require || field.length > 0) {
          val result = parser(line, None, t.head, t.tail, List(field)).reverse.toArray
          if (result.size < starts.size - 1) throw new IllegalArgumentException("too few values for template (${result.size}, expected ${starts.size})")
          else result
        } else Array()
      }
    } else Array()
  }

  /** Build composite schemas from input lines. Each composite definition is terminated either by the end of input or by
    * a blank line followed by a separator line. Items in a composite definition are defined on consecutive lines, with
    * no blank lines. The headers template must have three fields, matching the leading text (ignored), composite
    * identifier, and composite name. The body template must have five fields, matching the item number, modification
    * indicator (ignored), element identifier, element name, and usage code.
    *
    * @param lines input iterator
    * @param headtmpl field positions for header line of definition
    * @param bodytmpl field positions for body lines of definition
    * @param elements id to element map
    */
  def readComposites(lines: LineIterator, headtmpl: List[Int], bodytmpl: List[Int], elements: Map[String, Element]) = {
    @tailrec
    def buildr(acc: List[Composite]): List[Composite] = {
      def collectItems(cid: String) = {
        @tailrec
        def itemr(acc: List[SegmentComponent]): List[SegmentComponent] = {
          val fields = parseTemplate(bodytmpl, true, lines)
          if (fields.length == 0) acc reverse
          else if (fields.length == 5) {
            val position = fields(0).toInt
            val id = fields(2)
            if (!elements.contains(id)) throw new IllegalArgumentException(s"unknown element id $id")
            val element = elements(id)
            val usage = convertUsage(fields(4))
            val next = ElementComponent(element, Some(fields(3)), cid + position, position, usage, 1) :: acc
            if (lines.hasNext) itemr(next)
            else next reverse
          } else throw new IllegalArgumentException(s"wrong number of fields in input (${fields.toList}, expected 5)")
        }
        itemr(Nil)
      }

      if (skipBreakLine(lines)) {
        skipBlankLine(lines)
        val heads = parseTemplate(headtmpl, false, lines)
        skipBlankLine(lines)
        skipPastBlankLine(lines)
        val items = collectItems(heads(1))
        buildr(Composite(heads(1), heads(2), items, Nil) :: acc)
      } else acc reverse
    }

    buildr(Nil)
  }

  /** Build segment schemas from input lines. Each segment definition is terminated either by the end of input or by
    * a blank line followed by a separator line. Items in a segment definition are separated by blank lines, and
    * composite items have their component elements listed on consecutive lines (which need to be ignored) after the
    * composite reference. The headers template must have three fields, matching the leading text (ignored), segment
    * identifier, and segment name. The body template must have five fields, matching the item position, modification
    * indicator (ignored), element or composite identifier, element or composite name, and usage code.
    *
    * @param lines input iterator
    * @param headtmpl field positions for header line of definition
    * @param bodytmpl field positions for body lines of definition
    * @param elements id to element map
    * @param composite id to composite map
    */
  def readSegments(lines: LineIterator, headtmpl: List[Int], bodytmpl: List[Int],
    elements: Map[String, Element], composites: Map[String, Composite]) = {
    @tailrec
    def buildr(acc: List[Segment]): List[Segment] = {

      if (skipBreakLine(lines)) {
        skipBlankLine(lines)
        val heads = parseTemplate(headtmpl, false, lines)
        skipBlankLine(lines)
        skipPastBlankLine(lines)

        @tailrec
        def itemr(acc: List[SegmentComponent]): List[SegmentComponent] = {
          def checkNext = {
            skipPastBlankLine(lines)
            lines.hasNext && !isBreak(lines.peek)
          }
          val fields = parseTemplate(bodytmpl, true, lines)
          if (fields.length == 0) acc reverse
          else if (fields.length == 5) {
            val position = fields(0).toInt
            val id = fields(2)
            val usage = convertUsage(fields(4))
            val key = keyName(heads(1), position)
            val comp =
              if (elements.contains(id)) ElementComponent(elements(id), Some(fields(3)), key, position, usage, 1)
              else if (composites.contains(id)) CompositeComponent(composites(id), Some(fields(3)), key, position, usage, 1)
              else throw new IllegalArgumentException(s"unknown element or composite id $id")
            val next = comp :: acc
            if (checkNext) itemr(next)
            else next reverse
          } else throw new IllegalArgumentException(s"wrong number of fields in input (${fields.toList}, expected 5)")
        }

        val items = itemr(Nil)
        buildr(Segment(heads(1), heads(2), items, Nil) :: acc)
      } else acc reverse
    }

    buildr(Nil)
  }

  /** Builds schemas from EDIFACT table data and outputs the schemas in YAML form. The arguments are 1) path to the
    * directory containing the EDIFACT table data files, and 2) path to the directory for the YAML output files. All
    * existing files are deleted from the output directory before writing any output files. Each transaction is output
    * as a separate file, with the transaction ID used as the file name (with extension ".esl"). A separate base YAML
    * named "base.esl" contains the segment, composite, and element definitions.
    */
  def main(args: Array[String]): Unit = {
    val edifactdir = new File(args(0))
    val yamldir = new File(args(2))
    if (yamldir.exists) yamldir.listFiles.foreach { f => f.delete }
    else yamldir.mkdirs
    val elemfile = new File(edifactdir, elementDefsName + "." + args(1))
    val elemstream = new FileInputStream(elemfile)
    val elements = readElements(elemstream)
    elemstream close
  }
}