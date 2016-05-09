package com.mulesoft.flatfile.schema.convert

import java.io.{ BufferedReader, File, FileInputStream, FileOutputStream, FileWriter, InputStream, InputStreamReader, OutputStreamWriter }

import scala.annotation.tailrec
import scala.io.Source

import com.mulesoft.flatfile.lexical.EdifactConstants
import com.mulesoft.flatfile.schema.{ EdiSchema, EdiSchemaVersion, EdifactSchemaDefs, YamlReader, YamlWriter }
import com.mulesoft.flatfile.schema.EdiSchema._

/** Application to generate EDIFACT structure schemas from table data.
  */
object EdifactTablesConverter {

  // YAML file extension
  val yamlExtension = ".esl"

  // file names
  val codeListsName = "UNCL"
  val elementDefsName = "EDED"
  val compositeDefsName = "EDCD"
  val segmentDefsName = "EDSD"
  val messageTemplateName = "message-template.txt"
  val messagesDirName = "messages"

  /** Check if a break line (one starting with hyphens or special break characters). */
  def isBreak(line: String) = line.startsWith("---") || line.startsWith("ÄÄÄ")

  /** Simple iterator-style access to lines of input. */
  case class LineIterator(is: InputStream, enc: String) {
    val buffer = new BufferedReader(new InputStreamReader(is, enc))

    var lastLine = ""
    var nextLine = ""
    var lineNumber = 0

    def hasNext = nextLine != null

    def peek = nextLine

    def next =
      if (nextLine == null) throw new IllegalStateException("past end of input")
      else {
        lastLine = nextLine
        nextLine = buffer.readLine
        lineNumber += 1
        lastLine
      }

    def trimmed = next trim

    /** Skip lines until break line is skipped, or end of input. */
    def skipBreakLine = {
      while (hasNext && !isBreak(next)) {}
      hasNext
    }

    /** Skip the next line, which must be blank. */
    def skipBlankLine = {
      val skip = trimmed
      if (skip.length > 0) throw new IllegalStateException(s"Expected empty line, found: '$skip'")
    }

    /** Skip next line if it's blank. */
    def skipIfBlank = if (peek.trim.length == 0) next

    /** Skip lines until a blank line or end of input is found, returning with the iterator positioned after the blank
      * line or at end.
      */
    def skipPastBlankLine = while (hasNext && next.trim.length > 0) Unit

    /** Skip lines until one is found with a non-blank in the first character position. */
    def skipToLead = while (hasNext && (peek.length == 0 || peek.startsWith(" "))) next

    /** Skip lines until a non-blank line or end of input is found, returning with the iterator positioned at the non-
      * blank line or at end.
      */
    def skipBlankLines = while (hasNext && peek.trim.length == 0) next

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

  /** Read input file to build all element schemas. */
  def readElements(lines: LineIterator) = {

    @tailrec
    def merger(acc: String): String = {
      val line = lines.trimmed
      if (line.length > 0) merger(acc + " " + line)
      else acc
    }

    /** Get next trimmed line of text, ignoring leading character positions of line (change indicators). */
    def nextBlob: String = {
      val line = lines.next.dropWhile { _ != ' ' }
      merger(line.trim)
    }

    @tailrec
    def buildr(acc: List[Element]): List[Element] =
      if (lines.skipBreakLine) {
        lines.skipBlankLine
        lines.skipBlankLines
        val nametext = splitLead(nextBlob)
        val desctext = splitLead(nextBlob)
        val reprtext = splitLead(nextBlob)
        if (desctext._1 != "Desc:" || reprtext._1 != "Repr:") throw new IllegalStateException("Error parsing element definition")
        if (!nametext._2.endsWith("[I]")) {
          val typsplit = reprtext._2 span (c => c.isLetter)
          val sizesplit = typsplit._2 span (c => c == '.')
          val maxsize = sizesplit._2.toString.toInt
          val minsize = if (sizesplit._1.isEmpty) maxsize else 0
          val typ = EdifactConstants.buildType(typsplit._1.toString, minsize, maxsize)
          val rawname = nametext._2
          val splitat = rawname.indexOf('[')
          if (splitat > 0 && !rawname.endsWith("]")) throw new IllegalStateException("Invalid element name format, expected type in [X] form")
          val name = if (splitat > 0) rawname.substring(0, splitat) else rawname
          buildr(Element(nametext._1, name.trim, typ) :: acc)
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
    if (to <= text.length) text.substring(from, to) trim
    else if (from >= text.length) ""
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

  /** Parse a line based on template. If wrapping is enabled this checks if the following line is all spaces up to the
    * start of the wrapping field, and if so continues the wrapping field on that next line - but since the EDIFACT
    * tables are inconsistent on wrapping the following fields, this checks for data beyond the end of the wrapped
    * field on the folloiwng line before discarding the rest of the current line. To handle special cases (such as notes
    * included in the definition) the <code>require</code> flag can be used to abort the processing of lines which do
    * not have a value present in the first template field.
    *
    * @param starts field start positions (at least two values, last value is past end of last field)
    * @param require value required for first field
    * @param wrap field index to check for wrapping (-1 if none)
    * @param lines input iterator
    * @returns trimmed fields
    */
  def parseTemplate(starts: List[Int], require: Boolean, wrap: Int, lines: LineIterator): Array[String] = {

    /** Parse a line, continuing partial fields to next line. */
    @tailrec
    def parser(line: String, start: Int, rem: List[Int], acc: List[String]): List[String] =
      rem match {
        case h :: t => parser(line, h, t, safeSubstring(line, start, h) :: acc)
        case _ => acc.reverse
      }

    val (first :: rest) = starts

    /** Merged fields for wrapped line. */
    def mergeWrapping(fields: List[String]): List[String] = {
      def checkWrapped = {
        val from = starts(wrap)
        val to = starts(wrap + 1)
        val line = lines.peek
        line.size > from && line.substring(0, from).forall { _ == ' ' } &&
          !line.substring(from, Math.min(to, line.size)).forall { _ == ' ' }
      }
      if (wrap >= 0 && lines.hasNext && checkWrapped) {
        fields.zip(parser(lines.next, first, rest, Nil)) map {
          case ("", b) => b
          case (a, "") => a
          case (a, b) => a + " " + b
        }
      } else fields
    }

    if (lines.hasNext) {
      val line = lines.next
      if (line.length == 0) Array()
      else {
        val field = safeSubstring(line, first, rest.head)
        if (!require || field.length > 0) {
          val fields = mergeWrapping(parser(line, rest.head, rest.tail, List(field))).toArray
          if (fields.last.length == 0) throw new IllegalArgumentException(s"too few values for template at line ${lines.lineNumber} (expected ${starts.size - 1})")
          else fields
        } else Array()
      }
    } else Array()
  }

  /** Build composite schemas from input lines. Each composite definition is terminated either by the end of input or by
    * a blank line followed by a separator line. Items in a composite definition are defined on consecutive lines, with
    * no blank lines. The headers template must have three fields, matching the leading text (ignored), composite
    * identifier, and composite name. The body template must have five fields, matching the item number, modification
    * indicator (ignored), element identifier, element name, and usage code. As a special case, a item number field only
    * one column wide indicates that item numbers are missing and must be generated automatically (used for handling the
    * D.93A tables, which don't include item numbers).
    *
    * @param lines input iterator
    * @param headtmpl field positions for header line of definition
    * @param bodytmpl field positions for body lines of definition
    * @param elements id to element map
    */
  def readComposites(lines: LineIterator, headtmpl: List[Int], bodytmpl: List[Int], elements: Map[String, Element]) = {
    val genNumber = bodytmpl(0) == bodytmpl(1) - 1

    @tailrec
    def buildr(acc: List[Composite]): List[Composite] = {
      def collectItems(cid: String) = {
        @tailrec
        def itemr(acc: List[SegmentComponent], number: Int): List[SegmentComponent] = {
          val fields = parseTemplate(bodytmpl, !genNumber, 3, lines)
          if (fields.length == 0) acc reverse
          else if (fields.length == 5) {
            val position = if (genNumber) number + 10 else fields(0).toInt
            val id = fields(2)
            if (!elements.contains(id)) throw new IllegalArgumentException(s"unknown element id $id at line ${lines.lineNumber}")
            val element = elements(id)
            val usage = convertUsage(fields(4))
            val next = ElementComponent(element, Some(fields(3)), cid + position, position, usage, 1) :: acc
            if (lines.hasNext) itemr(next, position)
            else next reverse
          } else throw new IllegalArgumentException(s"wrong number of fields in input (${fields.toList}, expected 5)")
        }
        itemr(Nil, 0)
      }

      if (lines.skipBreakLine) {
        lines.skipBlankLine
        lines.skipBlankLines
        val heads = parseTemplate(headtmpl, false, 2, lines)
        lines.skipBlankLine
        lines.skipPastBlankLine
        if (!genNumber) lines.skipToLead
        val items = collectItems(heads(1))
        if (items.isEmpty) throw new IllegalArgumentException(s"No values defined for composite ${heads(1)}")
        buildr(Composite(heads(1), heads(2), items, Nil, 0) :: acc)
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
    val genNumber = bodytmpl(0) == bodytmpl(1) - 1

    @tailrec
    def buildr(acc: List[Segment]): List[Segment] = {

      if (lines.skipBreakLine) {
        lines.skipBlankLine
        lines.skipBlankLines
        val heads = parseTemplate(headtmpl, false, 2, lines)
        lines.skipBlankLine
        lines.skipPastBlankLine

        @tailrec
        def itemr(acc: List[SegmentComponent], number: Int): List[SegmentComponent] = {
          def checkNext = {
            lines.skipPastBlankLine
            lines.hasNext && !isBreak(lines.peek)
          }
          val fields = parseTemplate(bodytmpl, !genNumber, 3, lines)
          if (fields.length == 0) acc reverse
          else if (fields.length == 5 || fields.length == 6) {
            val position = if (genNumber) number else fields(0).toInt
            val id = fields(2)
            val usage = convertUsage(fields(4))
            val key = EdiFact.keyName(heads(1), fields(3), position)
            val count = if (fields.length == 6 && fields(5).length > 0) fields(5).toInt else 1
            val comp =
              if (elements.contains(id)) ElementComponent(elements(id), Some(fields(3)), key, position, usage, count)
              else if (composites.contains(id)) CompositeComponent(composites(id), Some(fields(3)), key, position, usage, count)
              else throw new IllegalArgumentException(s"unknown element or composite id $id")
            val next = comp :: acc
            if (checkNext) itemr(next, number + 10)
            else next reverse
          } else throw new IllegalArgumentException(s"wrong number of fields in input (${fields.toList}, expected 6)")
        }

        if (!genNumber) lines.skipToLead
        val items = itemr(Nil, 10)
        if (items.isEmpty) throw new IllegalArgumentException(s"No values defined for segment ${heads(1)}")
        buildr(new Segment(heads(1), heads(2), items, Nil) :: acc)
      } else acc reverse
    }

    buildr(Nil)
  }

  /** Build message from file input in standard (post-D.93A) form. This looks for the message name as the third
    * non-blank line of the input, then skips everything to the expected "4.3.1 Segment table" heading. It parses the
    * rest of the input as the structure set description.
    */
  def readMessage(ident: String, lines: LineIterator, tmpl: List[Int], segments: Map[String, Segment],
    version: EdiSchemaVersion) = {

    /** Skip message description to start of segment table. */
    def skipToTable = {
      while (lines.hasNext && !lines.nextLine.startsWith("4.3.1")) lines.next
      if (!lines.hasNext) throw new IllegalArgumentException("Missing structure segment table")
      if (!lines.trimmed.endsWith("Segment table")) throw new IllegalArgumentException("Missing structure segment table")
      lines.skipBlankLine
      if (!lines.next.startsWith("Pos")) throw new IllegalArgumentException("Missing expected column headers in structure segment table")
      lines.skipBlankLine
    }

    /** Check if next line is a section marker. */
    def atSection = lines.hasNext && lines.peek.endsWith("SECTION")

    /** Check if next line is an annex. */
    def atAnnex = lines.hasNext && lines.peek.startsWith("Annex")

    // boundary for separator lines (all blank to at least this point)
    val limit = tmpl(5)
    
    /** Convert input to a component definition, checking and handling loops. This is called recursively to handle nested
      * loops.
      * @param table index
      * @param depth current nesting depth
      * @return components
      */
    def convert(table: Int, depth: Int): List[StructureComponent] = {

      /** Recursively convert input to a component definition, checking and handling loops. This needs to process all
        * input up to the end of the current loop (as indicated by a line which is blank, except for up to the depth
        * character count) or the end of the current area (as indicated by a new section header, or end of input).
        */
      @tailrec
      def liner(acc: List[StructureComponent]): List[StructureComponent] = {
        def spaceCount = lines.peek.takeWhile { _ == ' ' }.length
        val count = if (lines.hasNext) spaceCount else 0
        if (!lines.hasNext || atSection || atAnnex) acc.reverse
        else if (lines.peek.length == 0 || spaceCount > limit) {
          // loop separator line, check if it shows end of current loop
          val nesting = lines.peek.trim.length
          if (depth > nesting) acc.reverse
          else {
            lines.next
            liner(acc)
          }
        } else {
          val length = lines.peek.length
          val nesting = if (length > limit) lines.peek.substring(limit).filter(c => !c.isDigit && c != ' ').length else 0
          if (depth > nesting) {
            acc.reverse
          } else {
            val fields = parseTemplate(tmpl, true, 3, lines)
            val usage = convertUsage(fields(4))
            val repeat = fields(5).filter { _.isDigit }.toInt
            val position = new DefinedPosition(table, fields(0))
            if (fields(2) == "") {
              // start of a new loop definition
              if (fields(3) == "") throw new IllegalArgumentException("Missing expected group name")
              val discard = fields(3).charAt(0)
              val name = fields(3).filter { _ != discard }.trim
              val seq = StructureSequence(true, convert(table, depth + 1))
              val group = GroupComponent(name, usage, repeat, seq, None, Nil, None, Some(position))
              liner(group :: acc)
            } else segments.get(fields(2)) match {
              case Some(s) => {
                if ("UNS" == fields(2) && (depth > 0 || !acc.isEmpty)) throw new IllegalArgumentException(s"UNS out of order in $ident")
                val segref = ReferenceComponent(s, position, usage, repeat)
                liner(segref :: acc)
              }
              case _ =>
                if (Set("UNH", "UNT", "UGH", "UGT") contains fields(2)) liner(acc)
                else throw new IllegalArgumentException(s"Unknown segment reference ${fields(2)}")
            }
          }
        }
      }
      liner(Nil)
    }
    
    def optSeq(items: List[StructureComponent]) = if (items.isEmpty) None else Some(StructureSequence(false, items))

    lines.skipIfBlank
    lines.skipPastBlankLine
    lines.skipPastBlankLine
    val name = lines.trimmed
    skipToTable
    if (atSection) {
      if (!lines.trimmed.startsWith("HEADER")) throw new IllegalStateException("Missing expected HEADER SECTION")
      val header = convert(0, 0)
      val detail = if (atSection) {
        if (lines.trimmed.startsWith("DETAIL")) convert(0, 0)
        else throw new IllegalStateException("Missing expected DETAIL SECTION")
      } else Nil
      val summary = if (atSection && lines.trimmed.startsWith("SUMMARY")) convert(0, 0) else Nil
      if (lines.hasNext && !atAnnex) throw new IllegalStateException("Not at end of description")
      Structure(ident, name, None, optSeq(header), optSeq(detail), optSeq(summary), version)
    } else {
      val comps = convert(0, 0)
      if (lines.hasNext && !atAnnex) throw new IllegalStateException("Not at end of description")
      Structure(ident, name, None, optSeq(comps), None, None, version)
    }
  }

  /** Build message from file input in D.93A form. This looks for the message name as the third non-blank line of the
    * input, then skips everything to the expected "4.2.2 Segment Table" heading. It parses the rest of the input as the
    * structure set description until reaching the "4.3" heading.
    */
  def readMessageD93A(ident: String, lines: LineIterator, tmpl1: List[Int], tmpl2: List[Int], segments: Map[String, Segment],
    version: EdiSchemaVersion) = {

    /** Skip message description to start of segment table. */
    def skipToTable = {
      // normally the segment table is in section 4.3.1, but for D93A it's in 4.2.2
      while (lines.hasNext && !lines.next.startsWith("4.2.2 Segment Table")) {}
      if (!lines.hasNext) throw new IllegalArgumentException("Missing structure segment table")
      lines.skipBlankLines
    }

    /** Check if next line is a section marker. */
    def atSection = lines.hasNext && lines.peek.endsWith("SECTION")

    /** Skip past the column headings line. */
    def skipHeadings =
      while (lines.hasNext && !lines.next.startsWith("  TAG")) {}

    /** Check if next line is start of next section. */
    def atEnd = lines.hasNext && lines.peek.startsWith("4.3")

    // boundary for separator lines (all blank to at least this point)
    val limit = tmpl1(5)

    // position of segment tag, or separator characters if a segment group start
    val tagstart = tmpl1(2)

    // last column for data in normal segment reference line (following characters show nesting)
    val lastcol = tmpl1(6)

    var position = 10000

    /** Convert input to a component definition, checking and handling loops. This is called recursively to handle nested
      * loops.
      * @param table index
      * @param depth current nesting depth
      * @return components
      */
    def convert(table: Int, depth: Int): List[StructureComponent] = {
    
      def segmentPosition = new DefinedPosition(table, position.toString.substring(1))

      /** Recursively convert input to a component definition, checking and handling loops. This needs to process all
        * input up to the end of the current loop (as indicated by a line which is blank, except for up to the depth
        * character count) or the end of the current area (as indicated by a new section header, or end of input).
        */
      @tailrec
      def liner(acc: List[StructureComponent]): List[StructureComponent] = {
        def spaceCount = lines.peek.takeWhile { _ == ' ' }.length
        if (!lines.hasNext || atSection || atEnd) acc.reverse
        else if (lines.peek.length == 0 || spaceCount > limit) {
          // loop separator line, check if it shows end of current loop
          val nesting = lines.peek.trim.length
          if (depth > nesting) acc.reverse
          else {
            lines.next
            liner(acc)
          }
        } else if (lines.peek(tagstart) == 'Ä') {
          // start of a new loop definition
          position += 10
          val fields = parseTemplate(tmpl2, false, -1, lines)
          if (fields(1) == "") throw new IllegalArgumentException("Missing expected group name")
          val usage = convertUsage(fields(2))
          val repeat = fields(3).takeWhile { _.isDigit }.toInt
          val name = fields(1).filter { _ != 'Ä' }.trim
          val seq = StructureSequence(true, convert(table, depth + 1))
          val group = GroupComponent(name, usage, repeat, seq, None, Nil, None, Some(segmentPosition))
          liner(group :: acc)
          //        } else if ((lines.peek.length > lastcol && lines.peek.substring(lastcol).length < depth) || lines.peek.length <= lastcol && depth > 0) {
          //          acc.reverse
        } else {
          val length = lines.peek.length
          val nesting = if (length > lastcol) lines.peek.substring(lastcol).trim.length else 0
          if (depth > nesting) {
            acc.reverse
          } else {
            position += 10
            val fields = parseTemplate(tmpl1, false, 3, lines)
            val usage = convertUsage(fields(4))
            val repeat = fields(5).takeWhile { _.isDigit }.toInt
            segments.get(fields(2)) match {
              case Some(s) => {
                if ("UNS" == fields(2) && (depth > 0 || !acc.isEmpty)) throw new IllegalArgumentException(s"UNS out of order in $ident")
                val segref = ReferenceComponent(s, segmentPosition, usage, repeat)
                liner(segref :: acc)
              }
              case _ =>
                if (Set("UNH", "UNT", "UGH", "UGT") contains fields(2)) liner(acc)
                else throw new IllegalArgumentException(s"Unknown segment reference ${fields(2)}")
            }
          }
        }
      }
      liner(Nil)
    }

    /** Convert section, skipping past the column headers line and converting the body of the section. */
    def convertSection = {
      skipHeadings
      lines.skipPastBlankLine
      convert(0, 0)
    }
    
    def optSeq(items: List[StructureComponent]) = if (items.isEmpty) None else Some(StructureSequence(false, items))

    lines.skipIfBlank
    lines.next
    lines.skipBlankLines
    lines.next
    lines.skipBlankLines
    val name = lines.trimmed
    skipToTable
    if (atSection) {
      if (!lines.trimmed.startsWith("HEADER")) throw new IllegalStateException("Missing expected HEADER SECTION")
      val header = convertSection
      val detail = if (atSection) {
        if (lines.trimmed.startsWith("DETAIL")) convertSection
        else throw new IllegalStateException("Missing expected DETAIL SECTION")
      } else Nil
      val summary = if (atSection && lines.trimmed.startsWith("SUMMARY")) convertSection else Nil
      if (lines.hasNext && !atEnd) {
        throw new IllegalStateException("Not at end of description")
        }
      Structure(ident, name, None, optSeq(header), optSeq(detail), optSeq(summary), version)
    } else {
      val comps = convertSection
      if (lines.hasNext && !atEnd) throw new IllegalStateException("Not at end of description")
      Structure(ident, name, None, optSeq(comps), None, None, version)
    }
  }

  /** Write schema to file. */
  def writeSchema(schema: EdiSchema, name: String, imports: Array[String], outdir: File) = {
    println(s"writing schema $name")
    val file = new File(outdir, name + yamlExtension)
    val writer = new OutputStreamWriter(new FileOutputStream(file), "UTF-8")
    YamlWriter.write(schema, imports, writer)
    writer.close
  }

  /** Verify schema written to file. */
  def verifySchema(baseSchema: EdiSchema, name: String, outdir: File, yamlrdr: YamlReader) = {
    val reader = new InputStreamReader(new FileInputStream(new File(outdir, name + yamlExtension)), "UTF-8")
    val readSchema = yamlrdr.loadYaml(reader, Array(outdir.getParentFile.getParentFile.getParentFile.getAbsolutePath))
    //    if (baseSchema != readSchema) throw new IllegalStateException(s"Verification error on schema $name")
  }

  /** Builds schemas from EDIFACT table data and outputs the schemas in YAML form. The arguments are 1) path to the
    * directory containing the EDIFACT table data files, and 2) path to the directory for the YAML output files. All
    * existing files are deleted from the output directory before writing any output files. Each structure is output
    * as a separate file, with the structure ID used as the file name (with extension ".esl"). A separate base YAML
    * named "base.esl" contains the segment, composite, and element definitions.
    */
  def main(args: Array[String]): Unit = {
    val edifactdir = new File(args(0))
    val yamldir = new File(args(1))
    if (yamldir.exists) yamldir.listFiles.foreach { version =>
      if (version.exists && version.isDirectory) {
        version.listFiles.foreach { f => f.delete }
        version.delete
      }
    }
    else yamldir.mkdirs
    val yamlrdr = new YamlReader()
    edifactdir.listFiles.foreach (version => {

      /** Process file from input directory using line iterator with safe close. */
      def processFile[T](name: String, op: LineIterator => T) = {
        val file = new File(version, name)
        if (!file.exists) throw new IllegalArgumentException(s"Missing required file $name")
        try {
          val stream = new FileInputStream(file)
          try { op(LineIterator(stream, "ISO-8859-1")) } finally { stream close }
        } catch {
          case t: Throwable => throw new IllegalArgumentException(s"Error processing $name: ${t.getMessage}", t)
        }
      }

      /** Build map from extracted key to object for all items in list. */
      def buildMap[T](list: List[T], key: T => String) =
        list.foldLeft(Map[String, T]()){ (acc, t) => acc + (key(t) -> t) }

      println(s"Processing ${version.getName}")
      val extension = "." + version.getName.toUpperCase.substring(1)
      val elements = processFile(elementDefsName + extension, readElements(_))
      val elemDefs = buildMap[Element](elements, (e: Element) => e.ident)
      val composites = processFile(compositeDefsName + extension, iter =>
        readComposites(iter, buildTemplate(iter.next), buildTemplate(iter.next), elemDefs))
      val compDefs = buildMap[Composite](composites, (c: Composite) => c.ident)
      val segments = processFile(segmentDefsName + extension, iter =>
        readSegments(iter, buildTemplate(iter.next), buildTemplate(iter.next), elemDefs, compDefs))
      val segDefs = buildMap[Segment](segments, (s: Segment) => s.ident)
      val schemaVersion = EdiSchemaVersion(EdiFact, version.getName)
      val baseSchema = EdiSchema(schemaVersion, elemDefs, compDefs, segDefs, Map[String, Structure]())
      val outdir = new File(yamldir, version.getName)
      outdir.mkdirs
      writeSchema(baseSchema, "basedefs", Array(), outdir)
      verifySchema(baseSchema, "basedefs", outdir, yamlrdr)

      /** Build structures from directory. */
      def buildStructures(builder: (String, LineIterator) => Structure, indir: File, outdir: File) = {
        indir.listFiles.sorted.map { f =>
          try {
            val stream = new FileInputStream(f)
            try {
              val name = f.getName.takeWhile { _ != '_' }
              val transact = builder(name, LineIterator(stream, "ISO-8859-1"))
              val schema = EdiSchema(schemaVersion, Map[String, Element](), Map[String, Composite](), Map[String, Segment](),
                Map(transact.ident -> transact))
              writeSchema(schema, transact.ident, Array(s"/edifact/${version.getName}/basedefs$yamlExtension"), outdir)
              println(s"Processed ${f.getName}")
              transact
            } finally { stream close }
          } catch {
            case t: Throwable => throw new IllegalArgumentException(s"Error processing message ${f.getName}: ${t.getMessage}", t)
          }
        }
      }

      val messagesdir = new File(version, messagesDirName)
      if (!messagesdir.exists) throw new IllegalArgumentException(s"Missing required $messagesDirName directory")
      val builder = if (version.getName == "d93a") {
        val (msgtmpl1, msgtmpl2) = processFile(messageTemplateName, iter => (buildTemplate(iter.next), buildTemplate(iter.next)))
        (name: String, lines: LineIterator) => readMessageD93A(name, lines, msgtmpl1, msgtmpl2, segDefs, schemaVersion)
      } else {
        val msgtmpl = processFile(messageTemplateName, iter => buildTemplate(iter.next))
        (name: String, lines: LineIterator) => readMessage(name, lines, msgtmpl, segDefs, schemaVersion)
      }
      val transacts = buildStructures(builder, messagesdir, outdir)

      val listWriter = new FileWriter(new File(outdir, "structures.txt"))
      transacts.foreach { transact => listWriter.write(transact.ident + '\n') }
      listWriter.close
      println(s"Read ${transacts.size} message definitions")
    })
  }
}