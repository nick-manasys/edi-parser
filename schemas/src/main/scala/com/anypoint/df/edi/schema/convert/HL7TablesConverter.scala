package com.anypoint.df.edi.schema.convert

import java.io.{ File, FileInputStream, FileOutputStream, FileWriter, InputStream, InputStreamReader, OutputStreamWriter }

import scala.annotation.tailrec
import scala.io.Source

import com.anypoint.df.edi.lexical.EdiConstants
import com.anypoint.df.edi.lexical.EdiConstants.DataType
import com.anypoint.df.edi.schema.{ EdiSchema, EdiSchemaVersion, YamlReader, YamlWriter }
import com.anypoint.df.edi.schema.EdiSchema._

/** Application to generate HL7 message schemas from table data.
  */
object HL7TablesConverter {

  // YAML file extension
  val yamlExtension = ".esl"

  // file names
  val messageNames = "message_types.txt"
  val eventCodes = "events.txt"
  val eventMessages = "event_message_types.txt"
  val messageStructures = "msg_struct_id_segments.txt"
  val segmentNames = "segments.txt"
  val segmentStructures = "segment_data_elements.txt"
  val dataElements = "data_elements.txt"
  val dataStructureNames = "data_structures.txt"
  val dataStructureComponents = "data_structure_components.txt"
  val componentDetails = "components.txt"
  // (data_structure, seq_no), comp_no, table_id, min_length, max_length, req_opt

  /** Specialized usage code conversion for HL7. */
  def convertUsage(code: String) = code match {
    case "B" | "(B) R" => OptionalUsage
    case "W" => UnusedUsage
    case "R" | "" => MandatoryUsage
    case _ => EdiSchema.convertUsage(code)
  }

  /** Split comma-separated quoted values from string into list. */
  def splitValues(s: String) = {
    def stripComma(remain: Seq[Char], acc: List[String]): List[String] =
      if (remain.isEmpty) acc reverse
      else if (remain.head == ',') splitQuotes(remain.tail, acc)
      else throw new IllegalArgumentException(s"missing expected comma after closing quote: $s ($remain)")
    def splitQuotes(remain: Seq[Char], acc: List[String]): List[String] =
      if (remain.head == '"') {
        val (text, rest) = remain.tail span (c => c != '"')
        if (rest.isEmpty) throw new IllegalArgumentException(s"missing closing quote character: $s")
        else stripComma(rest.tail, text.toString :: acc)
      } else throw new IllegalArgumentException(s"missing required quote character: $s")
    splitQuotes(s, Nil)
  }

  /** Get input stream for file in directory. */
  def fileInput(dir: File, name: String) = new FileInputStream(new File(dir, name))

  /** Accumulate the result of applying an operation to the lists of values for each line in input. Note that the result
    * will be in reversed order (if ordered).
    */
  def foldInput[T](in: InputStream, z: T)(f: (T, List[String]) => T) =
    Source.fromInputStream(in, "ISO-8859-1").getLines.filter(line => line.length > 0).foldLeft(z)((z, line) =>
      f(z, splitValues(line)))

  type LineFields = List[Array[String]]
  /** Convert input to list of arrays of strings (list reverse ordered). */
  def lineList(in: InputStream) = foldInput(in, Nil.asInstanceOf[LineFields]) ((list, line) => line.toArray :: list)

  /** Generate map from first column of data to list of remaining values in row. */
  def nameMap(in: InputStream) = foldInput(in, Map.empty[String, Array[String]])((map, list) =>
    list match {
      case name :: t => map + (name -> t.toArray)
      case _ => throw new IllegalArgumentException("wrong number of values in file")
    })

  /** List of pairs, with the first item a key value and the second item a list of lists of associated values. */
  type ListOfKeyedLists = List[(String, List[List[String]])]
  val emptyListOfKeyedLists = Nil.asInstanceOf[ListOfKeyedLists]

  /** Gather list of values into list of lists based on the first column value. This supports the detail file formats
    * where the first column is a repeating ID value. The output is in the form of a list of pairs, with the first item
    * the common first column value and the second item a list of the remaining column values. Input order is preserved
    * in the output, and the column count is verified.
    *
    * @param in
    * @param count
    * @param fill value used when last column not present (if supplied)
    * @return List(id, List[List[String]])
    */
  def gatherGroups(source: String, in: InputStream, count: Int, fill: Option[String]): ListOfKeyedLists = {
    def append(id: String, next: List[String], acc: ListOfKeyedLists) = acc match {
      case (key, values) :: tail if (key == id) => (key, next :: values) :: tail
      case tail => (id, next :: Nil) :: tail
    }
    val result = foldInput(in, emptyListOfKeyedLists)((acc, list) => {
      val length = list.length
      list match {
        case id :: rest if (length == count) => append(id, rest, acc)
        case id :: rest if (fill.nonEmpty && length == count - 1) => append(id, rest :+ fill.get, acc)
        case _ => throw new IllegalArgumentException(s"wrong number of values in input $source: list")
      }
    })
    result.foldLeft(emptyListOfKeyedLists)((acc, pair) => pair match {
      case (key, list) => (key, list.reverse) :: acc
    })
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

  /** Build composite definition from data structure components list. */
  def buildComposite(lines: LineFields, names: Map[String, String], elems: Map[String, Array[String]],
    comps: Map[String, ComponentBase]) = {
    val ident = lines.head(0)
    println(s"building composite $ident")
    val compList = lines.map { line =>
      comps(elems(line(2))(2)) match {
        case Element(id, nm, typ, mn, mx) => {
          val max = if (line(5).length > 0) line(5).toInt else mx
          val elem = Element(id, nm, typ, mn, max)
          ElementComponent(elem, None, "", line(1).toInt, convertUsage(line(6)), 1)
        }
        case c: Composite => CompositeComponent(c, None, "", line(1).toInt, convertUsage(line(6)), 1)
      }
    }
    comps + (ident -> Composite(ident, names(ident), compList, Nil, 0))
  }

  @tailrec
  def buildComposites(grouped: Map[String, LineFields], names: Map[String, String],
    elems: Map[String, Array[String]], built: Map[String, ComponentBase]): Map[String, ComponentBase] = {

    /** Build composite definition from data structure components list. */
    def buildComposite(lines: List[Array[String]], comps: Map[String, ComponentBase]) = {
      val ident = lines.head(0)
      val compList = lines.map { line =>
        comps(elems(line(2))(2)) match {
          case Element(id, nm, typ, mn, mx) => {
            val max = if (line(5).length > 0) line(5).toInt else mx
            val elem = Element(id, nm, typ, mn, max)
            ElementComponent(elem, None, "", line(1).toInt, convertUsage(line(6)), 1)
          }
          case c: Composite => CompositeComponent(c, None, "", line(1).toInt, convertUsage(line(6)), 1)
        }
      }
      Composite(ident, names(ident), compList, Nil, 0)
    }

    val building = grouped.filter {
      case (ident, lines) =>
        lines.forall { line => built.contains(elems(line(2))(3)) }
    }.map{ case (ident, lines) => ident }.toSet
    val merged = building.foldLeft(built)((acc, ident) => acc + (ident -> buildComposite(grouped(ident), acc)))
    val remain = grouped.filter { case (ident, lines) => !building.contains(ident) }
    if (remain.isEmpty) merged
    else buildComposites(remain, names, elems, merged)
  }

  def buildSegments(grouped: Map[String, LineFields], names: Map[String, String],
    comps: Map[String, (String, ComponentBase)]) = {
    names.keys.foldLeft(Map[String, Segment]())((map, ident) => {
      val compList = grouped.get(ident) match {
        case Some(list) => list.foldLeft(List[SegmentComponent]())((acc, line) =>
          {
            val repeats = if (line(4) == "Y") line(5).toInt else 1
            val (nmtext, comp) = comps(line(2))
            val name = if (nmtext.isEmpty) None else Some(nmtext)
            comp match {
              case e: Element => ElementComponent(e, name, "", line(1).toInt, convertUsage(line(3)), repeats) :: acc
              case c: Composite => CompositeComponent(c, name, "", line(1).toInt, convertUsage(line(3)), repeats) :: acc
            }
          })
        case None => Nil
      }
      map + (ident -> Segment(ident, names(ident), compList.reverse, Nil))
    })
  }

  def buildStructures(grouped: Map[String, LineFields], segs: Map[String, Segment], version: EdiSchemaVersion) = {
    @tailrec
    def zeroPad(value: String, length: Int): String = if (value.length < length) zeroPad("0" + value, length) else value
    def buildGroup(lines: LineFields, close: String) = {
      val (remain, nested) = buildr(lines, Nil)
      remain match {
        case close :: t => (t, StructureSequence(true, nested))
        case _ => throw new IllegalArgumentException(s"Missing expected segment group close '$close'")
      }
    }
    def buildr(remain: LineFields, comps: List[StructureComponent]): (LineFields, List[StructureComponent]) =
      remain match {
        case h :: t => {
          val code = h(2)
          segs.get(code) match {
            case Some(seg) => {
              val segpos = SegmentPosition(0, zeroPad(h(1), 2))
              val count = h(4) match {
                case "0" => 1
                case "1" => 0
              }
              val usage = h(5) match {
                case "0" => MandatoryUsage
                case "1" => OptionalUsage
              }
              buildr(t, ReferenceComponent(seg, segpos, usage, count) :: comps)
            }
            case None => code match {
              case "{" => {
                val (rest, nested) = buildGroup(t, "}")
                buildr(rest, new GroupComponent(h(3), MandatoryUsage, 0, nested, None, Nil) :: comps)
              }
              case "[" => {
                val (rest, nested) = buildGroup(t, "]")
                buildr(rest, new GroupComponent(h(3), OptionalUsage, 1, nested, None, Nil) :: comps)
              }
              case "[{" => {
                val (rest, nested) = buildGroup(t, "}]")
                buildr(rest, new GroupComponent(h(3), OptionalUsage, 0, nested, None, Nil) :: comps)
              }
              case "<" => {
                val (rest, nested) = buildGroup(t, "}")
                buildr(rest, new GroupComponent(h(3), MandatoryUsage, 0, nested, None, Nil, ch = true) :: comps)
              }
              case "|" => buildr(t, comps)
              case "}" | "]" | "}]" | ">" => (remain, comps.reverse)
              case _ => throw new IllegalArgumentException(s"Unknown segment ${h(2)}")
            }
          }
        }
        case _ => (Nil, comps.reverse)
      }

    grouped.foldLeft(List[Structure]()){
      case (list, (ident, lines)) => {
        Structure(ident, ident, None, Some(StructureSequence(false, buildr(lines, Nil)._2)), None, None, version)
      } :: list
    }
  }

  /** Builds schemas from HL7 table data and outputs the schemas in YAML form. The arguments are 1) path to the
    * directory containing the HL7 table data files, and 2) path to the directory for the YAML output files. All
    * existing files are deleted from the output directory before writing any output files. Each message is output
    * as a separate file, with the structure ID used as the file name (with extension ".yaml").
    */
  def main(args: Array[String]): Unit = {
    val hl7dir = new File(args(0))
    val yamldir = new File(args(1))
    if (yamldir.exists) yamldir.listFiles.foreach { version =>
      if (version.exists && version.isDirectory) {
        version.listFiles.foreach { f => f.delete }
        version.delete
      }
    }
    else yamldir.mkdirs
    val yamlrdr = new YamlReader()
    hl7dir.listFiles.foreach (version => {
      println(s"Processing ${version.getName}")
      val vernum = version.getName.drop(1).replace('_', '.')
      val schemaVersion = EdiSchemaVersion(HL7, vernum)

      // comp_no, description, table_id, data_type_code, data_structure
      val compDetails = nameMap(fileInput(version, componentDetails))
      // data_structure, description, data_type_code, repeating, elementary
      val dataStructs = lineList(fileInput(version, dataStructureNames))
      val structNames = dataStructs.foldLeft(Map[String, String]())((acc, line) => acc + (line(0) -> line(1)))
      // (data_structure, seq_no), comp_no, table_id, min_length, max_length, req_opt
      val groupedDataStructs = lineList(fileInput(version, dataStructureComponents)).reverse.groupBy { _(0) }

      // build element definitions for elementary components
      val elemDefs = dataStructs.filter { _(4) == "1" }.foldLeft(Map.empty[String, Element])((map, line) => {
        val ident = line(0)
        val compline = groupedDataStructs(ident).head
        val mintext = compline(4)
        val minlen = if (mintext.length > 0) mintext.toInt else 1
        val maxlen = compline(5).toInt
        map + (ident -> Element(line(0), line(1), EdiConstants.toHL7Type(line(2)), minlen, if (maxlen == 0) 9999 else maxlen))
      })
      println("Generated element definitions:")
      elemDefs.keys.foreach { ident => println(ident) }
      val simpleComps = compDetails.filter { case (ident, line) => elemDefs.contains(line(3)) }
      val compMap = buildComposites(groupedDataStructs, structNames, compDetails, elemDefs)
      compMap.foreach {
        case (key, elem: Element) => println(s"$key => ${elem.name} (element)")
        case (key, comp: Composite) => println(s"$key => ${comp.name} (composite with ${comp.components.length} components)")
      }

      // data_item, description, data_structure, min_length, max_length
      val elemMap = lineList(fileInput(version, dataElements)).foldLeft(Map[String, (String, ComponentBase)]())((acc, line) =>
        compMap.get(line(2)) match {
          case Some(c) => acc + (line(0) -> (line(1), c))
          case None if (line(2) == "-") => acc + (line(0) -> (line(1), compMap("varies")))
          case _ => throw new IllegalStateException(s"failed lookup for ident ${line(2)}")
        })

      // seg_code, seq_no, data_item, req_opt, repetitional, repetitions
      val groupedSegStructs = lineList(fileInput(version, segmentStructures)).reverse.groupBy { _(0) }
      // seg_code, description, visible
      val segNames = lineList(fileInput(version, segmentNames)).filter { line => (line(2) == "1" ) }.
        foldLeft(Map[String, String]()) {
          (acc, line) => acc + (line(0) -> line(1))
        }
      val segments = buildSegments(groupedSegStructs, segNames, elemMap)
      println("Segments:")
      segments.foreach {
        case (key, comp) => println(s" $key => ${comp.name} with ${comp.components.size} top-level components")
      }
      val compDefs = compMap.foldLeft(Map[String, Composite]()) {
        case (acc, (key, value: Composite)) => acc + (key -> value)
        case (acc, _) => acc
      }

      // message_structure, seq_no, seg_code, groupname, repetitional, optional
      val groupedMsgStructs = lineList(fileInput(version, messageStructures)).reverse.groupBy { _(0) }
      val structures = buildStructures(groupedMsgStructs, segments, schemaVersion).sortBy { _.ident }

      val baseSchema = EdiSchema(schemaVersion, elemDefs, compDefs, segments, Map[String, Structure]())
      val outdir = new File(yamldir, version.getName)
      outdir.mkdirs
      writeSchema(baseSchema, "basedefs", Array(), outdir)
      verifySchema(baseSchema, "basedefs", outdir, yamlrdr)
      
      val listWriter = new FileWriter(new File(outdir, "structures.txt"))
      structures foreach (struct => {
        val schema = EdiSchema(schemaVersion, Map[String, Element](), Map[String, Composite](),
          Map[String, Segment](), Map(struct.ident -> struct))
        writeSchema(schema, struct.ident, Array(s"/hl7/${version.getName}/basedefs$yamlExtension"), outdir)
        listWriter.write(struct.ident + '\n')
      })
      listWriter.close
    })
  }
}