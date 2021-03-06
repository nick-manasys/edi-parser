package com.mulesoft.flatfile.schema.convert

import java.io.{ File, FileInputStream, FileOutputStream, FileWriter, InputStream, InputStreamReader, OutputStreamWriter }
import com.mulesoft.flatfile.lexical.HL7Support
import com.mulesoft.flatfile.schema.{ EdiSchema, EdiSchemaVersion, YamlReader, YamlWriter }
import com.mulesoft.flatfile.schema.EdiSchema._
import scala.annotation.tailrec
import scala.collection.{ immutable => sci, mutable => scm }
import scala.io.Source

import com.mulesoft.flatfile.lexical.HL7Support
import com.mulesoft.flatfile.schema.EdiSchema
import com.mulesoft.flatfile.schema.EdiSchema.ComponentBase
import com.mulesoft.flatfile.schema.EdiSchema.Composite
import com.mulesoft.flatfile.schema.EdiSchema.CompositeComponent
import com.mulesoft.flatfile.schema.EdiSchema.DefinedPosition
import com.mulesoft.flatfile.schema.EdiSchema.Element
import com.mulesoft.flatfile.schema.EdiSchema.ElementComponent
import com.mulesoft.flatfile.schema.EdiSchema.GroupComponent
import com.mulesoft.flatfile.schema.EdiSchema.HL7
import com.mulesoft.flatfile.schema.EdiSchema.MandatoryUsage
import com.mulesoft.flatfile.schema.EdiSchema.OptionalUsage
import com.mulesoft.flatfile.schema.EdiSchema.ReferenceComponent
import com.mulesoft.flatfile.schema.EdiSchema.Segment
import com.mulesoft.flatfile.schema.EdiSchema.SegmentComponent
import com.mulesoft.flatfile.schema.EdiSchema.Structure
import com.mulesoft.flatfile.schema.EdiSchema.StructureComponent
import com.mulesoft.flatfile.schema.EdiSchema.StructureSequence
import com.mulesoft.flatfile.schema.EdiSchema.UnusedUsage
import com.mulesoft.flatfile.schema.EdiSchema.Usage
import com.mulesoft.flatfile.schema.EdiSchemaVersion
import com.mulesoft.flatfile.schema.YamlReader
import com.mulesoft.flatfile.schema.YamlWriter
import scala.collection.mutable.MutableList
import scala.collection.mutable.HashMap

/**
 * Application to generate HL7 message schemas from table data.
 */
object HL7TablesConverter {

  // YAML file extension
  val yamlExtension = ".esl"

  // file names
  //  val messageNames = "message_types.txt"
  //  val eventCodes = "events.txt"
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
    case "B" | "(B) R" | "RE" | "" => OptionalUsage
    case "W" | "X"                 => UnusedUsage
    case "R"                       => MandatoryUsage
    case _                         => EdiSchema.convertUsage(code)
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

  /**
   * Accumulate the result of applying an operation to the lists of values for each line in input. Note that the result
   * will be in reversed order (if ordered).
   */
  def foldInput[T](in: InputStream, z: T)(f: (T, List[String]) => T) =
    Source.fromInputStream(in, "UTF-8").getLines.filter(line => line.length > 0).foldLeft(z)((z, line) =>
      f(z, splitValues(line)))

  type LineFields = List[Array[String]]
  /** Convert input to list of arrays of strings (list reverse ordered). */
  def lineList(in: InputStream) = foldInput(in, Nil.asInstanceOf[LineFields])((list, line) => line.toArray :: list)

  /** Generate map from first column of data to list of remaining values in row. */
  def nameMap(in: InputStream) = foldInput(in, Map[String, Array[String]]())((map, list) =>
    list match {
      case name :: t => map + (name -> t.toArray)
      case _         => throw new IllegalArgumentException("wrong number of values in file")
    })

  /** List of pairs, with the first item a key value and the second item a list of lists of associated values. */
  type ListOfKeyedLists = List[(String, List[List[String]])]
  val emptyListOfKeyedLists = Nil.asInstanceOf[ListOfKeyedLists]

  /**
   * Gather list of values into list of lists based on the first column value. This supports the detail file formats
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
      case tail                                 => (id, next :: Nil) :: tail
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
    //    println(s"writing schema $name")
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
    //    println(s"building composite $ident")
    val compList = lines.map { line =>
      comps(elems(line(2))(2)) match {
        case Element(id, nm, typ) => {
          val ntype =
            if (line(5).length > 0) HL7Support.buildType(typ.typeCode, typ.minLength, line(5).toInt)
            else typ
          val elem = Element(id, nm, ntype)
          ElementComponent(elem, None, "", line(1).toInt, convertUsage(line(6)), 1)
        }
        case c: Composite => CompositeComponent(c, None, "", line(1).toInt, convertUsage(line(6)), 1)
      }
    }
    comps + (ident -> Composite(ident, names(ident), compList, Nil, 0))
  }

  @tailrec
  def buildComposites(grouped: Map[String, LineFields], names: Map[String, String],
                      compdets: Map[String, Array[String]], built: Map[String, ComponentBase]): Map[String, ComponentBase] = {

    val missingDetails = scm.Set[String]()

    def compDetail(key: String): Array[String] = compdets.get(key) match {
      case Some(array) => array
      case None =>
        missingDetails += key
        Array("", "", "", "", "", "")
    }

    /** Build composite definition from data structure components list. */
    def buildComposite(lines: List[Array[String]], comps: Map[String, ComponentBase]) = {
      val ident = lines.head(0)
      val compList = lines.map { line =>
        val compdet = compDetail(line(2))
        val name = Some(compdet(0))
        comps(compdet(3)) match {
          case Element(id, nm, typ) => {
            val ntype = if (line(5).length > 0) {
              val max = line(5).toInt
              val min = Math.min(typ.minLength, max)
              HL7Support.buildType(typ.typeCode, min, max)
            } else typ
            val elem = Element(id, nm, ntype)
            ElementComponent(elem, name, "", line(1).toInt, convertUsage(line(6)), 1)
          }
          case c: Composite => CompositeComponent(c, name, "", line(1).toInt, convertUsage(line(6)), 1)
        }
      }
      Composite(ident, names(ident), compList, Nil, 0)
    }

    val building = grouped.filter {
      case (ident, lines) => lines.forall { line => built.contains(compDetail(line(2))(3)) }
    }.map { case (ident, lines) => ident }.toSet
    val merged = building.foldLeft(built)((acc, ident) => acc + (ident -> buildComposite(grouped(ident), acc)))
    val remain = grouped.filter { case (ident, lines) => !building.contains(ident) }
    if (missingDetails.nonEmpty) throw new IllegalStateException(s"Missing component definitions for: $missingDetails")
    if (remain.isEmpty) merged
    else if (grouped.size == remain.size) {
      val keys = remain.map { case (name: String, _) => name }
      throw new IllegalStateException(s"Circular or undefined references in definitions for data structures: $keys")
    } else buildComposites(remain, names, compdets, merged)
  }

  def buildSegments(grouped: Map[String, LineFields], names: Map[String, String],
                    comps: Map[String, (String, ComponentBase)]) = {
    var failed = false
    val result = names.keys.toList.sorted.foldLeft(sci.ListMap[String, Segment]())((map, ident) => {
      val compList = grouped.get(ident) match {
        case Some(list) => list.foldLeft(List[SegmentComponent]())((acc, line) =>
          try {
            val repeats =
              if (line(4) == "Y") {
                if (line(5).length == 0) 0
                else line(5).toInt
              } else 1
            val (nmtext, comp) = comps(line(2))
            val name = if (nmtext.isEmpty) None else Some(nmtext)
            comp match {
              case e: Element   => ElementComponent(e, name, "", line(1).toInt, convertUsage(line(3)), repeats) :: acc
              case c: Composite => CompositeComponent(c, name, "", line(1).toInt, convertUsage(line(3)), repeats) :: acc
            }
          } catch {
            case e: Exception =>
              println(s"ERROR: processing segment $ident ${line(1)} got exception $e")
              failed = true
              acc
          })
        case None => Nil
      }
      map + (ident -> new Segment(ident, names(ident), compList.reverse, Nil))
    })
    if (failed) throw new RuntimeException("Processing aborted")
    result
  }

  def buildStructures(grouped: Map[String, LineFields], segs: Map[String, Segment], version: EdiSchemaVersion) = {
    @tailrec
    def zeroPad(value: String, length: Int): String = if (value.length < length) zeroPad("0" + value, length) else value
    def buildGroup(id: String, lines: LineFields, close: String, use: Usage, count: Int, choice: Boolean): (LineFields, StructureComponent) = {
      val (remain, nested) = buildr(lines, Nil)
      remain match {
        case close :: t =>
          val ident = if (id.nonEmpty) id else nested.head.ident
          val group =
            if (nested.size == 1) {
              val comp = nested.head
              val mrguse = (use, comp.usage) match {
                case (OptionalUsage, _) | (_, OptionalUsage) => OptionalUsage
                case _                                       => use
              }
              val mrgcnt = (count, comp.count) match {
                case (0, _) | (_, 0) => 0
                case _               => count
              }
              comp match {
                case r: ReferenceComponent => new ReferenceComponent(r.segment, r.position, mrguse, mrgcnt)
                case g: GroupComponent     => new GroupComponent(ident, mrguse, 0, g.seq, None, Nil, ch = choice)
              }
            } else new GroupComponent(ident, use, count, StructureSequence(true, nested), None, Nil, ch = choice)
          (t, group)
        case _ => throw new IllegalArgumentException(s"Missing expected segment group close '$close'")
      }
    }
    def buildr(remain: LineFields, comps: List[StructureComponent]): (LineFields, List[StructureComponent]) =
      remain match {
        case h :: t => {
          val code = h(2)
          segs.get(code) match {
            case Some(seg) => {
              val segpos = new DefinedPosition(0, zeroPad(h(1), 2))
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
                val (rest, group) = buildGroup(h(3), t, "}", MandatoryUsage, 0, false)
                buildr(rest, group :: comps)
              }
              case "[" => {
                val (rest, group) = buildGroup(h(3), t, "]", OptionalUsage, 1, false)
                buildr(rest, group :: comps)
              }
              case "[{" => {
                val (rest, group) = buildGroup(h(3), t, "}]", OptionalUsage, 0, false)
                buildr(rest, group :: comps)
              }
              case "<" => {
                val (rest, group) = buildGroup(h(3), t, "}", MandatoryUsage, 0, true)
                buildr(rest, group :: comps)
              }
              case "|"                    => buildr(t, comps)
              case "}" | "]" | "}]" | ">" => (remain, comps.reverse)
              case _                      => throw new IllegalArgumentException(s"Unknown segment ${h(2)}")
            }
          }
        }
        case _ => (Nil, comps.reverse)
      }

    grouped.foldLeft(List[Structure]()) {
      case (list, (ident, lines)) => {
        Structure(ident, ident, None, Some(StructureSequence(false, buildr(lines, Nil)._2)), None, None, version)
      } :: list
    }
  }

  /**
   * Builds schemas from HL7 table data and outputs the schemas in YAML form. The arguments are 1) path to the
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
    hl7dir.listFiles.filter { x => true || x.getAbsoluteFile().getName.contains("2_3_1") }.foreach(version => {
      println(s"Processing ${version.getName}")
      val vernum = version.getName.drop(1).replace('_', '.')
      val schemaVersion = EdiSchemaVersion(HL7, vernum)

      // comp_no, description, table_id, data_type_code, data_structure
      var compDetails = nameMap(fileInput(version, componentDetails))
      // NJI compDetails = compDetails.+(("9999", Array("9999","varies","0","ST","ST")))
      // data_structure, description, data_type_code, repeating, elementary
      val dataStructs = lineList(fileInput(version, dataStructureNames))
      dataStructs.++(Array("varies", "var", "varies", "0", "1"))
      val structNames = dataStructs.foldLeft(Map[String, String]())((acc, line) => acc + (line(0) -> line(1)))
      // (data_structure, seq_no), comp_no, table_id, min_length, max_length, req_opt
      val groupedDataStructs = lineList(fileInput(version, dataStructureComponents)) /* NJI .+:(Array("varies","1","9999","0","","0","O")) */ .reverse.groupBy { _(0) }

      // build element definitions for elementary components
      val missingComps = scm.Set[String]()
      val elems = dataStructs.filter { _(4) == "1" }.foldLeft(List[Element]())((acc, line) => {
        val ident = line(0)
        groupedDataStructs.get(ident) match {
          case Some(list) =>
            if (list.length > 1) println(s"WARNING: Ignoring multiple components for elementary data structure $ident")
            val compline = list.head
            val mintext = compline(4)
            val minlen = if (mintext.length > 0) mintext.toInt else 1
            val maxtext = compline(5)
            val maxlen = if (maxtext.length > 0) maxtext.toInt else 0
            Element(line(0), line(1), HL7Support.buildType(line(2), minlen, if (maxlen == 0) 9999 else maxlen)) :: acc
          case None =>
            missingComps += ident
            Element(ident, "", HL7Support.buildType(ident, 1, 9999)) :: acc
        }
      }).sortBy { _.ident }
      val elemDefs = elems.foldLeft(sci.ListMap[String, Element]())((map, elem) => map + (elem.ident -> elem))
      if (missingComps.nonEmpty) println(s"WARNING: Missing components for data structures: $missingComps")
      //      println("Generated element definitions:")
      //      elemDefs.keys.foreach { ident => println(ident) }
      //      val simpleComps = compDetails.filter { case (ident, line) => elemDefs.contains(line(3)) }
      val compMap = buildComposites(groupedDataStructs, structNames, compDetails, elemDefs)
      if (false) {
        compMap.foreach {
          case (key, elem: Element)   => println(s"$key => ${elem.name} (element)")
          case (key, comp: Composite) => println(s"$key => ${comp.name} (composite with ${comp.components.length} components)")
        }
      }

      // data_item, description, data_structure, min_length, max_length
      val elemMap = lineList(fileInput(version, dataElements)).foldLeft(Map[String, (String, ComponentBase)]())((acc, line) =>
        compMap.get(line(2)) match {
          case Some(c)                  => acc + (line(0) -> (line(1), c))
          case None if (line(2) == "-") => acc + (line(0) -> (line(1), compMap("varies")))
          case _ =>
            println(s"WARNING: Failed lookup for ident ${line(2)}")
            acc
        })

      // seg_code, seq_no, data_item, req_opt, repetitional, repetitions
      val groupedSegStructs = lineList(fileInput(version, segmentStructures)).reverse.groupBy { _(0) }
      // seg_code, description, visible
      val segNames = lineList(fileInput(version, segmentNames)).filter { line => (line(2) == "1") }.
        foldLeft(Map[String, String]()) { (acc, line) => acc + (line(0) -> line(1)) }
      val segments = buildSegments(groupedSegStructs, segNames, elemMap)
      if (false) {
        println("Segments:")
        segments.foreach {
          case (key, comp) =>
            println(s" $key => ${comp.name} with ${comp.components.size} top-level components")
        }
      }
      val compDefs = compMap.keys.toList.sorted.foldLeft(sci.ListMap[String, Composite]()) { (acc, key) =>
        compMap(key) match {
          case c: Composite => acc + (key -> c)
          case _            => acc
        }
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

      if (true) {
        processStructures(groupedMsgStructs)
      }
    })
  }

  // --------------------------------------------------------

  def processStructures(groupedMsgStructs: Map[String, LineFields]): Unit = {
    /**
     *
     */
    var structs = new HashMap[String, List[Line]]()
    var structsMap = new HashMap[String, Message]()
    for ((messageType, lines) <- groupedMsgStructs) {
      // print(messageType)
      //print(lines)
      // 
      for (fields <- lines) {
        // print(fields)
        val x = Line(fields);
        // println("    " + x)
        if (!structs.contains(fields(0)))
          structs.put(fields(0), List())
        structs.put(fields(0), structs(fields(0)).++(List(x)))
      }
    }

    // for each key (message_type, event_id) get ordered list of lines and construct the structure from them
    for ((key, value) <- structs) {
      // println(key)
      // println(value)
      val structure = constructStructure(value)
      structsMap.put(key, structure)
    }

    // compute similarities/differences
    var mapForAllVersion = new HashMap[String, String]
    var mapForEachVersion = new HashMap[String, String]

    // display results

  }

  /**
   *
   */
  def compareStructures() {

  }

  /**
   * Construct a structure from a list of lines
   */
  def constructStructure(lines: List[Line]): Message = {
    var message = new Message(lines(0).code, MutableList())
    var line: Line = null
    var linesRest = lines
    // println(lines)
    while (!linesRest.isEmpty) {
      line = linesRest.head
      linesRest = linesRest.tail
      // println(line)
      if (groupStart(line.segmentName)) {
        val (l, sG) = constructSegmentGroupStructure(line.group, line.segmentName.contains("{"), line.segmentName.contains("["), linesRest)
        linesRest = l
        message.add(sG)
      } else
        message.add(new HL7Segment(line.segmentName, "1".equals(line.cardinality), "1".equals(line.optional)))
    }
    println(message)
    message
  }

  def constructSegmentGroupStructure(name: String, cardinality: Boolean, optional: Boolean, lines: List[Line]): (List[Line], HL7SegmentGroup) = {
    var result = new HL7SegmentGroup(name, cardinality, optional)
    var line: Line = null
    var linesRest = lines
    while (!linesRest.isEmpty) {
      line = linesRest.head
      linesRest = linesRest.tail
      if (groupStart(line.segmentName)) {
        val (l, sG) = constructSegmentGroupStructure(line.group, line.segmentName.contains("{"), line.segmentName.contains("["), linesRest)
        linesRest = l
        result.add(sG)
      } else if (groupEnd(line.segmentName)) {
        return (linesRest, result)
      } else {
        result.add(new HL7Segment(line.segmentName, "1".equals(line.cardinality), "1".equals(line.optional)))
      }
    }
    (linesRest, result)
  }

  def groupStart(segmentName: String) = {
    "[".equals(segmentName) || "{".equals(segmentName) || "[{".equals(segmentName)
  }

  def groupEnd(segmentName: String) = {
    "]".equals(segmentName) || "}".equals(segmentName) || "}]".equals(segmentName)
  }
}

object Line {
  def apply(fields: Array[String]): Line = {
    val code = fields(0)
    val position = fields(1)
    val messageType = fields(2)
    val group = fields(3)
    val cardinality = fields(4)
    val optional = fields(5)
    new Line(code, position, messageType, group, cardinality, optional)
  }
}

class Line(val code: String, position: String, val segmentName: String, val group: String, val cardinality: String, val optional: String) {
  override def toString(): String = "(" + code + ", " + position + ", " + segmentName + ", " + group + ", " + cardinality + ", " + optional + ")";
}

/**
 * Base class for HL7 Segment
 */
class HL7SegmentBase {

}

class HL7Segment(val name: String, cardinality: Boolean, optional: Boolean) extends HL7SegmentBase {
  override def toString(): String = {
    var result = name
    if (cardinality) result = "{ " + result + " }"
    if (optional) result = "[ " + result + " ]"
    result
  }
}

class HL7SegmentGroup(val name: String, cardinality: Boolean, optional: Boolean) extends HL7SegmentBase {
  val segments = new MutableList[HL7SegmentBase]()

  def add(segment: HL7SegmentBase): Unit = {
    segments += segment
  }

  override def toString(): String = {
    var result = name + "\n"
    for (segment <- segments) {
      result += "    " + segment.toString() + "\n"
    }
    if (cardinality) result = "{ " + result + "    }"
    if (optional) result = "[ " + result + "    ]"
    result = result + " " + name + "\n"
    result
  }
}

class Message(val code: String, val segments: MutableList[HL7SegmentBase]) {
  def add(segment: HL7SegmentBase): Unit = {
    segments += segment
  }

  override def toString(): String = {
    var result = ""
    result += "Message Structure: " + code + "\n=====>\n"
    for (segment <- segments) {
      result += "    " + segment + "\n"
    }
    result += "\n<===="
    result
  }

}
