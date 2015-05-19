package com.anypoint.df.edi.schema.convert

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.io.InputStreamReader
import java.io.OutputStreamWriter

import scala.annotation.tailrec
import scala.io.Source

import com.anypoint.df.edi.lexical.EdiConstants
import com.anypoint.df.edi.lexical.EdiConstants.DataType
import com.anypoint.df.edi.schema.EdiSchema
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.schema.YamlReader
import com.anypoint.df.edi.schema.YamlWriter

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

  /** Convert input to list of arrays of strings (list reverse ordered). */
  def lineList(in: InputStream) = foldInput(in, List[Array[String]]()) ((list, line) => line.toArray :: list)

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

  /** Build map from composite ids to definitions. This assumes composites are only defined in terms of elements, which
    * appears to be correct for X12.
    */
  //  def defineComposites(elemNames: Map[String, String], elements: Map[String, Element], compNames: Map[String, String],
  //    groups: ListOfKeyedLists) =
  //    groups.foldLeft(Map.empty[String, Composite])((map, pair) =>
  //      pair match {
  //        case (key, list) => {
  //          val comps = list.foldLeft(List[SegmentComponent]())((acc, vals) => vals match {
  //            case pos :: elem :: req :: Nil =>
  //              val position = pos.toInt
  //              ElementComponent(elements(elem), None, X12.keyName(key, position), position, convertUsage(req), 1) :: acc
  //            case _ => throw new IllegalStateException("wrong number of items in list")
  //          }).reverse
  //          map + (key -> Composite(key, compNames(key), comps, Nil))
  //        }
  //      })

  /** Build map from segment ids to definitions. */
  def defineSegments(elemNames: Map[String, String], elements: Map[String, Element], compNames: Map[String, String],
    composites: Map[String, Composite], segNames: Map[String, String], groups: ListOfKeyedLists) =
    groups.foldLeft(Map.empty[String, Segment])((map, pair) =>
      pair match {
        case (key, list) => {
          val comps = list.foldLeft(List[SegmentComponent]())((acc, vals) => vals match {
            case pos :: ident :: req :: reps :: Nil => {
              val count = reps.toInt
              val usage = convertUsage(req)
              val position = pos.toInt
              val ckey = X12.keyName(key, position)
              if (elements.contains(ident)) ElementComponent(elements(ident), None, ckey, position, usage, count) :: acc
              else CompositeComponent(composites(ident), None, ckey, position, usage, count) :: acc
            }
            case _ => throw new IllegalStateException("wrong number of items in list")
          }).reverse
          map + (key -> Segment(key, segNames(key), comps, Nil))
        }
      })

  /** Build map from transaction ids to definitions. */
  def defineTransactions(segments: Map[String, Segment], transHeads: Map[String, (String, String)],
    groups: ListOfKeyedLists, excludeSegs: Set[Segment]) = {

    /** Converted form of component information from transaction details row. */
    case class ComponentInfo(segment: Segment, seq: String, usage: Usage, repeat: Int, loop: List[ComponentInfo])

    /** Map from area name to component information list. */
    type AreaMap = Map[String, List[ComponentInfo]]

    /** Convert transaction components from lists of strings to trees of data tuples, grouping them by area. */
    def convertComponents(rows: List[List[String]]): AreaMap = {

      /** Convert string values to component information structure. */
      def info(segid: String, seq: String, req: String, max: String, nested: List[ComponentInfo]) = {
        val segment = segments(segid)
        val usage = convertUsage(req)
        val repeat = if (max == ">1") 0 else max.toInt
        ComponentInfo(segment, seq, usage, repeat, nested)
      }

      /** Start a new loop (need separate function to keep convertr tail recursive). */
      def descend(remain: List[List[String]], depth: Int): (List[List[String]], List[ComponentInfo]) =
        convertr(remain, depth, true, Nil)

      /** Recursively convert lists of strings to data tuples, checking and handling loops.
        * @param remain list of lists of strings from input lines still to be processed
        * @param depth current nesting depth, corresponding to sixth value in input line
        * @param loop first line of loop flag, set on recursive call to collect components in the loop
        * @param acc information for components representing lines processed so far
        * @return (list of strings still to be processed, list of components at level in reverse order)
        */
      @tailrec
      def convertr(remain: List[List[String]], depth: Int, loop: Boolean,
        acc: List[ComponentInfo]): (List[List[String]], List[ComponentInfo]) = remain match {
        case (areaid :: seq :: segid :: req :: max :: level :: repeat :: loopid :: Nil) :: tail => {
          val at = level.toInt
          if (depth > at || (!loop && depth == at && loopid != "")) {
            // loop closed (either back to containing level, or starting another loop at same level)
            (remain, acc)
          } else if (depth == at) {
            // continuing at current loop level
            convertr(tail, at, false, info(segid, seq, req, max, Nil) :: acc)
          } else {
            // starting a nested loop
            val (rest, nested) = descend(remain, depth + 1)
            convertr(rest, depth, false, info(segid, seq, req, repeat, nested) :: acc)
          }
        }
        case Nil => (Nil, acc)
        case _ => throw new IllegalArgumentException("wrong number of values")
      }

      rows.groupBy(_.head) map { case (key, list) => key -> convertr(list, 0, false, Nil)._2 }
    }

    /** Recursively convert component information list into transaction component list. */
    def buildComps(table: Int, infos: List[ComponentInfo]) = {
      def descend(remain: List[ComponentInfo]) = buildr(remain, Nil)
      @tailrec
      def buildr(remain: List[ComponentInfo], acc: List[TransactionComponent]): List[TransactionComponent] =
        remain match {
          case ComponentInfo(segment, seq, usage, repeat, loop) :: tail =>
            if (loop.isEmpty) {
              val position = SegmentPosition(table, seq)
              if (segment.ident == "LS") acc match {
                case (group: GroupComponent) :: (leref: ReferenceComponent) :: t => {
                  val wrap = LoopWrapperComponent(segment, leref.segment, position, leref.position,
                    OptionalUsage, group.leadSegmentRef.segment.ident, group)
                  buildr(tail, wrap :: t)
                }
                case _ => throw new IllegalStateException("Malformed LS/LE loop")
              }
              else if (excludeSegs.contains(segment)) {
                if (usage == MandatoryUsage) Nil
                else buildr(tail, acc)
              } else buildr(tail, ReferenceComponent(segment, position, usage, repeat) :: acc)
            } else {
              val comps = descend(loop)
              if (comps.isEmpty) buildr(tail, acc)
              else buildr(tail, GroupComponent(segment ident, usage, repeat, comps, None, Nil) :: acc)
            }
          case _ => acc
        }
      buildr(infos, Nil)
    }

    groups.foldLeft(Map.empty[String, Transaction]) {
      case (map, (key, list)) => map + (key -> {
        val tables = convertComponents(list).map {
          case (key, list) => {
            val table = key.toInt
            table -> buildComps(table, list)
          }
        }
        val (name, group) = transHeads(key)
        Transaction(key, name, Some(group), tables.getOrElse(1, Nil), tables.getOrElse(2, Nil), tables.getOrElse(3, Nil))
      })
    }
  }

  /** Convert element data type, extending base conversion to allow empty type. */
  def convertType(text: String) = if (text.length > 0) EdiConstants.toX12Type(text) else DataType.ALPHANUMERIC

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
  def buildComposite(lines: List[Array[String]], names: Map[String, String], elems: Map[String, Array[String]], comps: Map[String, ComponentBase]) = {
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
  def buildComposites(grouped: Map[String, List[Array[String]]], names: Map[String, String],
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

  def buildSegments(grouped: Map[String, List[Array[String]]], names: Map[String, String],
    comps: Map[String, ComponentBase]) = {
    names.keys.foldLeft(Map[String, Segment]())((map, ident) => {
      val compList = grouped.get(ident) match {
        case Some(list) => list.foldLeft(List[SegmentComponent]())((acc, line) =>
          {
            val repeats = if (line(4) == "Y") line(5).toInt else 1
            comps(line(2)) match {
              case Element(id, nm, typ, mn, mx) => {
                val max = if (line(5).length > 0) line(5).toInt else mx
                val elem = Element(id, nm, typ, mn, max)
                ElementComponent(elem, None, "", line(1).toInt, convertUsage(line(3)), 1) :: acc
              }
              case c: Composite => CompositeComponent(c, None, "", line(1).toInt, convertUsage(line(3)), 1) :: acc
            }
          })
        case None => Nil
      }
      map + (ident -> Segment(ident, names(ident), compList.reverse, Nil))
    })
  }

  /** Builds schemas from HL7 table data and outputs the schemas in YAML form. The arguments are 1) path to the
    * directory containing the HL7 table data files, and 2) path to the directory for the YAML output files. All
    * existing files are deleted from the output directory before writing any output files. Each message is output
    * as a separate file, with the transaction ID used as the file name (with extension ".yaml").
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

      // comp_no, description, table_id, data_type_code, data_structure
      val compDetails = nameMap(fileInput(version, componentDetails))
      // data_structure, description, data_type_code, repeating, elementary
      val dataStructs = lineList(fileInput(version, dataStructureNames))
      val structNames = dataStructs.foldLeft(Map[String, String]())((acc, line) => acc + (line(0) -> line(1)))

      // build element definitions for elementary components
      val elemDefs = dataStructs.filter { _(4) == "1" }.foldLeft(Map.empty[String, Element])((map, line) =>
        map + (line(0) -> Element(line(0), line(1), EdiConstants.toHL7Type(line(2)), 0, 0)))
      println("Generated element definitions:")
      elemDefs.keys.foreach { ident => println(ident) }
      val simpleComps = compDetails.filter { case (ident, line) => elemDefs.contains(line(3)) }
      // (data_structure, seq_no), comp_no, table_id, min_length, max_length, req_opt
      val groupedStructs = lineList(fileInput(version, dataStructureComponents)).reverse.groupBy { _(0) }
      val compMap = buildComposites(groupedStructs, structNames, compDetails, elemDefs)
      compMap.foreach {
        case (key, elem: Element) => println(s"$key => ${elem.name} (element)")
        case (key, comp: Composite) => println(s"$key => ${comp.name} (composite with ${comp.components.length} components)")
      }

      // data_item, description, data_structure, min_length, max_length
      val elemMap = lineList(fileInput(version, dataElements)).foldLeft(Map[String, ComponentBase]())((acc, line) =>
        compMap.get(line(2)) match {
          case Some(c) => acc + (line(0) -> c)
          case None if (line(2) == "-") => acc + (line(0) -> compMap("varies"))
          case _ => throw new IllegalStateException(s"failed lookup for ident ${line(2)}")
        })

      // seg_code, seq_no, data_item, req_opt, repetitional, repetitions
      val groupedSegStructs = lineList(fileInput(version, segmentStructures)).reverse.groupBy { _(0) }
      // seg_code, description, visible
      val segNames = lineList(fileInput(version, segmentNames)).filter { line => (line(2) == "1" && line(0) != "Hxx") }.
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
      val baseSchema = EdiSchema(HL7, version.getName, elemDefs, compDefs, segments, Map[String, Transaction]())
      val outdir = new File(yamldir, version.getName)
      outdir.mkdirs
      writeSchema(baseSchema, "basedefs", Array(), outdir)
      verifySchema(baseSchema, "basedefs", outdir, yamlrdr)

      //      simpleIdents.map { x => ??? }
      //      println(simpleIdents)
      //      println(groupedStructs.keys.filter { !simpleIdents.contains(_) })
      //      lineList(fileInput(version, dataStructureComponents)).filter { line => line(5).length > 0 }.foreach(line => println(line(0)))

      //      val elemDefs = foldInput(fileInput(version, elementDetailsName), Map.empty[String, Element])((map, list) =>
      //        list match {
      //          case number :: typ :: min :: max :: Nil =>
      //            map + (number -> Element(number, elemNames(number), convertType(typ), convertLength(min), convertLength(max)))
      //          case _ => throw new IllegalArgumentException("wrong number of values in file")
      //        })
      //      val compNames = nameMap(fileInput(version, compositeHeadersName))
      //      val compGroups = gatherGroups(compositeDetailsName, fileInput(version, compositeDetailsName), 4, None)
      //      val compDefs = defineComposites(elemNames, elemDefs, compNames, compGroups)
      //      val segNames = nameMap(fileInput(version, segmentHeadersName))
      //      val segGroups = gatherGroups(segmentDetailsName, fileInput(version, segmentDetailsName), 5, Some("1"))
      //      val segDefs = defineSegments(elemNames, elemDefs, compNames, compDefs, segNames, segGroups)
      //      val setHeads = foldInput(fileInput(version, transHeadersName), Map.empty[String, (String, String)])((map, list) =>
      //        list match {
      //          case number :: name :: group :: Nil => map + (number -> (name, group))
      //          case _ => throw new IllegalArgumentException("wrong number of values in file")
      //        })
      //      val setGroups = gatherGroups(transDetailsName, fileInput(version, transDetailsName), 9, None)
      //      val vnum = version.getName
      //      val baseSchema = EdiSchema(X12, vnum, elemDefs, compDefs, segDefs, Map[String, Transaction]())
      //      val outdir = new File(yamldir, version.getName)
      //      outdir.mkdirs
      //      writeSchema(baseSchema, "basedefs", Array(), outdir)
      //      verifySchema(baseSchema, "basedefs", outdir, yamlrdr)
      //      val binSegs = segDefs.get("BIN").toSet ++ segDefs.get("BDS").toSet
      //      val transactions = defineTransactions(segDefs, setHeads, setGroups, binSegs).values.filter {
      //        trans => binSegs.forall { seg => !trans.segmentsUsed.contains(seg) }
      //      }
      //      transactions foreach (transact => {
      //        val schema = EdiSchema(X12, vnum, Map[String, Element](), Map[String, Composite](), Map[String, Segment](),
      //          Map(transact.ident -> transact))
      //        writeSchema(schema, transact.ident, Array(s"/x12/${version.getName}/basedefs$yamlExtension"), outdir)
      //      })
    })
  }
}