package com.mulesoft.flatfile.schema.convert

import java.io.{ File, FileInputStream, FileOutputStream, FileWriter, InputStream, InputStreamReader, OutputStreamWriter }

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source

import com.mulesoft.flatfile.lexical.X12Constants
import com.mulesoft.flatfile.schema.{ EdiSchema, EdiSchemaVersion, YamlReader, YamlWriter }
import com.mulesoft.flatfile.schema.EdiSchema._

/** Application to generate X12 structure schemas from table data.
  */
object X12TablesConverter {

  // YAML file extension
  val yamlExtension = ".esl"

  // file names
  val transHeadersName = "sethead.txt"
  val transDetailsName = "setdetl.txt"
  val segmentHeadersName = "seghead.txt"
  val segmentDetailsName = "segdetl.txt"
  val compositeHeadersName = "comhead.txt"
  val compositeDetailsName = "comdetl.txt"
  val elementHeadersName = "elehead.txt"
  val elementDetailsName = "eledetl.txt"
  
  val groupIdentSuffix = "_Loop"

  /** Split comma-separated quoted values from string into list. */
  def splitValues(s: String) = {
    def stripComma(remain: Seq[Char], acc: List[String]): List[String] =
      if (remain.isEmpty) acc reverse
      else if (remain.head == ',') splitQuotes(remain.tail, acc)
      else throw new IllegalArgumentException(s"missing expected comma after closing quote: $s")
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

  /** Generate map from id to name from an input with two columns. */
  def nameMap(in: InputStream) = foldInput(in, Map.empty[String, String])((map, list) =>
    list match {
      case number :: name :: Nil => map + (number -> name)
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
  def defineComposites(elemNames: Map[String, String], elements: Map[String, Element], compNames: Map[String, String],
    groups: ListOfKeyedLists) = {
    val list = groups.foldLeft(List[Composite]())((acc, pair) =>
      pair match {
        case (key, list) => {
          val comps = list.foldLeft(List[SegmentComponent]())((acc, vals) => vals match {
            case pos :: elem :: req :: Nil =>
              val position = pos.toInt
              val elemKey = X12.keyName(key, "", "", position)
              ElementComponent(elements(elem), None, elemKey, position, convertUsage(req), 1) :: acc
            case _ => throw new IllegalStateException("wrong number of items in list")
          }).reverse
          Composite(key, compNames(key), comps, Nil, 0) :: acc
        }
      }).sortBy { _.ident }
    list.foldLeft(ListMap[String, Composite]())((map, comp) => map + (comp.ident -> comp))
  }

  /** Build map from segment ids to definitions. */
  def defineSegments(elemNames: Map[String, String], elements: Map[String, Element], compNames: Map[String, String],
    composites: Map[String, Composite], segNames: Map[String, String], groups: ListOfKeyedLists) = {
    val list = groups.foldLeft(List[Segment]())((acc, pair) =>
      pair match {
        case (key, list) => {
          val comps = list.foldLeft(List[SegmentComponent]())((acc, vals) => vals match {
            case pos :: ident :: req :: reps :: Nil => {
              val count = reps.toInt
              val usage = convertUsage(req)
              val position = pos.toInt
              val compKey = X12.keyName(key, "", "", position)
              if (elements.contains(ident)) ElementComponent(elements(ident), None, compKey, position, usage, count) :: acc
              else CompositeComponent(composites(ident), None, compKey, position, usage, count) :: acc
            }
            case _ => throw new IllegalStateException("wrong number of items in list")
          }).reverse
          new Segment(key, segNames(key), comps, Nil) :: acc
        }
      }).sortBy { _.ident }
    list.foldLeft(ListMap[String, Segment]())((map, seg) => map + (seg.ident -> seg))
  }

  /** Build map from structure ids to definitions. */
  def defineStructures(segments: Map[String, Segment], transHeads: Map[String, (String, String)],
    groups: ListOfKeyedLists, excludeSegs: Set[Segment], version: EdiSchemaVersion) = {

    /** Converted form of component information from structure details row. */
    case class ComponentInfo(segment: Segment, seq: String, usage: Usage, repeat: Int, loopId: String, loop: List[ComponentInfo])

    /** Map from area name to component information list. */
    type AreaMap = Map[String, List[ComponentInfo]]

    /** Convert structure components from lists of strings to trees of data tuples, grouping them by area. */
    def convertComponents(rows: List[List[String]]): AreaMap = {

      /** Convert string values to component information structure. */
      def info(segid: String, seq: String, req: String, max: String, loopId: String, nested: List[ComponentInfo]) = {
        val segment = segments(segid)
        val usage = convertUsage(req)
        val repeat = if (max == ">1") 0 else max.toInt
        ComponentInfo(segment, seq, usage, repeat, loopId, nested)
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
            convertr(tail, at, false, info(segid, seq, req, max, loopid, Nil) :: acc)
          } else {
            // starting a nested loop
            val (rest, nested) = descend(remain, depth + 1)
            convertr(rest, depth, false, info(segid, seq, req, repeat, loopid, nested) :: acc)
          }
        }
        case Nil => (Nil, acc)
        case _ => throw new IllegalArgumentException("wrong number of values")
      }

      rows.groupBy(_.head) map { case (key, list) => key -> convertr(list, 0, false, Nil)._2 }
    }

    /** Recursively convert component information list into structure component list. */
    def buildComps(table: Int, infos: List[ComponentInfo]) = {
      def descend(remain: List[ComponentInfo]) = buildr(remain, Nil)
      @tailrec
      def buildr(remain: List[ComponentInfo], acc: List[StructureComponent]): List[StructureComponent] =
        remain match {
          case ComponentInfo(segment, seq, usage, repeat, loopId, loop) :: tail =>
            if (loop.isEmpty) {
              val position = new DefinedPosition(table, seq)
              if (segment.ident == "LS") acc match {
                case (group: GroupComponent) :: (leref: ReferenceComponent) :: t => {
                  val grpid = group.ident.stripSuffix(groupIdentSuffix)
                  val wrap = LoopWrapperComponent(segment, leref.segment, position, leref.position,
                    OptionalUsage, grpid, group)
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
              else {
                val seq = StructureSequence(true, comps)
                buildr(tail, GroupComponent(loopId + groupIdentSuffix, usage, repeat, seq, None, Nil) :: acc)
              }
            }
          case _ => acc
        }
      buildr(infos, Nil)
    }
    
    def optSeq(optlist: Option[List[StructureComponent]]) = optlist match {
      case Some(list) => if (list.isEmpty) None else Some(StructureSequence(false, list))
      case None => None
    }

    groups.foldLeft(Map.empty[String, Structure]) {
      case (map, (key, list)) => map + (key -> {
        val tables = convertComponents(list).map {
          case (key, list) => {
            val table = key.toInt
            table -> buildComps(table, list)
          }
        }
        val (name, group) = transHeads(key)
        Structure(key, name, Some(group), optSeq(tables.get(1)), optSeq(tables.get(2)), optSeq(tables.get(3)), version)
      })
    }
  }

  /** Convert length value, which may use exponential notation. */
  def convertLength(text: String) = {
    val split = text.indexOf("E+")
    if (split < 0) text.toInt
    else {
      val value: Long = text.substring(0, split).toInt
      val exponent: Long = text.substring(split + 2).toInt
      val result = value * 10 ^ exponent
      if (result > Int.MaxValue) Int.MaxValue
      else result.asInstanceOf[Int]
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

  /** Builds schemas from X12 table data and outputs the schemas in YAML form. The arguments are 1) path to the
    * directory containing the X12 table data files, and 2) path to the directory for the YAML output files. All
    * existing files are deleted from the output directory before writing any output files. Each structure is output
    * as a separate file, with the structure ID used as the file name (with extension ".yaml").
    */
  def main(args: Array[String]): Unit = {
    val x12dir = new File(args(0))
    println(x12dir.getAbsolutePath)
    val yamldir = new File(args(1))
    if (yamldir.exists) yamldir.listFiles.foreach { version =>
      if (version.exists && version.isDirectory) {
        version.listFiles.foreach { f => f.delete }
        version.delete
      }
    }
    else yamldir.mkdirs
    val yamlrdr = new YamlReader()
    x12dir.listFiles.foreach (version => {
      println(s"Processing ${version.getName}")
      val schemaVersion = EdiSchemaVersion(X12, version.getName)
      val elemNames = nameMap(fileInput(version, elementHeadersName))
      val elemList = foldInput(fileInput(version, elementDetailsName), List.empty[Element])((acc, detail) =>
        detail match {
          case number :: typ :: min :: max :: Nil =>
            val usetyp = if (typ.isEmpty()) "AN" else typ
            val format = X12Constants.buildType(usetyp, convertLength(min), convertLength(max))
            Element(number, elemNames(number), format) :: acc
          case _ => throw new IllegalArgumentException("wrong number of values in file")
        }).sortBy { _.ident }
      val elemDefs = elemList.foldLeft(ListMap[String, Element]()) { (acc, e) => acc + (e.ident -> e) }
      val compNames = nameMap(fileInput(version, compositeHeadersName))
      val compGroups = gatherGroups(compositeDetailsName, fileInput(version, compositeDetailsName), 4, None)
      val compDefs = defineComposites(elemNames, elemDefs, compNames, compGroups)
      val segNames = nameMap(fileInput(version, segmentHeadersName))
      val segGroups = gatherGroups(segmentDetailsName, fileInput(version, segmentDetailsName), 5, Some("1"))
      val segDefs = defineSegments(elemNames, elemDefs, compNames, compDefs, segNames, segGroups)
      val setHeads = foldInput(fileInput(version, transHeadersName), Map.empty[String, (String, String)])((map, list) =>
        list match {
          case number :: name :: group :: Nil => map + (number -> (name, group))
          case _ => throw new IllegalArgumentException("wrong number of values in file")
        })
      val setGroups = gatherGroups(transDetailsName, fileInput(version, transDetailsName), 9, None)
      val vnum = version.getName
      val baseSchema = EdiSchema(schemaVersion, elemDefs, compDefs, segDefs, Map[String, Structure]())
      val outdir = new File(yamldir, version.getName)
      outdir.mkdirs
      writeSchema(baseSchema, "basedefs", Array(), outdir)
      verifySchema(baseSchema, "basedefs", outdir, yamlrdr)
      val binSegs = segDefs.get("BIN").toSet ++ segDefs.get("BDS").toSet
      val structures = defineStructures(segDefs, setHeads, setGroups, binSegs, schemaVersion).values.filter {
        trans => binSegs.forall { seg => !trans.segmentsUsed.contains(seg) }
      }.toSeq.sortBy { _.ident }
      val listWriter = new FileWriter(new File(outdir, "structures.txt"))
      structures foreach (transact => {
        val schema = EdiSchema(schemaVersion, Map[String, Element](), Map[String, Composite](), Map[String, Segment](),
          Map(transact.ident -> transact))
        writeSchema(schema, transact.ident, Array(s"/x12/${version.getName}/basedefs$yamlExtension"), outdir)
        listWriter.write(transact.ident + '\n')
      })
      listWriter.close
    })
  }
}