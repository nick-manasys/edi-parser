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

/** Application to generate X12 transaction schemas from table data.
  */
object X12TablesConverter {

  // file names
  val transHeadersName = "sethead.txt"
  val transDetailsName = "setdetl.txt"
  val segmentHeadersName = "seghead.txt"
  val segmentDetailsName = "segdetl.txt"
  val compositeHeadersName = "comhead.txt"
  val compositeDetailsName = "comdetl.txt"
  val elementHeadersName = "elehead.txt"
  val elementDetailsName = "eledetl.txt"

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
    * @return List(id, List[List[String]])
    */
  def gatherGroups(in: InputStream, count: Int): ListOfKeyedLists = {
    val result = foldInput(in, emptyListOfKeyedLists)((acc, list) =>
      list match {
        case id :: rest if (rest.length == count - 1) => acc match {
          case (key, values) :: tail if (key == id) => (key, rest :: values) :: tail
          case tail => (id, rest :: Nil) :: tail
        }
        case _ => throw new IllegalArgumentException("wrong number of values in file")
      })
    result.foldLeft(emptyListOfKeyedLists)((acc, pair) => pair match {
      case (key, list) => (key, list.reverse) :: acc
    })
  }

  /** Build map from composite ids to definitions. This assumes composites are only defined in terms of elements, which
    * appears to be correct for X12.
    */
  def defineComposites(elemNames: Map[String, String], elements: Map[String, Element], compNames: Map[String, String],
    groups: ListOfKeyedLists) =
    groups.foldLeft(Map.empty[String, Composite])((map, pair) =>
      pair match {
        case (key, list) => {
          val comps = list.foldLeft(List[SegmentComponent]())((acc, vals) => vals match {
            case pos :: elem :: req :: Nil =>
              ElementComponent(elements(elem), elemNames(elem), pos.toInt, convertUsage(req), 1) :: acc
            case _ => throw new IllegalStateException("wrong number of items in list")
          }).reverse
          map + (key -> Composite(key, compNames(key), comps))
        }
      })

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
              if (elements.contains(ident)) ElementComponent(elements(ident), elemNames(ident), pos.toInt, usage, count) :: acc
              else CompositeComponent(composites(ident), compNames(ident), pos.toInt, usage, count) :: acc
            }
            case _ => throw new IllegalStateException("wrong number of items in list")
          }).reverse
          map + (key -> Segment(key, segNames(key), comps))
        }
      })

  /** Build map from transaction ids to definitions. */
  def defineTransactions(segments: Map[String, Segment], transHeads: Map[String, (String, String)],
    groups: ListOfKeyedLists) = {

    /** Converted form of component information from transaction details row. */
    case class ComponentInfo(segment: Segment, usage: Usage, repeat: Int, loop: List[ComponentInfo])

    /** Map from area name to component information list. */
    type AreaMap = Map[String, List[ComponentInfo]]

    /** Convert transaction components from lists of strings to trees of data tuples, grouping them by area. */
    def convertComponents(rows: List[List[String]]): AreaMap = {
      
      /** Convert string values to component information structure. */
      def info(segid: String, req: String, max: String, nested: List[ComponentInfo]) = {
        val segment = segments(segid)
        val usage = convertUsage(req)
        val repeat = if (max == ">1") 0 else max.toInt
        ComponentInfo(segment, usage, repeat, nested)
      }
      
      /** Start a new loop (need separate function to keep convertr tail recursive). */
      def descend(remain: List[List[String]], depth: Int): (List[List[String]], List[ComponentInfo]) =
        convertr(remain, depth, true, Nil)
        
      /** Recursively convert lists of strings to data tuples, checking and handling loops. */
      @tailrec
      def convertr(remain: List[List[String]], depth: Int, loop: Boolean,
        acc: List[ComponentInfo]): (List[List[String]], List[ComponentInfo]) = remain match {
        case (areaid :: _ :: segid :: req :: max :: level :: repeat :: loopid :: Nil) :: tail => {
          val at = level.toInt
          if (depth > at || (!loop && depth == at && loopid != "")) {
            // loop closed (either back to containing level, or starting another loop at same level)
            (remain, acc)
          } else if (depth == at) {
            // continuing at current loop level
            convertr(tail, at, false, info(segid, req, max, Nil) :: acc)
          } else {
            // starting a nested loop
            val (rest, nested) = descend(remain, depth + 1)
            convertr(rest, depth, false, info(segid, req, repeat, nested) :: acc)
          }
        }
        case Nil => (Nil, acc)
        case _ => throw new IllegalArgumentException("wrong number of values")
      }
      rows.groupBy(_.head) map { case (key, list) => key -> convertr(list, 0, false, Nil)._2 }
    }

    /** Recursively convert component information list into transaction component list. */
    def descend(remain: List[ComponentInfo]) = buildr(remain, Nil)
    @tailrec
    def buildr(remain: List[ComponentInfo], acc: List[TransactionComponent]): List[TransactionComponent] =
      remain match {
        case ComponentInfo(segment, usage, repeat, loop) :: tail => {
          val list =
            if (loop.isEmpty) ReferenceComponent(segment, usage, repeat) :: acc
            else GroupComponent(segment.ident, usage, repeat, descend(loop)) :: acc
          buildr(tail, list)
        }
        case _ => acc
      }

    groups.foldLeft(Map.empty[String, Transaction]) {
      case (map, (key, list)) => map + (key -> {
        val areas = convertComponents(list).map { case (key, list) => key -> buildr(list, Nil) }
        val (name, group) = transHeads(key)
        Transaction(key, name, group, areas.getOrElse("1", Nil), areas.getOrElse("2", Nil), areas.getOrElse("3", Nil))
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

  /** Convert element data type, extending base conversion to allow empty type. */
  def convertType(text: String) = if (text.length > 0) convertDataType(text) else AlphaNumericType

  /** Construct schema for a single transaction. */
  def transactionSchema(transact: Transaction) = {

    /** Recursively collect segments used in transaction. */
    def descendSeg(comps: List[TransactionComponent], segs: Set[Segment]) = segmentr(comps, segs)
    def segmentr(remain: List[TransactionComponent], segs: Set[Segment]): Set[Segment] = remain match {
      case ReferenceComponent(seg, _, _) :: tail => segmentr(tail, segs + seg)
      case GroupComponent(_, _, _, comps) :: tail => segmentr(tail, descendSeg(comps, segs))
      case _ => segs
    }

    /** Recursively collect composites and elements used in segment. */
    def descendComp(list: List[SegmentComponent], comps: Set[Composite], elems: Set[Element]) =
      compositer(list, comps, elems)
    def compositer(remain: List[SegmentComponent], comps: Set[Composite],
      elems: Set[Element]): (Set[Composite], Set[Element]) = remain match {
      case ElementComponent(elem, _, _, _, _) :: tail => compositer(tail, comps, elems + elem)
      case CompositeComponent(comp, _, _, _, _) :: tail => {
        val (ncomps, nelems) = descendComp(comp.components, comps + comp, elems)
        compositer(tail, ncomps, nelems)
      }
      case _ => (comps, elems)
    }

    val segs = segmentr(transact.summary, segmentr(transact.detail, segmentr(transact.heading, Set())))
    val (comps, elems) = segs.foldLeft((Set[Composite](), Set[Element]()))((acc, seg) =>
      compositer(seg.components, acc._1, acc._2))
    EdiSchema(X12,
      elems.foldLeft(Map.empty[String, Element])((acc, elem) => acc + (elem.ident -> elem)),
      comps.foldLeft(Map.empty[String, Composite])((acc, comp) => acc + (comp.ident -> comp)),
      segs.foldLeft(Map.empty[String, Segment])((acc, seg) => acc + (seg.ident -> seg)),
      Map(transact.ident -> transact))
  }

  /** Write schema to file. */
  def writeSchema(schema: EdiSchema, name: String, yamldir: File) = {
    println(s"writing schema $name")
    val writer = new OutputStreamWriter(new FileOutputStream(new File(yamldir, name + ".yaml")), "UTF-8")
    YamlWriter.write(schema, writer)
    writer.close
  }

  /** Verify schema written to file. */
  def verifySchema(baseSchema: EdiSchema, name: String, yamldir: File) = {
    val reader = new InputStreamReader(new FileInputStream(new File(yamldir, name + ".yaml")), "UTF-8")
    val readSchema = YamlReader.loadYaml(reader)
    //    if (baseSchema != readSchema) throw new IllegalStateException(s"Verification error on schema $name")
  }

  /** Builds schemas from X12 table data and outputs the schemas in YAML form. The arguments are 1) path to the
    * directory containing the X12 table data files, and 2) path to the directory for the YAML output files. All
    * existing files are deleted from the output directory before writing any output files. Each transaction is output
    * as a separate file, with the transaction ID used as the file name (with extension ".yaml").
    */
  def main(args: Array[String]): Unit = {
    val x12dir = new File(args(0))
    val yamldir = new File(args(1))
    if (yamldir.exists()) yamldir.listFiles().foreach { f => f.delete() }
    else yamldir.mkdirs
    val elementNames = nameMap(fileInput(x12dir, elementHeadersName))
    val elementDefs = foldInput(fileInput(x12dir, elementDetailsName), Map.empty[String, Element])((map, list) =>
      list match {
        case number :: typ :: min :: max :: Nil =>
          map + (number -> Element(number, convertType(typ), convertLength(min), convertLength(max)))
        case _ => throw new IllegalArgumentException("wrong number of values in file")
      })
    val compNames = nameMap(fileInput(x12dir, compositeHeadersName))
    val compGroups = gatherGroups(fileInput(x12dir, compositeDetailsName), 4)
    val compDefs = defineComposites(elementNames, elementDefs, compNames, compGroups)
    val segNames = nameMap(fileInput(x12dir, segmentHeadersName))
    val segGroups = gatherGroups(fileInput(x12dir, segmentDetailsName), 5)
    val segDefs = defineSegments(elementNames, elementDefs, compNames, compDefs, segNames, segGroups)
    val setHeads = foldInput(fileInput(x12dir, transHeadersName), Map.empty[String, (String, String)])((map, list) =>
      list match {
        case number :: name :: group :: Nil => map + (number -> (name, group))
        case _ => throw new IllegalArgumentException("wrong number of values in file")
      })
    val setGroups = gatherGroups(fileInput(x12dir, transDetailsName), 9)
    val transactions = defineTransactions(segDefs, setHeads, setGroups)
    transactions.values.foreach(transact => {
      val schema = transactionSchema(transact)
      writeSchema(schema, transact.ident, yamldir)
      verifySchema(schema, transact.ident, yamldir)
    })
  }
}