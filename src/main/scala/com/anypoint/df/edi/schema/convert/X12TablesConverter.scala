package com.anypoint.df.edi.schema.convert

import scala.io.Source
import java.io.File
import java.io.InputStream
import scala.annotation.tailrec
import com.anypoint.df.edi.schema.EdiSchema._
import java.io.FileInputStream

/** Application to generate X12 transaction schemas from table data.
  */
object X12TablesConverter {

  // file names
  val transHeadersName = "SETHEAD.TXT"
  val transDetailsName = "SETDETL.TXT"
  val segmentHeadersName = "SEGHEAD.TXT"
  val segmentDetailsName = "SEGDETL.TXT"
  val compositeHeadersName = "COMHEAD.TXT"
  val compositeDetailsName = "COMDETL.TXT"
  val elementHeadersName = "ELEHEAD.TXT"
  val elementDetailsName = "ELEDETL.TXT"

  /** Split comma-separated quoted values from string into list. */
  def splitValues(s: String) = {

    /** Extract quoted substring from string. */
    def extract(start: Int, end: Int) =
      if (s.charAt(start) == '"' && s.charAt(end - 1) == '"') s.substring(start + 1, end - 1)
      else throw new IllegalArgumentException("missing required quote characters")

    /** Recursively split values from input string. */
    @tailrec
    def splitr(from: Int, acc: List[String]): List[String] =
      (from until s.length()).find(i => s.charAt(i) == ',') match {
        case Some(to) => splitr(to + 1, extract(from, to) :: acc)
        case None => extract(from, s.length()) :: acc
      }

    splitr(0, Nil).reverse
  }

  /** Get input stream for file in directory. */
  def fileInput(dir: File, name: String) = new FileInputStream(new File(dir, name))

  /** Accumulate the result of applying an operation to the lists of values for each line in input. Note that the result
    * will be in reversed order (if ordered).
    */
  def foldInput[T](in: InputStream, z: T)(f: (T, List[String]) => T) =
    Source.fromInputStream(in).getLines.foldLeft(z)((z, line) => f(z, splitValues(line)))

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
            case _ :: elem :: req :: Nil =>
              ElementComponent(elements(elem), elemNames(elem), convertUsage(req), 1) :: acc
            case _ => throw new IllegalStateException("wrong number of items in list")
          }).reverse
          map + (key -> Composite(key, compNames(key), comps))
        }
      })

  /** Build map from segment ids to definitions.
    */
  def defineSegments(elemNames: Map[String, String], elements: Map[String, Element], compNames: Map[String, String],
    composites: Map[String, Composite], segNames: Map[String, String], groups: ListOfKeyedLists) =
    groups.foldLeft(Map.empty[String, Segment])((map, pair) =>
      pair match {
        case (key, list) => {
          val comps = list.foldLeft(List[SegmentComponent]())((acc, vals) => vals match {
            case _ :: ident :: req :: reps :: Nil => {
              val count = reps.toInt
              val usage = convertUsage(req)
              if (elements.contains(ident)) ElementComponent(elements(ident), elemNames(ident), usage, count) :: acc
              else CompositeComponent(composites(ident), compNames(ident), usage, count) :: acc
            }
            case _ => throw new IllegalStateException("wrong number of items in list")
          }).reverse
          map + (key -> Segment(key, segNames(key), comps))
        }
      })
      
  /** Build map from transaction ids to definitions.
    */
  def defineTransactions(segments: Map[String, Segment], transNames: Map[String, String], groups: ListOfKeyedLists) = {
    type ComponentInfo = (Segment, Usage, Int, Int, Int, String)
    def convertMax(text: String) = if (text == ">1") 0 else text.toInt
    def splitArea(area: String, details: List[List[String]]): (List[List[String]], List[ComponentInfo]) = {
      @tailrec
      def splitr(remain: List[List[String]], acc: List[ComponentInfo]): (List[List[String]], List[ComponentInfo]) =
        remain match {
        case (areaid :: _ :: segid :: req :: max :: level :: repeat :: loopid :: Nil) :: tail if (area == areaid) => {
          val segment = segments(segid)
          val usage = convertUsage(req)
          val count = if (max == ">1") 0 else max.toInt
          splitr(tail, (segment, usage, count, level.toInt, repeat.toInt, loopid) :: acc)
          }
        case _ => (remain, acc.reverse)
      }
      splitr(details, Nil)
    }
    def splitDepth(infos: List[ComponentInfo], depth: Int): (List[ComponentInfo], List[ComponentInfo]) = {
      @tailrec
      def splitr(remain: List[ComponentInfo], acc: List[ComponentInfo]): (List[ComponentInfo], List[ComponentInfo]) =
        remain match {
        case head :: tail if (head._4 >= depth) => splitr(tail, head :: acc)
        case _ => (acc.reverse, remain)
      }
      splitr(infos, Nil)
    }
    def buildr(remain: List[ComponentInfo], depth: Int, acc: List[TransactionComponent]): List[TransactionComponent] =
      remain match {
      case (segment, usage, count, level, repeat, loop) :: tail =>
        if (depth == level) buildr(tail, depth, ReferenceComponent(segment, usage, count) :: acc)
        else if (depth > level) acc
        else acc
      case _ => acc.reverse
    }
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
          map + (number -> Element(number, convertDataType(typ), min.toInt, max.toInt))
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
  }
}