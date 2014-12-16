package com.anypoint.df.edi.schema

import java.io.FileInputStream
import java.io.PrintWriter
import java.io.InputStreamReader
import scala.annotation.tailrec
import EdiSchema._
import java.io.File

/** Print the code to create a segment or transaction. */
class SchemaDump(writer: PrintWriter) {

  val BREAK_LINE_LENGTH = 100

  class IndentingBuilder(initialIndent: Int) {
    val builder = new StringBuilder
    var depth = initialIndent
    var prepend = ""

    break

    def append(text: String) = builder ++= prepend ++= text

    def breakAppend(text: String) = {
      builder ++= prepend
      break
      builder ++= text
    }

    def indent() = depth += 1
    def outdent() = depth -= 1

    def break() = {
      writer.println(builder.toString)
      builder.clear
      (0 to depth) foreach { _ => (builder ++= "  ") }
    }

    def length() = builder.length
  }

  val builder = new IndentingBuilder(1)

  def genName(elem: Element) = s"elem${elem.ident}"

  def genName(comp: Composite) = s"comp${comp.ident}"

  def genName(segment: Segment) = s"seg${segment.ident}"

  /** Build element inline definition. */
  def inlineElement(elem: Element) =
    s"""Element("${elem.ident}", ${elem.dataType}, ${elem.minLength}, ${elem.maxLength})"""

  def inlineSegmentComponent(comp: SegmentComponent, elemrefs: Set[Element]) = comp match {
    case elem: ElementComponent => {
      val element = elem.element
      val ref = if (elemrefs.contains(element)) genName(element) else inlineElement(element)
      s"""ElementComponent($ref, "${elem.name}", ${elem.position}, ${elem.usage}, ${elem.count})"""
    }
    case comp: CompositeComponent =>
      s"""CompositeComponent(${genName(comp.composite)}, "${comp.name}", ${comp.position}, ${comp.usage}, ${comp.count})"""
  }

  /** Build segment components code. */
  def componentList(comps: List[SegmentComponent], elemrefs: Set[Element]): Unit = {
    comps.foreach(comp => {
      builder.breakAppend(inlineSegmentComponent(comp, elemrefs))
      builder.prepend = ", "
    })
    builder.prepend = ""
  }

  def defineElement(element: Element) = {
    builder.append(s"""val ${genName(element)} = Element("${element.ident}", ${element.dataType}, ${element.minLength}, ${element.maxLength})""")
    builder.break
  }

  /** Build composite definition code. */
  def defineComposite(comp: Composite, elemrefs: Set[Element]) = {
    builder.append(s"""val ${genName(comp)} = Composite("${comp.ident}", "${comp.name}", List[SegmentComponent](""")
    builder.indent
    componentList(comp.components, elemrefs)
    builder.append("))")
    builder.outdent
    builder.break
  }

  def defineSegment(segment: Segment, elemrefs: Set[Element]) = {
    builder.append(s"""val ${genName(segment)} = Segment("${segment.ident}", "${segment.name}", List[SegmentComponent](""")
    builder.indent
    componentList(segment.components, elemrefs)
    builder.append("))")
    builder.outdent
    builder.break
  }

  def componentList(comps: List[TransactionComponent]): Unit = {
    builder.prepend = ""
    comps.foreach(comp => {
      comp match {
        case ref: ReferenceComponent => {
          val text = s"ReferenceComponent(${genName(ref.segment)}, ${ref.use}, ${ref.count})"
          if (builder.length > BREAK_LINE_LENGTH) builder.breakAppend(text)
          else builder.append(text)
        }
        case group: GroupComponent => {
          builder.breakAppend(s"""GroupComponent("${group.ident}", ${group.use}, ${group.count}, List[TransactionComponent](""")
          builder.indent
          builder.break
          componentList(group.items)
          builder.append("))")
          builder.outdent
        }
      }
      builder.prepend = ", "
    })
    builder.prepend = ""
  }

  def toCode(trans: Transaction) = {
    def referencedSegments(comps: List[TransactionComponent]): Set[Segment] = {
      def referencer(comps: List[TransactionComponent], segments: Set[Segment]): Set[Segment] =
        comps.foldLeft(segments)((segs, comp) => comp match {
          case ref: ReferenceComponent => segs + ref.segment
          case group: GroupComponent => referencer(group.items, segs)
        })
      referencer(comps, Set[Segment]())
    }
    val segments = referencedSegments(trans.heading) ++ referencedSegments(trans.detail) ++
      referencedSegments(trans.summary)
    val elemrefs = segments.foldLeft(Map[Element, Int]())((acc, segment) =>
      segment.components.foldLeft(acc)((acc, comp) =>
        comp match {
          case elem: ElementComponent => acc + (elem.element -> (acc.getOrElse(elem.element, 0) + 1))
          case _ => acc
        }))
    val elemreps = elemrefs.filter(pair => pair._2 > 1).map(pair => pair._1).toSet
    elemreps.toList.sortBy(element => element.ident.toInt).foreach(element => defineElement(element))
    val composites = segments.foldLeft(Set[Composite]())((acc, segment) =>
      segment.components.foldLeft(acc)((acc, comp) =>
        comp match {
          case ccomp: CompositeComponent => acc + ccomp.composite
          case _ => acc
        }))
    composites.toList.sortBy(comp => comp.ident).foreach(comp => defineComposite(comp, elemreps))
    segments.toList.sortBy(segment => segment.ident).foreach(segment => defineSegment(segment, elemreps))
    builder.break
    builder.append(s"""val trans${trans.ident} = Transaction("${trans.ident}", "${trans.name}", "${trans.group}", List[TransactionComponent](""")
    builder.indent
    if (!trans.heading.isEmpty) {
      builder.break
      componentList(trans.heading)
      builder.break
    }
    builder.append("), List[TransactionComponent](")
    if (!trans.detail.isEmpty) {
      builder.break
      componentList(trans.detail)
      builder.break
    }
    builder.append("), List[TransactionComponent](")
    if (!trans.summary.isEmpty) {
      builder.break
      componentList(trans.summary)
      builder.break
    }
    builder.append("))")
    builder.outdent
    builder.break
  }
}

object SchemaToCode {

  /** Builds schemas from X12 table data and outputs the schemas in YAML form. The arguments are 1) path to the
    * directory containing the X12 table data files, and 2) path to the directory for the YAML output files. All
    * existing files are deleted from the output directory before writing any output files. Each transaction is output
    * as a separate file, with the transaction ID used as the file name (with extension ".yaml").
    */
  def main(args: Array[String]): Unit = {
    val yamlin = new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8")
    val schema = YamlReader.loadYaml(yamlin)
    val writer = new PrintWriter(System.out)
    val dumper = new SchemaDump(writer)
    schema.transactions.values.foreach(transact => dumper.toCode(transact))
    writer.flush
    writer.close
  }
}