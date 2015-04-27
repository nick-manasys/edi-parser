package com.anypoint.df.edi.schema.tools

import java.io.FileInputStream
import java.io.PrintWriter
import java.io.InputStreamReader
import java.io.File
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.schema.YamlReader
import scala.annotation.migration

/** Print the code to create a segment or transaction. Note that this does not handle variant groups. */
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

    def indent = depth += 1
    def outdent = depth -= 1

    def break = {
      writer.println(builder.toString)
      builder.clear
      (0 to depth) foreach { _ => (builder ++= "  ") }
    }

    def length = builder.length
  }

  val builder = new IndentingBuilder(1)

  def genName(elem: Element) = s"elem${elem.ident}"

  def genName(comp: Composite) = s"comp${comp.ident}"

  def genName(segment: Segment) = s"seg${segment.ident}"
  
  def optionText(option: Option[String]) = if (option.isDefined) s"""Some("${option.get}")""" else "None"

  /** Build element inline definition. */
  def inlineElement(elem: Element) =
    s"""Element("${elem.ident}", "${elem.name}", ${elem.dataType}, ${elem.minLength}, ${elem.maxLength})"""

  def inlineSegmentComponent(comp: SegmentComponent, compnames: Map[Composite, String], elemrefs: Set[Element]) =
    comp match {
      case elem: ElementComponent => {
        val element = elem.element
        val ref = if (elemrefs.contains(element)) genName(element) else inlineElement(element)
        val name = if (comp.name == element.name) "None" else s"""Some("${comp.name}")"""
        s"""ElementComponent($ref, $name, "${comp.key}", ${elem.position}, ${elem.usage}, ${elem.count})"""
      }
      case comp: CompositeComponent =>
        s"""CompositeComponent(${compnames(comp.composite)}, Some("${comp.name}"), "${comp.key}", ${comp.position}, ${comp.usage}, ${comp.count})"""
    }

  /** Build segment components code. */
  def componentList(comps: List[SegmentComponent], compnames: Map[Composite, String], elemrefs: Set[Element]): Unit = {
    comps.foreach(comp => {
      builder.breakAppend(inlineSegmentComponent(comp, compnames, elemrefs))
      builder.prepend = ", "
    })
    builder.prepend = ""
  }

  def defineElement(element: Element) = {
    builder.append(s"""val ${genName(element)} = Element("${element.ident}", "${element.name}", ${element.dataType}, ${element.minLength}, ${element.maxLength})""")
    builder.break
  }

  /** Build composite definition code. */
  def defineComposite(comp: Composite, name: String, compnames: Map[Composite, String], elemrefs: Set[Element]) = {
    builder.append(s"""val $name = Composite("${comp.ident}", "${comp.name}", List[SegmentComponent](""")
    builder.indent
    componentList(comp.components, compnames, elemrefs)
    builder.append("), Nil)")
    builder.outdent
    builder.break
  }

  def defineSegment(segment: Segment, compnames: Map[Composite, String], elemrefs: Set[Element]) = {
    builder.append(s"""val ${genName(segment)} = Segment("${segment.ident}", "${segment.name}", List[SegmentComponent](""")
    builder.indent
    componentList(segment.components, compnames, elemrefs)
    builder.append("), Nil)")
    builder.outdent
    builder.break
  }

  def componentList(comps: List[TransactionComponent]): Unit = {
    builder.prepend = ""
    comps.foreach(comp => {
      comp match {
        case ref: ReferenceComponent => {
          val position = s"""SegmentPosition(${ref.position.table}, "${ref.position.position}")"""
          val text = s"ReferenceComponent(${genName(ref.segment)}, $position, ${ref.use}, ${ref.count})"
          if (builder.length > BREAK_LINE_LENGTH) builder.breakAppend(text)
          else builder.append(text)
        }
        case group: GroupComponent => {
          builder.breakAppend(s"""GroupComponent("${group.ident}", ${group.use}, ${group.count}, List[TransactionComponent](""")
          builder.indent
          builder.break
          componentList(group.items)
          builder.break
          builder.append("),")
          builder.append(optionText(group.varkey))
          builder.append(", Nil")
          builder.append(")")
          builder.outdent
        }
      }
      builder.prepend = ", "
    })
    builder.prepend = ""
  }

  def toCode(segments: List[Segment]): Unit = {
    val elemrefs = segments.foldLeft(Map[Element, Int]())((acc, segment) =>
      segment.components.foldLeft(acc)((acc, comp) =>
        comp match {
          case elem: ElementComponent => acc + (elem.element -> (acc.getOrElse(elem.element, 0) + 1))
          case _ => acc
        }))
    val elemreps = elemrefs.filter(pair => pair._2 > 1).map(pair => pair._1).toSet
    elemreps.toList.sortBy(element => element.ident).foreach(element => defineElement(element))
    val composites = segments.foldLeft(Set[Composite]())((acc, segment) =>
      segment.components.foldLeft(acc)((acc, comp) =>
        comp match {
          case ccomp: CompositeComponent => acc + ccomp.composite
          case _ => acc
        }))
    val compgroups = composites.toList.groupBy { comp => comp.ident }
    val compnames = compgroups.keys.toList.sorted.foldLeft(Map[Composite, String]()) {
      case (map, id) => {
        val list = compgroups(id)
        val first = list.head
        val basename = genName(first)
        if (list.tail.isEmpty) {
          defineComposite(first, basename, map, elemreps)
          map + (first -> basename)
        } else list.zipWithIndex.foldLeft(map) {
          case (map, (comp, index)) => {
            val name = basename + "_" + index
            defineComposite(comp, name, map, elemreps)
            map + (comp -> name)
          }
        }
      }
    }
    segments.toList.sortBy(segment => segment.ident).foreach(segment => defineSegment(segment, compnames, elemreps))
  }

  def toCode(trans: Transaction): Unit = {
    def referencedSegments(comps: List[TransactionComponent]): Set[Segment] = {
      def referencer(comps: List[TransactionComponent], segments: Set[Segment]): Set[Segment] =
        comps.foldLeft(segments)((segs, comp) => comp match {
          case ref: ReferenceComponent => segs + ref.segment
          case wrap: LoopWrapperComponent => referencer(wrap.loopGroup.items, segs + wrap.open + wrap.close)
          case group: GroupComponent => referencer(group.items, segs)
        })
      referencer(comps, Set[Segment]())
    }
    def dumpSection(comps: List[TransactionComponent]) =
      if (comps.nonEmpty) {
        builder.break
        componentList(comps)
        builder.break
      }
    val segments = referencedSegments(trans.heading) ++ referencedSegments(trans.detail) ++
      referencedSegments(trans.summary)
    toCode(segments.toList)
    builder.break
    builder.append(s"""val trans${trans.ident} = Transaction("${trans.ident}", "${trans.name}", ${optionText(trans.group)}, List[TransactionComponent](""")
    builder.indent
    dumpSection(trans.heading)
    builder.append("), List[TransactionComponent](")
    dumpSection(trans.detail)
    builder.append("), List[TransactionComponent](")
    dumpSection(trans.summary)
    builder.append("))")
    builder.outdent
    builder.break
  }
}

object SegmentsToCode {

  /** Reads a schema definition and outputs one or more segment definitions as Scala code dumped to the console.
    */
  def main(args: Array[String]): Unit = {
    val yamlin = new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8")
    val schema = new YamlReader().loadYaml(yamlin, Array())
    val writer = new PrintWriter(System.out)
    new SchemaDump(writer).toCode(args.toList.tail.map { ident => schema.segments(ident) })
    writer.flush
    writer.close
  }
}

object SchemaToCode {

  /** Reads a schema definition and outputs the transactions as Scala code dumped to the console.
    */
  def main(args: Array[String]): Unit = {
    val yamlin = new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8")
    val schema = new YamlReader().loadYaml(yamlin, Array())
    val writer = new PrintWriter(System.out)
    val dumper = new SchemaDump(writer)
    schema.transactions.values.foreach(transact => dumper.toCode(transact))
    writer.flush
    writer.close
  }
}