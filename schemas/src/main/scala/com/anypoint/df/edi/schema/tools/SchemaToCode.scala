package com.anypoint.df.edi.schema.tools

import java.io.FileInputStream
import java.io.PrintWriter
import java.io.InputStreamReader
import java.io.File
import com.anypoint.df.edi.schema.EdiSchema
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.schema.YamlReader

import scala.annotation.migration
import scala.annotation.tailrec

/** Print the code to create a segment or transaction. Note that this does not handle variant groups. */
class SchemaDump(schema: EdiSchema, writer: PrintWriter) {

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

  /** Build inline segment component definition.
   *  @param scomp
   *  @param compnames component ident to (single use, component name) map
   *  @param elemrefs predefined elements
   */
  def inlineSegmentComponent(scomp: SegmentComponent, compnames: Map[String, (Boolean, String)], elemrefs: Set[Element]) =
    scomp match {
      case ecomp: ElementComponent => {
        val element = ecomp.element
        val ref = if (elemrefs.contains(element)) genName(element) else inlineElement(element)
        val name = if (scomp.name == element.name) "None" else s"""Some("${scomp.name}")"""
        s"""ElementComponent($ref, $name, "${scomp.key}", ${ecomp.position}, ${ecomp.usage}, ${ecomp.count})"""
      }
      case ccomp: CompositeComponent => {
        val ident = ccomp.composite.ident
        val cname = compnames(ident)._2
        val comp =
          if (ccomp.count != 1 || compnames(ident)._1) cname
          else s"""$cname.rewrite("${ccomp.key}", convertEdiForm("${schema.ediForm}"))"""
        s"""CompositeComponent($comp, Some("${ccomp.name}"), "${ccomp.key}", ${ccomp.position}, ${ccomp.usage}, ${ccomp.count})"""
      }
    }

  /** Build segment components code.
   *  @param comps
   *  @param compnames component ident to (single use, component name) map
   *  @param elemrefs predefined elements
   */
  def componentList(comps: List[SegmentComponent], compnames: Map[String, (Boolean, String)], elemrefs: Set[Element]): Unit = {
    comps.foreach(comp => {
      builder.breakAppend(inlineSegmentComponent(comp, compnames, elemrefs))
      builder.prepend = ", "
    })
    builder.prepend = ""
  }

  /** Build element definition code. */
  def defineElement(element: Element) = {
    builder.append(s"""val ${genName(element)} = Element("${element.ident}", "${element.name}", ${element.dataType}, ${element.minLength}, ${element.maxLength})""")
    builder.break
  }

  /** Build composite definition code.
   *  @param comp
   *  @param name
   *  @param compnames component ident to (single use, component name) map
   *  @param elemrefs predefined elements
   */
  def defineComposite(comp: Composite, name: String, compnames: Map[String, (Boolean, String)], elemrefs: Set[Element]) = {
    builder.append(s"""val $name = Composite("${comp.ident}", "${comp.name}", List[SegmentComponent](""")
    builder.indent
    componentList(comp.components, compnames, elemrefs)
    builder.append(s"), Nil, ${comp.maxLength})")
    builder.outdent
    builder.break
  }

  /** Build segment definition code.
   *  @param segment
   *  @param compnames component ident to (single use, component name) map
   *  @param elemrefs predefined elements
   */
  def defineSegment(segment: Segment, compnames: Map[String, (Boolean, String)], elemrefs: Set[Element]) = {
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

  def toCode(segments: List[Segment], composites: Map[String, Composite]): Unit = {

    def elemRefCounts(list: List[SegmentComponent], counts: Map[Element, Int]): Map[Element, Int] =
      list.foldLeft(counts)((acc, scomp) => scomp match {
        case ccomp: CompositeComponent => elemRefCounts(ccomp.composite.components, acc)
        case ecomp: ElementComponent => acc + (ecomp.element -> (acc.getOrElse(ecomp.element, 0) + 1))
      })

    val elemrefs = segments.foldLeft(Map[Element, Int]())((acc, segment) => elemRefCounts(segment.components, acc))
    val elemreps = elemrefs.filter(pair => pair._2 > 1).map(_._1).toSet
    elemreps.toList.sortBy(_.ident).foreach(element => defineElement(element))

    def compRefCounts(list: List[SegmentComponent], counts: Map[String, Int]): Map[String, Int] =
      list.foldLeft(counts)((acc, scomp) => scomp match {
        case ccomp: CompositeComponent =>
          val ident = ccomp.composite.ident
          if (acc.contains(ident)) acc + (ident -> (acc(ident) + 1))
          else compRefCounts(ccomp.composite.components, acc + (ident -> 1))
        case _ => acc
      })
      
    val comprefs = segments.foldLeft(Map[String, Int]())((acc, segment) => compRefCounts(segment.components, acc))
    val compsings = comprefs.filter(_._2 == 1).map(_._1).toSet
    
    def defineComposites(comps: List[Composite], built: Map[String, (Boolean, String)]): Map[String, (Boolean, String)] = {
      val tobuild = comps.filter { comp =>
        comp.components.forall {
          case ccomp: CompositeComponent => built.contains(ccomp.composite.ident)
          case _ => true
        }
      }.sortBy(_.ident)
      val merged = tobuild.foldLeft(built)((acc, comp) => {
        val name = genName(comp)
        val single = compsings.contains(comp.ident)
        val build = if (single) comp else composites(comp.ident)
        defineComposite(build, name, acc, elemreps)
        acc + (comp.ident -> (single, name))
      })
      val remain = comps.filter { !tobuild.contains(_) }
      if (remain.isEmpty) merged
      else defineComposites(remain, merged)
    }

    val multcomps = comprefs.filter(_._2 != 1).map(pair => composites(pair._1)).toList
    val allcomps = segments.foldLeft(multcomps)((acc, segment) =>
      segment.components.foldLeft(acc)((acc, scomp) => scomp match {
        case ccomp: CompositeComponent if (compsings.contains(ccomp.composite.ident)) => ccomp.composite :: acc
        case _ => acc
      }))
    val compnames = defineComposites(allcomps, Map[String, (Boolean, String)]())
    segments.toList.sortBy(segment => segment.ident).foreach(segment => defineSegment(segment, compnames, elemreps))
  }

  def toCode(trans: Transaction, composites: Map[String, Composite]): Unit = {
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
    toCode(segments.toList, composites)
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
    new SchemaDump(schema, writer).toCode(args.toList.tail.map { ident => schema.segments(ident) }, schema.composites)
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
    val dumper = new SchemaDump(schema, writer)
    schema.transactions.values.foreach(transact => dumper.toCode(transact, schema.composites))
    writer.flush
    writer.close
  }
}