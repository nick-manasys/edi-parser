package com.mulesoft.flatfile.schema.tools

import com.mulesoft.flatfile.schema.{ EdiSchema, YamlReader, YamlWriter }
import com.mulesoft.flatfile.schema.EdiSchema._
import java.io._

object InlineSchema {

  def inlineComponents(comps: List[SegmentComponent]): List[SegmentComponent] = {
    comps.map { comp =>
      comp match {
        case e: ElementComponent =>
          val elem = e.element
          val relem = Element("", elem.name, elem.typeFormat)
          ElementComponent(relem, e.nm, e.ky, e.pos, e.use, e.cnt, e.tagPart, e.value)
        case c: CompositeComponent =>
          val compos = c.composite
          val rcompos = Composite("", compos.nm, inlineComponents(compos.components), compos.rules, compos.maxLength)
          CompositeComponent(rcompos, c.nm, c.ky, c.pos, c.use, c.cnt)
      }
    }
  }

  def inlineSequence(strseq: StructureSequence): StructureSequence = {
    val items = strseq.items.map { item =>
      item match {
        case r: ReferenceComponent =>
          val seg = r.segment
          val rseg = Segment("", seg.name, inlineComponents(seg.components), seg.rules)
          ReferenceComponent(rseg, r.pos, r.use, r.cnt)
        case g: GroupComponent =>
          GroupComponent(g.ident, g.use, g.cnt, inlineSequence(g.ssq), g.varkey, g.variants, g.ky, g.pos, g.ch)
        case _ => throw new IllegalArgumentException(s"Unsupported component type ${item.getClass.getName}")
      }
    }
    new StructureSequence(strseq.loop, items)
  }

  def main(args: Array[String]): Unit = {
    val input = new FileReader(new File(args(0)))
    val ischema = new YamlReader().loadYaml(input, Array())
    val structs = ischema.structures.foldLeft(Map[String, Structure]()) {
      case (map, (ident, istruct)) =>
        val heading = istruct.heading.map { inlineSequence(_) }
        val detail = istruct.detail.map { inlineSequence(_) }
        val summary = istruct.summary.map { inlineSequence(_) }
        val ostruct = Structure(istruct.ident, istruct.name, istruct.group, heading, detail, summary, istruct.version)
        map + (ident -> ostruct)
    }
    val oschema = EdiSchema(ischema.ediVersion, Map(), Map(), Map(), structs)
    val writer = new FileWriter(new File(args(1)))
    YamlWriter.write(oschema, true, Array(), writer)
    writer.close
  }
}