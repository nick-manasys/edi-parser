package com.anypoint.df.edi.schema.tools

import collection.JavaConverters._
import collection.mutable.Buffer
import annotation.tailrec
import util.{ Failure, Success }

import java.io.{ File, FileInputStream, FileWriter, InputStreamReader, Reader, Writer }
import java.{ util => ju }

import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.schema._
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.schema.SchemaJavaValues._

object OverlayByExample extends WritesYaml with YamlDefs with SchemaJavaDefs {

  /** Merge all values from the first map into the second map. If the same key exist in both maps, the value of the
    * second map is retained. The second map (which may have been modified by the method call) is always returned as
    * the result.
    */
  def mergeMaps(map1: ValueMap, map2: ValueMap): ValueMap = {
    map1.keySet.asScala.toList.foreach(key => {
      def forceMerge(m: ValueMap) = {
        if (!map2.containsKey(key)) map2 put (key, new ValueMapImpl)
        mergeMaps(m, map2.get(key).asInstanceOf[ValueMap])
      }
      map1.get(key) match {
        case l: MapList => {
          val flat = new ValueMapImpl
          l.asScala.foreach {
            case m: ValueMap => mergeMaps(m, flat)
            case _ => throw new IllegalStateException("list value can only be merged with map")
          }
          forceMerge(flat)
        }
        case m: ValueMap => forceMerge(m)
        case v => if (!map2.containsKey(key)) map2 put (key, v)
      }
    })
    map2
  }

  sealed abstract class StructureModification(val ident: String, val position: String)
  case class DropSegment(id: String, pos: String) extends StructureModification(id, pos)
  case class ModifyLoop(id: String, pos: String, val nested: List[StructureModification])
    extends StructureModification(id, pos)
  case class ModifyWrapper(id: String, pos: String, val nested: List[StructureModification])
    extends StructureModification(id, pos)

  case class StructureChanges(ident: String, headMods: List[StructureModification],
    detailMods: List[StructureModification], summaryMods: List[StructureModification])

  case class SegmentModification(val ident: String, val trim: Option[Int], val drops: List[Int])

  /** How to write overlays? Since segments are separate from structures in the schema, I need to collect all the
    * information for trnasactions in one set of maps, and all the information for segments in another set of maps.
    * Then I can use the first map for writing the structure overlay, and the second map for writing the segment
    * overlay. This doesn't allow customizing segments on a per-occurrance basis, though. Is that a problem?
    */
  def writeOverlay(base: EdiSchema, basePath: String, transmods: List[StructureChanges],
    segmods: List[SegmentModification], writer: Writer) {

    /** Write list of modifications to segments in a structure. Recurses when a loop has embedded changes. */
    def writeMods(indent: Int, mods: List[StructureModification]): Unit = mods.foreach {
      case DropSegment(id, pos) =>
        writeIndented(s"""- { ${keyValueQuote(idRefKey, id)}, ${keyValueQuote(positionKey, pos)}, ${keyValuePair(usageKey, UnusedUsage.code)} }""", indent, writer)
      case ModifyLoop(id, posThere, nested) =>
        writeIndented(s"- ${keyValueQuote(groupIdRefKey, id)}", indent, writer)
        writeIndented(s"${keyValueQuote(positionKey, posThere)}", indent + 1, writer)
        writeSection(indent + 1, itemsKey, nested)
      case ModifyWrapper(id, posThere, nested) =>
        writeIndented(s"- ${keyValueQuote(wrapIdRefKey, id)}", indent, writer)
        writeIndented(s"${keyValueQuote(positionKey, posThere)}", indent + 1, writer)
        if (nested.isEmpty) writeIndented(keyValuePair(usageKey, UnusedUsage.code), indent + 1, writer)
        else writeSection(indent + 1, groupKey, nested)
    }

    /** Write list of modifications to segments in a structure with leading label. */
    def writeSection(indent: Int, label: String, mods: List[StructureModification]) = {
      writeIndented(s"$label:", indent, writer)
      writeMods(indent, mods)
    }

    // start with schema type and version, and import base
    writeIndented(keyValuePair(formKey, base.ediVersion.ediForm.text), 0, writer)
    writeIndented(keyValueQuote(versionKey, base.ediVersion.version), 0, writer)
    writer.append(s"$importsKey: [ '$basePath' ]\n")
    if (transmods.nonEmpty) {

      // write structure modifications
      writeIndented(s"$structuresKey:", 0, writer)
      transmods foreach (transmod => {
        writeIndented(s"- ${keyValueQuote(idRefKey, transmod.ident)}", 0, writer)
        if (transmod.headMods.nonEmpty) writeSection(1, headingKey, transmod.headMods)
        if (transmod.detailMods.nonEmpty) writeSection(1, detailKey, transmod.detailMods)
        if (transmod.summaryMods.nonEmpty) writeSection(1, summaryKey, transmod.summaryMods)
      })
    }
    if (segmods.nonEmpty) {

      // write segment modifications
      writeIndented(s"$segmentsKey:", 0, writer)
      segmods foreach {
        case SegmentModification(ident, trim, drops) =>
          if (drops.isEmpty)
            writeIndented(s"- { ${keyValuePair(idRefKey, ident)}, ${keyValuePair(trimKey, trim.get)} }", 0, writer)
          else {
            writeIndented(s"- ${keyValuePair(idRefKey, ident)}", 0, writer)
            writeIndented(s"$itemsKey:", 1, writer)
            drops.foreach {
              drop => writeIndented(s"- { ${keyValuePair(positionKey, drop)}, ${keyValuePair(usageKey, UnusedUsage.code)} }", 1, writer)
            }
          }
      }
    }
  }

  type SegmentKeys = Map[Segment, Set[String]]

  /** Reads a schema and parses one or more documents using that schema, then generates an overlay schema based on the
    * sample documents which marks as unused all segments and elements/composites which were not present in any of the
    * samples. The first two arguments are the schema used for the documents and the output overlay schema; remaining
    * arguments are the documents to be parsed and used as examples.
    */
  def main(args: Array[String]): Unit = {
    
    import com.anypoint.df.edi.schema.X12SchemaDefs.{ transactionsMap, setKey}

    val yaml = new YamlReader()
    val is = yaml.findSchema(args(0), Array())
    val schema = yaml.loadYaml(new InputStreamReader(is), Array())
    val examples = args.toList.tail.tail
    val config = X12ParserConfig(true, true, true, true, true, true, true, true, false, -1,
      CharacterRestriction.EXTENDED)

    /** Strip metadata (structure set and group links) out of structure data to avoid excess overhead on merge. */
    def stripMeta(trans: ValueMap) = trans.asScala.foreach {
      case (_, list: MapList) => list.asScala.foreach {
        case m: ValueMap => {
          m.remove(setKey)
          m.remove(groupKey)
          m.remove(interchangeKey)
        }
        case _ => throw new IllegalStateException("structure list items must be maps")
      }
      case _ => throw new IllegalStateException("structure map values must be lists")
    }

    /** Recursively scan structure component structure to find modifications. */
    def structureMods(segments: List[StructureComponent], data: ValueMap): List[StructureModification] =
      segments.foldLeft(List[StructureModification]())((acc, comp) => comp match {
        case group: GroupComponent =>
          if (data.containsKey(comp.key)) {
            val nested = structureMods(group.items, getRequiredValueMap(comp.key, data))
            if (nested.isEmpty) acc
            else ModifyLoop(group.ident, group.position.position, nested) :: acc
          } else DropSegment(group.ident, group.position.position) :: acc
        case wrap: LoopWrapperComponent =>
          if (data.containsKey(comp.key)) {
            val nested = structureMods(wrap.loopGroup :: Nil, getRequiredValueMap(comp.key, data))
            if (nested.isEmpty) acc
            else ModifyWrapper(wrap.ident, wrap.position.position, nested) :: acc
          } else ModifyWrapper(wrap.ident, wrap.position.position, Nil) :: acc
        case ref: ReferenceComponent =>
          if (data.containsKey(comp.key) || schema.ediVersion.ediForm.isEnvelopeSegment(ref.segment.ident)) acc
          else DropSegment(ref.segment.ident, ref.position.position) :: acc
      }).reverse

    /** Recursively scan structure component structure to collect segment item usage information. */
    def collectSegments(segments: List[StructureComponent], data: ValueMap, prior: SegmentKeys): SegmentKeys = {
      def mergeSet(data: ValueMap, used: Set[String]) = data.keySet.asScala.foldLeft(used)((acc, key) => acc + key)
      def collectr(segments: List[StructureComponent], data: ValueMap, segmaps: SegmentKeys): SegmentKeys =
        segments.foldLeft(segmaps)((acc, comp) => comp match {
          case group: GroupComponent =>
            if (data.containsKey(comp.key)) collectr(group.items, getRequiredValueMap(comp.key, data), acc)
            else acc
          case wrap: LoopWrapperComponent =>
            if (data.containsKey(comp.key)) collectr(wrap.loopGroup :: Nil, getRequiredValueMap(comp.key, data), acc)
            else acc
          case ref: ReferenceComponent =>
            if (data.containsKey(comp.key)) acc + (ref.segment ->
              mergeSet(getRequiredValueMap(comp.key, data), acc.getOrElse(ref.segment, Set())))
            else acc
        })
      collectr(segments, data, prior)
    }

    /** Scan segment component structure to find modifications. */
    def segmentMods(segmaps: SegmentKeys): List[SegmentModification] =
      segmaps.foldLeft (List[SegmentModification]()) {
        case (acc, (segment, idset)) => {
          def skipr(ids: List[String], comp: SegmentComponent): List[String] = ids match {
            case h :: t if (h.startsWith(comp.key)) => skipr(t, comp)
            case _ => ids
          }
          def trimr(ids: List[String], comps: List[SegmentComponent], drops: List[Int]): Option[SegmentModification] =
            ids match {
              case hid :: tid => comps match {
                case hc :: tc =>
                  if (hid.startsWith(hc.key)) trimr(skipr(tid, hc), tc, drops)
                  else trimr(ids, tc, hc.position :: drops)
                case _ => throw new IllegalStateException("key not defined for segment")
              }
              case _ =>
                if (comps.nonEmpty) Some(SegmentModification(segment.ident, Some(comps.head.position), drops.reverse))
                else if (drops.nonEmpty) Some(SegmentModification(segment.ident, None, drops.reverse))
                else None
            }
          trimr(idset.toList.sorted, segment.components, List()) match {
            case Some(mod) => mod :: acc
            case _ => acc
          }
        }
      }.reverse

    // merge structure data from all documents into a single map structure (excluding 997s, since those are fixed)
    val merged = new ValueMapImpl
    examples.foreach (path => {
      println(s"merging $path")
      val is = yaml.findSchema(path, Array())
      val parser = new X12InterchangeParser(is, ASCII_CHARSET, new DefaultX12EnvelopeHandler(config, schema))
      parser.parse match {
        case Success(x) => {
          val transacts = x.get(transactionsMap).asInstanceOf[ValueMap]
          transacts.remove("997")
          stripMeta(transacts)
          println(transacts)
          mergeMaps(transacts, merged)
          println(merged)
        }
        case Failure(e) => throw new IllegalArgumentException(s"error parsing example $path: '${e.getMessage}'")
      }
    })

    // match resulting map against schema structure, writing differences as output overlay
    val overFile = new File(args(1))
    val writer = new FileWriter(overFile)
    val buffer = Buffer[StructureChanges]()
    val segmaps = merged.asScala.toList.foldLeft(Map[Segment, Set[String]]()) {
      case (acc, (key, map: ValueMap)) => {
        val transact = schema.structures(key)
        println(map)
        val headmap = map.get(structureHeading).asInstanceOf[ValueMap]
        val headmods = structureMods(transact.heading, headmap)
        val detailmap = map.get(structureDetail).asInstanceOf[ValueMap]
        val detailmods = structureMods(transact.detail, detailmap)
        val summarymap = map.get(structureSummary).asInstanceOf[ValueMap]
        val summarymods = structureMods(transact.summary, summarymap)
        if (headmods.nonEmpty || detailmods.nonEmpty || summarymods.nonEmpty) buffer += StructureChanges(transact.ident, headmods, detailmods, summarymods)
        collectSegments(transact.summary, summarymap, collectSegments(transact.detail, detailmap, collectSegments(transact.heading, headmap, acc)))
      }
    }
    segmaps.foreach{ case (segment, idset) => println(s"segment ${segment.ident} => " + idset) }
    val segs = segmentMods(segmaps).sortBy { mod => mod.ident }
    segs.foreach { mod => println(mod) }
    writeOverlay(schema, args(0), buffer.toList, segs, writer)
    writer.close
  }
}