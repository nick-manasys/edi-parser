package com.anypoint.df.edi.schema

import java.io.File
import java.io.FileInputStream
import java.io.FileReader
import java.io.InputStream
import java.io.InputStreamReader
import java.io.Reader
import java.io.StringWriter
import java.io.StringReader

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.AbstractConstruct
import org.yaml.snakeyaml.constructor.Constructor
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.nodes.NodeId

import collection.JavaConverters._
import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import scala.collection.immutable.AbstractMap
import scala.collection.mutable

import com.anypoint.df.edi.lexical.EdiConstants

/** Read YAML representation of EDI schema.
  *
  * @author MuleSoft, Inc.
  */
class YamlReader extends YamlDefs with SchemaJavaDefs {

  import EdiSchema._

  val schemaCache = mutable.Map[String, EdiSchema]()

  /** Get child list value (error if not found). */
  def getChildList(key: String, map: ValueMap): SimpleList = map.get(key) match {
    case child: SimpleList => child
    case null => throw new IllegalArgumentException("Missing required array '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a list")
  }

  /** Build segment components from input data.
    * @param parentId identifier for parent segment or composite
    * @param list list of maps of component data
    * @param rules list of maps of rules
    * @param elements known elements map
    * @param composites known composites map
    * @param form EDI form
    * @returns components
    */
  def parseSegmentComponents(parentId: String, list: MapList, elements: Map[String, Element],
    composites: Map[String, Composite], form: EdiForm): List[SegmentComponent] = {
    def convertComponent(values: ValueMap, dfltpos: Int) = {
      val id = getRequiredString(idRefKey, values)
      val name = if (values.containsKey(nameKey)) Some(getRequiredString(nameKey, values)) else None
      val position = if (values.containsKey(positionKey)) getRequiredInt(positionKey, values) else dfltpos
      val key = form.keyName(parentId, position)
      val use = convertUsage(getRequiredString(usageKey, values))
      val count = values.get(countKey) match {
        case n: Integer => n.toInt
        case ">1" => Integer.MAX_VALUE
        case null => 1
        case _ => throw new IllegalArgumentException(s"Value $countKey must be an integer")
      }

      if (elements.contains(id)) ElementComponent(elements(id), name, key, position, use, count)
      else if (composites.contains(id)) {
        val composite = composites(id)
        if (count > 1) CompositeComponent(composite, name, key, position, use, count)
        else CompositeComponent(composite.rewrite(key, form), name, key, position, use, count)
      } else throw new IllegalArgumentException(s"No element or composite with id '$id'")
    }
    @tailrec
    def parseComponent(remain: List[ValueMap], position: Int, prior: List[SegmentComponent]): List[SegmentComponent] =
      remain match {
        case values :: t => parseComponent(t, position + 1, convertComponent(values, position) :: prior)
        case _ => prior.reverse
      }
    parseComponent(list.asScala.toList, 1, Nil)
  }

  def getUsageOverride(values: ValueMap, dflt: Usage) =
    if (values.containsKey(usageKey)) convertUsage(getRequiredString(usageKey, values)) else dflt

  def getCountOverride(values: ValueMap, dflt: Int) =
    if (values.containsKey(countKey)) values.get(countKey) match {
      case n: Integer => n.toInt
      case null => 1
      case _ => throw new IllegalArgumentException(s"Value $countKey must be an integer")
    }
    else dflt

  /** Build segment components with input data overlaying base definition. For overlays the position value is required
    * on the component and uniquely identifies the base component being modified.
    * @param parentId identifier for parent segment or composite
    * @param list list of maps of component data
    * @param rules list of maps of rules
    * @param elements known elements map
    * @param composites known composites map
    * @param base segment components in base definition (overlayed by parsed values)
    * @returns components
    */
  def parseSegmentOverlayComponents(parentId: String, list: MapList, elements: Map[String, Element],
    composites: Map[String, Composite], base: List[SegmentComponent]): List[SegmentComponent] = {
    def convertComponent(values: ValueMap, comp: SegmentComponent) = {
      val ident = comp match {
        case elem: ElementComponent => elem.element.ident
        case comp: CompositeComponent => comp.composite.ident
      }
      val id = getAs(idRefKey, ident, values)
      if (id != ident) throw new IllegalArgumentException(s"component at position ${comp.position} does not match id $id")
      val name = Some(getAs(nameKey, comp.name, values))
      val use = getUsageOverride(values, comp.usage)
      val count = getCountOverride(values, comp.count)
      comp match {
        case elem: ElementComponent => ElementComponent(elem.element, name, elem.key, elem.position, use, count)
        case comp: CompositeComponent => CompositeComponent(comp.composite, name, comp.key,
          comp.position, use, count)
      }
    }
    @tailrec
    def parseComponent(remain: List[ValueMap], prior: List[SegmentComponent], base: List[SegmentComponent]): List[SegmentComponent] =
      remain match {
        case values :: t => {
          val pos = getRequiredInt(positionKey, values)
          base match {
            case (comp: SegmentComponent) :: bt if (comp.position == pos) =>
              parseComponent(t, convertComponent(values, comp) :: prior, bt)
            case bh :: bt => parseComponent(remain, prior, bt)
            case Nil => prior.reverse
          }
        }
        case _ => prior.reverse ::: base
      }
    parseComponent(list.asScala.toList, Nil, base)
  }

  /** Build rules from input data.
    * @param rules list of maps of rule data
    * @param comps components rule applies to
    * @returns rules
    */
  def parseRules(rules: MapList, comps: List[SegmentComponent]): List[OccurrenceRule] = {
    val posidx = comps.foldLeft(Map[Int, SegmentComponent]())((acc, comp) => acc + (comp.position -> comp))
    def convertRule(map: ValueMap) = {
      val comps = getChildList(valuesKey, map).asScala.toList.map(value => value match {
        case pos: Integer => posidx(pos.toInt)
        case _ => throw new IllegalArgumentException("Not a valid component position for rule")
      })
      val typ = getRequiredString(typeKey, map)
      typ match {
        case OneOrMoreCode => OneOrMore(comps)
        case IfFirstThenAllCode => IfFirstThenAll(comps)
        case OneOrNoneCode => OneOrNone(comps)
        case IfFirstThenMoreCode => IfFirstThenMore(comps)
        case AllOrNoneCode => AllOrNone(comps)
        case OneAndOnlyOneCode => OneAndOnlyOne(comps)
        case IfFirstThenNoneCode => IfFirstThenNone(comps)
        case _ => throw new IllegalArgumentException("Not a valid rule type code")
      }
    }
    rules.asScala.toList.map(map => convertRule(map))
  }

  /** Build structure components from input data.
    * @param maps list of maps of component data
    * @param form EDI form
    * @param table structure table index
    * @param segments known segments map
    * @returns components
    */
  def parseStructureComponents(maps: List[ValueMap], form: EdiForm, table: Int, segments: Map[String, Segment]) = {
    def convertComponent(values: ValueMap): StructureComponent = {
      val use = convertUsage(getRequiredString(usageKey, values))
      val count = values.get(countKey) match {
        case n: Integer => n.toInt
        case s: String if (s == anyRepeatsValue) => -1
        case null => 1
        case _ => throw new IllegalArgumentException(s"Value $countKey must be an integer or the string '$anyRepeatsValue'")
      }
      if (values.containsKey(itemsKey)) {
        val ident = getRequiredString(groupIdKey, values)
        val items = getRequiredMapList(itemsKey, values)
        val postext = getAsString(positionKey, values)
        val position = if (postext == null) None else Some(SegmentPosition(table, postext))
        val seq = StructureSequence(true, parseComponent(items.asScala.toList, Nil))
        val tagStart = getIntOption(tagStartKey, values)
        val tagLength = getIntOption(tagLengthKey, values)
        GroupComponent(ident, use, count, seq, None, Nil, None, position, false, tagStart, tagLength)
      } else if (values.containsKey(wrapIdKey)) {
        val wrapid = getRequiredString(wrapIdKey, values)
        val list = getRequiredMapList(groupKey, values)
        if (list.size != 1) throw new IllegalArgumentException(s"Single group definition required for loop with $wrapIdKey $wrapid")
        val group = convertComponent(list.iterator.next).asInstanceOf[GroupComponent]
        if (segments.contains(form.loopWrapperStart) && segments.contains(form.loopWrapperEnd)) {
          val start = SegmentPosition(table, getRequiredString(positionKey, values))
          val end = SegmentPosition(table, getRequiredString(endPositionKey, values))
          LoopWrapperComponent(segments(form.loopWrapperStart), segments(form.loopWrapperEnd), start, end, OptionalUsage, wrapid, group)
        } else throw new IllegalArgumentException(s"Missing loop wrapper segment definition (${form.loopWrapperStart} or ${form.loopWrapperEnd})")
      } else {
        val id = getRequiredString(idRefKey, values)
        val position = getRequiredString(positionKey, values)
        if (segments.contains(id)) ReferenceComponent(segments(id), SegmentPosition(table, position), use, count)
        else throw new IllegalArgumentException(s"No segment with id '$id'")
      }
    }
    @tailrec
    def parseComponent(remain: List[ValueMap], prior: List[StructureComponent]): List[StructureComponent] =
      remain match {
        case values :: t => parseComponent(t, convertComponent(values) :: prior)
        case _ => prior.reverse
      }
    parseComponent(maps, Nil)
  }

  /** Build structure part from input data.
    * @param name part name
    * @param values map of component data lists
    * @param form EDI form
    * @param table structure table index
    * @param segments known segments map
    * @returns components
    */
  def parseStructurePart(name: String, values: ValueMap, form: EdiForm, table: Int, segments: Map[String, Segment]) =
    if (values.containsKey(name)) {
      val vallist = getRequiredMapList(name, values).asScala.toList
      parseStructureComponents(vallist, form, table, segments)
    } else Nil

  /** Build structure part as overlay to base part.
    * @param name part name
    * @param overs map of component data overlay lists
    * @param base original components
    * @param segments known segments map
    * @returns components
    */
  def parseStructureOverlayPart(name: String, overs: ValueMap, base: List[StructureComponent],
    segments: Map[String, Segment]) =
    if (overs.containsKey(name)) {
      def overLevel(overs: MapList, base: List[StructureComponent]) =
        overr(overs.asScala.toList, Nil, base)
      def overGroup(over: ValueMap, group: GroupComponent) =
        overr(over :: Nil, Nil, group :: Nil).head.asInstanceOf[GroupComponent]
      @tailrec
      def overr(remain: List[ValueMap], prior: List[StructureComponent], base: List[StructureComponent]): List[StructureComponent] = remain match {
        case values :: t => {
          val position = getRequiredString(positionKey, values)
          base match {
            case bh :: bt =>
              if (bh.position.position == position) bh match {
                case (refc: ReferenceComponent) => {
                  if (values.containsKey(idRefKey)) {
                    val compare = getRequiredString(idRefKey, values)
                    if (compare != refc.segment.ident) throw new IllegalStateException(s"segment at position $position is not $compare")
                  }
                  val use = getUsageOverride(values, refc.usage)
                  val count = getCountOverride(values, refc.count)
                  overr(t, ReferenceComponent(refc.segment, refc.position, use, count) :: prior, bt)
                }
                case (wrap: LoopWrapperComponent) => {
                  if (values.containsKey(wrapIdRefKey)) {
                    val compare = getRequiredString(wrapIdRefKey, values)
                    if (compare != wrap.groupId) throw new IllegalStateException(s"wrapper at position $position is not $compare")
                  }
                  val use = getUsageOverride(values, wrap.usage)
                  val count = getCountOverride(values, wrap.count)
                  val group =
                    if (values.containsKey(groupKey)) overGroup(getRequiredValueMap(groupKey, values), wrap.wrapped)
                    else wrap.wrapped
                  overr(t, LoopWrapperComponent(wrap.open, wrap.close, wrap.position, wrap.endPosition, use, wrap.groupId, group) :: prior, bt)
                }
                case (group: GroupComponent) => {
                  if (values.containsKey(groupIdRefKey)) {
                    val compare = getRequiredString(groupIdRefKey, values)
                    if (compare != group.ident) throw new IllegalStateException(s"loop at position $position is not $compare")
                  }
                  val use = getUsageOverride(values, group.usage)
                  val count = getCountOverride(values, group.count)
                  val seq =
                    if (values.containsKey(itemsKey)) {
                      StructureSequence(true, overLevel(getRequiredMapList(itemsKey, values), group.seq.items))
                    } else group.seq
                  val tagStart = getIntOption(tagStartKey, group.tagStart, values)
                  val tagLength = getIntOption(tagLengthKey, group.tagLength, values)
                  overr(t, GroupComponent(group.ident, use, count, seq, group.varkey, group.variants, group.ky,
                    group.pos, group.ch, tagStart, tagLength) :: prior, bt)
                }
              }
              else overr(remain, bh :: prior, bt)
            case Nil => prior.reverse
          }
        }
        case _ => prior.reverse ::: base
      }

      overLevel(getRequiredMapList(name, overs), base)
    } else base

  /** Convert element definitions. */
  private def convertElements(input: ValueMap, basedefs: Map[String, Element]) =
    getRequiredMapList(elementsKey, input).asScala.toList.foldLeft(basedefs)(
      (map, elmmap) => {
        val ident = getRequiredString(idKey, elmmap)
        val name = getRequiredString(nameKey, elmmap)
        val typ = EdiConstants.toX12Type(getRequiredString(typeKey, elmmap))
        val min = getRequiredInt(minLengthKey, elmmap)
        val max = getRequiredInt(maxLengthKey, elmmap)
        val result = map + (ident -> Element(ident, name, typ, min, max))
        result
      })

  /** Convert composite definitions. To work with HL7 this makes multiple passes through the input, each time processing
    * definitions which only use already-defined values. That's not the most efficient approach since it could
    * theoretically require may passes, but it's easy to implement and should be fine for actual schemas.
    */
  private def convertComposites(input: ValueMap, form: EdiForm, elements: Map[String, Element],
    basedefs: Map[String, Composite]) = {

    /** Build dependency set for composite, as set of component idents. */
    def depSet(compmap: ValueMap) = getRequiredMapList(valuesKey, compmap).asScala.
      foldLeft(Set[String]())((set, item) => set + getRequiredString(idRefKey, item))

    type MapsWithDeps = List[(ValueMap, Set[String])]

    /** Recursively build composites which only use already-defined composites until all are built. */
    @tailrec
    def convertr(remain: MapsWithDeps, deferred: MapsWithDeps, done: Map[String, Composite]): Map[String, Composite] =
      remain match {
        case h :: t =>
          if (h._2.forall { ident => done.contains(ident) || elements.contains(ident) }) {
            val compmap = h._1
            val ident = getRequiredString(idKey, compmap)
            val name = getRequiredString(nameKey, compmap)
            val list = getRequiredMapList(valuesKey, compmap)
            val comps = parseSegmentComponents(ident, list, elements, done, form)
            val rules = if (compmap.containsKey(rulesKey)) {
              val data = getRequiredMapList(rulesKey, compmap)
              parseRules(data, comps)
            } else Nil
            val max = if (compmap.containsKey(maxLengthKey)) getRequiredInt(maxLengthKey, compmap) else 0
            convertr(t, deferred, done + (ident -> Composite(ident, name, comps, rules, max)))
          } else convertr(t, h :: deferred, done)
        case _ =>
          if (deferred.isEmpty) done else convertr(deferred, Nil, done)
      }

    val compList = getRequiredMapList(compositesKey, input).asScala.toList.map(map => (map, depSet(map)))
    convertr(compList, Nil, basedefs)
  }

  /** Convert segment definitions. */
  private def convertSegments(input: ValueMap, form: EdiForm, elements: Map[String, Element],
    composites: Map[String, Composite], basedefs: Map[String, Segment]) = {
    def getRules(segmap: ValueMap, comps: List[SegmentComponent]) =
      if (segmap.containsKey(rulesKey)) {
        val data = getRequiredMapList(rulesKey, segmap)
        parseRules(data, comps)
      } else Nil

    def trimComps(comps: List[SegmentComponent], trim: Int) = {
      val (keep, drop) = comps.splitAt(trim)
      keep ::: (drop.foldLeft(List[SegmentComponent]())((acc, comp) => comp match {
        case ElementComponent(element, oname, key, pos, _, count, value) =>
          ElementComponent(element, oname, key, pos, UnusedUsage, count, value) :: acc
        case CompositeComponent(composite, oname, key, pos, _, count) =>
          CompositeComponent(composite, oname, key, pos, UnusedUsage, count) :: acc
      }).reverse)
    }

    getRequiredMapList(segmentsKey, input).asScala.toList.
      foldLeft(basedefs)((map, segmap) => if (segmap.containsKey(idKey)) {
        val ident = getRequiredString(idKey, segmap)
        val tag = getAs(tagKey, ident, segmap)
        val list = getAs[MapList](valuesKey, segmap)
        val comps = if (list == null) Nil else parseSegmentComponents(ident, list, elements, composites, form)
        val name = getAs(nameKey, "", segmap)
        map + (ident -> Segment(ident, tag, name, comps, getRules(segmap, comps)))
      } else if (segmap.containsKey(idRefKey)) {
        val idref = getRequiredString(idRefKey, segmap)
        basedefs.get(idref) match {
          case Some(basedef) => {
            val name = getAs(nameKey, basedef.name, segmap)
            val basecomps = if (segmap.containsKey(trimKey)) trimComps(basedef.components, getRequiredInt(trimKey, segmap)) else basedef.components
            if (segmap.containsKey(valuesKey)) {
              val list = getRequiredMapList(valuesKey, segmap)
              val comps = parseSegmentOverlayComponents(idref, list, elements, composites, basecomps)
              map + (idref -> Segment(idref, basedef.tag, name, comps, getRules(segmap, comps)))
            } else {
              map + (idref -> Segment(idref, basedef.tag, name, basedef.components, getRules(segmap, basecomps)))
            }
          }
          case None => throw new IllegalStateException(s"referenced segment $idref is not defined")
        }
      } else throw new IllegalStateException(s"segment definition needs either $idKey or $idRefKey value"))
  }

  /** Convert structure definitions. */
  private def convertStructures(input: ValueMap, version: EdiSchemaVersion, segments: Map[String, Segment],
    basedefs: EdiSchema.StructureMap) = {
    def optSeq(items: List[StructureComponent]) = if (items.isEmpty) None else Some(StructureSequence(false, items))
    def sectionItems(optseq: Option[StructureSequence]): List[StructureComponent] = optseq match {
      case Some(seq) => seq.items
      case None => Nil
    }
    def dataSection(key: String, values: ValueMap, index: Int): Option[StructureSequence] =
      if (values.containsKey(key)) {
        if (!version.ediForm.sectioned) throw new IllegalArgumentException(s"section $key is not allowed for schemas of type ${version.ediForm.text}")
        optSeq(parseStructurePart(key, values, version.ediForm, index, segments))
      } else None
    val firstkey = if (version.ediForm.sectioned) headingKey else dataKey
    getRequiredMapList(structuresKey, input).asScala.toList.
      foldLeft(basedefs)((map, transmap) => if (transmap.containsKey(idKey)) {
        val ident = getRequiredString(idKey, transmap)
        val name = getRequiredString(nameKey, transmap)
        val group = getStringOption(classKey, transmap)
        val heading = optSeq(parseStructurePart(firstkey, transmap, version.ediForm, 0, segments))
        val detail = dataSection(detailKey, transmap, 1)
        val summary = dataSection(summaryKey, transmap, 2)
        val tagStart = getIntOption(tagStartKey, transmap)
        val tagLength = getIntOption(tagLengthKey, transmap)
        val struct = Structure(ident, name, group, heading, detail, summary, version, tagStart, tagLength)
        map + (ident -> struct)
      } else if (transmap.containsKey(idRefKey)) {
        val idref = getRequiredString(idRefKey, transmap)
        if (!basedefs.contains(idref)) throw new IllegalStateException(s"referenced structure $idref is not defined")
        val basedef = basedefs(idref)
        val name = getAs(nameKey, basedef.name, transmap)
        val group = getStringOption(classKey, basedef.group, transmap)
        val heading = optSeq(parseStructureOverlayPart(firstkey, transmap, sectionItems(basedef.heading), segments))
        val detail = optSeq(parseStructureOverlayPart(detailKey, transmap, sectionItems(basedef.detail), segments))
        val summary = optSeq(parseStructureOverlayPart(summaryKey, transmap, sectionItems(basedef.summary), segments))
        val tagStart = getIntOption(tagStartKey, basedef.tagStart, transmap)
        val tagLength = getIntOption(tagLengthKey, basedef.tagLength, transmap)
        map + (idref -> Structure(idref, name, group, heading, detail, summary, version, tagStart, tagLength))
      } else throw new IllegalStateException(s"structure definition needs either $idKey or $idRefKey value"))
    }

  /** Find schema and return input stream. This first looks at using the file system, trying each base string as a
    * prefix to the supplied path. If none of the bases work, it tries loading off the classpath.
    * @param path simple file path (use absolute path to load from schema jar).
    * @param basePaths prefix paths for file system access (need to include empty string to load from current directory)
    */
  def findSchema(path: String, basePaths: Array[String]) = {
    def findr(bases: List[String]): InputStream = bases match {
      case h :: t => {
        val file = new File(h + path)
        if (file.exists) new FileInputStream(file)
        else findr(t)
      }
      case _ => {
        val is = getClass.getResourceAsStream(path)
        if (is == null) {
          val is1 = getClass.getClassLoader.getResourceAsStream(path)
          if (is1 == null) {
            val is2 = Thread.currentThread.getContextClassLoader.getResourceAsStream(path)
            if (is2 == null) throw new IllegalArgumentException(s"file $path not found on any classpath")
            else is2
          } else is1
        } else is
      }
    }

    findr(basePaths.toList)
  }

  /** Read schema from YAML document.
    *
    * @param reader
    * @param basePaths prefix paths for file system access
    * @returns schema
    */
  def loadYaml(reader: Reader, basePaths: Array[String]) = {
    val snake = new Yaml(new IgnoringConstructor)

    /** Read schema from YAML document, recursively reading and building on imported schemas. */
    def loadFully(reader: Reader): EdiSchema = {
      val input = snake.loadAs(reader, classOf[ValueMap]).asInstanceOf[ValueMap]
      val version = EdiSchemaVersion(convertEdiForm(input.get(formKey).toString), getAsString(versionKey, input))
      val baseSchema = if (input.containsKey(importsKey)) {
        val impsin = getChildList(importsKey, input).asInstanceOf[java.util.List[String]]
        impsin.asScala.toList.foldLeft(new EdiSchema(version))((acc, path) => {
          schemaCache.get(path) match {
            case Some(schema) => acc.merge(schema)
            case None => {
              val is = findSchema(path, basePaths)
              if (is == null) throw new IllegalArgumentException(s"base schema $path not found")
              val schema = loadFully(new InputStreamReader(is, "UTF-8"))
              schemaCache += path -> schema
              acc.merge(schema)
            }
          }
        })
      } else new EdiSchema(version)
      val elements =
        if (input.containsKey(elementsKey)) convertElements(input, baseSchema.elements)
        else baseSchema.elements
      val composites =
        if (input.containsKey(compositesKey)) convertComposites(input, version.ediForm, elements, baseSchema.composites)
        else baseSchema.composites
      val segments =
        if (input.containsKey(segmentsKey)) convertSegments(input, version.ediForm, elements, composites, baseSchema.segments)
        else baseSchema.segments
      val structures =
        if (input.containsKey(structuresKey)) convertStructures(input, version, segments, baseSchema.structures)
        else baseSchema.structures
      val count = structures.size
      EdiSchema(version, elements, composites, segments, structures)
    }

    val loaded = loadFully(reader)
    EdiSchema(loaded.ediVersion, loaded.elements, loaded.composites, loaded.segments, loaded.structures)
  }
}

/** Constructor for YAML parser to just build lists and maps of values. */
private class IgnoringConstructor extends Constructor {
  val original = this.yamlConstructors.get(null)
  this.yamlConstructors.put(null, new AbstractConstruct {
    def construct(node: Node): Object = {
      if (node.getTag.startsWith("!KnownTag")) original.construct(node)
      else node.getNodeId match {
        case NodeId.scalar => yamlConstructors.get(Tag.STR).construct(node);
        case NodeId.sequence => yamlConstructors.get(Tag.SEQ).construct(node);
        case NodeId.mapping => yamlConstructors.get(Tag.MAP).construct(node);
        case _ => throw new YAMLException("Unexpected node");
      }
    }
  })
}