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

  var ediForm: EdiForm = null
  var identElements = Map[String, Element]()
  var identComposites = Map[String, Composite]()
  var identSegments = Map[String, Segment]()

  /** Get child list value (error if not found). */
  def getChildList(key: String, map: ValueMap): SimpleList = map.get(key) match {
    case child: SimpleList => child
    case null => throw new IllegalArgumentException("Missing required array '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a list")
  }

  def convertCount(values: ValueMap) = {
    values.get(countKey) match {
      case n: Integer => n.toInt
      case ">1" => 0
      case null => 1
      case _ => throw new IllegalArgumentException(s"Value $countKey must be an integer")
    }
  }

  def getUsage(values: ValueMap) = {
    if (ediForm.fixed) {
      if (values.containsKey(usageKey)) {
        val usage = convertUsage(getRequiredString(usageKey, values))
        usage match {
          case MandatoryUsage | UnusedUsage | IgnoredUsage => usage
          case _ => throw new IllegalArgumentException(s"Invalid usage value ${usage.code} for fixed width format")
        }
      } else MandatoryUsage
    } else convertUsage(getRequiredString(usageKey, values))
  }

  /** Build segment components from input data.
    * @param parentId identifier for parent segment or composite
    * @param list list of maps of component data
    * @param composites ident to known composite map
    * @returns components
    */
  def parseSegmentComponents(parentId: String, list: MapList,
    composites: Map[String, Composite]): List[SegmentComponent] = {
    /** Convert a single component, which may have inlined element and/or composite definitions.
      */
    def convertComponent(values: ValueMap, dfltpos: Int) = {
      val name = if (values.containsKey(nameKey)) Some(getRequiredString(nameKey, values)) else None
      val position = if (values.containsKey(positionKey)) getRequiredInt(positionKey, values) else dfltpos
      val key = ediForm.keyName(parentId, name.getOrElse(""), position)
      val use = getUsage(values)
      val count = convertCount(values)
      if (values.containsKey(idRefKey)) {
        val id = getRequiredString(idRefKey, values)
        if (identElements.contains(id)) ElementComponent(identElements(id), name, key, position, use, count)
        else if (composites.contains(id)) {
          val composite = composites(id)
          if (count > 1) CompositeComponent(composite, name, key, position, use, count)
          else CompositeComponent(composite.rewrite(key, ediForm), name, key, position, use, count)
        } else throw new IllegalArgumentException(s"No element or composite with id '$id'")
      } else if (values.containsKey(valuesKey)) {
        val component = convertComposite("", parentId, values, composites)
        CompositeComponent(component, name, key, position, use, count)
      } else {
        val code = getRequiredString(typeKey, values)
        val typ = ediForm.readFormat(code, values)
        val element = Element("", name.getOrElse(""), typ)
        ElementComponent(element, name, key, position, use, count)
      }
    }
    @tailrec
    def parseComponent(remain: List[ValueMap], position: Int, prior: List[SegmentComponent]): List[SegmentComponent] =
      remain match {
        case values :: t => {
          val comp = convertComponent(values, position)
          parseComponent(t, comp.position + 1, comp :: prior)
        }
        case _ => prior.reverse
      }
    parseComponent(list.asScala.toList, 1, Nil)
  }

  /** Convert element definitions. */
  private def convertElements(input: MapList) =
    input.asScala.toList.foldLeft(identElements)(
      (map, elmmap) => {
        val ident = getRequiredString(idKey, elmmap)
        val name = getAs(nameKey, "", elmmap)
        val code = getRequiredString(typeKey, elmmap)
        val typ = ediForm.readFormat(code, elmmap)
        val result = map + (ident -> Element(ident, name, typ))
        result
      })

  private def convertComposite(ident: String, parentId: String, input: ValueMap, knowns: Map[String, Composite]) = {
    val name = getAs(nameKey, "", input)
    val list = getRequiredMapList(valuesKey, input)
    val comps = parseSegmentComponents(parentId, list, knowns)
    val rules = if (input.containsKey(rulesKey)) {
      val data = getRequiredMapList(rulesKey, input)
      parseRules(data, comps)
    } else Nil
    val max = if (input.containsKey(maxLengthKey)) getRequiredInt(maxLengthKey, input) else 0
    Composite(ident, name, comps, rules, max)
  }

  /** Convert composite definitions. To work with HL7 this makes multiple passes through the input, each time processing
    * definitions which only use already-defined values. That's not the most efficient approach since it could
    * theoretically require many passes, but it's easy to implement and should be fine for actual schemas.
    */
  private def convertComposites(input: MapList) = {

    /** Build dependency set for composite, as set of component idents. */
    def depSet(compmap: ValueMap) = getRequiredMapList(valuesKey, compmap).asScala.
      foldLeft(Set[String]())((set, item) =>
        if (item.containsKey(idRefKey)) set + getRequiredString(idRefKey, item) else set)

    type MapsWithDeps = List[(ValueMap, Set[String])]

    /** Recursively build composites which only use already-defined composites until all are built. */
    @tailrec
    def convertr(remain: MapsWithDeps, deferred: MapsWithDeps, knowns: Map[String, Composite]): Map[String, Composite] =
      remain match {
        case h :: t =>
          if (h._2.forall { ident => knowns.contains(ident) || identElements.contains(ident) }) {
            val compmap = h._1
            val ident = getRequiredString(idKey, compmap)
            val name = getAs(nameKey, "", compmap)
            val list = getRequiredMapList(valuesKey, compmap)
            val comps = parseSegmentComponents(ident, list, knowns)
            val rules = if (compmap.containsKey(rulesKey)) {
              val data = getRequiredMapList(rulesKey, compmap)
              parseRules(data, comps)
            } else Nil
            val max = if (compmap.containsKey(maxLengthKey)) getRequiredInt(maxLengthKey, compmap) else 0
            convertr(t, deferred, knowns + (ident -> convertComposite(ident, ident, compmap, knowns)))
          } else convertr(t, h :: deferred, knowns)
        case _ =>
          if (deferred.nonEmpty) convertr(deferred, Nil, knowns)
          else knowns
      }

    val compList = input.asScala.toList.map(map => (map, depSet(map)))
    convertr(compList, Nil, identComposites)
  }

  def getUsageOverride(values: ValueMap, dflt: Usage) =
    if (values.containsKey(usageKey)) getUsage(values) else dflt

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
    * @param base segment components in base definition (overlayed by parsed values)
    * @returns components
    */
  def parseSegmentOverlayComponents(parentId: String, list: MapList, base: List[SegmentComponent]): List[SegmentComponent] = {
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

  def getRules(segmap: ValueMap, comps: List[SegmentComponent]) =
    if (segmap.containsKey(rulesKey)) {
      val data = getRequiredMapList(rulesKey, segmap)
      parseRules(data, comps)
    } else Nil

  def buildSegment(ident: String, segmap: ValueMap) = {
    val tag = getAs(tagKey, ident, segmap)
    val list = getAs[MapList](valuesKey, segmap)
    val comps = if (list == null) Nil else parseSegmentComponents(ident, list, identComposites)
    val name = getAs(nameKey, "", segmap)
    Segment(ident, tag, name, comps, getRules(segmap, comps))
  }

  /** Build structure components from input data.
    * @param maps list of maps of component data
    * @param table structure table index
    * @returns components
    */
  def parseStructureComponents(maps: List[ValueMap], table: Int) = {
    val digits = {
      @tailrec
      def countDigits(value: Int, digits: Int): Int = if (value < 10) digits else countDigits(value / 10, digits + 1)
      countDigits(maps.size, 1)
    }
    def convertPosition(number: Int) = {
      val text = number.toString
      if (text.size < digits) "0" * (digits - text.size)
      else text
    }
    def convertComponent(values: ValueMap, seqPos: Int): StructureComponent = {
      def definePosition(key: String) = {
        if (ediForm.segmentsPositioned) new DefinedPosition(table, getRequiredString(key, values))
        else StartPosition
      }
      val use = convertUsage(getAs(usageKey, MandatoryUsage.code, values))
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
        val position = if (postext == null) None else Some(new DefinedPosition(table, postext))
        val seq = StructureSequence(true, parseComponent(items.asScala.toList, Nil, seqPos))
        val tagStart = getIntOption(tagStartKey, values)
        val tagLength = getIntOption(tagLengthKey, values)
        GroupComponent(ident, use, count, seq, None, Nil, None, position, false, tagStart, tagLength)
      } else if (values.containsKey(wrapIdKey)) {
        val wrapid = getRequiredString(wrapIdKey, values)
        val list = getRequiredMapList(groupKey, values)
        if (list.size != 1) throw new IllegalArgumentException(s"Single group definition required for loop with $wrapIdKey $wrapid")
        val group = convertComponent(list.iterator.next, seqPos).asInstanceOf[GroupComponent]
        if (identSegments.contains(ediForm.loopWrapperStart) && identSegments.contains(ediForm.loopWrapperEnd)) {
          val start = definePosition(positionKey)
          val end = definePosition(endPositionKey)
          LoopWrapperComponent(identSegments(ediForm.loopWrapperStart), identSegments(ediForm.loopWrapperEnd), start,
            end, OptionalUsage, wrapid, group)
        } else throw new IllegalArgumentException(s"Missing loop wrapper segment definition (${ediForm.loopWrapperStart} or ${ediForm.loopWrapperEnd})")
      } else {
        val position = 
          if (ediForm.segmentsPositioned) new DefinedPosition(table, getAs(positionKey, convertPosition(seqPos), values))
          else StartPosition
        if (values.containsKey(idRefKey)) {
          val id = getRequiredString(idRefKey, values)
          if (identSegments.contains(id)) {
            ReferenceComponent(identSegments(id), position, use, count)
          } else throw new IllegalArgumentException(s"No segment with id '$id'")
        } else ReferenceComponent(buildSegment("", values), position, use, count)
      }
    }
    @tailrec
    def parseComponent(remain: List[ValueMap], prior: List[StructureComponent], number: Int): List[StructureComponent] =
      remain match {
        case values :: t => parseComponent(t, convertComponent(values, number) :: prior, number + 1)
        case _ => prior.reverse
      }
    parseComponent(maps, Nil, 1)
  }

  /** Build structure part from input data.
    * @param name part name
    * @param values map of component data lists
    * @param table structure table index
    * @returns components
    */
  def parseStructurePart(name: String, values: ValueMap, table: Int) =
    if (values.containsKey(name)) {
      val vallist = getRequiredMapList(name, values).asScala.toList
      parseStructureComponents(vallist, table)
    } else Nil

  /** Build structure part as overlay to base part.
    * @param name part name
    * @param overs map of component data overlay lists
    * @param base original components
    * @param segments known segments map
    * @returns components
    */
  def parseStructureOverlayPart(name: String, overs: ValueMap, base: List[StructureComponent]) =
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

  /** Convert segment definitions. */
  private def convertSegments(input: MapList) = {

    def trimComps(comps: List[SegmentComponent], trim: Int) = {
      val (keep, drop) = comps.splitAt(trim)
      keep ::: (drop.foldLeft(List[SegmentComponent]())((acc, comp) => comp match {
        case ElementComponent(element, oname, key, pos, _, count, value) =>
          ElementComponent(element, oname, key, pos, UnusedUsage, count, value) :: acc
        case CompositeComponent(composite, oname, key, pos, _, count) =>
          CompositeComponent(composite, oname, key, pos, UnusedUsage, count) :: acc
      }).reverse)
    }

    input.asScala.toList.
      foldLeft(identSegments)((map, segmap) => if (segmap.containsKey(idKey)) {
        val ident = getRequiredString(idKey, segmap)
        map + (ident -> buildSegment(ident, segmap))
      } else if (segmap.containsKey(idRefKey)) {
        val idref = getRequiredString(idRefKey, segmap)
        identSegments.get(idref) match {
          case Some(basedef) => {
            val name = getAs(nameKey, basedef.name, segmap)
            val basecomps = if (segmap.containsKey(trimKey)) trimComps(basedef.components, getRequiredInt(trimKey, segmap)) else basedef.components
            if (segmap.containsKey(valuesKey)) {
              val list = getRequiredMapList(valuesKey, segmap)
              val comps = parseSegmentOverlayComponents(idref, list, Nil)
              map + (idref -> Segment(idref, basedef.tag, name, comps, getRules(segmap, comps)))
            } else {
              map + (idref -> Segment(idref, basedef.tag, name, basedef.components, getRules(segmap, basecomps)))
            }
          }
          case None => throw new IllegalStateException(s"referenced segment $idref is not defined")
        }
      } else throw new IllegalStateException(s"segment definition needs either $idKey or $idRefKey value"))
  }
  def optSeq(items: List[StructureComponent]) = if (items.isEmpty) None else Some(StructureSequence(false, items))
  def sectionItems(optseq: Option[StructureSequence]): List[StructureComponent] = optseq match {
    case Some(seq) => seq.items
    case None => Nil
  }
  def dataSection(key: String, values: ValueMap, index: Int): Option[StructureSequence] =
    if (values.containsKey(key)) {
      if (!ediForm.layout.sectioned) throw new IllegalArgumentException(s"section $key is not allowed for schemas of type ${ediForm.text}")
      optSeq(parseStructurePart(key, values, index))
    } else None

  private def buildStructure(ident: String, transmap: ValueMap, version: EdiSchemaVersion) = {
    val name = getRequiredString(nameKey, transmap)
    val group = getStringOption(classKey, transmap)
    val firstkey = if (ediForm.layout.sectioned) headingKey else dataKey
    val heading = optSeq(parseStructurePart(firstkey, transmap, 0))
    val detail = dataSection(detailKey, transmap, 1)
    val summary = dataSection(summaryKey, transmap, 2)
    val tagStart = getIntOption(tagStartKey, transmap)
    val tagLength = getIntOption(tagLengthKey, transmap)
    Structure(ident, name, group, heading, detail, summary, version, tagStart, tagLength)
  }

  private def overlayStructure(transmap: ValueMap, version: EdiSchemaVersion, basedef: Structure) = {
    val name = getAs(nameKey, basedef.name, transmap)
    val group = getStringOption(classKey, basedef.group, transmap)
    val firstkey = if (ediForm.layout.sectioned) headingKey else dataKey
    val heading = optSeq(parseStructureOverlayPart(firstkey, transmap, sectionItems(basedef.heading)))
    val detail = optSeq(parseStructureOverlayPart(detailKey, transmap, sectionItems(basedef.detail)))
    val summary = optSeq(parseStructureOverlayPart(summaryKey, transmap, sectionItems(basedef.summary)))
    val tagStart = getIntOption(tagStartKey, basedef.tagStart, transmap)
    val tagLength = getIntOption(tagLengthKey, basedef.tagLength, transmap)
    Structure(basedef.ident, name, group, heading, detail, summary, version, tagStart, tagLength)
  }

  /** Convert structure definitions. */
  private def convertStructures(input: ValueMap, version: EdiSchemaVersion, basedefs: EdiSchema.StructureMap) = {
    getRequiredMapList(structuresKey, input).asScala.toList.
      foldLeft(basedefs)((map, transmap) => if (transmap.containsKey(idKey)) {
        val ident = getRequiredString(idKey, transmap)
        map + (ident -> buildStructure(ident, transmap, version))
      } else if (transmap.containsKey(idRefKey)) {
        val idref = getRequiredString(idRefKey, transmap)
        if (!basedefs.contains(idref)) throw new IllegalStateException(s"referenced structure $idref is not defined")
        val basedef = basedefs(idref)
        map + (idref -> overlayStructure(transmap, version, basedef))
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
      ediForm = convertEdiForm(input.get(formKey).toString)
      val version = EdiSchemaVersion(ediForm, getAsString(versionKey, input))
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
      identElements = baseSchema.elements
      identComposites = baseSchema.composites
      identSegments = baseSchema.segments
      if (!ediForm.layout.structures && input.containsKey(valuesKey)) {

        // schema with a single segment definition
        val ident = getAs(idKey, "", input)
        identSegments = Map(ident -> buildSegment(ident, input))
        EdiSchema(version, identElements, identComposites, identSegments, Map())

      } else if ((!ediForm.layout.sectioned && input.containsKey(dataKey)) ||
        (ediForm.layout.sectioned && (input.containsKey(headingKey) || input.containsKey(detailKey) ||
          input.containsKey(summaryKey)))) {

        // schema with a single structure definition or overlay definition
        val struct = if (input.containsKey(idRefKey)) {
          val idref = getRequiredString(idRefKey, input)
          val basedefs = baseSchema.structures
          if (!basedefs.contains(idref)) throw new IllegalStateException(s"referenced structure $idref is not defined")
          val basedef = basedefs(idref)
          overlayStructure(input, version, basedef)
        } else {
          val ident = getAs(idKey, "", input)
          buildStructure(ident, input, version)
        }
        val identStructs = Map(struct.ident -> struct)
        EdiSchema(version, identElements, identComposites, identSegments, identStructs)

      } else {

        // normal schema with all sections potentially present
        if (input.containsKey(elementsKey)) identElements = convertElements(getRequiredMapList(elementsKey, input))
        if (input.containsKey(compositesKey)) identComposites =
          convertComposites(getRequiredMapList(compositesKey, input))
        if (input.containsKey(segmentsKey)) identSegments = convertSegments(getRequiredMapList(segmentsKey, input))
        val structures =
          if (input.containsKey(structuresKey)) convertStructures(input, version, baseSchema.structures)
          else baseSchema.structures
        EdiSchema(version, identElements, identComposites, identSegments, structures)
      }
    }

    loadFully(reader)
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