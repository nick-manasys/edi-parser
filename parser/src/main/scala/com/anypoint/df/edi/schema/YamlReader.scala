package com.anypoint.df.edi.schema

import java.io.File
import java.io.FileReader
import java.io.Reader
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
import java.io.StringWriter
import java.io.StringReader
import scala.collection.immutable.AbstractMap
import com.anypoint.df.edi.lexical.EdiConstants
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.InputStream

/** Read YAML representation of EDI schema.
  *
  * @author MuleSoft, Inc.
  */
object YamlReader extends YamlDefs with SchemaJavaDefs {

  import EdiSchema._

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
    * @returns components
    */
  def parseSegmentComponents(parentId: String, list: MapList, elements: Map[String, Element],
    composites: Map[String, Composite]): List[SegmentComponent] = {
    def convertComponent(values: ValueMap, dfltpos: Int) = {
      val id = getRequiredString(idRefKey, values)
      val name = if (values.containsKey(nameKey)) Some(getRequiredString(nameKey, values)) else None
      val position = if (values.containsKey(positionKey)) getRequiredInt(positionKey, values) else dfltpos
      val key = keyName(parentId, position)
      val use = convertUsage(getRequiredString(usageKey, values))
      val count = values.get(countKey) match {
        case n: Integer => n.toInt
        case null => 1
        case _ => throw new IllegalArgumentException(s"Value $countKey must be an integer")
      }

      if (elements.contains(id)) ElementComponent(elements(id), name, key, position, use, count)
      else if (composites.contains(id)) {
        val composite = composites(id)
        if (count > 1) CompositeComponent(composite, name, key, position, use, count)
        else CompositeComponent(composite.rewrite(key), name, key, position, use, count)
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
        case comp: CompositeComponent => CompositeComponent(comp.composite, name, comp.key, comp.position, use, count)
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

  /** Build transaction components from input data.
    * @param maps list of maps of component data
    * @param form EDI form
    * @param table transaction table index
    * @param segments known segments map
    * @returns components
    */
  def parseTransactionComponents(maps: List[ValueMap], form: EdiForm, table: Int, segments: Map[String, Segment]) = {
    def convertComponent(values: ValueMap): TransactionComponent = {
      val use = convertUsage(getRequiredString(usageKey, values))
      val count = values.get(countKey) match {
        case n: Integer => n.toInt
        case s: String if (s == anyRepeatsValue) => -1
        case null => 1
        case _ => throw new IllegalArgumentException(s"Value $countKey must be an integer or the string '$anyRepeatsValue'")
      }
      if (values.containsKey(itemsKey)) {
        val items = getRequiredMapList(itemsKey, values)
        GroupComponent(getRequiredString(loopIdKey, values), use, count, parseComponent(items.asScala.toList, Nil), None, Nil)
      } else if (values.containsKey(wrapIdKey)) {
        val wrapid = getRequiredString(wrapIdKey, values)
        val list = getRequiredMapList(loopKey, values)
        if (list.size != 1) throw new IllegalArgumentException(s"Single group definition required for loop with $wrapIdKey $wrapid")
        val group = convertComponent(list.get(0)).asInstanceOf[GroupComponent]
        if (segments.contains(form.loopWrapperStart) && segments.contains(form.loopWrapperEnd)) {
          val start = SegmentPosition(table, getRequiredString(positionKey, values))
          val end = SegmentPosition(table, getRequiredString(endPositionKey, values))
          LoopWrapperComponent(segments(form.loopWrapperStart), segments(form.loopWrapperEnd), start, end,  wrapid, group)
        } else throw new IllegalArgumentException(s"Missing loop wrapper segment definition (${form.loopWrapperStart} or ${form.loopWrapperEnd})")
      } else {
        val id = getRequiredString(idRefKey, values)
        val position = getRequiredString(positionKey, values)
        if (segments.contains(id)) ReferenceComponent(segments(id), SegmentPosition(table, position), use, count)
        else throw new IllegalArgumentException(s"No segment with id '$id'")
      }
    }
    @tailrec
    def parseComponent(remain: List[ValueMap], prior: List[TransactionComponent]): List[TransactionComponent] =
      remain match {
        case values :: t => parseComponent(t, convertComponent(values) :: prior)
        case _ => prior.reverse
      }
    parseComponent(maps, Nil)
  }

  /** Build transaction part from input data.
    * @param name part name
    * @param values map of component data lists
    * @param form EDI form
    * @param table transaction table index
    * @param segments known segments map
    * @returns components
    */
  def parseTransactionPart(name: String, values: ValueMap, form: EdiForm, table: Int, segments: Map[String, Segment]) =
    if (values.containsKey(name)) {
      val vallist = getRequiredMapList(name, values).asScala.toList
      parseTransactionComponents(vallist, form, table, segments)
    } else Nil

  /** Build transaction part as overlay to base part.
    * @param name part name
    * @param overs map of component data overlay lists
    * @param base original components
    * @param segments known segments map
    * @returns components
    */
  def parseTransactionOverlayPart(name: String, overs: ValueMap, base: List[TransactionComponent],
    segments: Map[String, Segment]) =
    if (overs.containsKey(name)) {
      def overLevel(overs: MapList, base: List[TransactionComponent]) =
        overr(overs.asScala.toList, Nil, base)
      def overGroup(over: ValueMap, group: GroupComponent) =
        overr(over :: Nil, Nil, group :: Nil).head.asInstanceOf[GroupComponent]
      @tailrec
      def overr(remain: List[ValueMap], prior: List[TransactionComponent], base: List[TransactionComponent]): List[TransactionComponent] = remain match {
        case values :: t => {
          val position = getRequiredString(positionKey, values)
          base match {
            case (refc: ReferenceComponent) :: bt if (refc.position == position) => {
              if (values.containsKey(idRefKey)) {
                val compare = getRequiredString(idRefKey, values)
                if (compare != refc.segment.ident) throw new IllegalStateException(s"segment at position $position is not $compare")
              }
              val use = getUsageOverride(values, refc.usage)
              val count = getCountOverride(values, refc.count)
              overr(t, ReferenceComponent(refc.segment, refc.position, use, count) :: prior, bt)
            }
            case (wrap: LoopWrapperComponent) :: bt if (wrap.position == position) => {
              if (values.containsKey(wrapIdRefKey)) {
                val compare = getRequiredString(wrapIdRefKey, values)
                if (compare != wrap.ident) throw new IllegalStateException(s"wrapper at position $position is not $compare")
              }
              val use = getUsageOverride(values, wrap.usage)
              val count = getCountOverride(values, wrap.count)
              val group =
                if (values.containsKey(loopKey)) overGroup(getRequiredValueMap(loopKey, values), wrap.loopGroup)
                else wrap.loopGroup
              overr(t, LoopWrapperComponent(wrap.open, wrap.close, wrap.position, wrap.endPosition, wrap.ident, group) :: prior, bt)
            }
            case (group: GroupComponent) :: bt if (group.position == position) => {
              if (values.containsKey(loopIdRefKey)) {
                val compare = getRequiredString(loopIdRefKey, values)
                if (compare != group.ident) throw new IllegalStateException(s"loop at position $position is not $compare")
              }
              val use = getUsageOverride(values, group.usage)
              val count = getCountOverride(values, group.count)
              val items =
                if (values.containsKey(itemsKey)) overLevel(getRequiredMapList(itemsKey, values), group.items)
                else group.items
              overr(t, GroupComponent(group.ident, use, count, items, group.varkey, group.variants) :: prior, bt)
            }
            case bh :: bt => overr(remain, bh :: prior, bt)
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

  /** Convert composite definitions. */
  private def convertComposites(input: ValueMap, elements: Map[String, Element], basedefs: Map[String, Composite]) =
    getRequiredMapList(compositesKey, input).asScala.toList.foldLeft(basedefs)(
      (map, compmap) => {
        val ident = getRequiredString(idKey, compmap)
        val name = getRequiredString(nameKey, compmap)
        val list = getRequiredMapList(valuesKey, compmap)
        val comps = parseSegmentComponents(ident, list, elements, map)
        val rules = if (compmap.containsKey(rulesKey)) {
          val data = getRequiredMapList(rulesKey, compmap)
          parseRules(data, comps)
        } else Nil
        map + (ident -> Composite(ident, name, comps, rules))
      })

  /** Convert segment definitions. */
  private def convertSegments(input: ValueMap, elements: Map[String, Element], composites: Map[String, Composite],
    basedefs: Map[String, Segment]) = {
    def getRules(segmap: ValueMap, comps: List[SegmentComponent]) =
      if (segmap.containsKey(rulesKey)) {
        val data = getRequiredMapList(rulesKey, segmap)
        parseRules(data, comps)
      } else Nil

    def trimComps(comps: List[SegmentComponent], trim: Int) = {
      val (keep, drop) = comps.splitAt(trim)
      keep ::: (drop.foldLeft(List[SegmentComponent]())((acc, comp) => comp match {
        case ElementComponent(element, oname, key, pos, _, count) => ElementComponent(element, oname, key, pos, UnusedUsage, count) :: acc
        case CompositeComponent(composite, oname, key, pos, _, count) => CompositeComponent(composite, oname, key, pos, UnusedUsage, count) :: acc
      }).reverse)
    }

    getRequiredMapList(segmentsKey, input).asScala.toList.
      foldLeft(basedefs)((map, segmap) => if (segmap.containsKey(idKey)) {
        val ident = getRequiredString(idKey, segmap)
        val list = getRequiredMapList(valuesKey, segmap)
        val comps = parseSegmentComponents(ident, list, elements, composites)
        val name = getRequiredString(nameKey, segmap)
        map + (ident -> Segment(ident, name, comps, getRules(segmap, comps)))
      } else if (segmap.containsKey(idRefKey)) {
        val idref = getRequiredString(idRefKey, segmap)
        basedefs.get(idref) match {
          case Some(basedef) => {
            val name = getAs(nameKey, basedef.name, segmap)
            val basecomps = if (segmap.containsKey(trimKey)) trimComps(basedef.components, getRequiredInt(trimKey, segmap)) else basedef.components
            if (segmap.containsKey(valuesKey)) {
              val list = getRequiredMapList(valuesKey, segmap)
              val comps = parseSegmentOverlayComponents(idref, list, elements, composites, basecomps)
              map + (idref -> Segment(idref, name, comps, getRules(segmap, comps)))
            } else {
              map + (idref -> Segment(idref, name, basedef.components, getRules(segmap, basecomps)))
            }
          }
          case None => throw new IllegalStateException(s"referenced segment $idref is not defined")
        }
      } else throw new IllegalStateException(s"segment definition needs either $idKey or $idRefKey value"))
  }

  /** Convert transaction definitions. */
  private def convertTransactions(input: ValueMap, form: EdiForm, segments: Map[String, Segment],
    basedefs: EdiSchema.TransactionMap) =
    getRequiredMapList(transactionsKey, input).asScala.toList.
      foldLeft(basedefs)((map, transmap) => if (transmap.containsKey(idKey)) {
        val ident = getRequiredString(idKey, transmap)
        val name = getRequiredString(nameKey, transmap)
        val group = if (transmap.containsKey(groupKey)) transmap.get(groupKey).asInstanceOf[String] else ""
        val heading = parseTransactionPart(headingKey, transmap, form, 0, segments)
        val detail = parseTransactionPart(detailKey, transmap, form, 1, segments)
        val summary = parseTransactionPart(summaryKey, transmap, form, 2, segments)
        map + (ident -> Transaction(ident, name, group, heading, detail, summary))
      } else if (transmap.containsKey(idRefKey)) {
        val idref = getRequiredString(idRefKey, transmap)
        if (!basedefs.contains(idref)) throw new IllegalStateException(s"referenced transaction $idref is not defined")
        val basedef = basedefs(idref)
        val name = getAs(nameKey, basedef.name, transmap)
        val group = getAs(groupKey, basedef.group, transmap)
        val heading = parseTransactionOverlayPart(headingKey, transmap, basedef.heading, segments)
        val detail = parseTransactionOverlayPart(detailKey, transmap, basedef.detail, segments)
        val summary = parseTransactionOverlayPart(summaryKey, transmap, basedef.summary, segments)
        map + (idref -> Transaction(idref, name, group, heading, detail, summary))
      } else throw new IllegalStateException(s"transaction definition needs either $idKey or $idRefKey value"))

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
            if (is2 == null) {
              println(System.getProperty("java.class.path"))
              throw new IllegalArgumentException(s"file $path not found on any classpath")
            } else is2
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
    val snake = new Yaml(new IgnoringConstructor())

    /** Read schema from YAML document, recursively reading and building on imported schemas. */
    def loadFully(reader: Reader): EdiSchema = {
      val input = snake.loadAs(reader, classOf[ValueMap]).asInstanceOf[ValueMap];
      val form = convertEdiForm(input.get(formKey).toString)
      val version = getRequiredString(versionKey, input)
      val baseSchema = if (input.containsKey(importsKey)) {
        val impsin = getChildList(importsKey, input).asInstanceOf[java.util.List[String]]
        impsin.asScala.toList.foldLeft(new EdiSchema(version))((acc, path) => {
          val is = findSchema(path, basePaths)
          if (is == null) throw new IllegalArgumentException(s"base schema $path not found")
          acc.merge(loadFully(new InputStreamReader(is, "UTF-8")))
        })
      } else new EdiSchema(version)
      val elements =
        if (input.containsKey(elementsKey)) convertElements(input, baseSchema.elements)
        else baseSchema.elements
      val composites =
        if (input.containsKey(compositesKey)) convertComposites(input, elements, baseSchema.composites)
        else baseSchema.composites
      val segments =
        if (input.containsKey(segmentsKey)) convertSegments(input, elements, composites, baseSchema.segments)
        else baseSchema.segments
      val transactions =
        if (input.containsKey(transactionsKey)) convertTransactions(input, form, segments, baseSchema.transactions)
        else baseSchema.transactions
      EdiSchema(form, version, elements, composites, segments, transactions)
    }

    val loaded = loadFully(reader)
    EdiSchema(loaded.ediForm, loaded.version, loaded.elements, loaded.composites, loaded.segments,
      loaded.transactions.map{
        case (k, Transaction(ident, name, group, heading, detail, summary)) => (k,
          Transaction(ident, name, group, heading, detail, summary))
      })
  }
}

/** Constructor for YAML parser to just build lists and maps of values. */
private class IgnoringConstructor extends Constructor {
  val original = this.yamlConstructors.get(null)
  this.yamlConstructors.put(null, new AbstractConstruct {
    def construct(node: Node): Object = {
      if (node.getTag().startsWith("!KnownTag")) original.construct(node)
      else node.getNodeId() match {
        case NodeId.scalar => yamlConstructors.get(Tag.STR).construct(node);
        case NodeId.sequence => yamlConstructors.get(Tag.SEQ).construct(node);
        case NodeId.mapping => yamlConstructors.get(Tag.MAP).construct(node);
        case _ => throw new YAMLException("Unexpected node");
      }
    }
  })
}