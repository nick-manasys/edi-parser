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
object YamlReader extends YamlDefs {

  import EdiSchema._

  type JavaMap[K, V] = java.util.Map[K, V]
  type AnyMap = JavaMap[String, Any]
  type JavaList[V] = java.util.List[V]

  //
  // functions for working with parsed data

  /** Get child map value (error if not found). */
  def getChildMap[K, V](key: String, map: JavaMap[K, V]): JavaMap[K, V] = map.get(key) match {
    case child: JavaMap[K, V] => child
    case null => throw new IllegalArgumentException("Missing required map value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a map")
  }

  /** Get child list value (error if not found). */
  def getChildList[K, V](key: String, map: JavaMap[K, V]): JavaList[V] = map.get(key) match {
    case child: JavaList[V] => child
    case null => throw new IllegalArgumentException("Missing required array '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a list")
  }

  /** Get child list of maps value (error if not found). */
  def getRequiredMapList(key: String, map: AnyMap): JavaList[AnyMap] = map.get(key) match {
    case child: JavaList[AnyMap] => child
    case null => throw new IllegalArgumentException("Missing required list of maps value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a list of maps")
  }

  /** Get child list of strings value (error if not found). */
  def getRequiredStringList(key: String, map: AnyMap): JavaList[String] = map.get(key) match {
    case child: JavaList[String] => child
    case null => throw new IllegalArgumentException("Missing required list of strings value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a list of srings")
  }

  /** Get child string value (error if not found). */
  def getRequiredString(key: String, map: AnyMap): String = map.get(key) match {
    case child: String => child
    case null => throw new IllegalArgumentException("Missing required string value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a string")
  }

  /** Get child int value (error if not found). */
  def getRequiredInt(key: String, map: AnyMap): Int = map.get(key) match {
    case n: Int => n
    case null => throw new IllegalArgumentException("Missing required integer value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not an integer")
  }

  /** Build segment components from input data.
    * @param parentId identifier for parent segment or composite
    * @param list list of maps of component data
    * @param rules list of maps of rules
    * @param elements known elements map
    * @param composites known composites map
    * @returns components
    */
  def parseSegmentComponents(parentId: String, list: JavaList[AnyMap], elements: Map[String, Element],
    composites: Map[String, Composite]): List[SegmentComponent] = {
    def convertComponent(values: AnyMap, dfltpos: Int) = {
      val id = getRequiredString(idRefKey, values)
      val name = if (values.containsKey(nameKey)) Some(getRequiredString(nameKey, values)) else None
      val position = if (values.containsKey(positionKey)) getRequiredInt(positionKey, values) else dfltpos
      val key = keyName(parentId, position)
      val use = convertUsage(getRequiredString(usageKey, values))
      val repeat = values.get(countKey) match {
        case n: Int => n
        case null => 1
        case _ => throw new IllegalArgumentException(s"Value $countKey must be an integer")
      }

      if (elements.contains(id)) ElementComponent(elements(id), name, key, position, use, repeat)
      else if (composites.contains(id)) {
        val composite = composites(id)
        if (repeat > 1) CompositeComponent(composite, name, key, position, use, repeat)
        else CompositeComponent(composite.rewrite(key), name, key, position, use, repeat)
      } else throw new IllegalArgumentException(s"No element or composite with id '$id'")
    }
    @tailrec
    def parseComponent(remain: List[AnyMap], position: Int, prior: List[SegmentComponent]): List[SegmentComponent] =
      remain match {
        case values :: t => parseComponent(t, position + 1, convertComponent(values, position) :: prior)
        case _ => prior.reverse
      }
    parseComponent(list.asScala.toList, 1, Nil)
  }

  /** Build rules from input data.
    * @param rules list of maps of rule data
    * @param comps components rule applies to
    * @returns rules
    */
  def parseRules(rules: JavaList[AnyMap], comps: List[SegmentComponent]): List[OccurrenceRule] = {
    val posidx = comps.foldLeft(Map[Int, SegmentComponent]())((acc, comp) => acc + (comp.position -> comp))
    def convertRule(map: AnyMap) = {
      val comps = getChildList(valuesKey, map).asScala.toList.map(value => value match {
        case pos: Int => posidx(pos)
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
    *
    * @param list list of maps of component data
    * @param segments known segments map
    * @returns components
    */
  def parseTransactionComponents(base: List[AnyMap], over: List[AnyMap], segments: Map[String, Segment]) = {
    def mergeOverlay = {
      @tailrec
      def merger(after: String, baserem: List[AnyMap], overrem: List[AnyMap], acc: List[AnyMap]): List[AnyMap] =
        overrem match {
          case overh :: overt =>
            if (after == overh.get(afterKey)) merger(after, baserem, overt, overh :: acc)
            else baserem match {
              case baseh :: baset => {
                val id = baseh.get(idRefKey).asInstanceOf[String]
                if (id == overh.get(idRefKey)) {
                  val keyiter = overh.keySet.iterator
                  while (keyiter.hasNext) {
                    val key = keyiter.next
                    baseh.put(key, overh.get(key))
                  }
                  merger(id, baset, overt, baseh :: acc)
                } else merger(id, baset, overrem, baseh :: acc)
              }
              case _ => throw new IllegalArgumentException(s"overlay value ${overh.get(idRefKey)} does not match base")
            }
          case _ => baserem.reverse ::: acc
        }
      merger("", base, over, Nil) reverse
    }
    def convertComponent(values: AnyMap) = {
      val use = convertUsage(getRequiredString(usageKey, values))
      val count = values.get(countKey) match {
        case n: Int => n
        case s: String if (s == anyRepeatsValue) => -1
        case null => 1
        case _ => throw new IllegalArgumentException(s"Value $countKey must be an integer or the string '$anyRepeatsValue'")
      }
      if (values.containsKey(itemsKey)) {
        val items = getRequiredMapList(itemsKey, values)
        GroupComponent(getRequiredString(loopIdKey, values), use, count, parseComponent(items.asScala.toList, Nil), None, Nil)
      } else {
        val id = getRequiredString(idRefKey, values)
        val position = getRequiredString(positionKey, values)
        if (segments.contains(id)) ReferenceComponent(segments(id), position, use, count)
        else throw new IllegalArgumentException(s"No segment with id '$id'")
      }
    }
    @tailrec
    def parseComponent(remain: List[AnyMap], prior: List[TransactionComponent]): List[TransactionComponent] =
      remain match {
        case values :: t => parseComponent(t, convertComponent(values) :: prior)
        case _ => prior.reverse
      }
    parseComponent(mergeOverlay, Nil)
  }

  /** Build transaction part from input data.
    *
    * @param name part name
    * @param values map of component data lists
    * @param segments known segments map
    * @returns components
    */
  def parseTransactionPart(name: String, values: AnyMap, overs: AnyMap, segments: Map[String, Segment]) =
    if (values.containsKey(name)) {
      val vallist = getRequiredMapList(name, values).asScala.toList
      val overlist = if (overs.containsKey(name)) getRequiredMapList(name, overs).asScala.toList else Nil
      parseTransactionComponents(vallist, overlist, segments)
    } else Nil

  /** Map with iteration order matching insertion order. This should only be used when the map is rarely, if ever,
    * going to have an element removed since it uses a linear search for this case.
    */
  private class InsertionOrderedMap[K, V](base: Map[K, V], keys: List[K]) extends AbstractMap[K, V] {
    def this() = this(Map.empty[K, V], Nil)
    def +[V1 >: V](kv: (K, V1)): Map[K, V1] = new InsertionOrderedMap(base + kv, keys :+ kv._1)
    def -(key: K) = {
      @tailrec
      def dropKey(past: List[K], remain: List[K]): List[K] = remain match {
        case h :: t =>
          if (key == h) past.reverse ::: t
          else dropKey(h :: past, t)
        case Nil => past.reverse
      }
      new InsertionOrderedMap(base - key, dropKey(Nil, keys))
    }
    def get(key: K) = base get (key)
    def iterator = keys.iterator.map(k => (k, base(k)))
  }

  /** Convert element definitions. */
  private def convertElements(input: AnyMap) = {
    val elmsin = getRequiredMapList(elementsKey, input)
    elmsin.asScala.toList.foldLeft[Map[String, Element]](new InsertionOrderedMap[String, Element])(
      (map, elmmap) =>
        {
          val ident = getRequiredString(idKey, elmmap)
          val name = getRequiredString(nameKey, elmmap)
          val typ = EdiConstants.toX12Type(getRequiredString(typeKey, elmmap))
          val min = getRequiredInt(minLengthKey, elmmap)
          val max = getRequiredInt(maxLengthKey, elmmap)
          map + (ident -> Element(ident, name, typ, min, max))
        })
  }

  /** Convert composite definitions. */
  private def convertComposites(input: AnyMap, elements: Map[String, Element]) = {
    val compsin = getRequiredMapList(compositesKey, input)
    compsin.asScala.toList.foldLeft[Map[String, Composite]](new InsertionOrderedMap[String, Composite])((map, compmap) =>
      {
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
  }

  /** Convert segment definitions. */
  private def convertSegments(input: AnyMap, elements: Map[String, Element], composites: Map[String, Composite]) = {
    val segsin = getRequiredMapList(segmentsKey, input)
    segsin.asScala.toList.foldLeft[Map[String, Segment]](new InsertionOrderedMap[String, Segment])(
      (map, segmap) =>
        {
          val ident = getRequiredString(idKey, segmap)
          val name = getRequiredString(nameKey, segmap)
          val list = getRequiredMapList(valuesKey, segmap)
          val comps = parseSegmentComponents(ident, list, elements, composites)
          val rules = if (segmap.containsKey(rulesKey)) {
            val data = getRequiredMapList(rulesKey, segmap)
            parseRules(data, comps)
          } else Nil
          map + (ident -> Segment(ident, name, comps, rules))
        })

  }

  /** Convert transaction definitions. */
  private def convertTransactions(input: AnyMap, segments: Map[String, Segment]) = {
    val transin = getRequiredMapList(transactionsKey, input)
    transin.asScala.toList.foldLeft[Map[String, Transaction]](
      new InsertionOrderedMap[String, Transaction])((map, transmap) =>
        {
          val ident = getRequiredString(idKey, transmap)
          val name = getRequiredString(nameKey, transmap)
          val group = if (transmap.containsKey(groupKey)) transmap.get(groupKey).asInstanceOf[String] else ""
          val emptymap = new java.util.HashMap[String, Any]
          val heading = parseTransactionPart(headingKey, transmap, emptymap, segments)
          val detail = parseTransactionPart(detailKey, transmap, emptymap, segments)
          val summary = parseTransactionPart(summaryKey, transmap, emptymap, segments)
          map + (ident -> Transaction(ident, name, group, heading, detail, summary))
        })

  }

  /** Read schema from YAML document.
    *
    * @param reader
    * @returns schema
    */
  def loadYaml(reader: Reader, basePaths: Array[String]) = {
    val snake = new Yaml(new IgnoringConstructor())
    val bases = basePaths.toList

    /** Read schema from YAML document, recursively reading and building on imported schemas. */
    def loadFully(reader: Reader): EdiSchema = {
      val input = snake.loadAs(reader, classOf[JavaMap[Any, Any]]).asInstanceOf[AnyMap];
      val form = convertEdiForm(input.get(formKey).toString)
      val version = getRequiredString(versionKey, input)
      val baseSchema = if (input.containsKey(importsKey)) {
        val impsin = getRequiredStringList(importsKey, input)
        impsin.asScala.toList.foldLeft(new EdiSchema(version))((acc, path) => {
          def findImport(bases: List[String]): InputStream = bases match {
            case h :: t => {
              val file = new File(h + path)
              if (file.exists) new FileInputStream(file)
              else findImport(t)
            }
            case _ => {
              val is = getClass.getResourceAsStream(path)
              if (is == null) {
                val is1 = getClass.getClassLoader.getResourceAsStream(path)
                if (is1 == null) {
                  val is2 = Thread.currentThread.getContextClassLoader.getResourceAsStream(path)
                  if (is2 == null) {
                    println(System.getProperty("java.class.path"))
                    throw new IllegalArgumentException(s"base schema $path not found on any classpath")
                  } else is2
                } else is1
              } else is
              }
          }
          val is = findImport(bases)
          if (is == null) throw new IllegalArgumentException(s"base schema $path not found")
          acc.merge(loadFully(new InputStreamReader(is, "UTF-8")))
        })

      } else new EdiSchema(version)
      val elements = (if (input.containsKey(elementsKey)) convertElements(input)
      else Map[String, Element]()) ++ baseSchema.elements
      val composites = (if (input.containsKey(compositesKey)) convertComposites(input, elements)
      else Map.empty[String, Composite]) ++ baseSchema.composites
      val segments = (if (input.containsKey(segmentsKey)) convertSegments(input, elements, composites)
      else Map.empty[String, Segment]) ++ baseSchema.segments
      val transactions =
        if (input.containsKey(transactionsKey)) convertTransactions(input, segments)
        else Map.empty[String, Transaction]
      EdiSchema(form, version, elements, Map.empty[String, Composite], segments, transactions)
    }

    loadFully(reader)
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