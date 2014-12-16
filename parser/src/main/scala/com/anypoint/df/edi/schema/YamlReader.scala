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

/**
 * Read YAML representation of EDI schema.
 *
 * @author MuleSoft, Inc.
 */
object YamlReader {

  import EdiSchema._

  type JavaMap[K, V] = java.util.Map[K, V]
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

  /** Get child string value (error if not found). */
  def getRequiredString(key: String, map: JavaMap[Any, Any]): String = map.get(key) match {
    case child: String => child
    case null => throw new IllegalArgumentException("Missing required string value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a string")
  }

  /** Get child int value (error if not found). */
  def getRequiredInt(key: String, map: JavaMap[Any, Any]): Int = map.get(key) match {
    case n: Int => n
    case null => throw new IllegalArgumentException("Missing required integer value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not an integer")
  }

  /**
   * Build segment components from input data.
   * @param list list of maps of component data
   * @param rules list of maps of rules
   * @param elements known elements map
   * @param composites known composites map
   * @returns components
   */
  def parseSegmentComponents(list: JavaList[JavaMap[Any, Any]], elements: Map[String, Element],
    composites: Map[String, Composite]): List[SegmentComponent] = {
    def convertComponent(values: JavaMap[Any, Any], dfltpos: Int) = {
      val id = getRequiredString("idRef", values)
      val name = getRequiredString("name", values)
      val position = if (values.containsKey("position")) getRequiredInt("position", values) else dfltpos
      val use = convertUsage(getRequiredString("usage", values))
      val repeat = values.get("repeat") match {
        case n: Int => n
        case null => 1
        case _ => throw new IllegalArgumentException("Value 'repeat' must be an integer")
      }
      if (elements.contains(id)) ElementComponent(elements(id), name, position, use, repeat)
      else if (composites.contains(id)) CompositeComponent(composites(id), name, position, use, repeat)
      else throw new IllegalArgumentException(s"No element or composite with id '$id'")
    }
    @tailrec
    def parseComponent(remain: List[JavaMap[Any, Any]], position: Int, prior: List[SegmentComponent]): List[SegmentComponent] =
      remain match {
      case values :: t => parseComponent(t, position + 1, convertComponent(values, position) :: prior)
      case _ => prior.reverse
    }
    parseComponent(list.asScala.toList, 1, Nil)
  }
  
  /**
   * Build rules from input data.
   * @param rules list of maps of rule data
   * @param comps components rule applies to
   * @returns rules
   */
  def parseRules(rules: JavaList[JavaMap[Any, Any]], comps: List[SegmentComponent]): List[OccurrenceRule] = {
    val posidx = comps.foldLeft(Map[Int, SegmentComponent]())((acc, comp) => acc + (comp.position -> comp))
    def convertRule(map: JavaMap[Any, Any]) = {
      val comps = getChildList("values", map).asScala.toList.map(value => value match {
        case pos: Int => posidx(pos)
        case _ => throw new IllegalArgumentException("Not a valid component position for rule")
      })
      val typ = getRequiredString("type", map)
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

  /**
   * Build transaction components from input data.
   *
   * @param list list of maps of component data
   * @param segments known segments map
   * @returns components
   */
  def parseTransactionComponents(list: JavaList[JavaMap[Any, Any]], segments: Map[String, Segment]):
  List[TransactionComponent] = {
    def convertComponent(values: JavaMap[Any, Any]) = {
      val use = convertUsage(getRequiredString("usage", values))
      val count = values.get("count") match {
        case n: Int => n
        case ">1" => -1
        case null => 1
        case _ => throw new IllegalArgumentException("Value 'count' must be an integer or a string")
      }
      if (values.containsKey("items")) {
        val items = getChildList("items", values).asInstanceOf[JavaList[JavaMap[Any, Any]]]
        GroupComponent(getRequiredString("loopId", values), use, count, parseComponent(items.asScala.toList, Nil))
      } else {
        val id = getRequiredString("idRef", values)
        if (!segments.contains(id)) {
          throw new IllegalArgumentException(s"No segment with id '$id'")
        }
        ReferenceComponent(segments(id), use, count)
      }
    }
    @tailrec
    def parseComponent(remain: List[JavaMap[Any, Any]], prior: List[TransactionComponent]): List[TransactionComponent] =
      remain match {
      case values :: t => parseComponent(t, convertComponent(values) :: prior)
      case _ => prior.reverse
    }
    parseComponent(list.asScala.toList, Nil)
  }

  /**
   * Build transaction part from input data.
   *
   * @param name part name
   * @param values map of component data lists
   * @param segments known segments map
   * @returns components
   */
  def parseTransactionPart(name: String, values: JavaMap[Any, Any], segments: Map[String, Segment]) =
    if (values.containsKey(name)) {
      val list = getChildList(name, values).asInstanceOf[JavaList[JavaMap[Any, Any]]]
      parseTransactionComponents(list, segments)
    } else Nil

  /**
   * Map with iteration order matching insertion order. This should only be used when the map is rarely, if ever,
   *  going to have an element removed since it uses a linear search for this case.
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
  
  /** Convert composite definitions. */
  private def convertComposites(input: JavaMap[Any, Any], elements: Map[String, Element]) = {
    val compsin = getChildList("composites", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    compsin.asScala.toList.foldLeft[Map[String, Composite]](new InsertionOrderedMap[String, Composite])(
      (map, compmap) =>
      {
        val ident = getRequiredString("id", compmap)
        val name = getRequiredString("name", compmap)
        val list = getChildList("values", compmap).asInstanceOf[JavaList[JavaMap[Any, Any]]]
        val comps = parseSegmentComponents(list, elements, map)
        val rules = if (compmap.containsKey("rules")) {
          val data = getChildList("rules", compmap).asInstanceOf[JavaList[JavaMap[Any, Any]]]
          parseRules(data, comps)
        } else Nil
        map + (ident -> Composite(ident, name, comps, rules))
      })
  }

  /**
   * Read schema from YAML document.
   *
   *  @param reader
   *  @returns schema
   */
  def loadYaml(reader: Reader) = {
    val yaml = new Yaml(new IgnoringConstructor());
    val input = yaml.loadAs(reader, classOf[JavaMap[Any, Any]]);
    val elmsin = getChildList("elements", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    val elements = elmsin.asScala.toList.foldLeft[Map[String, Element]](new InsertionOrderedMap[String, Element])(
      (map, elmmap) =>
      {
        val ident = getRequiredString("id", elmmap)
        val typ = EdiConstants.toType(getRequiredString("type", elmmap))
        val min = getRequiredInt("minLength", elmmap)
        val max = getRequiredInt("maxLength", elmmap)
        map + (ident -> Element(ident, typ, min, max))
      })
    val composites =
      if (input.containsKey("composites")) convertComposites(input, elements)
      else Map.empty[String, Composite]
    val segsin = getChildList("segments", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    val segments = segsin.asScala.toList.foldLeft[Map[String, Segment]](new InsertionOrderedMap[String, Segment])(
      (map, segmap) =>
      {
        val ident = getRequiredString("id", segmap)
        val name = getRequiredString("name", segmap)
        val list = getChildList("values", segmap).asInstanceOf[JavaList[JavaMap[Any, Any]]]
        val comps = parseSegmentComponents(list, elements, composites)
        val rules = if (segmap.containsKey("rules")) {
          val data = getChildList("rules", segmap).asInstanceOf[JavaList[JavaMap[Any, Any]]]
          parseRules(data, comps)
        } else Nil
        map + (ident -> Segment(ident, name, comps, rules))
      })

    val transin = getChildList("transactions", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    val transactions = transin.asScala.toList.foldLeft[Map[String, Transaction]](
      new InsertionOrderedMap[String, Transaction])((map, transmap) =>
      {
        val ident = getRequiredString("id", transmap)
        val name = getRequiredString("name", transmap)
        val group = if (transmap.containsKey("group")) transmap.get("group").asInstanceOf[String] else ""
        val heading = parseTransactionPart("heading", transmap, segments)
        val detail = parseTransactionPart("detail", transmap, segments)
        val summary = parseTransactionPart("summary", transmap, segments)
        map + (ident -> Transaction(ident, name, group, heading, detail, summary))
      })

    val form = convertEdiForm(input.get("form").toString)
    EdiSchema(X12, elements, Map.empty[String, Composite], segments, transactions)
  }
}

/**
 * Constructor for YAML parser to just build lists and maps of values.
 */
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