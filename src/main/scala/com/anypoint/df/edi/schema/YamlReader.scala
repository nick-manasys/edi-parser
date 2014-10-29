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
   * @param elements known elements map
   * @returns components
   */
  def parseSegmentComponents(list: JavaList[JavaMap[Any, Any]], elements: Map[String, Element]): List[SegmentComponent] = {
    def convertComponent(values: JavaMap[Any, Any]) = {
      val id = getRequiredString("idRef", values)
      if (!elements.contains(id)) {
        throw new IllegalArgumentException(s"No element with id '$id'")
      }
      val name = getRequiredString("name", values)
      val use = convertUsage(getRequiredString("usage", values))
      val repeat = values.get("repeat") match {
        case n: Int => n
        case null => 1
        case _ => throw new IllegalArgumentException("Value 'repeat' must be an integer")
      }
      ElementComponent(elements(id), name, use, repeat)
    }
    @tailrec
    def parseComponent(remain: List[JavaMap[Any, Any]], prior: List[SegmentComponent]): List[SegmentComponent] = remain match {
      case values :: t => parseComponent(t, convertComponent(values) :: prior)
      case _ => prior.reverse
    }
    parseComponent(list.asScala.toList, Nil)
  }

  /**
   * Build transaction components from input data.
   *
   * @param list list of maps of component data
   * @param segments known segments map
   * @returns components
   */
  def parseTransactionComponents(list: JavaList[JavaMap[Any, Any]], segments: Map[String, Segment]): List[TransactionComponent] = {
    def convertComponent(values: JavaMap[Any, Any]) = {
      val use = convertUsage(getRequiredString("usage", values))
      val repeat = values.get("repeat") match {
        case n: Int => n
        case ">1" => -1
        case null => 1
        case _ => throw new IllegalArgumentException("Value 'repeat' must be an integer or a string")
      }
      if (values.containsKey("items")) {
        val items = getChildList("items", values).asInstanceOf[JavaList[JavaMap[Any, Any]]]
        GroupComponent(getRequiredString("loopId", values), use, repeat, parseComponent(items.asScala.toList, Nil))
      } else {
        val id = getRequiredString("idRef", values)
        if (!segments.contains(id)) {
          throw new IllegalArgumentException(s"No segment with id '$id'")
        }
        ReferenceComponent(segments(id), use, repeat)
      }
    }
    @tailrec
    def parseComponent(remain: List[JavaMap[Any, Any]], prior: List[TransactionComponent]): List[TransactionComponent] = remain match {
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

  /**
   * Read schema from YAML document.
   *
   *  @param reader
   *  @returns schema
   */
  def loadYaml(reader: Reader) = {
    val yaml = new Yaml(new IgnoringConstructor());
    val input = yaml.loadAs(reader, classOf[JavaMap[_, _]]);
    val elmsin = getChildList("elements", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    val elements = elmsin.asScala.toList.foldLeft[Map[String, Element]](new InsertionOrderedMap[String, Element])((map, elmmap) =>
      {
        val ident = getRequiredString("id", elmmap)
        val typ = convertDataType(getRequiredString("type", elmmap))
        val min = getRequiredInt("minLength", elmmap)
        val max = getRequiredInt("maxLength", elmmap)
        map + (ident -> Element(ident, typ, min, max))
      })
    val segsin = getChildList("segments", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    val segments = segsin.asScala.toList.foldLeft[Map[String, Segment]](new InsertionOrderedMap[String, Segment])((map, segmap) =>
      {
        val ident = getRequiredString("id", segmap)
        val name = getRequiredString("name", segmap)
        val list = getChildList("values", segmap).asInstanceOf[JavaList[JavaMap[Any, Any]]]
        map + (ident -> Segment(ident, name, parseSegmentComponents(list, elements)))
      })

    val transin = getChildList("transactions", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    val transactions = transin.asScala.toList.foldLeft[Map[String, Transaction]](new InsertionOrderedMap[String, Transaction])((map, transmap) =>
      {
        val ident = getRequiredString("id", transmap)
        val name = getRequiredString("name", transmap)
        val heading = parseTransactionPart("heading", transmap, segments)
        val detail = parseTransactionPart("detail", transmap, segments)
        val summary = parseTransactionPart("summary", transmap, segments)
        map + (ident -> Transaction(ident, name, heading, detail, summary))
      })

    // TODO: add EDIFACT vs. X12 flag to YAML
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