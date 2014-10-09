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

/**
 * TODO
 *
 */
object YamlReader {

  import EdiSchema._

  type JavaMap[K, V] = java.util.Map[K, V]
  type JavaList[V] = java.util.List[V]

  def getChildMap[K, V](key: String, map: JavaMap[K, V]): JavaMap[K, V] = map.get(key) match {
    case child: JavaMap[K, V] => child
    case null => throw new IllegalArgumentException("Missing required map value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a map")
  }

  def getChildList[K, V](key: String, map: JavaMap[K, V]): JavaList[V] = map.get(key) match {
    case child: JavaList[V] => child
    case null => throw new IllegalArgumentException("Missing required array '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a list")
  }

  def getRequiredString(key: String, map: JavaMap[Any, Any]): String = map.get(key) match {
    case child: String => child
    case null => throw new IllegalArgumentException("Missing required string value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not a string")
  }

  def getRequiredInt(key: String, map: JavaMap[Any, Any]): Int = map.get(key) match {
    case n: Int => n
    case null => throw new IllegalArgumentException("Missing required integer value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not an integer")
  }

  def parseTransactionComponents(list: JavaList[JavaMap[Any, Any]]): List[TransactionComponent] = {
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
        GroupComponent(getRequiredString("loopId", values), use, repeat, parseTransactionComponents(items))
      } else ReferenceComponent(getRequiredString("idRef", values), use, repeat)
    }
    @tailrec
    def parseComponent(remain: List[JavaMap[Any, Any]], prior: List[TransactionComponent]): List[TransactionComponent] = remain match {
      case values :: t => parseComponent(t, convertComponent(values) :: prior)
      case _ => prior.reverse
    }
    parseComponent(list.asScala.toList, Nil)
  }

  def parseSegmentComponents(list: JavaList[JavaMap[Any, Any]]): List[SegmentComponent] = {
    def convertComponent(values: JavaMap[Any, Any]) = {
      val id = getRequiredString("idRef", values)
      val name = getRequiredString("name", values)
      val use = convertUsage(getRequiredString("usage", values))
      val repeat = values.get("repeat") match {
        case n: Int => n
        case null => 1
        case _ => throw new IllegalArgumentException("Value 'repeat' must be an integer")
      }
      ElementComponent(id, name, use, repeat)
    }
    @tailrec
    def parseComponent(remain: List[JavaMap[Any, Any]], prior: List[SegmentComponent]): List[SegmentComponent] = remain match {
      case values :: t => parseComponent(t, convertComponent(values) :: prior)
      case _ => prior.reverse
    }
    parseComponent(list.asScala.toList, Nil)
  }

  def parseTransactionPart(name: String, values: JavaMap[Any, Any]) =
    if (values.containsKey(name)) {
      val list = getChildList(name, values).asInstanceOf[JavaList[JavaMap[Any, Any]]]
      parseTransactionComponents(list)
    } else Nil

  def loadYaml(reader: Reader) = {
    val yaml = new Yaml(new IgnoringConstructor());
    val input = yaml.loadAs(reader, classOf[JavaMap[_, _]]);
    val transin = getChildList("transactions", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    println(transin)
    val transactions = transin.asScala.toList.map { transmap =>
      {
        val ident = getRequiredString("id", transmap)
        val heading = parseTransactionPart("heading", transmap)
        val detail = parseTransactionPart("detail", transmap)
        val summary = parseTransactionPart("summary", transmap)
        Transaction(ident, heading, detail, summary)
      }
    }
    println(transactions)
    val segsin = getChildList("segments", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    println(segsin)
    val segments = segsin.asScala.toList.map { segmap =>
      {
        val ident = getRequiredString("id", segmap)
        val name = getRequiredString("name", segmap)
        val list = getChildList("values", segmap).asInstanceOf[JavaList[JavaMap[Any, Any]]]
        Segment(ident, name, parseSegmentComponents(list))
      }
    }
    println(segments)
    val elmsin = getChildList("elements", input).asInstanceOf[JavaList[JavaMap[Any, Any]]]
    println(elmsin)
    val elements = elmsin.asScala.toList.map { elmmap =>
      {
        val ident = getRequiredString("id", elmmap)
        val typ = convertDataType(getRequiredString("type", elmmap))
        val min = getRequiredInt("minLength", elmmap)
        val max = getRequiredInt("maxLength", elmmap)
        Element(ident, typ, min, max)
      }
    }
    println(elements)
    Schema(elements, Nil, segments, transactions)
  }
}

object TestRead extends App {
  val file = new File("/home/dennis/projects/mule/edi/yaml/schema-cdw850v3.yaml")
  val schema = YamlReader.loadYaml(new FileReader(file))
  val writer = new StringWriter
  YamlWriter.write(schema, writer)
  println(writer.toString)
  val reader = new StringReader(writer.toString)
  val schema2 = YamlReader.loadYaml(reader)
  val writer2 = new StringWriter
  YamlWriter.write(schema2, writer2)
  println(writer2.toString)
}

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
