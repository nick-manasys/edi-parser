package com.anypoint.df.edi.schema.tools

import collection.JavaConverters._
import com.anypoint.df.edi.schema.SchemaJavaDefs
import com.anypoint.df.edi.schema.WritesYaml
import com.anypoint.df.edi.schema.YamlDefs
import com.anypoint.df.edi.schema.YamlReader
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.Reader
import java.io.File
import com.anypoint.df.edi.schema.X12ParserConfig
import com.anypoint.df.edi.schema.X12SchemaParser
import scala.util.Failure
import scala.util.Success
import com.anypoint.df.edi.schema.IdentityInformation
import com.anypoint.df.edi.schema.SchemaJavaValues._

object OverlayByExample extends WritesYaml with YamlDefs with SchemaJavaDefs {

  /** Merge all values from the first ap into the second map. If the same key exist in both maps, the value of the
    * second map is retained. The second map (which may have been modified by the method call) is always returned as
    * the result.
    */
  def mergeMaps(map1: ValueMap, map2: ValueMap): Unit =
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

  /** Reads a schema and parses one or more documents using that schema, then generates an overlay schema based on the
    * sample documents which marks as unused all segments and elements/composites which were not present in any of the
    * samples.
    */
  def main(args: Array[String]): Unit = {
    def stripMeta(trans: ValueMap) = trans.asScala.foreach {
      case (_, list: MapList) => list.asScala.foreach {
        case m: ValueMap => {
          m.remove(transactionSet)
          m.remove(transactionGroup)
        }
        case _ => throw new IllegalStateException("transaction list items must be maps")
      }
      case _ => throw new IllegalStateException("transaction map values must be lists")
    }
    val schemaFile = new File(args(0))
    val schema = YamlReader.loadYaml(new InputStreamReader(new FileInputStream(schemaFile)), Array(args(1)))
    val examples = args.toList.tail.tail
    val config = X12ParserConfig(true, true, true, true, true, true, true, true, true, true, true,
      Array[IdentityInformation](), Array[IdentityInformation]())
    val merged = new ValueMapImpl
    examples.foreach (path => {
      println(s"merging $path")
      val is = new FileInputStream(new File(path))
      val parser = X12SchemaParser(is, schema, config)
      parser.parse match {
        case Success(x) => {
          val transacts = x.get(transactionsMap).asInstanceOf[ValueMap]
          stripMeta(transacts)
          println(transacts)
          mergeMaps(transacts, merged)
        }
        case Failure(e) => throw new IllegalArgumentException(s"error parsing example $path: '${e.getMessage}'")
      }
    })
    println(merged)
  }
}