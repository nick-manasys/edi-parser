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

object OverlayByExample extends WritesYaml with YamlDefs with SchemaJavaDefs {

  def mergeMaps(map1: ValueMap, map2: ValueMap): ValueMap = {
    map1.keySet.asScala.toList.foldLeft(map2)((acc, key) => {

      // get value with lists of maps flattened to maps
      val val1 = {
        val value = map1.get(key)
        if (value.isInstanceOf[MapList]) value.asInstanceOf[MapList].asScala.toList.
          foldLeft[ValueMap](new ValueMapImpl)((acc, map) => mergeMaps(map, acc))
        else value
      }

      // combine with existing value, if any
      if (map2.containsKey(key)) {
        if (map2.get(key).isInstanceOf[MapList]) {
          println("error")
        }
        if (val1.isInstanceOf[ValueMap]) map2.put(key,
          mergeMaps(val1.asInstanceOf[ValueMap], map2.get(key).asInstanceOf[ValueMap]))
      } else map2.put(key, val1)
      map2
    })
  }

  /** Builds schemas from X12 table data and outputs the schemas in YAML form. The arguments are 1) path to the
    * directory containing the X12 table data files, and 2) path to the directory for the YAML output files. All
    * existing files are deleted from the output directory before writing any output files. Each transaction is output
    * as a separate file, with the transaction ID used as the file name (with extension ".yaml").
    */
  def main(args: Array[String]): Unit = {
    val schemaFile = new File(args(0))
    val schema = YamlReader.loadYaml(new InputStreamReader(new FileInputStream(schemaFile)), Array(args(1)))
    val examples = args.toList.tail.tail
    val config = X12ParserConfig(true, true, true, true, true, true, true, true, true)
    val merged = examples.map (path => {
      val is = new FileInputStream(new File(path))
      val parser = X12SchemaParser(is, schema, config)
      parser.parse match {
        case Success(x) => x
        case Failure(e) => throw new IllegalArgumentException(s"error parsing example $path: '${e.getMessage}'")
      }
    }).foldLeft[ValueMap](new ValueMapImpl)((acc, map) => mergeMaps(map, acc))
    println(merged)
  }
}