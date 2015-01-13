package com.anypoint.df.edi.schema.tools

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader

import scala.util.Failure
import scala.util.Success

import com.anypoint.df.edi.schema.X12ParserConfig
import com.anypoint.df.edi.schema.X12SchemaParser
import com.anypoint.df.edi.schema.YamlReader

object ParseDocument {

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found.
    */
  def main(args: Array[String]): Unit = {
    val schemaFile = new File(args(0))
    val schema = YamlReader.loadYaml(new InputStreamReader(new FileInputStream(schemaFile)), Array(args(1)))
    val examples = args.toList.tail.tail
    val config = X12ParserConfig(true, true, true, true, true, true, true, true, true)
    examples.map (path => {
      val is = new FileInputStream(new File(path))
      val parser = X12SchemaParser(is, schema, config)
      parser.parse match {
        case Success(x) => x
        case Failure(e) => throw new IllegalArgumentException(s"error parsing example $path: '${e.getMessage}'")
      }
    })
    println("all documents parsed successfully")
  }
}