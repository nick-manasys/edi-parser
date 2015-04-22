package com.anypoint.df.edi.schema.tools

import java.io.File
import java.io.FileInputStream
import java.io.FileWriter
import java.io.InputStreamReader

import com.anypoint.df.edi.schema.YamlReader
import com.anypoint.df.edi.schema.YamlWriter

object DumpSchema {

  /** Reads a schema and base schema path, then dumps the combined schema.
    */
  def main(args: Array[String]): Unit = {
    val schemaFile = new File(args(0))
    val schema = new YamlReader().loadYaml(new InputStreamReader(new FileInputStream(schemaFile)), Array(args(1)))
    val writer = new FileWriter(args(2))
    YamlWriter.write(schema, Array[String](), writer)
    writer.close
    println("wrote output schema")
  }
}