package com.anypoint.df.edi.schema

import java.io.InputStreamReader
import java.io.StringReader
import java.io.StringWriter
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.io.Source

class RoundtripTest extends FlatSpec with Matchers {

  import EdiSchema._

  "YamlReader and YamlWriter" should "roundtrip a YAML schema file correctly" in {
    val stream = getClass.getClassLoader.getResourceAsStream("yaml/cdw850schema.yaml")
    val input = Source.fromInputStream(stream).mkString
    val schema = YamlReader.loadYaml(new StringReader(input))
    val writer = new StringWriter
    YamlWriter.write(schema, writer)
    val result = writer.toString()
    assert(input === result)
  }
}