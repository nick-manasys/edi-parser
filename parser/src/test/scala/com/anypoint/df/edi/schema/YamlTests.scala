package com.anypoint.df.edi.schema

import java.io.InputStreamReader
import java.io.StringReader
import java.io.StringWriter
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.io.Source

class RoundtripTest extends FlatSpec with Matchers {

  import EdiSchema._

  "YamlReader and YamlWriter" should "roundtrip an ordered (all parts by id) YAML schema file correctly" in {
    val stream = getClass.getClassLoader.getResourceAsStream("esl/cdw850schema.esl")
    val input = Source.fromInputStream(stream).mkString
    val schema = YamlReader.loadYaml(new StringReader(input), Array())
    val writer = new StringWriter
    YamlWriter.write(schema, Array(), writer)
    val result = writer.toString()
    result should be (input)
  }
}