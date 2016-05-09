package com.mulesoft.flatfile.schema

import java.io.InputStreamReader
import java.io.StringReader
import java.io.StringWriter
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.io.Source

class RoundtripTest extends FlatSpec with Matchers {

  import EdiSchema._
  
  behavior of "YamlReader and YamlWriter"
  
  def readFile(path: String) = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    Source.fromInputStream(stream).mkString
  }
  
  def parseSchema(text: String) = new YamlReader().loadYaml(new StringReader(text), Array())
  
  def writeSchema(schema: EdiSchema) = {
    val writer = new StringWriter
    YamlWriter.write(schema, Array(), writer)
    writer.toString()
  }

  it should "roundtrip a simple single-segment YAML schema file correctly" in {
    val input = readFile("esl/fixed.ffd")
    val result = writeSchema(parseSchema(input))
    result should be (input)
  }

  it should "roundtrip a multiple-segment YAML schema file correctly" in {
    val input = readFile("esl/copybook.ffd")
    val result = writeSchema(parseSchema(input))
    result should be (input)
  }

  it should "roundtrip an ordered (all parts by id) YAML schema file correctly" in {
    val input = readFile("esl/cdw850schema.esl")
    val result = writeSchema(parseSchema(input))
    result should be (input)
  }
}