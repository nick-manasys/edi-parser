package com.anypoint.df.edi.schema

import java.io.InputStreamReader
import java.io.StringReader
import java.io.StringWriter
import java.math.BigDecimal
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import collection.JavaConverters._
import scala.io.Source
import java.io.ByteArrayInputStream
import java.util.GregorianCalendar
import scala.util.Success
import java.util.Calendar
import java.io.ByteArrayOutputStream
import java.io.FileInputStream
import java.io.File
import com.anypoint.df.edi.lexical.EdiConstants
import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.WriteException
import com.anypoint.df.edi.schema.tools.YamlSupport
import javax.xml.datatype.XMLGregorianCalendar

class FlatFileSchemaParserWriterTests extends FlatSpec with Matchers with SchemaJavaDefs {
  
  import EdiSchema._
  import HL7Identity._
  import HL7SchemaDefs._
  import SchemaJavaValues._
  
  val line1 = "1MISSION   201308020800MISSIONAUSTRALIA              2009110401                                                                                                                                                                                           \n"
  val line9 = "90100000001000000010000MISSION   MISSIONAUSTRALIA              2009110401                                                                                                                                                                                 \n"

  /** Reads a copy of the test document into memory, standardizing line endings. */
  val testDoc = {
    val lines = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("edi/QB-FFSampleRequest.txt")).getLines
    val builder = new StringBuilder
    lines.foreach(line => {
      if (!builder.isEmpty) builder.append('\n')
      builder.append(line)
    })
    builder.toString
  }

  val testSchema = new YamlReader().loadYaml(new InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/QBRequest.esl"), "UTF-8"), Array())
  
  val testStructure = testSchema.structures("QBRequest")

  def parseDoc(doc: String) = {
    val ins = new ByteArrayInputStream(doc.getBytes(ASCII_CHARSET))
    val parser = FlatFileSchemaParser(ins, testSchema.structures.values.head)
    parser.parse.get
  }

  behavior of "FlatFileSchemaParser"

  it should "parse a simple message" in {
    val in = new ByteArrayInputStream((line1 + line9).getBytes())
    val parser = FlatFileSchemaParser(in, testSchema.structures.values.head)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
    input get("Id") should be ("QBRequest")
    val data = input.get("Data").asInstanceOf[ValueMap]
    val seg1 = data.get("1_1").asInstanceOf[ValueMap]
    seg1 get("101") should be ("MISSION")
    val date = seg1.get("102").asInstanceOf[GregorianCalendar]
    date get(Calendar.YEAR) should be (2013)
    date get(Calendar.MONTH) should be (7)
    date get(Calendar.DAY_OF_MONTH) should be (2)
    seg1 get("103") should be (Integer.valueOf(28800000))
    seg1 get("104") should be ("MISSIONAUSTRALIA")
    seg1 get("105") should be ("2009110401")
    seg1 get("106") should be ("")
  }

  behavior of "FlatFileSchemaWriter"

  it should "roundtrip a simplified document" in {
    val msg = line1 + line9
    val in = new ByteArrayInputStream(msg.getBytes())
    val parser = FlatFileSchemaParser(in, testSchema.structures.values.head)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
  }

  /** Reads a copy of a test document into memory, standardizing line endings. */
  def readDoc(path: String) = {
    val lines = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(path)).getLines
    val builder = new StringBuilder
    lines.foreach(line => {
      if (!builder.isEmpty) builder.append('\n')
      builder.append(line)
    })
    builder.toString
  }

  it should "roundtrip a complete document" in {
    val msg = readDoc("edi/QB-FFSampleRequest.txt")
    val in = new ByteArrayInputStream(msg.getBytes())
    val parser = FlatFileSchemaParser(in, testStructure)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
    val ywriter = new StringWriter
    YamlSupport.writeMap(input, ywriter)
    println(ywriter.toString)
    val out = new ByteArrayOutputStream
    val writer = FlatFileSchemaWriter(out, testStructure, FlatFileWriterConfig(true, ASCII_CHARSET))
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    println(text)
    val swriter = new StringWriter
    YamlSupport.writeMap(input, swriter)
    println(swriter.toString())
//    println("original text:\n" + msg + "\n")
//    println("returned text:\n" + text + "\n")
    text should be (msg)
  }
}