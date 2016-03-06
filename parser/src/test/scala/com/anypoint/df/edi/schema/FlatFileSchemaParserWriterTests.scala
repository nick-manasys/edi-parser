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

  val testDoc = readDoc("edi/QB-FFSampleRequest.txt")

  val testSchema = new YamlReader().loadYaml(new InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/QBRequest.esl"), "UTF-8"), Array())
  
  val testStructure = testSchema.structures("QBRequest")

  def parseDoc(doc: String) = {
    val ins = new ByteArrayInputStream(doc.getBytes(ASCII_CHARSET))
    val parser = new FlatFileStructureParser(ins, testSchema.structures.values.head)
    parser.parse.get
  }

  behavior of "FlatFileSchemaParser"

  it should "parse a simple message" in {
    val in = new ByteArrayInputStream((line1 + line9).getBytes())
    val parser = new FlatFileStructureParser(in, testSchema.structures.values.head)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
    input get("Id") should be ("QBRequest")
    val data = input.get("Data").asInstanceOf[ValueMap]
    val seg1 = data.get("1_FCH").asInstanceOf[ValueMap]
    seg1.size should be (5)
    seg1 get("FCH01") should be ("MISSION")
    val date = seg1.get("FCH02").asInstanceOf[GregorianCalendar]
    date get(Calendar.YEAR) should be (2013)
    date get(Calendar.MONTH) should be (7)
    date get(Calendar.DAY_OF_MONTH) should be (2)
    seg1 get("FCH03") should be (Integer.valueOf(28800000))
    seg1 get("FCH04") should be ("MISSIONAUSTRALIA")
    seg1 get("FCH05") should be ("2009110401")
    val seg2 = data.get("5_FCF").asInstanceOf[ValueMap]
    seg2.size should be (6)
    seg2 get("FCF01") should be (Integer.valueOf(1))
    seg2 get("FCF02") should be (Integer.valueOf(1))
    seg2 get("FCF03") should be (Integer.valueOf(10000))
    seg2 get("FCF04") should be ("MISSION")
    seg2 get("FCF05") should be ("MISSIONAUSTRALIA")
    seg2 get("FCF06") should be ("2009110401")
  }

  behavior of "FlatFileSchemaWriter"

  it should "roundtrip a complete document" in {
    val msg = readDoc("edi/QB-FFSampleRequest.txt")
    val in = new ByteArrayInputStream(msg.getBytes())
    val parser = new FlatFileStructureParser(in, testStructure)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
//    val ywriter = new StringWriter
//    YamlSupport.writeMap(input, ywriter)
//    println(ywriter.toString)
    val out = new ByteArrayOutputStream
    val writer = new FlatFileStructureWriter(out, testStructure, FlatFileWriterConfig(true, ASCII_CHARSET))
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
//    println(text)
//    val swriter = new StringWriter
//    YamlSupport.writeMap(input, swriter)
//    println(swriter.toString())
//    println("original text:\n" + msg + "\n")
//    println("returned text:\n" + text + "\n")
    text should be (msg)
  }
  
  val altMessage = """0Acme Corp.          123456127 Mega Way        Los Angles  CA900262135551212
1George    Sam            President      president@acme.corp                                   0000000000000000000020151130
1Sally     Smith          V.P. Sales     sallys@acme.corp                                      0000000000000000000020151210
0Ardvark Enterprises 1341335 Pikes Place       Seattle     WA980262065552341
1Jane      Jones          CEO            janiejones@ardvark.co                                 0000000000000000000020140812
"""

  val altSchema1 = new YamlReader().loadYaml(new InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/CompanyContacts.esl"), "UTF-8"), Array())
  
  val altStructure1 = altSchema1.structures("CompanyContacts")

  it should "roundtrip document with flatfile schema using references" in {
    val in = new ByteArrayInputStream(altMessage.getBytes())
    val parser = new FlatFileStructureParser(in, altStructure1)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
//    val ywriter = new StringWriter
//    YamlSupport.writeMap(input, ywriter)
//    println(ywriter.toString)
    val out = new ByteArrayOutputStream
    val writer = new FlatFileStructureWriter(out, altStructure1, FlatFileWriterConfig(true, ASCII_CHARSET))
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    val swriter = new StringWriter
    YamlSupport.writeMap(input, swriter)
//    println(swriter.toString())
//    println("returned text:\n" + text + "\n")
    text should be (altMessage)
  }

  val altSchema2 = new YamlReader().loadYaml(new InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/CompanyContactsCompact.esl"), "UTF-8"), Array())
  
  val altStructure2 = altSchema2.structures("CompanyContacts")

  it should "roundtrip document with flatfile schema using inlining" in {
    val in = new ByteArrayInputStream(altMessage.getBytes())
    val parser = new FlatFileStructureParser(in, altStructure2)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
//    val ywriter = new StringWriter
//    YamlSupport.writeMap(input, ywriter)
//    println(ywriter.toString)
    val out = new ByteArrayOutputStream
    val writer = new FlatFileStructureWriter(out, altStructure2, FlatFileWriterConfig(true, ASCII_CHARSET))
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    val swriter = new StringWriter
    YamlSupport.writeMap(input, swriter)
//    println(swriter.toString())
//    println("returned text:\n" + text + "\n")
    text should be (altMessage)
  }
}