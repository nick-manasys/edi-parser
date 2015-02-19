package com.anypoint.df.edi.schema

import java.io.InputStreamReader
import java.io.StringReader
import java.io.StringWriter
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.io.Source
import java.io.ByteArrayInputStream
import java.util.GregorianCalendar
import scala.util.Success
import java.util.Calendar
import java.io.ByteArrayOutputStream
import java.io.FileInputStream
import java.io.File
import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.X12Lexer.InterchangeStartStatus._

class X12SchemaParserWriterTests extends FlatSpec with Matchers with SchemaJavaDefs with X12SchemaDefs {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.X12Constants._
  import SchemaJavaValues._
  import X12SchemaValues._
  
  class DefaultNumberProvider extends NumberProvider {
    var number = 1
    def nextInterchange = {
      number += 1
      number
    }
  }

  val DATETIME = "090604*1205"
  val ISA = s"ISA*00*ABC       *00*DEF       *01*013227180      *ZZ*IJDIECAFOX     *$DATETIME*U*00401*000001244*0*P*>~"
  val GS = "GS*PO*006927180*IAIYUCAFOO*20080604*1205*168*X*004010~"
  val ST = "ST*850*000000176~"
  val IEA = "IEA*1*000001244~"

  def buildGE(count: Int) = s"GE*$count*168~"

  def buildSE(count: Int) = s"SE*$count*000000176~"

  val parserConfig = X12ParserConfig(true, true, true, true, true, true, true, true, true, true, true, ASCII_CHARSET,
    Array[IdentityInformation](), Array[IdentityInformation]())

  behavior of "X12SchemaParser"

  it should "parse the ISA segment when initialized" in {
    val in = new ByteArrayInputStream(ISA.getBytes())
    val parser = X12SchemaParser(in, EdiSchema(X12, "05010", Map.empty, Map.empty, Map.empty, Map.empty), parserConfig)
    val props = new ValueMapImpl
    parser.init(ASCII_CHARSET, props) should be (VALID)
    props.get(AUTHORIZATION_QUALIFIER) should be("00")
    props.get(AUTHORIZATION_INFO) should be("ABC")
    props.get(SECURITY_QUALIFIER) should be("00")
    props.get(SECURITY_INFO) should be("DEF")
    props.get(SENDER_ID_QUALIFIER) should be("01")
    props.get(SENDER_ID) should be("013227180")
    props.get(RECEIVER_ID_QUALIFIER) should be("ZZ")
    props.get(RECEIVER_ID) should be("IJDIECAFOX")
    val calendar = props.get(INTERCHANGE_DATE).asInstanceOf[java.util.Calendar]
    calendar.get(Calendar.YEAR) should be (2009)
    calendar.get(Calendar.MONTH) should be (5)
    calendar.get(Calendar.DAY_OF_MONTH) should be (4)
    props.get(INTERCHANGE_TIME) should be((12 * 60 + 5) * 60 * 1000)
    props.get(VERSION_ID) should be("00401")
    props.get(INTER_CONTROL) should be(1244)
    props.get(ACK_REQUESTED) should be("0")
    props.get(TEST_INDICATOR) should be("P")
  }

  it should "parse the envelope segments as requested" in {
    val in = new ByteArrayInputStream((ISA + GS + ST + buildSE(0) + buildGE(0) + IEA).getBytes())
    val parser = X12SchemaParser(in, EdiSchema(X12, "05010", Map.empty, Map.empty, Map.empty, Map.empty), parserConfig)
    val props = new ValueMapImpl
    parser.init(ASCII_CHARSET, props) should be (VALID)
    val gprops = parser.openGroup
    gprops.get(functionalIdentifierKey) should be("PO")
    gprops.get(applicationSendersKey) should be("006927180")
    gprops.get(applicationReceiversKey) should be("IAIYUCAFOO")
    gprops.get(groupDateKey) should be(new GregorianCalendar(2008, 5, 4))
    gprops.get(groupTimeKey) should be((12 * 60 + 5) * 60000)
    gprops.get(groupControlKey) should be(Integer.valueOf(168))
    gprops.get(responsibleAgencyKey) should be("X")
    gprops.get(versionIdentifierKey) should be("004010")
    val (transid, sprops) = parser.openSet
    transid should be("850")
    sprops.get(transactionSetIdentifierKey) should be("850")
    sprops.get(transactionSetControlKey) should be("000000176")
    sprops.containsKey(implementationConventionKey) should be(false)
    parser.isSetClose should be(true)
    parser.closeSet(sprops)
    parser.isGroupClose should be(true)
    parser.closeGroup(gprops)
  }

  it should "throw an exception when positioned at wrong segment" in {
    val in = new ByteArrayInputStream((ISA + GS + ST + buildSE(0) + buildGE(0) + IEA).getBytes())
    val parser = X12SchemaParser(in, EdiSchema(X12, "05010", Map.empty, Map.empty, Map.empty, Map.empty), parserConfig)
    val props = new ValueMapImpl
    parser.init(ASCII_CHARSET, props) should be (VALID)
    intercept[IllegalStateException] { parser.openSet }
    val gprops = parser.openGroup
    intercept[IllegalStateException] { parser.openGroup }
  }

  it should "parse a complete interchange message" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/cdw850schema.esl")
    val schema = YamlReader.loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")
    val parser = X12SchemaParser(messageIn, schema, parserConfig)
    val result = parser.parse
    result.isInstanceOf[Success[ValueMap]] should be (true)
    val map = result.get
  }

  behavior of "X12SchemaWriter"

  it should "write the ISA envelope when initialized and then terminated" in {
    val out = new ByteArrayOutputStream
    val create = SchemaWriter.create(out, EdiSchema(X12, "05010", Map.empty, Map.empty, Map.empty, Map.empty),
      new DefaultNumberProvider)
    assert(create.isSuccess)
    val writer = create.get
    val props = new ValueMapImpl
    props.put(AUTHORIZATION_QUALIFIER, "00")
    props.put(AUTHORIZATION_INFO, "ABC")
    props.put(SECURITY_QUALIFIER, "00")
    props.put(SECURITY_INFO, "DEF")
    props.put(SENDER_ID_QUALIFIER, "01")
    props.put(SENDER_ID, "013227180")
    props.put(RECEIVER_ID_QUALIFIER, "ZZ")
    props.put(RECEIVER_ID, "IJDIECAFOX")
    props.put(VERSION_ID, "00401")
    props.put(INTER_CONTROL, Integer.valueOf(1244))
    props.put(ACK_REQUESTED, "0")
    props.put(TEST_INDICATOR, "P")
    writer.init("*>U~", "UTF-8", props)
    writer.writer.countGroup()
    writer.term(props)
    val text = new String(out.toByteArray)
    val start = ISA indexOf (DATETIME)
    val end = start + DATETIME.length
    val compare = ISA + IEA
    text.substring(0, start) should be (compare.substring(0, start))
    text.substring(end) should be (compare.substring(end))
  }

  it should "roundtrip a parsed document" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/cdw850schema.esl")
    val schema = YamlReader.loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")
    val parser = X12SchemaParser(messageIn, schema, parserConfig)
    val parseResult = parser.parse
    parseResult.isInstanceOf[Success[ValueMap]] should be (true)
    val out = new ByteArrayOutputStream
    val createWriter = SchemaWriter.create(out, schema, new DefaultNumberProvider)
    assert(createWriter.isSuccess)
    val writer = createWriter.get
    val props = parseResult.get
    props.put(characterEncoding, "UTF-8")
    writer.write(props)
    val text = new String(out.toByteArray)
    val lines = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")).getLines
    val builder = new StringBuilder
    lines.foreach(line => builder.append(line))
    
    // TODO: fix comparison to ignore date/times in ISA and GS
    //    text should be (builder.toString)
  }
}