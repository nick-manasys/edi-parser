package com.anypoint.df.edi.schema

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStreamReader
import java.util.Calendar
import java.util.GregorianCalendar

import scala.io.Source
import scala.util.Success

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.anypoint.df.edi.lexical.EdiConstants.ASCII_CHARSET
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.lexical.X12Lexer.InterchangeStartStatus.VALID
import com.anypoint.df.edi.schema.tools.DefaultX12EnvelopeHandler
import com.anypoint.df.edi.schema.tools.DefaultX12NumberProvider

import EdiSchema.X12

class X12SchemaParserWriterTests extends FlatSpec with Matchers with SchemaJavaDefs {

  import com.anypoint.df.edi.lexical.X12Constants._
  import EdiSchema._
  import SchemaJavaValues._
  import X12SchemaDefs._

  val DATETIME = "090604*1205"
  val ISA = s"ISA*00*ABC       *00*DEF       *01*013227180      *ZZ*IJDIECAFOX     *$DATETIME*U*00401*000001244*0*P*>~"
  val GS = "GS*PO*006927180*IAIYUCAFOO*20080604*1205*168*X*004010~"
  val ST = "ST*850*000000176~"
  val IEA = "IEA*1*000001244~"

  def buildGE(count: Int) = s"GE*$count*168~"

  def buildSE(count: Int) = s"SE*$count*000000176~"

  val parserConfig = X12ParserConfig(true, true, true, true, true, true, true, true, false, -1,
    CharacterRestriction.EXTENDED)

  behavior of "X12SchemaParser"

  it should "parse the ISA segment when initialized" in {
    val in = new ByteArrayInputStream(ISA.getBytes())
    val parser = new X12InterchangeParser(in, ASCII_CHARSET,
      new DefaultX12EnvelopeHandler(parserConfig, new EdiSchema(EdiSchemaVersion(X12, "05010"))))
    val props = new ValueMapImpl
    parser.init(props) should be (VALID)
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

/*  it should "parse the envelope segments as requested" in {
    val in = new ByteArrayInputStream((ISA + GS + ST + buildSE(0) + buildGE(0) + IEA).getBytes())
    val parser = new X12InterchangeParser(in, ASCII_CHARSET,
      new DefaultX12EnvelopeHandler(parserConfig, new EdiSchema(EdiSchemaVersion(X12, "05010"))))
    val props = new ValueMapImpl
    parser.init(props) should be (VALID)
    val gprops = parser.openGroup
    gprops.get(groupFunctionalIdentifierKey) should be("PO")
    gprops.get(groupApplicationSenderKey) should be("006927180")
    gprops.get(groupApplicationReceiverKey) should be("IAIYUCAFOO")
    gprops.get(groupDateKey) should be(new GregorianCalendar(2008, 5, 4))
    gprops.get(groupTimeKey) should be((12 * 60 + 5) * 60000)
    gprops.get(groupControlNumberHeaderKey) should be(Integer.valueOf(168))
    gprops.get(groupResponsibleAgencyKey) should be("X")
    gprops.get(groupVersionReleaseIndustryKey) should be("004010")
    val (transid, sprops) = parser.openSet
    transid should be("850")
    sprops.get(setIdentifierCodeKey) should be("850")
    sprops.get(setControlNumberHeaderKey) should be("000000176")
    sprops.containsKey(setImplementationConventionKey) should be(false)
    parser.closeSet(sprops)
    parser.isGroupClose should be(true)
    parser.closeGroup(gprops)
  }
*/
/*  it should "throw an exception when positioned at wrong segment" in {
    val in = new ByteArrayInputStream((ISA + GS + ST + buildSE(0) + buildGE(0) + IEA).getBytes())
    val parser = new X12InterchangeParser(in, ASCII_CHARSET,
      new DefaultX12EnvelopeHandler(parserConfig, new EdiSchema(EdiSchemaVersion(X12, "05010"))))
    val props = new ValueMapImpl
    parser.init(props) should be (VALID)
    intercept[IllegalStateException] { parser.openSet }
    val gprops = parser.openGroup
    intercept[IllegalStateException] { parser.openGroup }
  }
*/
  it should "parse a complete interchange message" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/cdw850schema.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")
    val parser = new X12InterchangeParser(messageIn, ASCII_CHARSET,
      new DefaultX12EnvelopeHandler(parserConfig, schema))
    val result = parser.parse
    result.isInstanceOf[Success[ValueMap]] should be (true)
    val map = result.get
  }
  
  it should "generate a Functional Acknowledgment for a received group" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/cdw850schema.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")
    val parser = new X12InterchangeParser(messageIn, ASCII_CHARSET,
      new DefaultX12EnvelopeHandler(parserConfig, schema))
    val props = new ValueMapImpl
    val result = parser.parse
    result.isInstanceOf[Success[ValueMap]] should be (true)
    val map = result.get
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    acks.size should be (1)
  }
  
  it should "not generate a Functional Acknowledgment for a Functional Acknowledgment" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/cdw850schema.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val text = "ISA*00*          *00*          *ZZ*MULESOFT       *ZZ*MODUS          *150106*1201*U*00501*000000001*1*P*>~GS*FA*MULESOFT*MODUS*20150106*1201*1*X*005010~ST*999*0001~AK1*PO*123456789*005010~AK2*850*000000123~IK5*R*4~AK9*R*2*1*0*5~SE*6*0001~GE*1*1~IEA*1*000000001~"
    val parser = new X12InterchangeParser(new ByteArrayInputStream(text.getBytes), ASCII_CHARSET,
      new DefaultX12EnvelopeHandler(parserConfig, schema))
    val result = parser.parse
    result.isInstanceOf[Success[ValueMap]] should be (true)
    val map = result.get
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    acks.size should be (0)
  }

  behavior of "X12SchemaWriter"

  it should "write the ISA envelope when initialized and then terminated" in {
    val out = new ByteArrayOutputStream
    val config = X12WriterConfig(CharacterRestriction.BASIC, -1, ASCII_CHARSET, "*>U~", null)
    val writer = X12SchemaWriter(out, new DefaultX12NumberProvider, config)
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
    writer.writer.init(props)
    writer.writer.countGroup()
    writer.term(props)
    writer.writer.close
    val text = new String(out.toByteArray)
    val start = ISA indexOf (DATETIME)
    val end = start + DATETIME.length
    val compare = ISA + IEA
    text.substring(0, start) should be (compare.substring(0, start))
    text.substring(end) should be (compare.substring(end))
  }

  it should "roundtrip a parsed document" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/cdw850schema.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")
    val parser = new X12InterchangeParser(messageIn, ASCII_CHARSET,
      new DefaultX12EnvelopeHandler(parserConfig, schema))
    val parseResult = parser.parse
    parseResult.isInstanceOf[Success[ValueMap]] should be (true)
    val out = new ByteArrayOutputStream
    val config = X12WriterConfig(CharacterRestriction.EXTENDED, -1, ASCII_CHARSET, "*>U~", null)
    val provider = new DefaultX12NumberProvider
    val writer = X12SchemaWriter(out, provider, config)
    val props = parseResult.get
    props put (interchangeKey, new ValueMapImpl)
    provider.interNum = 1243
    provider.groupNum = 167
    provider.setNum = 175
    writer.write(props)
    val text = new String(out.toByteArray)
    val lines = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")).getLines
    val builder = new StringBuilder
    lines.foreach(line => builder.append(line))
    text should be (builder.toString)
  }
}