package com.anypoint.df.edi.schema

import java.io.InputStreamReader
import java.io.StringReader
import java.io.StringWriter
import java.math.BigDecimal
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
import com.anypoint.df.edi.lexical.EdifactConstants._
import com.anypoint.df.edi.schema.tools.{ DefaultEdifactNumberProvider, DefaultEdifactNumberValidator }

class EdifactSchemaParserWriterTests extends FlatSpec with Matchers with SchemaJavaDefs with EdifactSchemaDefs {

  import com.anypoint.df.edi.lexical.EdifactConstants._
  import EdiSchema._
  import SchemaJavaValues._

  val DATETIME = "090604:1205"
  val lead = s"UNA:+.? 'UNB+UNOC:3+5790001086626:14+7611937000723:14+$DATETIME+8'"
  val UNH = "UNH+800001+INVOIC:D:01B:UN:EAN010'"
  val UNT = "UNT+43+800001'"
  val UNZ = "UNZ+1+8'"

  def buildUNT(count: Int) = s"UNT+$count+800001'"
  
  def buildUNZ(count: Int) = s"UNZ+$count+8'"

  val parserConfig = EdifactParserConfig(true, true, true, true, true, true, true, -1,
    ASCII_CHARSET, Array[EdifactIdentityInformation](), Array[EdifactIdentityInformation]())

  behavior of "EdifactSchemaParser"

  it should "parse the UNB segment start when initialized" in {
    val in = new ByteArrayInputStream(lead.getBytes())
    val parser = EdifactSchemaParser(in, EdiSchema(EdiFact, "1997a", Map.empty, Map.empty, Map.empty, Map.empty),
      new DefaultEdifactNumberValidator, parserConfig)
    val props = new ValueMapImpl
    parser.init(props) should be (SyntaxVersion.VERSION3)
    props.get(SYNTAX_IDENTIFIER) should be ("UNOC")
    props.get(SYNTAX_VERSION_NUMBER) should be ("3")
    props.containsKey(SERVICE_CODE_LIST) should be (false)
    props.containsKey(CHARACTER_ENCODING) should be (false)
  }

  it should "parse the envelope segments as requested" in {
    val in = new ByteArrayInputStream((lead + UNH + buildUNT(0) + buildUNZ(1)).getBytes())
    val parser = EdifactSchemaParser(in, EdiSchema(EdiFact, "1997a", Map.empty, Map.empty, Map.empty, Map.empty),
      new DefaultEdifactNumberValidator, parserConfig)
    val props = new ValueMapImpl
    parser.init(props) should be (SyntaxVersion.VERSION3)
    parser.parseCompList(ControlV3Defs.segUNB.components.tail, ItemType.DATA_ELEMENT, ItemType.DATA_ELEMENT, props)
    props.get(interHeadSenderIdentKey) should be ("5790001086626")
    props.get(interHeadSenderQualKey) should be ("14")
    props.get(interHeadRecipientIdentKey) should be ("7611937000723")
    props.get(interHeadRecipientQualKey) should be ("14")
    props.get(interHeadDateKey) should be (new BigDecimal("90604"))
    props.get(interHeadTimeKey) should be (new BigDecimal(1205))
    props.get(interHeadReferenceKey) should be ("8")
    val (transid, sprops) = parser.openSet
    transid should be("INVOIC")
    sprops.get(msgHeadReferenceKey) should be ("800001")
    sprops.get(msgHeadMessageVersionKey) should be ("D")
    sprops.get(msgHeadMessageReleaseKey) should be ("01B")
    sprops.get(msgHeadMessageAgencyKey) should be ("UN")
    sprops.get(msgHeadMessageAssignedKey) should be ("EAN010")
    parser.closeSet(sprops)
  }

//  it should "throw an exception when positioned at wrong segment" in {
//    val in = new ByteArrayInputStream((ISA + GS + ST + buildSE(0) + buildGE(0) + IEA).getBytes())
//    val parser = EdifactSchemaParser(in, EdiSchema(EdiFact, "1997a", Map.empty, Map.empty, Map.empty, Map.empty),
//      new DefaultEdifactNumberValidator, parserConfig)
//    val props = new ValueMapImpl
//    parser.init(props) should be (VALID)
//    intercept[IllegalStateException] { parser.openSet }
//    val gprops = parser.openGroup
//    intercept[IllegalStateException] { parser.openGroup }
//  }

//  it should "parse a complete interchange message" in {
//    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/cdw850schema.esl")
//    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
//    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")
//    val parser = X12SchemaParser(messageIn, schema, new DefaultX12NumberValidator, parserConfig)
//    val result = parser.parse
//    result.isInstanceOf[Success[ValueMap]] should be (true)
//    val map = result.get
//  }
//
//  behavior of "X12SchemaWriter"
//
//  it should "write the ISA envelope when initialized and then terminated" in {
//    val out = new ByteArrayOutputStream
//    val config = X12WriterConfig(CharacterRestriction.BASIC, -1, ASCII_CHARSET, "*>U~", null)
//    val writer = X12SchemaWriter(out, EdiSchema(X12, "05010", Map.empty, Map.empty, Map.empty, Map.empty),
//      new DefaultX12NumberProvider, config)
//    val props = new ValueMapImpl
//    props.put(AUTHORIZATION_QUALIFIER, "00")
//    props.put(AUTHORIZATION_INFO, "ABC")
//    props.put(SECURITY_QUALIFIER, "00")
//    props.put(SECURITY_INFO, "DEF")
//    props.put(SENDER_ID_QUALIFIER, "01")
//    props.put(SENDER_ID, "013227180")
//    props.put(RECEIVER_ID_QUALIFIER, "ZZ")
//    props.put(RECEIVER_ID, "IJDIECAFOX")
//    props.put(VERSION_ID, "00401")
//    props.put(INTER_CONTROL, Integer.valueOf(1244))
//    props.put(ACK_REQUESTED, "0")
//    props.put(TEST_INDICATOR, "P")
//    writer.writer.init(props)
//    writer.writer.countGroup()
//    writer.term(props)
//    writer.writer.close
//    val text = new String(out.toByteArray)
//    val start = ISA indexOf (DATETIME)
//    val end = start + DATETIME.length
//    val compare = ISA + IEA
//    text.substring(0, start) should be (compare.substring(0, start))
//    text.substring(end) should be (compare.substring(end))
//  }
//
//  it should "roundtrip a parsed document" in {
//    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/cdw850schema.esl")
//    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
//    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")
//    val parser = X12SchemaParser(messageIn, schema, new DefaultX12NumberValidator, parserConfig)
//    val parseResult = parser.parse
//    parseResult.isInstanceOf[Success[ValueMap]] should be (true)
//    val out = new ByteArrayOutputStream
//    val config = X12WriterConfig(CharacterRestriction.BASIC, -1, ASCII_CHARSET, "*>U~", null)
//    val writer = X12SchemaWriter(out, schema, new DefaultX12NumberProvider, config)
//    val props = parseResult.get
//    writer.write(props)
//    val text = new String(out.toByteArray)
//    val lines = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")).getLines
//    val builder = new StringBuilder
//    lines.foreach(line => builder.append(line))
//    
//    // TODO: fix comparison to ignore date/times in ISA and GS
//    //    text should be (builder.toString)
//  }
}