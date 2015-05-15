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

class EdifactSchemaParserWriterTests extends FlatSpec with Matchers with SchemaJavaDefs {

  import com.anypoint.df.edi.lexical.EdifactConstants._
  import EdiSchema._
  import SchemaJavaValues._
  import EdifactSchemaDefs._

  val DATETIME = "090604:1205"
  val UNB = s"UNB+UNOC:3+5790001086626:14+7611937000723:14+$DATETIME+8'"
  val lead = UNB
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
    val parser = EdifactSchemaParser(in, EdiSchema(EdiFact, "01b", Map.empty, Map.empty, Map.empty, Map.empty),
      new DefaultEdifactNumberValidator, parserConfig)
    val props = new ValueMapImpl
    parser.init(props) should be (SyntaxVersion.VERSION3)
    parser.schemaDefs = ControlV3Defs
    parser.parseCompList(ControlV3Defs.segUNB.components.tail, ItemType.DATA_ELEMENT, ItemType.DATA_ELEMENT, props)
    props.get(interHeadSenderIdentKey) should be ("5790001086626")
    props.get(interHeadSenderQualKey) should be ("14")
    props.get(interHeadRecipientIdentKey) should be ("7611937000723")
    props.get(interHeadRecipientQualKey) should be ("14")
    props.get(interHeadDateKey) should be (90604)
    props.get(interHeadTimeKey) should be (1205)
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

  it should "throw an exception when positioned at wrong segment" in {
    val in = new ByteArrayInputStream((lead + UNH + buildUNT(0) + buildUNZ(1)).getBytes())
    val parser = EdifactSchemaParser(in, EdiSchema(EdiFact, "01b", Map.empty, Map.empty, Map.empty, Map.empty),
      new DefaultEdifactNumberValidator, parserConfig)
    val props = new ValueMapImpl
    parser.init(props) should be (SyntaxVersion.VERSION3)
    parser.schemaDefs = ControlV3Defs
    parser.parseCompList(ControlV3Defs.segUNB.components.tail, ItemType.DATA_ELEMENT, ItemType.DATA_ELEMENT, props)
    intercept[IllegalStateException] { parser.openGroup }
    parser.openSet
    intercept[IllegalStateException] { parser.openSet }
  }

  it should "parse a complete interchange message" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/ORDERS.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/edifact-orders.edi")
    val parser = EdifactSchemaParser(messageIn, schema, new DefaultEdifactNumberValidator, parserConfig)
    val result = parser.parse
    result.isInstanceOf[Success[ValueMap]] should be (true)
    val map = result.get
  }
  
  it should "generate an error when partner id doesn't match" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/ORDERS.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/edifact-orders.edi")
    val config = EdifactParserConfig(true, true, true, true, true, true, true, -1,
      ASCII_CHARSET, Array[EdifactIdentityInformation](), Array(EdifactIdentityInformation("ABCDEF", "01", null, null)))
    val parser = EdifactSchemaParser(messageIn, schema, new DefaultEdifactNumberValidator, config)
    val result = parser.parse
    result.isFailure should be (true)
  }

  behavior of "EdifactSchemaWriter"

  it should "write the UNB/UNZ envelope when initialized and then terminated" in {
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELC, SyntaxVersion.VERSION3, -1, '.', ASCII_CHARSET, "+: '?", "")
    val writer = EdifactSchemaWriter(out, EdiSchema(EdiFact, "ORDERS", Map.empty, Map.empty, Map.empty, Map.empty),
      new DefaultEdifactNumberProvider, config)
    val initprops = new ValueMapImpl
    initprops.put(interHeadSenderIdentKey, "5790001086626")
    initprops.put(interHeadSenderQualKey, "14")
    initprops.put(interHeadRecipientIdentKey, "7611937000723")
    initprops.put(interHeadRecipientQualKey, "14")
    initprops.put(interHeadDateKey, Integer.valueOf(90604))
    initprops.put(interHeadTimeKey, Integer.valueOf(1205))
    initprops.put(interHeadReferenceKey, "8")
    writer.init(initprops)
    val termprops = new ValueMapImpl
    termprops.put(interTrailCountKey, Integer.valueOf(1))
    termprops.put(interTrailReferenceKey, "8")
    writer.term(termprops)
    writer.writer.close
    val text = new String(out.toByteArray)
    val start = UNB indexOf (DATETIME)
    val end = start + DATETIME.length
    val compare = lead + UNZ
    text.substring(0, start) should be (compare.substring(0, start))
    text.substring(end) should be (compare.substring(end))
  }
  
  it should "roundtrip a parsed document" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/ORDERS.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/edifact-orders.edi")
    val parser = EdifactSchemaParser(messageIn, schema, new DefaultEdifactNumberValidator, parserConfig)
    val parseResult = parser.parse
    parseResult.isInstanceOf[Success[ValueMap]] should be (true)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELB, SyntaxVersion.VERSION4, -1, '.', ASCII_CHARSET, "+:*'?", "\n")
    val writer = EdifactSchemaWriter(out, schema, new DefaultEdifactNumberProvider, config)
    val props = parseResult.get
    val message = getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, props)).get(0)
    val inter = getRequiredValueMap(interchangeKey, message)
    swap(interHeadSenderQualKey, interHeadRecipientQualKey, inter)
    swap(interHeadSenderIdentKey, interHeadRecipientIdentKey, inter)
    move(interchangeKey, message, props)
    writer.write(props).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    val lines = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("edi/edifact-orders.edi")).getLines
    val builder = new StringBuilder
    lines.foreach(line => builder.append(line))

    // TODO: fix comparison to ignore date/times
//    text should be (builder.toString)
  }
}