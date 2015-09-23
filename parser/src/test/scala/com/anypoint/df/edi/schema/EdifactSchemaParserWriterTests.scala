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
import com.anypoint.df.edi.lexical.EdifactConstants._
import com.anypoint.df.edi.schema.tools.{ DefaultEdifactEnvelopeHandler, DefaultEdifactNumberProvider }
import com.anypoint.df.edi.lexical.WriteException

class EdifactSchemaParserWriterTests extends FlatSpec with Matchers with SchemaJavaDefs {

  import com.anypoint.df.edi.lexical.EdifactConstants._
  import EdiSchema._
  import SchemaJavaValues._
  import EdifactSchemaDefs._

  val DATETIME = "090604:1205"
  val UNB = s"UNB+UNOA:3+5790001086626:14+7611937000723:14+$DATETIME+8'"
  val lead = UNB
  val UNH = "UNH+800001+INVOIC:D:01B:UN:EAN010'"
  val UNT = "UNT+43+800001'"
  val UNZ = "UNZ+1+8'"
  val UNA = "UNA:+.? '"

  def buildUNT(count: Int) = s"UNT+$count+800001'"

  def buildUNZ(count: Int) = s"UNZ+$count+8'"

  val parserConfig = EdifactParserConfig(true, true, true, true, true, true, true, false, -1)

  /** Reads a copy of the test document into memory, standardizing line endings. */
  val testDoc = {
    val lines = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("edi/edifact-orders.edi")).getLines
    val builder = new StringBuilder
    lines.foreach(line => {
      if (!builder.isEmpty) builder.append('\n')
      builder.append(line)
    })
    builder.toString
  }

  val testSchema = new YamlReader().loadYaml(new InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/ORDERS.esl"), "UTF-8"), Array())

  def parseDoc(doc: String) = {
    val ins = new ByteArrayInputStream(doc.getBytes(ASCII_CHARSET))
    val parser = new EdifactInterchangeParser(ins, null, new DefaultEdifactEnvelopeHandler(parserConfig, testSchema))
    parser.parse.get
  }

  behavior of "EdifactSchemaParser"

  it should "parse the UNB segment start when initialized" in {
    val in = new ByteArrayInputStream(lead.getBytes())
    val schema = EdiSchema(EdiSchemaVersion(EdiFact, "1997a"), Map.empty, Map.empty, Map.empty, Map.empty)
    val parser = new EdifactInterchangeParser(in, null, new DefaultEdifactEnvelopeHandler(parserConfig, schema))
    val props = new ValueMapImpl
    parser.init(props) should be (SyntaxVersion.VERSION3)
    props.get(SYNTAX_IDENTIFIER) should be ("UNOA")
    props.get(SYNTAX_VERSION_NUMBER) should be ("3")
    props.containsKey(SERVICE_CODE_LIST) should be (false)
    props.containsKey(CHARACTER_ENCODING) should be (false)
  }

  it should "parse the envelope segments as requested" in {
    val in = new ByteArrayInputStream((lead + UNH + buildUNT(0) + buildUNZ(1)).getBytes())
    val schema = EdiSchema(EdiSchemaVersion(EdiFact, "01b"), Map.empty, Map.empty, Map.empty, Map.empty)
    val parser = new EdifactInterchangeParser(in, null, new DefaultEdifactEnvelopeHandler(parserConfig, schema))
    val props = new ValueMapImpl
    parser.init(props) should be (SyntaxVersion.VERSION3)
    parser.parseCompList(segUNBv3.components.tail, ItemType.DATA_ELEMENT, ItemType.DATA_ELEMENT, props)
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
    val schema = EdiSchema(EdiSchemaVersion(EdiFact, "01b"), Map.empty, Map.empty, Map.empty, Map.empty)
    val parser = new EdifactInterchangeParser(in, null, new DefaultEdifactEnvelopeHandler(parserConfig, schema))
    val props = new ValueMapImpl
    parser.init(props) should be (SyntaxVersion.VERSION3)
    parser.parseCompList(segUNBv3.components.tail, ItemType.DATA_ELEMENT, ItemType.DATA_ELEMENT, props)
    intercept[IllegalStateException] { parser.openGroup }
    parser.openSet
    intercept[IllegalStateException] { parser.openSet }
  }

  it should "parse a complete interchange message" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/ORDERS.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/edifact-orders.edi")
    val parser = new EdifactInterchangeParser(messageIn, null, new DefaultEdifactEnvelopeHandler(parserConfig, schema))
    val result = parser.parse
    result.isInstanceOf[Success[ValueMap]] should be (true)
    val map = result.get
  }
  
  def getSyntaxVersion(root: ValueMap) = {
    val messages = getRequiredValueMap(messagesMap, root)
    val trans = getRequiredValueMap("D96A", messages)
    val order = getRequiredMapList("ORDERS", trans).get(0)
    getRequiredValueMap(interchangeKey, order).get(interHeadSyntaxVersionKey)
  }

  it should "parse syntax version 1 as syntax version 2" in {
    val modDoc = UNA + testDoc.substring(0, 9) + '1' + testDoc.substring(10, 44) + testDoc.substring(46)
    val input = parseDoc(modDoc)
    getSyntaxVersion(input) should be ("1")
  }

  it should "throw an exception when date length in UNB doesn't match syntax version" in {
    val modDoc = UNA + testDoc.substring(0, 9) + '1' + testDoc.substring(10)
    intercept[EdifactInterchangeException] { parseDoc(modDoc) }
  }

  it should "parse syntax version 2" in {
    val modDoc = UNA + testDoc.substring(0, 9) + '2' + testDoc.substring(10, 44) + testDoc.substring(46)
    val input = parseDoc(modDoc)
    getSyntaxVersion(input) should be ("2")
  }

  it should "parse syntax version 3" in {
    val modDoc = UNA + "UNB+UNOB:3" + testDoc.substring(10, 44) + testDoc.substring(46)
    val input = parseDoc(modDoc)
    getSyntaxVersion(input) should be ("3")
  }

  it should "generate a CONTRL acknowledgment for a normal message" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/ORDERS.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/edifact-orders.edi")
    val parser = new EdifactInterchangeParser(messageIn, null, new DefaultEdifactEnvelopeHandler(parserConfig, schema))
    val map = parser.parse.get
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    acks.size should be (1)
  }

  it should "not generate a CONTRL acknowledgment for a CONTRL message" in {
    val schema = new EdiSchema(EdiSchemaVersion(EdiFact, "96a")).merge(transCONTRLv3)
    val messageIn = new ByteArrayInputStream("UNB+UNOA:3+MODUS:ZZZ+MULESOFT:ZZZ+150608:2032+1++ORDERS'UNH+1+CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UNT+3+1'UNZ+1+1'".getBytes)
    val parser = new EdifactInterchangeParser(messageIn, null, new DefaultEdifactEnvelopeHandler(parserConfig, schema))
    val map = parser.parse.get
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    acks.size should be (0)
  }

  val writerConfig = EdifactWriterConfig(LEVELB, SyntaxVersion.VERSION4, false, -1, '.', ASCII_CHARSET, "+:*'?", "\n", false)

  /** Get provider configured for the sample document. */
  def docProvider = {
    val provider = new DefaultEdifactNumberProvider
    provider.interNum = 581
    provider.setNum = 6423
    provider
  }

  behavior of "EdifactSchemaWriter"

  it should "write the UNB/UNZ envelope when initialized and then terminated" in {
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELA, SyntaxVersion.VERSION3, false, -1, '.', ASCII_CHARSET, "+: '?", "", false)
    val writer = EdifactSchemaWriter(out, new DefaultEdifactNumberProvider, config)
    val initprops = new ValueMapImpl
    initprops.put(interHeadSenderIdentKey, "5790001086626")
    initprops.put(interHeadSenderQualKey, "14")
    initprops.put(interHeadRecipientIdentKey, "7611937000723")
    initprops.put(interHeadRecipientQualKey, "14")
    initprops.put(interHeadDateKey, Integer.valueOf(90604))
    initprops.put(interHeadTimeKey, Integer.valueOf(1205))
    initprops.put(interHeadReferenceKey, "8")
    writer.init(initprops)
    writer.setCount += 1
    writer.term("8")
    writer.writer.close
    val text = new String(out.toByteArray)
    val start = UNB indexOf (DATETIME)
    val end = start + DATETIME.length
    val compare = lead + UNZ
    text.substring(0, start) should be (compare.substring(0, start))
    text.substring(end) should be (compare.substring(end))
  }

  it should "roundtrip a parsed document" in {
    val input = parseDoc(testDoc)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELB, SyntaxVersion.VERSION4, false, -1, '.', ASCII_CHARSET, "+:*'?", "\n", false)
    val writer = EdifactSchemaWriter(out, docProvider, config)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    println(text)
    text should be (testDoc)
  }

  it should "generate UNOA when forced" in {
    val input = parseDoc(testDoc)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELB, SyntaxVersion.VERSION4, false, -1, '.', ASCII_CHARSET, "+:*'?", "\n", true)
    val writer = EdifactSchemaWriter(out, docProvider, config)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    text should be ("UNA:+.?*'\n" + testDoc)
  }

  it should "roundtrip a parsed document using syntax version 2" in {
    val modDoc = ("UNB+UNOA:2" + testDoc.substring(10, 44) + testDoc.substring(46)).toUpperCase
    val input = parseDoc(modDoc)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELA, SyntaxVersion.VERSION2, false, -1, '.', ASCII_CHARSET, "+: '?", "\n", false)
    val writer = EdifactSchemaWriter(out, docProvider, config)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    text should be (modDoc)
  }

  it should "roundtrip a parsed document using syntax version 3" in {
    val modDoc = ("UNB+UNOA:3" + testDoc.substring(10, 44) + testDoc.substring(46)).toUpperCase
    val input = parseDoc(modDoc)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELA, SyntaxVersion.VERSION3, false, -1, '.', ASCII_CHARSET, "+: '?", "\n", false)
    val writer = EdifactSchemaWriter(out, docProvider, config)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    text should be (modDoc)
  }
  
  def extractMessage(vkey: String, mtype: String, root: ValueMap) = {
    val vmap = getRequiredValueMap(messagesMap, root)
    val mmap = getRequiredValueMap(vkey, vmap)
    getRequiredMapList(mtype, mmap).get(0)
  }

  it should "use interchange data at message level to override root level" in {
    val input = parseDoc(testDoc)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELB, SyntaxVersion.VERSION4, false, -1, '.', ASCII_CHARSET, "+:*'?", "\n", false)
    val writer = EdifactSchemaWriter(out, docProvider, config)
    val message = extractMessage("D96A", "ORDERS", input)
    val interMsg = getRequiredValueMap(interchangeKey, message)
    val interRoot = new ValueMapImpl(interMsg)
    swap(interHeadSenderQualKey, interHeadRecipientQualKey, interRoot)
    swap(interHeadSenderIdentKey, interHeadRecipientIdentKey, interRoot)
    input put (interchangeKey, interRoot)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    text should be (testDoc)
  }

  def oneshotWriter = EdifactSchemaWriter(new ByteArrayOutputStream, new DefaultEdifactNumberProvider, writerConfig)

  it should "throw an exception when missing required interchange data" in {
    val input = parseDoc(testDoc)
    extractMessage("D96A", "ORDERS", input).remove(interchangeKey)
    input remove (interchangeKey)
    intercept[WriteException] { oneshotWriter.write(input).get }
  }

  /** Data- and schema-dependent tests. */
  it should "throw an exception when missing required segment" in {
    val input = parseDoc(testDoc)
    val message = extractMessage("D96A", "ORDERS", input)
    getRequiredValueMap(structureHeading, message).remove("0020_BGM")
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when missing required segment list" in {
    val input = parseDoc(testDoc)
    val message = extractMessage("D96A", "ORDERS", input)
    getRequiredValueMap(structureHeading, message).remove("0030_DTM")
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when missing instance of required repeated segment" in {
    val input = parseDoc(testDoc)
    val message = extractMessage("D96A", "ORDERS", input)
    val list = getRequiredMapList("0030_DTM", getRequiredValueMap(structureHeading, message))
    list.clear
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when missing required component value" in {
    val input = parseDoc(testDoc)
    val message = extractMessage("D96A", "ORDERS", input)
    val segment = getRequiredMapList("0030_DTM", getRequiredValueMap(structureHeading, message)).get(0)
    segment remove ("DTM0101")
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when a number value is given a string" in {
    val input = parseDoc(testDoc)
    val message = extractMessage("D96A", "ORDERS", input)
    val segment = getRequiredMapList("2110_CNT", getRequiredValueMap(structureSummary, message)).get(0)
    segment put ("CNT0102", "1")
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when a string value is given a number" in {
    val input = parseDoc(testDoc)
    val message = extractMessage("D96A", "ORDERS", input)
    val segment = getRequiredMapList("2110_CNT", getRequiredValueMap(structureSummary, message)).get(0)
    segment put ("CNT0101", Integer.valueOf(2))
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
}