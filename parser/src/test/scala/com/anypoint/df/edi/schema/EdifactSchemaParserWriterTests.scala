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
import com.anypoint.df.edi.schema.tools.{ DefaultEdifactNumberProvider, DefaultEdifactNumberValidator }
import com.anypoint.df.edi.lexical.WriteException

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
    Array[EdifactIdentityInformation](), Array[EdifactIdentityInformation]())

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
    val parser = EdifactSchemaParser(ins, testSchema, new DefaultEdifactNumberValidator, parserConfig)
    parser.parse.get
  }

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
    val parser = EdifactSchemaParser(in, EdiSchema(EdiFact, "01b", Map.empty, Map.empty, Map.empty, Map.empty),
      new DefaultEdifactNumberValidator, parserConfig)
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
    val parser = EdifactSchemaParser(messageIn, schema, new DefaultEdifactNumberValidator, parserConfig)
    val result = parser.parse
    result.isInstanceOf[Success[ValueMap]] should be (true)
    val map = result.get
  }

  it should "parse syntax version 1 as syntax version 2" in {
    val modDoc = testDoc.substring(0, 9) + '1' + testDoc.substring(10, 44) + testDoc.substring(46)
    val input = parseDoc(modDoc)
    val interMsg = getRequiredValueMap(interchangeKey, input)
    interMsg.get(interHeadSyntaxVersionKey) should be ("1")
  }

  it should "throw an exception when date length in UNB doesn't match syntax version" in {
    val modDoc = testDoc.substring(0, 9) + '1' + testDoc.substring(10)
    intercept[EdifactInterchangeException] { parseDoc(modDoc) }
  }

  it should "parse syntax version 2" in {
    val modDoc = testDoc.substring(0, 9) + '2' + testDoc.substring(10, 44) + testDoc.substring(46)
    val input = parseDoc(modDoc)
    val interMsg = getRequiredValueMap(interchangeKey, input)
    interMsg.get(interHeadSyntaxVersionKey) should be ("2")
  }

  it should "parse syntax version 3" in {
    val modDoc = "UNB+UNOA:3" + testDoc.substring(10, 44) + testDoc.substring(46)
    val input = parseDoc(modDoc)
    val interMsg = getRequiredValueMap(interchangeKey, input)
    interMsg.get(interHeadSyntaxVersionKey) should be ("3")
  }

  it should "generate a CONTRL acknowledgment for a normal message" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("esl/ORDERS.esl")
    val schema = new YamlReader().loadYaml(new InputStreamReader(yamlIn, "UTF-8"), Array())
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/edifact-orders.edi")
    val parser = EdifactSchemaParser(messageIn, schema, new DefaultEdifactNumberValidator, parserConfig)
    val map = parser.parse.get
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    acks.size should be (1)
  }

  it should "not generate a CONTRL acknowledgment for a CONTRL message" in {
    val schema = new EdiSchema(EdiFact, "96a").merge(transCONTRLv3)
    val messageIn = new ByteArrayInputStream("UNB+UNOC:3+MODUS:ZZZ+MULESOFT:ZZZ+150608:2032+1++ORDERS'UNH+1+CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UNT+3+1'UNZ+1+1'".getBytes)
    val parser = EdifactSchemaParser(messageIn, schema, new DefaultEdifactNumberValidator, parserConfig)
    val map = parser.parse.get
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    acks.size should be (0)
  }

  val writerConfig = EdifactWriterConfig(LEVELB, SyntaxVersion.VERSION4, -1, '.', ASCII_CHARSET, "+:*'?", "\n")

  /** Get provider configured for the sample document. */
  def docProvider = {
    val provider = new DefaultEdifactNumberProvider
    provider.interNum = 581
    provider.setNum = 6423
    provider
  }

  /** Prepare a test input message to be output as the same mesaage. Removes the interchange information from the
    * actual message(s) and swaps interchange sender and receipient information in the root interchange map.
    */
  def prepareToSend(input: ValueMap) = {
    getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, input)).asScala.foreach {
      map => map.remove(interchangeKey)
    }
    val inter = getRequiredValueMap(interchangeKey, input)
    swap(interHeadSenderQualKey, interHeadRecipientQualKey, inter)
    swap(interHeadSenderIdentKey, interHeadRecipientIdentKey, inter)
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
    val input = parseDoc(testDoc)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELB, SyntaxVersion.VERSION4, -1, '.', ASCII_CHARSET, "+:*'?", "\n")
    val writer = EdifactSchemaWriter(out, testSchema, docProvider, config)
    prepareToSend(input)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    text should be (testDoc)
  }

  it should "roundtrip a parsed document using syntax version 2" in {
    val modDoc = "UNB+UNOB:2" + testDoc.substring(10, 44) + testDoc.substring(46)
    val input = parseDoc(modDoc)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELB, SyntaxVersion.VERSION2, -1, '.', ASCII_CHARSET, "+: '?", "\n")
    val writer = EdifactSchemaWriter(out, testSchema, docProvider, config)
    prepareToSend(input)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    text should be (modDoc)
  }

  it should "roundtrip a parsed document using syntax version 3" in {
    val modDoc = "UNB+UNOA:3" + testDoc.substring(10, 44) + testDoc.substring(46)
    val input = parseDoc(modDoc)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELA, SyntaxVersion.VERSION3, -1, '.', ASCII_CHARSET, "+: '?", "\n")
    val writer = EdifactSchemaWriter(out, testSchema, docProvider, config)
    prepareToSend(input)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    text should be (modDoc)
  }

  it should "use interchange data at message level to override root level" in {
    val input = parseDoc(testDoc)
    val out = new ByteArrayOutputStream
    val config = EdifactWriterConfig(LEVELB, SyntaxVersion.VERSION4, -1, '.', ASCII_CHARSET, "+:*'?", "\n")
    val writer = EdifactSchemaWriter(out, testSchema, docProvider, config)
    val message = getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, input)).get(0)
    val interMsg = getRequiredValueMap(interchangeKey, message)
    val interRoot = new ValueMapImpl(interMsg)
    swap(interHeadSenderQualKey, interHeadRecipientQualKey, interMsg)
    swap(interHeadSenderIdentKey, interHeadRecipientIdentKey, interMsg)
    input put (interchangeKey, interRoot)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    text should be (testDoc)
  }

  def oneshotWriter = EdifactSchemaWriter(new ByteArrayOutputStream, testSchema, new DefaultEdifactNumberProvider, writerConfig)

  it should "throw an exception when missing required interchange data" in {
    val input = parseDoc(testDoc)
    getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, input)).asScala.foreach {
      map => map.remove(interchangeKey)
    }
    input remove (interchangeKey)
    intercept[WriteException] { oneshotWriter.write(input).get }
  }

  /** Data- and schema-dependent tests. */
  it should "throw an exception when missing required segment" in {
    val input = parseDoc(testDoc)
    val message = getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, input)).get(0)
    getRequiredValueMap(transactionHeading, message).remove("0020 BGM")
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when missing required segment list" in {
    val input = parseDoc(testDoc)
    val message = getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, input)).get(0)
    getRequiredValueMap(transactionHeading, message).remove("0030 DTM")
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when missing instance of required repeated segment" in {
    val input = parseDoc(testDoc)
    val message = getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, input)).get(0)
    val list = getRequiredMapList("0030 DTM", getRequiredValueMap(transactionHeading, message))
    list.clear
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when missing required component value" in {
    val input = parseDoc(testDoc)
    val message = getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, input)).get(0)
    val segment = getRequiredMapList("0030 DTM", getRequiredValueMap(transactionHeading, message)).get(0)
    segment remove ("DTM0101")
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when a number value is given a string" in {
    val input = parseDoc(testDoc)
    val message = getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, input)).get(0)
    val segment = getRequiredMapList("2110 CNT", getRequiredValueMap(transactionSummary, message)).get(0)
    segment put ("CNT0102", "1")
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
  it should "throw an exception when a string value is given a number" in {
    val input = parseDoc(testDoc)
    val message = getRequiredMapList("ORDERS", getRequiredValueMap(messagesMap, input)).get(0)
    val segment = getRequiredMapList("2110 CNT", getRequiredValueMap(transactionSummary, message)).get(0)
    segment put ("CNT0101", Integer.valueOf(2))
    intercept[WriteException] { oneshotWriter.write(input).get }
  }
}