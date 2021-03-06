package com.mulesoft.flatfile.schema

import java.{ io => jio, lang => jl }
import java.math.BigDecimal
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import collection.JavaConverters._
import scala.io.Source
import java.util.GregorianCalendar
import scala.util.Success
import com.mulesoft.flatfile.lexical.EdiConstants
import com.mulesoft.flatfile.lexical.EdiConstants._
import com.mulesoft.flatfile.lexical.LexicalException
import com.mulesoft.flatfile.schema.tools.YamlSupport
import javax.xml.datatype.XMLGregorianCalendar

import org.threeten.bp.{ LocalDate, LocalTime }
import scala.collection.mutable.ListBuffer
import com.mulesoft.flatfile.lexical.WriteException
import com.mulesoft.flatfile.lexical.IBM037

class FlatFileSchemaParserWriterTests extends FlatSpec with Matchers with SchemaJavaDefs {
  
  import EdiSchema._
  import SchemaJavaValues._
  
  val line1 = "1MISSION   201308020800MISSIONAUSTRALIA              2009110401                                                                                                                                                                                           \n"
  val line9 = "90100000001000000010000MISSION   MISSIONAUSTRALIA              2009110401                                                                                                                                                                                 \n"
  val line1Long = "1MISSION   201308020800MISSIONAUSTRALIA              2009110401                                                                                                                                                                                                                      \n"
  val line9Short = "90100000001000000010000MISSION   MISSIONAUSTRALIA              \n"

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

  val testSchema = new YamlReader().loadYaml(new jio.InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/QBRequest.esl"), "UTF-8"), Array())
  
  val testStructure = testSchema.structures("QBRequest")

  val fixedSchemaText = """form: FIXEDWIDTH
values: 
- { name: 'field_0', type: Integer, format: { justify: zeroes }, length: 10 }
- { name: 'field_1', type: String, length: 10, usage: M }
- { name: 'field_2', type: String, length: 10 }
- { name: 'field_3', type: String, length: 10 }"""
  val fixedDataText = "0004567891QAZWSX    0987654   \u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"
  val fixedShort1Text = "1234567891QAZWSXEDCR0987654320"
  val fixedShort2Text = "1234567891QAZWSXEDCR"
  val fixedLong1Text = "1234567891QAZWSXEDCR09876543223abcdefghij   "
  val fixedLong2Text = "1234567891QAZWSXEDCR098765432233abcdefghij\u0000\u0000\u0000\u0000"
  val fixedMissingText = "0004567891\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u00000987654   \u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"
  
  val fixedSchema = new YamlReader().loadYaml(new jio.StringReader(fixedSchemaText), Array())
  val fixedSegment = fixedSchema.segments.values.head

  val altSchema1 = new YamlReader().loadYaml(new jio.InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/CompanyContacts.esl"), "UTF-8"), Array())
  
  val altStructure1 = altSchema1.structures("CompanyContacts")

  val altSchema2 = new YamlReader().loadYaml(new jio.InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/CompanyContactsCompact.esl"), "UTF-8"), Array())
  
  val altStructure2 = altSchema2.structures("CompanyContacts")

  val altSchema3 = new YamlReader().loadYaml(new jio.InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/luis-working.ffd"), "UTF-8"), Array())
  
  val altStructure3 = altSchema3.structures("Check")

  val copybookSchema1 = new YamlReader().loadYaml(new jio.InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/cb837-unordered.ffd"), "UTF-8"), Array())

  val copybookSchema2 = new YamlReader().loadYaml(new jio.InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/cb837-structure.ffd"), "UTF-8"), Array())
  
  val copybookStructure2 = copybookSchema2.structures("CB837")

  val copybookSchema3 = new YamlReader().loadYaml(new jio.InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/ECSRNAIC_v3.ffd"), "UTF-8"), Array())
  
  val copybookSegment3a = copybookSchema3.segments("SR-CONTAINER-WRAPPER-RQ")
  val copybookSegment3b = copybookSchema3.segments("SR-CONTAINER-WRAPPER-ALT")
  val copybookSegment3c = copybookSchema3.segments("SCB-CUST-BASE-ADD-RS")
  
  val baseConfig = FlatFileParserConfig(false, true, false, false, -1)
  val padConfig = FlatFileParserConfig(false, true, false, true, -1)
  val trimConfig = FlatFileParserConfig(false, true, true, false, -1)
  val writeBaseConfig = new FlatFileWriterConfig(true, ASCII_CHARSET, "\n", ' ')
  val write88591Config = new FlatFileWriterConfig(true, ISO88591_CHARSET, "\n", ' ')

  def parseDoc(doc: String) = {
    val ins = new jio.ByteArrayInputStream(doc.getBytes(ASCII_CHARSET))
    val parser = new FlatFileStructureParser(ins, EdiConstants.ISO88591_CHARSET, testSchema.structures.values.head,
      baseConfig)
    parser.parse.get
  }

  def streamBytes(is: jio.InputStream): Array[Byte] = {
    val buf = ListBuffer[Byte]()
    var b = is.read()
    while (b != -1) {
        buf.append(b.byteValue)
        b = is.read()
    }
    buf.toArray
  }

  behavior of "FlatFileSchemaParser"
  
  private def verifySimpleMessageData(input: ValueMap, hasFileId: Boolean) = {
    input get("Id") should be ("QBRequest")
    val data = input.get("Data").asInstanceOf[ValueMap]
    val seg1 = data.get("FCH").asInstanceOf[ValueMap]
    seg1.size should be (5)
    seg1 get("CommCode") should be ("MISSION")
    val date = seg1.get("CreatDate").asInstanceOf[LocalDate]
    date.getYear should be (2013)
    date.getMonthValue should be (8)
    date.getDayOfMonth should be (2)
    val time = seg1.get("CreatTime").asInstanceOf[LocalTime]
    time.getHour should be (8)
    time.getMinute should be (0)
    seg1 get("ClientName") should be ("MISSIONAUSTRALIA")
    seg1 get("FileId") should be ("2009110401")
    val seg2 = data.get("FCF").asInstanceOf[ValueMap]
    seg2.size should be (if (hasFileId) 6 else 5)
    seg2 get("FileBatchCount") should be (Integer.valueOf(1))
    seg2 get("FileTransCount") should be (Integer.valueOf(1))
    seg2 get("FileTransAmount") should be (Integer.valueOf(10000))
    seg2 get("CommCode") should be ("MISSION")
    seg2 get("ClientName") should be ("MISSIONAUSTRALIA")
    if (hasFileId) seg2 get("FileId") should be ("2009110401")
  }

  it should "parse a simple message" in {
    val in = new jio.ByteArrayInputStream((line1 + line9).getBytes)
    val parser = new FlatFileStructureParser(in, EdiConstants.ISO88591_CHARSET, testSchema.structures.values.head,
      baseConfig)
    val result = parser.parse
    result.isSuccess should be (true)
    verifySimpleMessageData(result.get, true)
  }
  
  it should "report an error on input too short" in {
    val in1 = new jio.ByteArrayInputStream(fixedShort1Text.getBytes)
    val parser1 = new FlatFileSegmentParser(in1, EdiConstants.ISO88591_CHARSET, fixedSegment, baseConfig)
    intercept[LexicalException] { parser1.parse.get }
    val in2 = new jio.ByteArrayInputStream(fixedShort2Text.getBytes)
    val parser2 = new FlatFileSegmentParser(in2, EdiConstants.ISO88591_CHARSET, fixedSegment, baseConfig)
    intercept[LexicalException] { parser2.parse.get }
  }
  
  it should "handle input too short when configured to pad" in {
    val in1 = new jio.ByteArrayInputStream(fixedShort1Text.getBytes)
    val parser1 = new FlatFileSegmentParser(in1, EdiConstants.ISO88591_CHARSET, fixedSegment, padConfig)
    parser1.parse.get
    val in2 = new jio.ByteArrayInputStream(fixedShort2Text.getBytes)
    val parser2 = new FlatFileSegmentParser(in2, EdiConstants.ISO88591_CHARSET, fixedSegment, padConfig)
    parser2.parse.get
  }
  
  it should "handle input too long when configured to trim" in {
    val in1 = new jio.ByteArrayInputStream(fixedLong1Text.getBytes)
    val parser1 = new FlatFileSegmentParser(in1, EdiConstants.ISO88591_CHARSET, fixedSegment, trimConfig)
    parser1.parse.get
    val in2 = new jio.ByteArrayInputStream(fixedLong2Text.getBytes)
    val parser2 = new FlatFileSegmentParser(in2, EdiConstants.ISO88591_CHARSET, fixedSegment, trimConfig)
    parser2.parse.get
  }

  it should "throw exception when missing required value if configured" in {
    val in = new jio.ByteArrayInputStream(fixedMissingText.getBytes)
    val config = FlatFileParserConfig(true, true, false, false, 0)
    val parser = new FlatFileSegmentParser(in, EdiConstants.ISO88591_CHARSET, fixedSegment, config)
    val thrown = intercept[LexicalException] { parser.parse.get }
    thrown.getMessage.contains("field_1") should be (true)
  }

  it should "parse message with long and short segments when configured" in {
    val in = new jio.ByteArrayInputStream((line1Long + line9Short).getBytes)
    val parser = new FlatFileStructureParser(in, EdiConstants.ISO88591_CHARSET, testSchema.structures.values.head,
      FlatFileParserConfig(false, true, true, true, -1))
    val result = parser.parse
    result.isSuccess should be (true)
    verifySimpleMessageData(result.get, false)
  }
  
  it should "read a copybook document with null characters" in {
    val in = getClass.getClassLoader.getResourceAsStream("edi/callCustBaseResp.out")
    val parser = new FlatFileSegmentParser(in, IBM037.charsetInstance, copybookSegment3c,
      FlatFileParserConfig(false, true, false, false, 0))
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
    val data = input.get("Data").asInstanceOf[MapList]
    data.size should be (1)
    val seg1 = data.iterator.next.asInstanceOf[ValueMap]
    val status = seg1.get("SCB-STATUS-RS").asInstanceOf[ValueMap]
    status.get("SCB-STATUSCODE-RS") should be (jl.Integer.valueOf(300))
    status.get("SCB-SEVERITY-RS").asInstanceOf[String].startsWith("Error") should be (true)
    seg1.get("SCB-CUSTIDREC-F-RS") should be (jl.Integer.valueOf(0))
    val idrec = seg1.get("SCB-CUSTIDREC-RS").asInstanceOf[ValueMap]
    idrec.size should be (0)
  }

  behavior of "FlatFileSchemaWriter"

  it should "roundtrip a complete document" in {
    val msg = readDoc("edi/QB-FFSampleRequest.txt")
    val in = new jio.ByteArrayInputStream(msg.getBytes)
    val parser = new FlatFileStructureParser(in, EdiConstants.ISO88591_CHARSET, testStructure, baseConfig)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
//    val ywriter = new StringWriter
//    YamlSupport.writeMap(input, ywriter)
//    println(ywriter.toString)
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileStructureWriter(out, testStructure, writeBaseConfig)
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
1Jane      Jones          CEO            janiejones@ardvark.co                                 0000000000000000000020140812"""

  it should "roundtrip document with flatfile schema using references" in {
    val in = new jio.ByteArrayInputStream(altMessage.getBytes)
    val parser = new FlatFileStructureParser(in, EdiConstants.ISO88591_CHARSET, altStructure1, baseConfig)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
//    val ywriter = new StringWriter
//    YamlSupport.writeMap(input, ywriter)
//    println(ywriter.toString)
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileStructureWriter(out, altStructure1, writeBaseConfig)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
//    val swriter = new StringWriter
//    YamlSupport.writeMap(input, swriter)
//    println(swriter.toString())
//    println("returned text:\n" + text + "\n")
    text should be (altMessage)
  }

  it should "roundtrip document with flatfile schema using inlining" in {
    val in = new jio.ByteArrayInputStream(altMessage.getBytes)
    val parser = new FlatFileStructureParser(in, EdiConstants.ISO88591_CHARSET, altStructure2, baseConfig)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
//    val ywriter = new StringWriter
//    YamlSupport.writeMap(input, ywriter)
//    println(ywriter.toString)
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileStructureWriter(out, altStructure2, writeBaseConfig)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
//    val swriter = new StringWriter
//    YamlSupport.writeMap(input, swriter)
//    println(swriter.toString())
//    println("returned text:\n" + text + "\n")
    text should be (altMessage)
  }
  
  it should "roundtrip documentation sample flatfile document" in {
    val doc = readDoc("edi/luis-data.txt")
    val in = new jio.ByteArrayInputStream(doc.getBytes)
    val parser = new FlatFileStructureParser(in, EdiConstants.ASCII_CHARSET, altStructure3, baseConfig)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileStructureWriter(out, altStructure3, writeBaseConfig)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
//    val swriter = new StringWriter
//    YamlSupport.writeMap(input, swriter)
    text should be (doc)
  }
  
  // TODO: reenable when reader null handling resolved
/*  it should "roundtrip single-segment flatfile document" in {
    val in = new jio.ByteArrayInputStream(fixedDataText.getBytes)
    val parser = new FlatFileSegmentParser(in, EdiConstants.ISO88591_CHARSET, fixedSegment)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileSegmentWriter(out, fixedSegment, new FlatFileWriterConfig(true, ASCII_CHARSET, true))
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
//    val swriter = new StringWriter
//    YamlSupport.writeMap(input, swriter)
    text should be (fixedDataText)
  }*/
  
  val fixedMultiText = """1234567891QAZWSXEDCR0987654321\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000
1234567891xzyWSXEDCR0987654321\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000
1234567891eeeWSXEDCR0987654321\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000
1234567891QadfadffCR0987654321\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"""
  
  // TODO: reenable when reader null handling resolved
/*  it should "roundtrip multiple-segment flatfile document" in {
    val in = new jio.ByteArrayInputStream(fixedMultiText.getBytes)
    val parser = new FlatFileSegmentParser(in, EdiConstants.ISO88591_CHARSET, fixedSegment)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileSegmentWriter(out, fixedSegment, new FlatFileWriterConfig(true, ASCII_CHARSET, true))
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
//    val swriter = new StringWriter
//    YamlSupport.writeMap(input, swriter)
    text should be (fixedMultiText)
  }*/
  
  it should "read unstructured copybook document with multi-part tags" in {
    val msg = readDoc("edi/cb837-unordered.txt")
    val in = new jio.ByteArrayInputStream(msg.getBytes)
    val parser = new FlatFileUnorderedParser(in, EdiConstants.ISO88591_CHARSET, copybookSchema1, baseConfig)
    val result = parser.parse
    result.isSuccess should be (true)
//    val input = result.get
//    val swriter = new StringWriter
//    YamlSupport.writeMap(input, swriter)
//    println(swriter.toString)
  }
  
  it should "read structured copybook document with multi-part tags" in {
    val msg = readDoc("edi/cb837-sample.txt")
    val in = new jio.ByteArrayInputStream(msg.getBytes)
    val parser = new FlatFileStructureParser(in, EdiConstants.ISO88591_CHARSET, copybookStructure2, baseConfig)
    val result = parser.parse
    result.isSuccess should be (true)
//    val input = result.get
//    val swriter = new StringWriter
//    YamlSupport.writeMap(input, swriter)
//    println(swriter.toString)
  }
  
  // TODO: reenable this when we have a valid data sample (the current one has spaces for fill)
/*  it should "roundtrip copybook document with BINARY data" in {
    val srcin = getClass.getClassLoader.getResourceAsStream("edi/ECSRNAIC_v3.txt")
    val bytes = streamBytes(srcin)
    val in = new jio.ByteArrayInputStream(bytes)
    val parser = new FlatFileSegmentParser(in, EdiConstants.ISO88591_CHARSET, copybookSegment3a)
    val result = parser.parse
    result.isSuccess should be (true)
    val input = result.get
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileSegmentWriter(out, copybookSegment3a,
      new FlatFileWriterConfig(true, EdiConstants.ISO88591_CHARSET, false))
    writer.write(input).get //isSuccess should be (true)
//    val text = new String(out.toByteArray, EdiConstants.ISO88591_CHARSET)
//    println(text)
//    val swriter = new jio.StringWriter
//    YamlSupport.writeMap(input, swriter)
//    println(swriter.toString)
    out.toByteArray should be (bytes)
  }
*/  
  it should "write copybook document with partial data" in {
    val input = new ValueMapImpl
    val seglist = new MapListImpl
    input.put(dataKey, seglist)
    val segdata = new ValueMapImpl
    seglist.add(segdata)
    val headdata = new ValueMapImpl
    segdata.put("SR-CONTAINERHEADER", headdata)
    val rqdata = new ValueMapImpl
    segdata.put("SR-NAICSINQ-RQ", rqdata)
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileSegmentWriter(out, copybookSegment3a, write88591Config)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray, EdiConstants.ISO88591_CHARSET)
    text.length should be (408)
  }
  
  it should "fail when missing required data" in {
    val input = new ValueMapImpl
    val seglist = new MapListImpl
    input.put(dataKey, seglist)
    val segdata = new ValueMapImpl
    seglist.add(segdata)
    val rqdata = new ValueMapImpl
    segdata.put("SR-NAICSINQ-RQ", rqdata)
    val userdata = new ValueMapImpl
    rqdata.put("SR-USERINFO-RQ", userdata)
    val out1 = new jio.ByteArrayOutputStream
    val writer1 = new FlatFileSegmentWriter(out1, copybookSegment3b, write88591Config)
    val thrown1 = intercept[WriteException] { writer1.write(input).get }
    thrown1.getMessage.contains("SR-CONTAINERHEADER") should be (true)
    val headdata = new ValueMapImpl
    segdata.put("SR-CONTAINERHEADER", headdata)
    val out2 = new jio.ByteArrayOutputStream
    val writer2 = new FlatFileSegmentWriter(out2, copybookSegment3b, write88591Config)
    val thrown2 = intercept[WriteException] { writer2.write(input).get }
    thrown2.getMessage.contains("SR-CHANNEL-CH") should be (true)
    headdata.put("SR-CHANNEL-CH", "ABC")
    val out3 = new jio.ByteArrayOutputStream
    val writer3 = new FlatFileSegmentWriter(out3, copybookSegment3b, write88591Config)
    writer3.write(input).get
    val text = new String(out3.toByteArray, EdiConstants.ISO88591_CHARSET)
    text.length should be (410)
//    println(s"text: '$text'")
  }
  
  behavior of "Missing value handling"
  
  def buildSegment3aData(channel: Boolean) = {
    val outmap = new ValueMapImpl
    val outsegs = new MapListImpl
    outmap.put(dataKey, outsegs)
    val outdata = new ValueMapImpl
    outsegs.add(outdata)
    val outhead = new ValueMapImpl
    outdata.put("SR-CONTAINERHEADER", outhead)
    if (channel) outhead.put("SR-CHANNEL-CH", "ABCD")
    outhead.put("SR-CSR-LOGIN-ID-CH", "XYZ")
    val outrq = new ValueMapImpl
    outdata.put("SR-NAICSINQ-RQ", outrq)
    outrq.put("SR-USERINFO-F-RQ", Integer.valueOf(5))
    outrq.put("SR-BUSINESSCATEGORYNAME-RQ", "KKKK")
    outrq.put("SR-BUSINESSCATEGORYNAME-F-RQ", Integer.valueOf(0))
    val outuser = new ValueMapImpl
    outrq.put("SR-USERINFO-RQ", outuser)
    outuser.put("SR-USERPHONENBR-F-RQ", Integer.valueOf(0))
    outmap
  }
  
  def verifySegment3aData(inmap: ValueMap) = {
    inmap.containsKey(dataKey) should be (true)
    val insegs = inmap.get(dataKey).asInstanceOf[MapList]
    insegs.size should be (1)
    val indata = insegs.iterator.next
    indata.size should be (2)
    indata.containsKey("SR-CONTAINERHEADER") should be (true)
    val inhead = getAsMap("SR-CONTAINERHEADER", indata)
    inhead.size should be (2)
    inhead.get("SR-CHANNEL-CH") should be ("ABCD")
    inhead.get("SR-CSR-LOGIN-ID-CH") should be ("XYZ")
    indata.containsKey("SR-NAICSINQ-RQ") should be (true)
    val inrq = getAsMap("SR-NAICSINQ-RQ", indata)
    inrq.size should be (4)
    inrq.get("SR-USERINFO-F-RQ") should be (Integer.valueOf(5))
    inrq.get("SR-BUSINESSCATEGORYNAME-RQ") should be ("KKKK")
    inrq.get("SR-BUSINESSCATEGORYNAME-F-RQ") should be (Integer.valueOf(0))
    inrq.containsKey("SR-USERINFO-RQ") should be (true)
    val inuser = getAsMap("SR-USERINFO-RQ", inrq)
    inuser.size should be (1)
    inuser.get("SR-USERPHONENBR-F-RQ") should be (Integer.valueOf(0))
  }
  
  it should "should write and read correctly using spaces" in {
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileSegmentWriter(out, copybookSegment3a, write88591Config)
    writer.write(buildSegment3aData(true)).get //isSuccess should be (true)
    val text = new String(out.toByteArray, EdiConstants.ISO88591_CHARSET)
    text.length should be (408)
    (334 until 366).foreach { text(_) should be (' ') }
    val in = new jio.ByteArrayInputStream(out.toByteArray)
    val config = FlatFileParserConfig(false, true, false, false, ' ')
    val parser = new FlatFileSegmentParser(in, EdiConstants.ISO88591_CHARSET, copybookSegment3a, config)
    val result = parser.parse
    result.isSuccess should be (true)
    verifySegment3aData(result.get)
  }
  
  it should "should write and read correctly using zeroes" in {
    val out = new jio.ByteArrayOutputStream
    val writer = new FlatFileSegmentWriter(out, copybookSegment3a,
      new FlatFileWriterConfig(true, EdiConstants.ISO88591_CHARSET, "\n", 0.toChar))
    writer.write(buildSegment3aData(true)).get //isSuccess should be (true)
    val text = new String(out.toByteArray, EdiConstants.ISO88591_CHARSET)
    text.length should be (408)
    (334 until 366).foreach { text(_) should be (0) }
    val in = new jio.ByteArrayInputStream(out.toByteArray)
    val config = FlatFileParserConfig(false, true, false, false, 0)
    val parser = new FlatFileSegmentParser(in, EdiConstants.ISO88591_CHARSET, copybookSegment3a, config)
    val result = parser.parse
    result.isSuccess should be (true)
    verifySegment3aData(result.get)
  }
}