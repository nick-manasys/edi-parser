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
import com.anypoint.df.edi.schema.tools.{ DefaultHL7NumberProvider, DefaultHL7NumberValidator }
import com.anypoint.df.edi.lexical.WriteException

class HL7SchemaParserWriterTests extends FlatSpec with Matchers with SchemaJavaDefs {

  import EdiSchema._
  import HL7Identity._
  import HL7SchemaDefs._
  import SchemaJavaValues._

  val DATETIME = "20140221003028"
  val MSH = s"MSH|^~\\&|APeX|UCSF|IE|UCSF|$DATETIME|HBBCKGRND|ADT^A08^ADT_A01|12869|T|2.5.1\r"
  val lead = MSH
  val EVN = s"EVN|A08|$DATETIME||REG_UPDATE|^HB^BACKGROUND^^^^^^UCSF\r"
  val PID = "PID|1||97529507^^^MRN^MRN||INTERFACE^IMORGON||19650101|M|INTERFACE^PSCRIBE~INTERFACE^POWERSCRIBE~INTERFACE^IMOR|U|123 MAIN ST APT #9^^SAN FRANCISCO^CA^94103^000^P^^SAN FRANCI|SAN FRANCI|(415)555-1212^P^PH^^^415^5551212~(408)555-1212^P^PH^^^408^5551212||ENG|M|||888-88-8888|||U||||||||N\r"

  val parserConfig = HL7ParserConfig(true, true, true, true, true, true, true, -1,
    Array[HL7IdentityInformation](), Array[HL7IdentityInformation]())

  /** Reads a copy of the test document into memory, standardizing line endings. */
  val testDoc = {
    val lines = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("edi/ADT_A01-partial.edi")).getLines
    val builder = new StringBuilder
    lines.foreach(line => {
      if (!builder.isEmpty) builder.append('\n')
      builder.append(line)
    })
    builder.toString
  }

  val testSchema = new YamlReader().loadYaml(new InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/ADT_A01-partial.esl"), "UTF-8"), Array())

  def parseDoc(doc: String) = {
    val ins = new ByteArrayInputStream(doc.getBytes(ASCII_CHARSET))
    val parser = HL7SchemaParser(ins, testSchema, new DefaultHL7NumberValidator, parserConfig)
    parser.parse.get
  }

  behavior of "HL7SchemaParser"

  it should "parse the MSH segment start when initialized" in {
    val in = new ByteArrayInputStream(lead.getBytes())
    val parser = HL7SchemaParser(in, EdiSchema(HL7, "2.5.1", Map.empty, Map.empty, Map.empty, Map.empty),
      new DefaultHL7NumberValidator, parserConfig)
    val props = new ValueMapImpl
    parser.init(props)
    props.get("MSH-03-01") should be ("APeX")
    props.get("MSH-04-01") should be ("UCSF")
    props.get("MSH-05-01") should be ("IE")
    props.get("MSH-06-01") should be ("UCSF")
    props.get("MSH-07-01") should be ("20140221003028")
    props.get("MSH-08") should be ("HBBCKGRND")
    props.get("MSH-09-01") should be ("ADT")
    props.get("MSH-09-02") should be ("A08")
    props.get("MSH-09-03") should be ("ADT_A01")
    props.get("MSH-10") should be ("12869")
    props.get("MSH-11-01") should be ("T")
    props.get("MSH-12-01") should be ("2.5.1")
  }

  it should "parse a complete message" in {
    val in = new ByteArrayInputStream((MSH + EVN + PID).getBytes())
    val parser = HL7SchemaParser(in, testSchema, new DefaultHL7NumberValidator, parserConfig)
    val result = parser.parse
    result.isSuccess should be (true)
    val map = result.get
    map.containsKey(mshKey) should be (true)
    val msh = map.get(mshKey).asInstanceOf[ValueMap]
    msh.get("MSH-03-01") should be ("APeX")
    msh.get("MSH-04-01") should be ("UCSF")
    msh.get("MSH-05-01") should be ("IE")
    msh.get("MSH-06-01") should be ("UCSF")
    msh.get("MSH-07-01") should be ("20140221003028")
    msh.get("MSH-08") should be ("HBBCKGRND")
    msh.get("MSH-09-01") should be ("ADT")
    msh.get("MSH-09-02") should be ("A08")
    msh.get("MSH-09-03") should be ("ADT_A01")
    msh.get("MSH-10") should be ("12869")
    msh.get("MSH-11-01") should be ("T")
    msh.get("MSH-12-01") should be ("2.5.1")
    map.get(transactionId) should be ("ADT_A01")
    map.containsKey(dataKey) should be (true)
    val data = map.get(dataKey).asInstanceOf[ValueMap]
    data.containsKey("ADT_A01") should be (true)
    val adt_ao1 = data.get("ADT_A01").asInstanceOf[ValueMap]
    adt_ao1.containsKey("03 EVN") should be (true)
    val evn = adt_ao1.get("03 EVN").asInstanceOf[ValueMap]
    evn.get("EVN-01") should be ("A08")
    evn.get("EVN-02-01") should be ("20140221003028")
    evn.get("EVN-03") should be (null)
    evn.get("EVN-04") should be ("REG_UPDATE")
    evn.get("EVN-05-01") should be (null)
    evn.get("EVN-05-02-01") should be ("HB")
    evn.get("EVN-05-03") should be ("BACKGROUND")
    evn.get("EVN-05-09-01") should be ("UCSF")
  }

  val writerConfig = HL7WriterConfig(-1, ASCII_CHARSET, "|^~\\&")

  /** Get provider configured for the sample document. */
  def docProvider = {
    val provider = new DefaultHL7NumberProvider
    provider.msgNum = 12868
    provider
  }

  /** Prepare a test input message to be output as the same mesaage. Swaps interchange sending and receiving information
    * in the root interchange map.
    */
  def prepareToSend(input: ValueMap) = {
    val mshmap = getRequiredValueMap(mshKey, input)
    def swapComps(c1: Composite, c2: Composite) = {
      val pairs = c1.components.zip(c2.components)
      pairs.foreach(p => swap(p._1.key, p._2.key, input))
    }
    swapComps(mshSendingFacility, mshReceivingFacility)
    swapComps(mshSendingApplication, mshReceivingApplication)
  }

  behavior of "HL7SchemaWriter"

  it should "roundtrip a parsed document" in {
    val msg = MSH + EVN + PID
    val in = new ByteArrayInputStream(msg.getBytes())
    val parser = HL7SchemaParser(in, testSchema, new DefaultHL7NumberValidator, parserConfig)
    val result = parser.parse
    val input = result.get
    val out = new ByteArrayOutputStream
    val writer = HL7SchemaWriter(out, testSchema, docProvider, writerConfig)
    writer.write(input).get //isSuccess should be (true)
    val text = new String(out.toByteArray)
    println("original text:\n" + msg + "\n")
    println("returned text:\n" + text + "\n")
    text should be (msg)
  }
}