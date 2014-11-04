package com.anypoint.df.edi.schema

import java.io.InputStreamReader
import java.io.StringReader
import java.io.StringWriter

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.io.Source

import java.io.ByteArrayInputStream

import java.util.GregorianCalendar
import java.math.BigInteger

class SchemaParserTests extends FlatSpec with Matchers with SchemaParserDefs {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.X12Constants._

  val ISA = "ISA*00*ABC       *00*DEF       *01*013227180      *ZZ*IJDIECAFOX     *090604*1205*U*00401*000001244*0*P*>~"
  val GS = "GS*PO*006927180*IAIYUCAFOO*20080604*1205*168*X*004010~"
  val ST = "ST*850*000000176~"
  val IEA = "IEA*1*000000244~"

  def buildGE(count: Int) = s"GE*$count*168~"

  def buildSE(count: Int) = s"SE*$count*000000176~"

  behavior of "X12SchemaParser"

  it should "parse the ISA segment when initialized" in {
    val in = new ByteArrayInputStream(ISA.getBytes())
    val create = SchemaParser.create(in, EdiSchema(X12, Map.empty, Map.empty, Map.empty, Map.empty))
    assert(create.isSuccess)
    val props = create.get.init
    props.get(AUTHORIZATION_QUALIFIER) should be("00")
    props.get(AUTHORIZATION_INFO) should be("ABC       ")
    props.get(SECURITY_QUALIFIER) should be("00")
    props.get(SECURITY_INFO) should be("DEF       ")
    props.get(SENDER_ID_QUALIFIER) should be("01")
    props.get(SENDER_ID) should be("013227180      ")
    props.get(RECEIVER_ID_QUALIFIER) should be("ZZ")
    props.get(RECEIVER_ID) should be("IJDIECAFOX     ")
    props.get(INTERCHANGE_DATE) should be("090604")
    props.get(INTERCHANGE_TIME) should be("1205")
    props.get(VERSION_ID) should be("00401")
    props.get(INTER_CONTROL) should be("000001244")
    props.get(ACK_REQUESTED) should be("0")
    props.get(TEST_INDICATOR) should be("P")
  }

  it should "parse the envelope segments as requested" in {
    val in = new ByteArrayInputStream((ISA + GS + ST + buildSE(0) + buildGE(0) + IEA).getBytes())
    val create = SchemaParser.create(in, EdiSchema(X12, Map.empty, Map.empty, Map.empty, Map.empty))
    assert(create.isSuccess)
    val parser = create.get
    val props = parser.init
    val gprops = parser.openGroup
    gprops.get(FUNCTIONAL_IDENTIFIER_CODE) should be("PO")
    gprops.get(APPLICATION_SENDERS_CODE) should be("006927180")
    gprops.get(APPLICATION_RECEIVERS_CODE) should be("IAIYUCAFOO")
    gprops.get(GROUP_DATE) should be(new GregorianCalendar(2008, 6, 4).getTime())
    gprops.get(GROUP_TIME) should be((12 * 60 + 5) * 60000)
    gprops.get(GROUP_CONTROL_NUMBER) should be(new BigInteger("168"))
    gprops.get(RESPONSIBLE_AGENCY_CODE) should be("X")
    gprops.get(VERSION_IDENTIFIER_CODE) should be("004010")
    val (transid, sprops) = parser.openSet
    transid should be("850")
    sprops.get(TRANSACTION_SET_IDENTIFIER_CODE) should be("850")
    sprops.get(TRANSACTION_SET_CONTROL_NUMBER) should be("000000176")
    sprops.containsKey(IMPLEMENTATION_CONVENTION_REFERENCE) should be(false)
    parser.isSetClose should be(true)
    parser.closeSet(sprops)
    parser.isGroupClose should be(true)
    parser.closeGroup(gprops)
  }

  it should "throw an exception when positioned at wrong segment" in {
    val in = new ByteArrayInputStream((ISA + GS + ST + buildSE(0) + buildGE(0) + IEA).getBytes())
    val create = SchemaParser.create(in, EdiSchema(X12, Map.empty, Map.empty, Map.empty, Map.empty))
    assert(create.isSuccess)
    val parser = create.get
    val props = parser.init
    intercept[IllegalStateException] { parser.openSet }
    intercept[IllegalStateException] { parser.closeSet(new ValueMapImpl()) }
    intercept[IllegalStateException] { parser.closeGroup(new ValueMapImpl()) }
    val gprops = parser.openGroup
    intercept[IllegalStateException] { parser.openGroup }
    intercept[IllegalStateException] { parser.closeSet(new ValueMapImpl()) }
    intercept[IllegalStateException] { parser.closeGroup(new ValueMapImpl()) }
  }

  it should "parse a complete interchange message" in {
    val yamlIn = getClass.getClassLoader.getResourceAsStream("yaml/cdw850schema.yaml")
    val schema = YamlReader.loadYaml(new InputStreamReader(yamlIn, "UTF-8"))
    val messageIn = getClass.getClassLoader.getResourceAsStream("edi/cdw850sample.edi")
    val create = SchemaParser.create(messageIn, schema)
    assert(create.isSuccess)
    val parser = create.get
    val props = parser.init
    val gprops = parser.openGroup
    val trans = parser.parseTransaction(schema.transactions("850"))
    parser.isGroupClose should be(true)
    parser.closeGroup(gprops)
  }
}