package com.anypoint.df.edi.schema.tools

import java.io.ByteArrayInputStream
import scala.collection.{ mutable => scm }
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.lexical.EdiConstants.DataType

class CopybookImportTests extends FlatSpec with Matchers {

  import CopybookImport._
  
  def dummyInput = new ByteArrayInputStream("ABC.\n".getBytes("UTF-8"))

  "abbreviateName" should "concatenate initial letters of words in name" in {
    val cbi = new CopybookImport(dummyInput, "UTF-8")
    cbi.abbreviateName("ABCD") should be ("A")
    cbi.abbreviateName("ABCD-EF-G") should be ("AEG")
    an [IllegalStateException] should be thrownBy cbi.abbreviateName("")
    an [IllegalStateException] should be thrownBy cbi.abbreviateName("-")
    an [IllegalStateException] should be thrownBy cbi.abbreviateName("A-B--")
  }
  
  "generateIdent" should "return unique identifiers" in {
    val cbi = new CopybookImport(dummyInput, "UTF-8")
    val map = new scm.HashMap[String, Int]
    cbi.generateIdent("AA", map) should be ("AA0")
    cbi.generateIdent("AA", map) should be ("AA1")
    cbi.generateIdent("AA", map) should be ("AA2")
    cbi.generateIdent("BA", map) should be ("BA0")
    cbi.generateIdent("BA", map) should be ("BA1")
    cbi.generateIdent("BA", map) should be ("BA2")
  }

  behavior of "convertPic"
  
  it should "handle simple patterns" in {
    val cbi = new CopybookImport(dummyInput, "UTF-8")
    cbi.convertPic("XXX") should be (Element("E01", "", DataType.ALPHANUMERIC, 0, 3))
    cbi.convertPic("XXXXX") should be (Element("E02", "", DataType.ALPHANUMERIC, 0, 5))
    cbi.convertPic("X(3)") should be (Element("E01", "", DataType.ALPHANUMERIC, 0, 3))
    cbi.convertPic("X(5)") should be (Element("E02", "", DataType.ALPHANUMERIC, 0, 5))
    cbi.convertPic("999") should be (Element("E03", "", DataType.NUMERIC, 0, 3))
    cbi.convertPic("99999") should be (Element("E04", "", DataType.NUMERIC, 0, 5))
    cbi.convertPic("9(3)") should be (Element("E03", "", DataType.NUMERIC, 0, 3))
    cbi.convertPic("9(5)") should be (Element("E04", "", DataType.NUMERIC, 0, 5))
    cbi.convertPic("999V9") should be (Element("E05", "", DataType.DECIMAL1, 0, 4))
    cbi.convertPic("99V9999") should be (Element("E06", "", DataType.DECIMAL4, 0, 6))
    cbi.convertPic("9(3)V9") should be (Element("E05", "", DataType.DECIMAL1, 0, 4))
    cbi.convertPic("9(2)V9(4)") should be (Element("E06", "", DataType.DECIMAL4, 0, 6))
  }
  
  val baseDef = """       01  MAILING-RECORD.
           05  COMPANY-NAME            PIC X(30).
           05  ADDRESS                 PIC X(15)."""
  val nestedDef = """
           05  CONTACTS.
               10  PRESIDENT.
                   15  LAST-NAME       PIC X(15).
                   15  FIRST-NAME      PIC X(8).
               10  VP-MARKETING.
                   15  LAST-NAME       PIC X(15).
                   15  FIRST-NAME      PIC X(8).
               10  ALTERNATE-CONTACT.
                   15  TITLE           PIC X(10).
                   15  LAST-NAME       PIC X(15).
                   15  FIRST-NAME      PIC X(8)."""
  
  val secondDef = """
       01  OTHER-RECORD.
           05  COMPANY-NAME            PIC X(30).
           05  ADDRESS                 PIC X(15)."""
  val trailDef = """
           05  ADDRESS                 PIC X(15).
           05  CITY                    PIC X(15).
           05  STATE                   PIC XX.
           05  ZIP                     PIC 9(5)."""
  
  behavior of "buildSegment"
  
  it should "build a simple segment" in {
    val cbi = new CopybookImport(new ByteArrayInputStream(baseDef.getBytes("UTF-8")), "UTF-8")
    val segment = cbi.buildSegment
    segment.name should be ("MAILING-RECORD")
    segment.ident should be ("MR0")
    val comps = segment.components
    comps.length should be (2)
    val comp1 = comps(0)
    comp1 should be (ElementComponent(Element("E01", "", DataType.ALPHANUMERIC, 0, 30), Some("COMPANY-NAME"), "MR01", 1, MandatoryUsage, 1, None))
    val comp2 = comps(1)
    comp2 should be (ElementComponent(Element("E02", "", DataType.ALPHANUMERIC, 0, 15), Some("ADDRESS"), "MR02", 2, MandatoryUsage, 1, None))
  }
  
  it should "build a segment with nested composites" in {
    val cbi = new CopybookImport(new ByteArrayInputStream((baseDef + nestedDef).getBytes("UTF-8")), "UTF-8")
    val segment = cbi.buildSegment
    segment.name should be ("MAILING-RECORD")
    segment.ident should be ("MR0")
    val comps = segment.components
    comps.length should be (3)
    val comp1 = comps(0)
    comp1 should be (ElementComponent(Element("E01", "", DataType.ALPHANUMERIC, 0, 30), Some("COMPANY-NAME"), "MR01", 1, MandatoryUsage, 1, None))
    val comp2 = comps(1)
    comp2 should be (ElementComponent(Element("E02", "", DataType.ALPHANUMERIC, 0, 15), Some("ADDRESS"), "MR02", 2, MandatoryUsage, 1, None))
    comps(2) shouldBe a [CompositeComponent]
    val comp3 = comps(2).asInstanceOf[CompositeComponent]
    comp3.name should be ("CONTACTS")
    comp3.key should be ("MR03")
    val compcomps = comp3.composite.components
    compcomps.length should be (3)
    compcomps(0) should be (CompositeComponent(Composite("P0", "PRESIDENT",
      List(ElementComponent(Element("E02", "", DataType.ALPHANUMERIC, 0, 15), Some("LAST-NAME"), "MR05", 5, MandatoryUsage, 1, None),
        ElementComponent(Element("E03", "", DataType.ALPHANUMERIC, 0, 8), Some("FIRST-NAME"), "MR06", 6, MandatoryUsage, 1, None)), Nil, 0),
      Some("PRESIDENT"), "MR04", 4, MandatoryUsage, 1))
    compcomps(1) should be (CompositeComponent(Composite("VM0", "VP-MARKETING",
      List(ElementComponent(Element("E02", "", DataType.ALPHANUMERIC, 0, 15), Some("LAST-NAME"), "MR06", 6, MandatoryUsage, 1, None),
        ElementComponent(Element("E03", "", DataType.ALPHANUMERIC, 0, 8), Some("FIRST-NAME"), "MR07", 7, MandatoryUsage, 1, None)), Nil, 0),
      Some("VP-MARKETING"), "MR05", 5, MandatoryUsage, 1))
  }
  
  it should "build multiple segments" in {
    val cbi = new CopybookImport(new ByteArrayInputStream((baseDef + nestedDef + secondDef).getBytes("UTF-8")), "UTF-8")
    val segment1 = cbi.buildSegment
    segment1.name should be ("MAILING-RECORD")
    segment1.ident should be ("MR0")
    val comps1 = segment1.components
    comps1.length should be (3)
    val segment2 = cbi.buildSegment
    segment2.name should be ("OTHER-RECORD")
    segment2.ident should be ("OR0")
    val comps2 = segment2.components
    comps2.length should be (2)
  }
}