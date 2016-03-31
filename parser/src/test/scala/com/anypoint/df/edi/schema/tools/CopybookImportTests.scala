package com.anypoint.df.edi.schema.tools

import java.io.ByteArrayInputStream
import scala.collection.{ mutable => scm }
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.anypoint.df.edi.schema.EdiSchema._
import java.io.StringWriter
import com.anypoint.df.edi.schema.YamlWriter
import java.io.StringReader
import com.anypoint.df.edi.schema.YamlReader

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
  
  // TODO: redo these tests when new copybook import implemented
//  it should "handle simple patterns" in {
//    val cbi = new CopybookImport(dummyInput, "UTF-8")
//    cbi.convertPic("E0", "XXX") should be (Element("", "E0", DataType.ALPHANUMERIC, 3, 3))
//    cbi.convertPic("E0", "XXXXX") should be (Element("", "E0", DataType.ALPHANUMERIC, 5, 5))
//    cbi.convertPic("E0", "X(3)") should be (Element("", "E0", DataType.ALPHANUMERIC, 3, 3))
//    cbi.convertPic("E0", "X(5)") should be (Element("", "E0", DataType.ALPHANUMERIC, 5, 5))
//    cbi.convertPic("E0", "999") should be (Element("", "E0", DataType.NUMERIC, 3, 3))
//    cbi.convertPic("E0", "99999") should be (Element("", "E0", DataType.NUMERIC, 5, 5))
//    cbi.convertPic("E0", "9(3)") should be (Element("", "E0", DataType.NUMERIC, 3, 3))
//    cbi.convertPic("E0", "9(5)") should be (Element("", "E0", DataType.NUMERIC, 5, 5))
//    cbi.convertPic("E0", "999V9") should be (Element("", "E0", DataType.DECIMAL1, 4, 4))
//    cbi.convertPic("E0", "99V9999") should be (Element("", "E0", DataType.DECIMAL4, 6, 6))
//    cbi.convertPic("E0", "9(3)V9") should be (Element("", "E0", DataType.DECIMAL1, 4, 4))
//    cbi.convertPic("E0", "9(2)V9(4)") should be (Element("", "E0", DataType.DECIMAL4, 6, 6))
//  }
  
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
  // TODO: redo these tests when new copybook import implemented
//    val comp1 = comps(0)
//    comp1 should be (ElementComponent(Element("", "COMPANY-NAME", DataType.ALPHANUMERIC, 30, 30), Some("COMPANY-NAME"), "MR01", 1, MandatoryUsage, 1, None))
//    val comp2 = comps(1)
//    comp2 should be (ElementComponent(Element("", "ADDRESS", DataType.ALPHANUMERIC, 15, 15), Some("ADDRESS"), "MR02", 2, MandatoryUsage, 1, None))
  }
  
  it should "build a segment with nested composites" in {
    val cbi = new CopybookImport(new ByteArrayInputStream((baseDef + nestedDef).getBytes("UTF-8")), "UTF-8")
    val segment = cbi.buildSegment
    segment.name should be ("MAILING-RECORD")
    segment.ident should be ("MR0")
    val comps = segment.components
    comps.length should be (3)
  // TODO: redo these tests when new copybook import implemented
//    val comp1 = comps(0)
//    comp1 should be (ElementComponent(Element("", "COMPANY-NAME", DataType.ALPHANUMERIC, 30, 30), Some("COMPANY-NAME"), "MR01", 1, MandatoryUsage, 1, None))
//    val comp2 = comps(1)
//    comp2 should be (ElementComponent(Element("", "ADDRESS", DataType.ALPHANUMERIC, 15, 15), Some("ADDRESS"), "MR02", 2, MandatoryUsage, 1, None))
    comps(2) shouldBe a [CompositeComponent]
    val comp3 = comps(2).asInstanceOf[CompositeComponent]
    comp3.name should be ("CONTACTS")
    comp3.key should be ("MR03")
    val compcomps = comp3.composite.components
    compcomps.length should be (3)
  // TODO: redo these tests when new copybook import implemented
//    compcomps(0) should be (CompositeComponent(Composite("", "PRESIDENT",
//      List(ElementComponent(Element("", "LAST-NAME", DataType.ALPHANUMERIC, 15, 15), Some("LAST-NAME"), "MR05", 5, MandatoryUsage, 1, None),
//        ElementComponent(Element("", "FIRST-NAME", DataType.ALPHANUMERIC, 8, 8), Some("FIRST-NAME"), "MR06", 6, MandatoryUsage, 1, None)), Nil, 0),
//      Some("PRESIDENT"), "MR04", 4, MandatoryUsage, 1))
//    compcomps(1) should be (CompositeComponent(Composite("", "VP-MARKETING",
//      List(ElementComponent(Element("", "LAST-NAME", DataType.ALPHANUMERIC, 15, 15), Some("LAST-NAME"), "MR06", 6, MandatoryUsage, 1, None),
//        ElementComponent(Element("", "FIRST-NAME", DataType.ALPHANUMERIC, 8, 8), Some("FIRST-NAME"), "MR07", 7, MandatoryUsage, 1, None)), Nil, 0),
//      Some("VP-MARKETING"), "MR05", 5, MandatoryUsage, 1))
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
  
//  it should "build a schema which can be written and read back in" in {
//    val cbi = new CopybookImport(new ByteArrayInputStream((baseDef + nestedDef + secondDef).getBytes("UTF-8")), "UTF-8")
//    val schema = cbi.buildSchema
//    val writer = new StringWriter
//    YamlWriter.write(schema, Array(), writer)
//    val text = writer.toString
//    println(text)
//    val schemax = new YamlReader().loadYaml(new StringReader(text), Array())
//    schemax.composites should be (schema.composites)
//    schemax.elements should be (schema.elements)
//    val segments = schema.segments.values.toList.sortBy { _.name }
//    val segmentsx = schemax.segments.values.toList.sortBy { _.name }
//    schemax.segments.size should be (segments.size)
    // disable until position numbering fixed
//    segments.zip(segmentsx).foreach { case (s1, s2) => s2 should be (s1) }
//  }
}