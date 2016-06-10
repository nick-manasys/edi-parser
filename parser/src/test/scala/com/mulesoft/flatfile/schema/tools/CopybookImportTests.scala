package com.mulesoft.flatfile.schema.tools

import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.schema.EdiSchema._
import com.mulesoft.flatfile.schema.{ YamlReader, YamlWriter }
import com.mulesoft.flatfile.schema.fftypes._
import java.io.{ ByteArrayInputStream, StringReader, StringWriter }
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.collection.{ mutable => scm }

class CopybookImportTests extends FlatSpec with Matchers {

  import CopybookImport._

  def dummyInput = new ByteArrayInputStream("ABC.\n".getBytes("UTF-8"))

  "abbreviateName" should "concatenate initial letters of words in name" in {
    val cbi = new CopybookImport(dummyInput, "UTF-8")
    cbi.abbreviateName("ABCD") should be ("A")
    cbi.abbreviateName("ABCD-EF-G") should be ("AEG")
    an[IllegalArgumentException] should be thrownBy cbi.abbreviateName("")
    an[IllegalArgumentException] should be thrownBy cbi.abbreviateName("-")
    an[IllegalArgumentException] should be thrownBy cbi.abbreviateName("A-B--")
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
    val map = scm.Map[String, String]()
    cbi.convertPic("E0", "XXX", DisplayUsage, false, map) should be (Element("", "E0", StringFormat(3, FillMode.LEFT)))
    cbi.convertPic("E0", "XXXXX", DisplayUsage, false, map) should be (Element("", "E0", StringFormat(5, FillMode.LEFT)))
    cbi.convertPic("E0", "X(3)", DisplayUsage, false, map) should be (Element("", "E0", StringFormat(3, FillMode.LEFT)))
    cbi.convertPic("E0", "X(5)", DisplayUsage, false, map) should be (Element("", "E0", StringFormat(5, FillMode.LEFT)))
    cbi.convertPic("E0", "999", DisplayUsage, false, map) should be (Element("", "E0", IntegerFormat(3, NumberSign.UNSIGNED, FillMode.LEFT)))
    cbi.convertPic("E0", "99999", DisplayUsage, false, map) should be (Element("", "E0", IntegerFormat(5, NumberSign.UNSIGNED, FillMode.LEFT)))
    cbi.convertPic("E0", "9(3)", DisplayUsage, false, map) should be (Element("", "E0", IntegerFormat(3, NumberSign.UNSIGNED, FillMode.LEFT)))
    cbi.convertPic("E0", "9(5)", DisplayUsage, false, map) should be (Element("", "E0", IntegerFormat(5, NumberSign.UNSIGNED, FillMode.LEFT)))
    cbi.convertPic("E0", "999V9", DisplayUsage, false, map) should be (Element("", "E0", DecimalFormat(4, NumberSign.UNSIGNED, 1, FillMode.LEFT, false)))
    cbi.convertPic("E0", "99V9999", DisplayUsage, false, map) should be (Element("", "E0", DecimalFormat(6, NumberSign.UNSIGNED, 4, FillMode.LEFT, false)))
    cbi.convertPic("E0", "9(3)V9", DisplayUsage, false, map) should be (Element("", "E0", DecimalFormat(4, NumberSign.UNSIGNED, 1, FillMode.LEFT, false)))
    cbi.convertPic("E0", "9(2)V9(4)", DisplayUsage, false, map) should be (Element("", "E0", DecimalFormat(6, NumberSign.UNSIGNED, 4, FillMode.LEFT, false)))
    cbi.convertPic("E0", "99V9(4)", DisplayUsage, false, map) should be (Element("", "E0", DecimalFormat(6, NumberSign.UNSIGNED, 4, FillMode.LEFT, false)))
    cbi.convertPic("E0", "S99V9(4)", DisplayUsage, false, map) should be (Element("", "E0", DecimalFormat(6, NumberSign.ALWAYS_RIGHT, 4, FillMode.LEFT, true)))
  }
  
  it should "throw exception on error in pattern" in {
    val cbi = new CopybookImport(dummyInput, "UTF-8")
    val map = scm.Map[String, String]()
    intercept[IllegalArgumentException] { cbi.convertPic("E0", "S99V9(4)P", DisplayUsage, false, map) }
    intercept[IllegalArgumentException] { cbi.convertPic("E0", "S99VX(4)", DisplayUsage, false, map) }
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
           05  UNSIGNED-INTEGER        PIC 9(4).
           05  SIGNED-INTEGER          PIC S9(4).
           05  SIGNED-DECIMAL          PIC S9(4) USAGE IS COMP-3.
           05  SIGNED-IMPLICIT         PIC S9(4)V99."""
  
  val varDef = """
       01  VARIOUS-PICS.
      *    05  PIC-99V9P3P         PIC 99V9(3).
      *    05  PIC-S99V9P3P        PIC S99V9(3).
           05  N-07-UNSIGN         PIC 9(7)V9(2).
           05  N-07-SIGNED         PIC S9(7)V9(2)."""
  
  val reDef = """
           05  MULTIPART-ADDRESS REDEFINES ADDRESS.
               10  STREET              PIC X(18).
               10  CITY                PIC X(15).
               10  STATE               PIC XX.
               10  ZIP                 PIC 9(5)."""
  
  val trailDef = """
           05  PRESIDENT.
               10  LAST-NAME           PIC X(15).
               10  FIRST-NAME          PIC X(8)."""

  behavior of "buildSegment"

  it should "build a simple segment" in {
    val cbi = new CopybookImport(new ByteArrayInputStream(baseDef.getBytes("UTF-8")), "UTF-8")
    val optseg = cbi.buildSegment
    cbi.problems should be (Nil)
    optseg match {
      case Some(segment) =>
        segment.ident should be ("MAILING-RECORD")
        val comps = segment.components
        comps.length should be (2)
        val comp1 = comps(0)
        comp1 should be (ElementComponent(Element("", "COMPANY-NAME", StringFormat(30, FillMode.LEFT)), None, "COMPANY-NAME", -1, OptionalUsage, 1))
        val comp2 = comps(1)
        comp2 should be (ElementComponent(Element("", "ADDRESS", StringFormat(15, FillMode.LEFT)), None, "ADDRESS", -1, OptionalUsage, 1))
      case None => fail
    }
  }

  it should "build a segment with nested composites" in {
    val cbi = new CopybookImport(new ByteArrayInputStream((baseDef + nestedDef).getBytes("UTF-8")), "UTF-8")
    val optseg = cbi.buildSegment
    cbi.problems should be (Nil)
    optseg match {
      case Some(segment) =>
        segment.ident should be ("MAILING-RECORD")
        val comps = segment.components
        comps.length should be (3)
        val comp1 = comps(0)
        comp1.name should be ("COMPANY-NAME")
        comp1.key should be ("COMPANY-NAME")
      case None => fail
    }
    // TODO: redo these tests when new copybook import implemented
    //    val comp1 = comps(0)
    //    comp1 should be (ElementComponent(Element("", "COMPANY-NAME", DataType.ALPHANUMERIC, 30, 30), Some("COMPANY-NAME"), "MR01", 1, MandatoryUsage, 1, None))
    //    val comp2 = comps(1)
    //    comp2 should be (ElementComponent(Element("", "ADDRESS", DataType.ALPHANUMERIC, 15, 15), Some("ADDRESS"), "MR02", 2, MandatoryUsage, 1, None))
    //    comps(2) shouldBe a [CompositeComponent]
    //    val comp3 = comps(2).asInstanceOf[CompositeComponent]
    //    comp3.name should be ("CONTACTS")
    //    comp3.key should be ("MR03")
    //    val compcomps = comp3.composite.components
    //    compcomps.length should be (3)
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
    val optseg1 = cbi.buildSegment
    cbi.problems should be (Nil)
    optseg1 match {
      case Some(segment) =>
        segment.ident should be ("MAILING-RECORD")
        val comps = segment.components
        comps.length should be (3)
      case None => fail
    }
    val optseg2 = cbi.buildSegment
    cbi.problems should be (Nil)
    optseg2 match {
      case Some(segment) =>
        segment.ident should be ("OTHER-RECORD")
        val comps = segment.components
        comps.length should be (4)
      case None => fail
    }
  }

  it should "handle both signed and unsigned numeric PICs" in {
    val cbi = new CopybookImport(new ByteArrayInputStream(secondDef.getBytes("UTF-8")), "UTF-8")
    val optseg = cbi.buildSegment
    cbi.problems should be (Nil)
    optseg match {
      case Some(segment) =>
        val comps = segment.components
        comps.length should be (4)
        val comp1 = comps(0)
        comp1 should be (ElementComponent(Element("", "UNSIGNED-INTEGER", IntegerFormat(4, NumberSign.UNSIGNED, FillMode.LEFT, false)), None, "UNSIGNED-INTEGER", -1, OptionalUsage, 1))
        val comp2 = comps(1)
        comp2 should be (ElementComponent(Element("", "SIGNED-INTEGER", IntegerFormat(4, NumberSign.ALWAYS_RIGHT, FillMode.LEFT, true)), None, "SIGNED-INTEGER", -1, OptionalUsage, 1))
        val comp3 = comps(2)
        comp3 should be (ElementComponent(Element("", "SIGNED-DECIMAL", PackedDecimalFormat(4, 0, true)), None, "SIGNED-DECIMAL", -1, OptionalUsage, 1))
        val comp4 = comps(3)
        comp4 should be (ElementComponent(Element("", "SIGNED-IMPLICIT", DecimalFormat(6, NumberSign.ALWAYS_RIGHT, 2, FillMode.LEFT, true)), None, "SIGNED-IMPLICIT", -1, OptionalUsage, 1))
      case None => fail
    }
  }
  
  it should "build segment with REDEFINE" in {
    val cbi = new CopybookImport(new ByteArrayInputStream((secondDef + reDef + trailDef).getBytes("UTF-8")), "UTF-8")
    val optseg1 = cbi.buildSegment
    cbi.problems.size should be (1)
    val error = cbi.problems.head
    error.error should be (false)
    error.line should be (7)
    error.message should be ("Ignoring unsupported REDEFINE")
    optseg1 match {
      case Some(segment) =>
        segment.ident should be ("OTHER-RECORD")
        val comps = segment.components
        comps.length should be (5)
      case None => fail
    }    
  }

  it should "build a schema which can be written and read back in" in {
    val cbi = new CopybookImport(new ByteArrayInputStream((baseDef + nestedDef + secondDef).getBytes("UTF-8")), "UTF-8")
    val (optschema, problems) = cbi.buildSchema
    problems should be (Nil)
    optschema match {
      case Some(schema) =>
        val writer = new StringWriter
        YamlWriter.write(schema, Array(), writer)
        val text = writer.toString
        //        println(text)
        val schemax = new YamlReader().loadYaml(new StringReader(text), Array())
        schemax.composites should be (schema.composites)
        schemax.elements should be (schema.elements)
        val segments = schema.segments.values.toList.sortBy { _.name }
        val segmentsx = schemax.segments.values.toList.sortBy { _.name }
        schemax.segments.size should be (segments.size)
      // disable until position numbering fixed
      //      segments.zip(segmentsx).foreach { case (s1, s2) => s2 should be (s1) }
      case None => fail
    }
  }

  it should "convert a complex copybook to a schema which can be written and read back in" in {
    val cbi = new CopybookImport(getClass.getClassLoader.getResourceAsStream("esl/claim.cpb"), "UTF-8")
    val (optschema, problems) = cbi.buildSchema
    problems should be (Nil)
    optschema match {
      case Some(schema) =>
        val writer = new StringWriter
        YamlWriter.write(schema, Array(), writer)
        val text = writer.toString
        //        println(text)
        val schemax = new YamlReader().loadYaml(new StringReader(text), Array())
        schemax.composites should be (schema.composites)
        schemax.elements should be (schema.elements)
        val segments = schema.segments.values.toList.sortBy { _.name }
        val segmentsx = schemax.segments.values.toList.sortBy { _.name }
        schemax.segments.size should be (segments.size)
      // disable until position numbering fixed
      //      segments.zip(segmentsx).foreach { case (s1, s2) => s2 should be (s1) }
      case None => fail
    }
  }
}