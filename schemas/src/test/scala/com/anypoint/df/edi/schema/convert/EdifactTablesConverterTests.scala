package com.anypoint.df.edi.schema.convert

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import java.io.ByteArrayInputStream
import scala.io.Source
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.lexical.EdiConstants.DataType

/** Tests for EDIFACT specification converter.
  */
class EdifactTablesConverterTests extends FlatSpec with Matchers {

  import EdifactTablesConverter._

  def stringStream(text: String) = new ByteArrayInputStream(text.getBytes("ISO-8859-1"))
  def stringLines(text: String) = LineIterator(stringStream(text), "ISO-8859-1")

  "splitLead" should "split string at first space into two trimmed pieces" in {
    splitLead("abc def") should be(("abc", "def"))
    splitLead("abc def   ") should be(("abc", "def"))
    splitLead("abc d e f") should be(("abc", "d e f"))
    splitLead("abcdef") should be(("abcdef", ""))
  }

  val input1 = s"""
----------------------------------------------------------------------

     1000  Document name                                           [B]

     Desc: Name of a document.

     Repr: an..35

----------------------------------------------------------------------

     1001  Document name code                                      [C]

     Desc: Code specifying the document name.

     Repr: n..3

"""

  val input2 = s"""
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

   1000  Document/message name                                     [B]

   Desc: Plain language identifier specifying the function of a
         document/message.

   Repr: an..35

ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

   1001  Document/message name, coded                              [C]

   Desc: Document/message identifier expressed in code.

   Repr: n..3

"""

  "skipBreakLine" should "skip lines until break line is skipped, or end of input" in {
    val lines1 = stringLines(input1)
    skipBreakLine(lines1) should be(true)
    lines1.next should be("")
    lines1.next should startWith("     1000")
    skipBreakLine(lines1) should be(true)
    lines1.next should be("")
    lines1.next should startWith("     1001")
    skipBreakLine(lines1) should be(false)
    val lines2 = stringLines(input2)
    skipBreakLine(lines2) should be(true)
    lines2.next should be("")
    lines2.next should startWith("   1000")
    skipBreakLine(lines2) should be(true)
    lines2.next should be("")
    lines2.next should startWith("   1001")
    skipBreakLine(lines2) should be(false)
  }

  "skipBlankLine" should "skip a single blank line" in {
    val lines = stringLines("\nabc\n   \n def\nghi")
    skipBlankLine(lines)
    lines.next should be("abc")
    skipBlankLine(lines)
    lines.next should be(" def")
    intercept[IllegalStateException] { skipBlankLine(lines) }
  }

  "readElements" should "read element definitions" in {
    val elems1 = readElements(stringStream(input1))
    elems1.size should be(2)
    elems1.head should be(Element("1000", "Document name", DataType.ALPHANUMERIC, 0, 35))
    elems1.tail.head should be(Element("1001", "Document name code", DataType.NUMBER, 0, 3))
    val elems2 = readElements(stringStream(input2))
    elems2.size should be(2)
    elems2.head should be(Element("1000", "Document/message name", DataType.ALPHANUMERIC, 0, 35))
    elems2.tail.head should be(Element("1001", "Document/message name, coded", DataType.NUMBER, 0, 3))
  }

  "trimTrailing" should "trim only trailing spaces from line" in {
    val lines = stringLines("\nabc\n   \n def   \n  ghi   ")
    trimTrailing(lines.next) should be("")
    trimTrailing(lines.next) should be("abc")
    trimTrailing(lines.next) should be("")
    trimTrailing(lines.next) should be(" def")
    trimTrailing(lines.next) should be("  ghi")
  }

  "safeSubstring" should "extract substring ranges which extend beyond end of string" in {
    safeSubstring("abc", 0, 3) should be("abc")
    safeSubstring("abc", 1, 2) should be("b")
    safeSubstring("abc", 1, 4) should be("bc")
  }

  "buildTemplate" should "find asterisk positions in a line" in {
    buildTemplate("**") should be(List(0, 1))
    buildTemplate("  *  *") should be(List(2, 5))
    buildTemplate("  *  *      *  *") should be(List(2, 5, 12, 15))
  }

  val ctemplate1a = "*      *    *                                          *"
  val ctemplate1b = "*  *   *    *                                          *    *"
  val composite1a = """       C817 ADDRESS USAGE

       Desc: To describe the usage of an address.

010    3299  Address purpose code                      C      an..3
020    3131  Address type code                         C      an..3
030    3475  Address status code                       C      an..3"""
  val ctemplate2a = "*     *     *                                              *"
  val ctemplate2b = "*  *  *     *                                              *  *"
  val composite2 = """      C042  NATIONALITY DETAILS

      Desc: To specify a nationality.

010   3293  Nationality, coded                             C  an..3
020   1131  Code list qualifier                            C  an..3
030   3055  Code list responsible agency, coded            C  an..3
040   3292  Nationality                                    C  a..35"""
  val stemplate1a = "*      *    *                                          *"
  val stemplate1b = "*  *   *    *                                          *    *"
  val segment1a = """       ALC  ALLOWANCE OR CHARGE

       Function: To identify allowance or charge details.

010    5463 ALLOWANCE OR CHARGE CODE QUALIFIER         M    1 an..3

020    C552 ALLOWANCE/CHARGE INFORMATION               C    1
       1230  Allowance or charge identifier            C      an..35
       5189  Allowance or charge identification code   C      an..3

030    4471 SETTLEMENT MEANS CODE                      C    1 an..3

040    1227 CALCULATION SEQUENCE CODE                  C    1 an..3"""
  val stemplate2a = "*     *     *                                              *"
  val stemplate2b = "*  *  *     *                                              *  *"
  val segment2 = """      ALC   ALLOWANCE OR CHARGE

      Function: To identify allowance or charge details.

010   5463  ALLOWANCE OR CHARGE QUALIFIER                  M  an..3

020   C552  ALLOWANCE/CHARGE INFORMATION                   C  
      1230   Allowance or charge number                    C  an..35
      5189   Charge/allowance description, coded           C  an..3

030   4471  SETTLEMENT, CODED                              C  an..3"""

  behavior of "parseTemplate"

  it should "return empty field list at empty line or end of input" in {
    val templ = buildTemplate("*  *")
    parseTemplate(templ, false, stringLines("\n\n")) should be(Array())
    parseTemplate(templ, false, stringLines("")) should be(Array())
  }

  it should "extract simple input fields based on template" in {
    val ctempl1a = buildTemplate(ctemplate1a)
    val ctempl1b = buildTemplate(ctemplate1b)
    val clines1 = stringLines(composite1a)
    parseTemplate(ctempl1a, false, clines1) should be(Array("", "C817", "ADDRESS USAGE"))
    skipBlankLine(clines1)
    skipPastBlankLine(clines1)
    parseTemplate(ctempl1b, true, clines1) should be(Array("010", "", "3299", "Address purpose code", "C"))
    parseTemplate(ctempl1b, true, clines1) should be(Array("020", "", "3131", "Address type code", "C"))
    parseTemplate(ctempl1b, true, clines1) should be(Array("030", "", "3475", "Address status code", "C"))
    val ctempl2a = buildTemplate(ctemplate2a)
    val ctempl2b = buildTemplate(ctemplate2b)
    val clines2 = stringLines(composite2)
    parseTemplate(ctempl2a, false, clines2) should be(Array("", "C042", "NATIONALITY DETAILS"))
    skipBlankLine(clines2)
    skipPastBlankLine(clines2)
    parseTemplate(ctempl2b, false, clines2) should be(Array("010", "", "3293", "Nationality, coded", "C"))
    parseTemplate(ctempl2b, false, clines2) should be(Array("020", "", "1131", "Code list qualifier", "C"))
    parseTemplate(ctempl2b, false, clines2) should be(Array("030", "", "3055", "Code list responsible agency, coded", "C"))
    parseTemplate(ctempl2b, false, clines2) should be(Array("040", "", "3292", "Nationality", "C"))
    val stempl1a = buildTemplate(stemplate1a)
    val stempl1b = buildTemplate(stemplate1b)
    val slines1 = stringLines(segment1a)
    parseTemplate(stempl1a, false, slines1) should be(Array("", "ALC", "ALLOWANCE OR CHARGE"))
    skipBlankLine(slines1)
    skipPastBlankLine(slines1)
    parseTemplate(stempl1b, true, slines1) should be(Array("010", "", "5463", "ALLOWANCE OR CHARGE CODE QUALIFIER", "M"))
    skipBlankLine(slines1)
    parseTemplate(stempl1b, true, slines1) should be(Array("020", "", "C552", "ALLOWANCE/CHARGE INFORMATION", "C"))
    parseTemplate(stempl1b, false, slines1) should be(Array("", "", "1230", "Allowance or charge identifier", "C"))
    parseTemplate(stempl1b, false, slines1) should be(Array("", "", "5189", "Allowance or charge identification code", "C"))
    skipBlankLine(slines1)
    parseTemplate(stempl1b, true, slines1) should be(Array("030", "", "4471", "SETTLEMENT MEANS CODE", "C"))
    skipBlankLine(slines1)
    parseTemplate(stempl1b, true, slines1) should be(Array("040", "", "1227", "CALCULATION SEQUENCE CODE", "C"))
    val stempl2a = buildTemplate(stemplate2a)
    val stempl2b = buildTemplate(stemplate2b)
    val slines2 = stringLines(segment2)
    parseTemplate(stempl2a, false, slines2) should be(Array("", "ALC", "ALLOWANCE OR CHARGE"))
    skipBlankLine(slines2)
    skipPastBlankLine(slines2)
    parseTemplate(stempl2b, true, slines2) should be(Array("010", "", "5463", "ALLOWANCE OR CHARGE QUALIFIER", "M"))
    skipBlankLine(slines2)
    parseTemplate(stempl2b, true, slines2) should be(Array("020", "", "C552", "ALLOWANCE/CHARGE INFORMATION", "C"))
    parseTemplate(stempl2b, false, slines2) should be(Array("", "", "1230", "Allowance or charge number", "C"))
    parseTemplate(stempl2b, false, slines2) should be(Array("", "", "5189", "Charge/allowance description, coded", "C"))
    skipBlankLine(slines2)
    parseTemplate(stempl2b, true, slines2) should be(Array("030", "", "4471", "SETTLEMENT, CODED", "C"))
  }

  val composite1b = """       C852 RISK OBJECT SUB-TYPE

       Desc: To provide identification details for a risk object
             sub-type.

010    7177  Risk object sub-type description
             identifier                                C      an..17
020    1131  Code list identification code             C      an..17
030    3055  Code list responsible agency code         C      an..3
040    7176  Risk object sub-type description          C      an..70"""
  val segment1b = """       SCD  STRUCTURE COMPONENT DEFINITION

       Function: To specify a component of a data structure (e.g.
                 an array or table).

010    7497 STRUCTURE COMPONENT FUNCTION CODE
            QUALIFIER                                  M    1 an..3

020    C786 STRUCTURE COMPONENT IDENTIFICATION         C    1
       7512  Structure component identifier            M      an..35
       7405  Object identification code qualifier      C      an..3"""

  it should "combine multiple lines when fields are continued to next line" in {
    val ctempl1a = buildTemplate(ctemplate1a)
    val ctempl1b = buildTemplate(ctemplate1b)
    val clines1 = stringLines(composite1b)
    parseTemplate(ctempl1a, false, clines1) should be(Array("", "C852", "RISK OBJECT SUB-TYPE"))
    skipBlankLine(clines1)
    skipPastBlankLine(clines1)
    parseTemplate(ctempl1b, true, clines1) should be(Array("010", "", "7177", "Risk object sub-type description identifier", "C"))
    parseTemplate(ctempl1b, true, clines1) should be(Array("020", "", "1131", "Code list identification code", "C"))
    parseTemplate(ctempl1b, true, clines1) should be(Array("030", "", "3055", "Code list responsible agency code", "C"))
    parseTemplate(ctempl1b, true, clines1) should be(Array("040", "", "7176", "Risk object sub-type description", "C"))
    val stempl1a = buildTemplate(stemplate1a)
    val stempl1b = buildTemplate(stemplate1b)
    val slines1 = stringLines(segment1b)
    parseTemplate(stempl1a, false, slines1) should be(Array("", "SCD", "STRUCTURE COMPONENT DEFINITION"))
    skipBlankLine(slines1)
    skipPastBlankLine(slines1)
    parseTemplate(stempl1b, true, slines1) should be(Array("010", "", "7497", "STRUCTURE COMPONENT FUNCTION CODE QUALIFIER", "M"))
    skipBlankLine(slines1)
    parseTemplate(stempl1b, true, slines1) should be(Array("020", "", "C786", "STRUCTURE COMPONENT IDENTIFICATION", "C"))
    parseTemplate(stempl1b, false, slines1) should be(Array("", "", "7512", "Structure component identifier", "M"))
    parseTemplate(stempl1b, false, slines1) should be(Array("", "", "7405", "Object identification code qualifier", "C"))
  }

  it should "throw an exception when fields are missing at end" in {
    val template = buildTemplate("* * *")
    intercept[IllegalArgumentException] { parseTemplate(template, true, stringLines("AA")) }
    intercept[IllegalArgumentException] { parseTemplate(template, true, stringLines("AA\n")) }
    intercept[IllegalArgumentException] { parseTemplate(template, true, stringLines("AA\nAA\n")) }
  }

  "readComposites" should "parse complete composite definitions" in {
    val ctempl1a = buildTemplate(ctemplate1a)
    val ctempl1b = buildTemplate(ctemplate1b)
    val element1 = Element("3299", "", DataType.ALPHANUMERIC, 0, 3)
    val element2 = Element("3131", "", DataType.ALPHANUMERIC, 0, 3)
    val element3 = Element("3475", "", DataType.ALPHANUMERIC, 0, 3)
    val element4 = Element("7177", "", DataType.ALPHANUMERIC, 0, 17)
    val element5 = Element("1131", "", DataType.ALPHANUMERIC, 0, 17)
    val element6 = Element("3055", "", DataType.ALPHANUMERIC, 0, 3)
    val element7 = Element("7176", "", DataType.ALPHANUMERIC, 0, 70)
    val elemap = Map(element1.ident -> element1, element2.ident -> element2, element3.ident -> element3,
      element4.ident -> element4, element5.ident -> element5, element6.ident -> element6, element7.ident -> element7)
    val comp1 = Composite("C817", "ADDRESS USAGE", List(
      ElementComponent(element1, Some("Address purpose code"), "C81701", 10, ConditionalUsage, 1),
      ElementComponent(element2, Some("Address type code"), "C81702", 20, ConditionalUsage, 1),
      ElementComponent(element3, Some("Address status code"), "C81703", 30, ConditionalUsage, 1)), Nil)
    val separator = "------\n\n"
    readComposites(stringLines(separator + composite1a), ctempl1a, ctempl1b, elemap) should be(List(comp1))
    val comp2 = Composite("C852", "RISK OBJECT SUB-TYPE", List(
      ElementComponent(element4, Some("Risk object sub-type description identifier"), "C85201", 10, ConditionalUsage, 1),
      ElementComponent(element5, Some("Code list identification code"), "C85202", 20, ConditionalUsage, 1),
      ElementComponent(element6, Some("Code list responsible agency code"), "C85203", 30, ConditionalUsage, 1),
      ElementComponent(element7, Some("Risk object sub-type description"), "C85204", 40, ConditionalUsage, 1)), Nil)
    readComposites(stringLines(separator + composite1b), ctempl1a, ctempl1b, elemap) should be(List(comp2))
    println(separator + composite1a + "\n\n" + separator + composite1b)
    readComposites(stringLines(separator + composite1a + "\n\n" + separator + composite1b),
        ctempl1a, ctempl1b, elemap) should be(List(comp1, comp2))
  }

  "readSegments" should "parse complete segment definitions" in {
    val stempl1a = buildTemplate(stemplate1a)
    val stempl1b = buildTemplate(stemplate1b)
    val element1 = Element("5463", "", DataType.ALPHANUMERIC, 0, 3)
    val element2 = Element("1230", "", DataType.ALPHANUMERIC, 0, 35)
    val element3 = Element("5189", "", DataType.ALPHANUMERIC, 0, 3)
    val element4 = Element("4471", "", DataType.ALPHANUMERIC, 0, 3)
    val element5 = Element("1227", "", DataType.ALPHANUMERIC, 0, 3)
    val element6 = Element("7497", "", DataType.ALPHANUMERIC, 0, 3)
    val element7 = Element("7512", "", DataType.ALPHANUMERIC, 0, 35)
    val element8 = Element("7405", "", DataType.ALPHANUMERIC, 0, 3)
    val elemap = Map(element1.ident -> element1, element2.ident -> element2, element3.ident -> element3,
      element4.ident -> element4, element5.ident -> element5, element6.ident -> element6, element7.ident -> element7,
      element8.ident -> element8)
    val comp1 = Composite("C552", "ALLOWANCE/CHARGE INFORMATION", List(
      ElementComponent(element2, Some("Allowance or charge identifier"), "C55201", 10, ConditionalUsage, 1),
      ElementComponent(element3, Some("Allowance or charge identification code"), "C55202", 20, ConditionalUsage, 1)), Nil)
    val comp2 = Composite("C786", "STRUCTURE COMPONENT IDENTIFICATION", List(
      ElementComponent(element7, Some("Structure component identifier"), "C78601", 10, MandatoryUsage, 1),
      ElementComponent(element8, Some("Object identification code qualifier"), "C78602", 20, ConditionalUsage, 1)), Nil)
    val compmap = Map(comp1.ident -> comp1, comp2.ident -> comp2)
    val seg1 = Segment("ALC", "ALLOWANCE OR CHARGE", List(
      ElementComponent(element1, Some("ALLOWANCE OR CHARGE CODE QUALIFIER"), "ALC01", 10, MandatoryUsage, 1),
      CompositeComponent(comp1, Some("ALLOWANCE/CHARGE INFORMATION"), "ALC02", 20, ConditionalUsage, 1),
      ElementComponent(element4, Some("SETTLEMENT MEANS CODE"), "ALC03", 30, ConditionalUsage, 1),
      ElementComponent(element5, Some("CALCULATION SEQUENCE CODE"), "ALC04", 40, ConditionalUsage, 1)), Nil)
    val separator = "------\n\n"
    readSegments(stringLines(separator + segment1a), stempl1a, stempl1b, elemap, compmap) should be(List(seg1))
    val seg2 = Segment("SCD", "STRUCTURE COMPONENT DEFINITION", List(
      ElementComponent(element6, Some("STRUCTURE COMPONENT FUNCTION CODE QUALIFIER"), "SDC01", 10, MandatoryUsage, 1),
      CompositeComponent(comp2, Some("STRUCTURE COMPONENT IDENTIFICATION"), "SDC02", 20, ConditionalUsage, 1)), Nil)
    readSegments(stringLines(separator + segment1b), stempl1a, stempl1b, elemap, compmap) should be(List(seg2))
    readSegments(stringLines(separator + segment1a + "\n\n" + separator + segment1b),
      stempl1a, stempl1b, elemap, compmap) should be(List(seg1, seg2))
  }
}