package com.mulesoft.flatfile.schema.fftypes

import java.{ lang => jl, math => jm, text => jt, util => ju }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.mulesoft.flatfile.lexical.LexicalException
import com.mulesoft.flatfile.lexical.TypeFormatConstants._

class DecimalFormatTest extends FlatSpec with Matchers {
  
  val unsigned2 = DecimalFormat(2, NumberSign.UNSIGNED, FillMode.RIGHT)
  val unsigned4 = DecimalFormat(4, NumberSign.UNSIGNED, FillMode.LEFT)
  val unsigned8 = DecimalFormat(8, NumberSign.UNSIGNED, FillMode.ZEROES)
  
  behavior of "Decimal unsigned form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString(".2", unsigned2) should be (new jm.BigDecimal(".2"))
    DemoSupport.parseString("9.", unsigned2) should be (Integer.valueOf(9))
    DemoSupport.parseString("22.3", unsigned4) should be (new jm.BigDecimal("22.3"))
    DemoSupport.parseString("2.8 ", unsigned4) should be (new jm.BigDecimal("2.8"))
    DemoSupport.parseString("0002.233", unsigned8) should be (new jm.BigDecimal("2.233"))
    DemoSupport.parseString(".2822313", unsigned8) should be (new jm.BigDecimal(".2822313"))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("  1 ", unsigned4) }
    intercept[LexicalException] { DemoSupport.parseString("true", unsigned4) }
    intercept[LexicalException] { DemoSupport.parseString(" -28", unsigned4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(2), unsigned2) should be ("2.")
    DemoSupport.writeString(new jm.BigDecimal("9."), unsigned2) should be ("9.")
    DemoSupport.writeString(new jm.BigDecimal("22.3"), unsigned4) should be ("22.3")
    DemoSupport.writeString(new jm.BigDecimal("2.8"), unsigned4) should be ("2.8 ")
    DemoSupport.writeString(new jm.BigDecimal("2.233"), unsigned8) should be ("0002.233")
    DemoSupport.writeString(new jm.BigDecimal(".2822313"), unsigned8) should be (".2822313")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("  ", unsigned2) }
    intercept[IOException] { DemoSupport.writeString(new jm.BigDecimal("22"), unsigned2) }
    intercept[IOException] { DemoSupport.writeString(null, unsigned4) }
    intercept[IOException] { DemoSupport.writeString(new jm.BigDecimal("28223134"), unsigned4) }
    intercept[IOException] { DemoSupport.writeString(new jm.BigDecimal("-123"), unsigned4) }
  }
  
  val negonly2 = DecimalFormat(2, NumberSign.NEGATIVE_ONLY, FillMode.RIGHT)
  val negonly4 = DecimalFormat(4, NumberSign.NEGATIVE_ONLY, FillMode.LEFT)
  val negonly8 = DecimalFormat(8, NumberSign.NEGATIVE_ONLY, FillMode.ZEROES)
  
  behavior of "Decimal negative only form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString(".2", negonly2) should be (new jm.BigDecimal(".2"))
    DemoSupport.parseString("9.", negonly2) should be (Integer.valueOf(9))
    DemoSupport.parseString("22.3", negonly4) should be (new jm.BigDecimal("22.3"))
    DemoSupport.parseString("-2.8", negonly4) should be (new jm.BigDecimal("-2.8"))
    DemoSupport.parseString("-002.233", negonly8) should be (new jm.BigDecimal("-2.233"))
    DemoSupport.parseString("-.282233", negonly8) should be (new jm.BigDecimal("-.282233"))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("  1 ", negonly4) }
    intercept[LexicalException] { DemoSupport.parseString("true", negonly4) }
    intercept[LexicalException] { DemoSupport.parseString("+28 ", negonly4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(new jm.BigDecimal(".2"), negonly2) should be (".2")
    DemoSupport.writeString(Integer.valueOf(9), negonly2) should be ("9.")
    DemoSupport.writeString(new jm.BigDecimal("22.3"), negonly4) should be ("22.3")
    DemoSupport.writeString(new jm.BigDecimal("-2.8"), negonly4) should be ("-2.8")
    DemoSupport.writeString(new jm.BigDecimal("-2.233"), negonly8) should be ("-002.233")
    DemoSupport.writeString(new jm.BigDecimal("-.282233"), negonly8) should be ("-.282233")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("  ", negonly2) }
    intercept[IOException] { DemoSupport.writeString(null, negonly4) }
    intercept[IOException] { DemoSupport.writeString(new jm.BigDecimal("28223134"), negonly4) }
  }
  
  val optional1 = DecimalFormat(1, NumberSign.OPTIONAL, FillMode.NONE)
  val optional2 = DecimalFormat(2, NumberSign.OPTIONAL, FillMode.RIGHT)
  val optional4 = DecimalFormat(4, NumberSign.OPTIONAL, FillMode.LEFT)
  val optional8 = DecimalFormat(8, NumberSign.OPTIONAL, FillMode.ZEROES)
  
  behavior of "Decimal optional sign form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString(".2", optional2) should be (new jm.BigDecimal(".2"))
    DemoSupport.parseString("9.", optional2) should be (Integer.valueOf(9))
    DemoSupport.parseString("+2.3", optional4) should be (new jm.BigDecimal("2.3"))
    DemoSupport.parseString("-2.8", optional4) should be (new jm.BigDecimal("-2.8"))
    DemoSupport.parseString("-002.233", optional8) should be (new jm.BigDecimal("-2.233"))
    DemoSupport.parseString("-.282233", optional8) should be (new jm.BigDecimal("-.282233"))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("  1 ", optional4) }
    intercept[LexicalException] { DemoSupport.parseString("true", optional4) }
    intercept[LexicalException] { DemoSupport.parseString("+28 ", optional4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(new jm.BigDecimal(".2"), optional2) should be (".2")
    DemoSupport.writeString(Integer.valueOf(9), optional2) should be ("9.")
    DemoSupport.writeString(new jm.BigDecimal("22.3"), optional4) should be ("22.3")
    DemoSupport.writeString(new jm.BigDecimal("-2.8"), optional4) should be ("-2.8")
    DemoSupport.writeString(new jm.BigDecimal("-2.233"), optional8) should be ("-002.233")
    DemoSupport.writeString(new jm.BigDecimal("-.282233"), optional8) should be ("-.282233")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("  ", optional2) }
    intercept[IOException] { DemoSupport.writeString(null, optional4) }
    intercept[IOException] { DemoSupport.writeString(new jm.BigDecimal("28223134"), optional4) }
  }
  
  val alwaysLeft4 = DecimalFormat(4, NumberSign.ALWAYS_LEFT, FillMode.LEFT)
  val alwaysLeft8 = DecimalFormat(8, NumberSign.ALWAYS_LEFT, FillMode.ZEROES)
  
  behavior of "Decimal always left sign form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("+2. ", alwaysLeft4) should be (Integer.valueOf(2))
    DemoSupport.parseString("-9.3", alwaysLeft4) should be (new jm.BigDecimal("-9.3"))
    DemoSupport.parseString("-28.", alwaysLeft4) should be (Integer.valueOf(-28))
    DemoSupport.parseString("+2.8", alwaysLeft4) should be (new jm.BigDecimal("2.8"))
    DemoSupport.parseString("-00.2233", alwaysLeft8) should be (new jm.BigDecimal("-.2233"))
    DemoSupport.parseString("+0822.34", alwaysLeft8) should be (new jm.BigDecimal("822.34"))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("T   ", alwaysLeft4) }
    intercept[LexicalException] { DemoSupport.parseString("t   ", alwaysLeft4) }
    intercept[LexicalException] { DemoSupport.parseString(" ", alwaysLeft4) }
    intercept[LexicalException] { DemoSupport.parseString("   1", alwaysLeft4) }
    intercept[LexicalException] { DemoSupport.parseString("true", alwaysLeft4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(2), alwaysLeft4) should be ("+2. ")
    DemoSupport.writeString(new jm.BigDecimal("-9"), alwaysLeft4) should be ("-9. ")
    DemoSupport.writeString(new jm.BigDecimal("-28"), alwaysLeft4) should be ("-28.")
    DemoSupport.writeString(new jm.BigDecimal("-.2233"), alwaysLeft8) should be ("-00.2233")
    DemoSupport.writeString(new jm.BigDecimal("822.34"), alwaysLeft8) should be ("+0822.34")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString(jl.Boolean.TRUE, alwaysLeft4) }
    intercept[IOException] { DemoSupport.writeString("  ", alwaysLeft4) }
    intercept[IOException] { DemoSupport.writeString(null, alwaysLeft4) }
    intercept[IOException] { DemoSupport.writeString(new jm.BigDecimal("233"), alwaysLeft4) }
    intercept[IOException] { DemoSupport.writeString(new jm.BigDecimal("28223134"), alwaysLeft8) }
  }
  
  val alwaysRight4 = DecimalFormat(4, NumberSign.ALWAYS_RIGHT, FillMode.LEFT)
  val alwaysRight8 = DecimalFormat(8, NumberSign.ALWAYS_RIGHT, FillMode.ZEROES)
  
  behavior of "Decimal always right sign form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("2.+ ", alwaysRight4) should be (Integer.valueOf(2))
    DemoSupport.parseString("9.3-", alwaysRight4) should be (new jm.BigDecimal("-9.3"))
    DemoSupport.parseString("28.-", alwaysRight4) should be (Integer.valueOf(-28))
    DemoSupport.parseString("2.8+", alwaysRight4) should be (new jm.BigDecimal("2.8"))
    DemoSupport.parseString("00.2233-", alwaysRight8) should be (new jm.BigDecimal("-.2233"))
    DemoSupport.parseString("0822.34+", alwaysRight8) should be (new jm.BigDecimal("822.34"))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("T   ", alwaysRight4) }
    intercept[LexicalException] { DemoSupport.parseString("t   ", alwaysRight4) }
    intercept[LexicalException] { DemoSupport.parseString(" ", alwaysRight4) }
    intercept[LexicalException] { DemoSupport.parseString("   1", alwaysRight4) }
    intercept[LexicalException] { DemoSupport.parseString("true", alwaysRight4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(2), alwaysRight4) should be ("2.+ ")
    DemoSupport.writeString(new jm.BigDecimal("-9"), alwaysRight4) should be ("9.- ")
    DemoSupport.writeString(new jm.BigDecimal("-28"), alwaysRight4) should be ("28.-")
    DemoSupport.writeString(new jm.BigDecimal("-.2233"), alwaysRight8) should be ("00.2233-")
    DemoSupport.writeString(new jm.BigDecimal("822.34"), alwaysRight8) should be ("0822.34+")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString(jl.Boolean.TRUE, alwaysRight4) }
    intercept[IOException] { DemoSupport.writeString("  ", alwaysRight4) }
    intercept[IOException] { DemoSupport.writeString(null, alwaysRight4) }
    intercept[IOException] { DemoSupport.writeString(new jm.BigDecimal("233"), alwaysRight4) }
    intercept[IOException] { DemoSupport.writeString(new jm.BigDecimal("28223134"), alwaysRight8) }
  }
  
  val implied8Left = DecimalFormat(8, NumberSign.OPTIONAL, 2, FillMode.LEFT)
  val implied8Right = DecimalFormat(8, NumberSign.OPTIONAL, 4, FillMode.RIGHT)
  val implied8Zeroes = DecimalFormat(8, NumberSign.OPTIONAL, 2, FillMode.ZEROES)
  
  behavior of "Decimal implied"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("12345   ", implied8Left) should be (new jm.BigDecimal("123.45"))
    DemoSupport.parseString("-12345  ", implied8Left) should be (new jm.BigDecimal("-123.45"))
    DemoSupport.parseString("1       ", implied8Left) should be (new jm.BigDecimal(".01"))
    DemoSupport.parseString("   12345", implied8Right) should be (new jm.BigDecimal("1.2345"))
    DemoSupport.parseString("  -12345", implied8Right) should be (new jm.BigDecimal("-1.2345"))
    DemoSupport.parseString("       1", implied8Right) should be (new jm.BigDecimal(".0001"))
    DemoSupport.parseString("00012345", implied8Zeroes) should be (new jm.BigDecimal("123.45"))
    DemoSupport.parseString("-0012345", implied8Zeroes) should be (new jm.BigDecimal("-123.45"))
    DemoSupport.parseString("00000001", implied8Zeroes) should be (new jm.BigDecimal(".01"))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("1           ", implied8Right) }
    intercept[LexicalException] { DemoSupport.parseString("1           ", implied8Zeroes) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(new jm.BigDecimal("123.45"), implied8Left) should be ("12345   ")
    DemoSupport.writeString(new jm.BigDecimal("-123.45"), implied8Left) should be ("-12345  ")
    DemoSupport.writeString(new jm.BigDecimal(".01"), implied8Left) should be ("1       ")
    DemoSupport.writeString(new jm.BigDecimal("1.2345"), implied8Right) should be ("   12345")
    DemoSupport.writeString(new jm.BigDecimal("-1.2345"), implied8Right) should be ("  -12345")
    DemoSupport.writeString(new jm.BigDecimal(".0001"), implied8Right) should be ("       1")
    DemoSupport.writeString(new jm.BigDecimal("123.45"), implied8Zeroes) should be ("00012345")
    DemoSupport.writeString(new jm.BigDecimal("-123.45"), implied8Zeroes) should be ("-0012345")
    DemoSupport.writeString(new jm.BigDecimal(".01"), implied8Zeroes) should be ("00000001")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("  ", implied8Left) }
  }
  
  val pattern12Left = DecimalFormat(12, "#,###,###.00X", new ju.Locale("en"), FillMode.LEFT)
  val pattern12Right = DecimalFormat(12, "#,###,###.00X", new ju.Locale("en"), FillMode.RIGHT)
  
  behavior of "Decimal pattern"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("1.00X       ", pattern12Left) should be (new jm.BigDecimal("1.00"))
    DemoSupport.parseString("12,345.23X  ", pattern12Left) should be (new jm.BigDecimal("12345.23"))
    DemoSupport.parseString("234,567.89X ", pattern12Left) should be (new jm.BigDecimal("234567.89"))
    DemoSupport.parseString("       1.00X", pattern12Right) should be (new jm.BigDecimal("1.00"))
    DemoSupport.parseString("  12,345.23X", pattern12Right) should be (new jm.BigDecimal("12345.23"))
    DemoSupport.parseString(" 234,567.89X", pattern12Right) should be (new jm.BigDecimal("234567.89"))
  }
  
  it should "throw exception on invalid input" in {
    intercept[jt.ParseException] { DemoSupport.parseString("1           ", pattern12Left) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(2), pattern12Left) should be ("2.00X       ")
    DemoSupport.writeString(new jm.BigDecimal("12345.23"), pattern12Left) should be ("12,345.23X  ")
    DemoSupport.writeString(new jm.BigDecimal("234567.89"), pattern12Left) should be ("234,567.89X ")
    DemoSupport.writeString(Integer.valueOf(2), pattern12Right) should be ("       2.00X")
    DemoSupport.writeString(new jm.BigDecimal("12345.23"), pattern12Right) should be ("  12,345.23X")
    DemoSupport.writeString(new jm.BigDecimal("234567.89"), pattern12Right) should be (" 234,567.89X")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("  ", pattern12Left) }
  }
}