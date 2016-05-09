package com.mulesoft.flatfile.schema.fftypes

import java.{ lang => jl, text => jt, util => ju }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.mulesoft.flatfile.lexical.LexicalException
import com.mulesoft.flatfile.lexical.TypeFormatConstants._

class IntegerFormatTest extends FlatSpec with Matchers {
  
  val unsigned1 = IntegerFormat(1, NumberSign.UNSIGNED, FillMode.NONE)
  val unsigned2 = IntegerFormat(2, NumberSign.UNSIGNED, FillMode.RIGHT)
  val unsigned4 = IntegerFormat(4, NumberSign.UNSIGNED, FillMode.LEFT)
  val unsigned8 = IntegerFormat(8, NumberSign.UNSIGNED, FillMode.ZEROES)
  
  behavior of "Integer unsigned form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("0", unsigned1) should be (Integer.valueOf(0))
    DemoSupport.parseString("9", unsigned1) should be (Integer.valueOf(9))
    DemoSupport.parseString("22", unsigned2) should be (Integer.valueOf(22))
    DemoSupport.parseString(" 9", unsigned2) should be (Integer.valueOf(9))
    DemoSupport.parseString("2233", unsigned4) should be (Integer.valueOf(2233))
    DemoSupport.parseString("28  ", unsigned4) should be (Integer.valueOf(28))
    DemoSupport.parseString("00002233", unsigned8) should be (Integer.valueOf(2233))
    DemoSupport.parseString("28223134", unsigned8) should be (Integer.valueOf(28223134))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("T", unsigned1) }
    intercept[LexicalException] { DemoSupport.parseString("t", unsigned1) }
    intercept[LexicalException] { DemoSupport.parseString(" ", unsigned1) }
    intercept[LexicalException] { DemoSupport.parseString("  1 ", unsigned4) }
    intercept[LexicalException] { DemoSupport.parseString("true", unsigned4) }
    intercept[LexicalException] { DemoSupport.parseString(" -28", unsigned4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(0), unsigned1) should be ("0")
    DemoSupport.writeString(Integer.valueOf(9), unsigned1) should be ("9")
    DemoSupport.writeString(Integer.valueOf(22), unsigned2) should be ("22")
    DemoSupport.writeString(Integer.valueOf(9), unsigned2) should be (" 9")
    DemoSupport.writeString(Integer.valueOf(2233), unsigned4) should be ("2233")
    DemoSupport.writeString(Integer.valueOf(28), unsigned4) should be ("28  ")
    DemoSupport.writeString(Integer.valueOf(2233), unsigned8) should be ("00002233")
    DemoSupport.writeString(Integer.valueOf(28223134), unsigned8) should be ("28223134")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString(jl.Boolean.TRUE, unsigned1) }
    intercept[IOException] { DemoSupport.writeString("  ", unsigned2) }
    intercept[IOException] { DemoSupport.writeString(null, unsigned4) }
    intercept[IOException] { DemoSupport.writeString(Integer.valueOf(28223134), unsigned4) }
    intercept[IOException] { DemoSupport.writeString(Integer.valueOf(-123), unsigned4) }
  }
  
  val negonly1 = IntegerFormat(1, NumberSign.NEGATIVE_ONLY, FillMode.NONE)
  val negonly2 = IntegerFormat(2, NumberSign.NEGATIVE_ONLY, FillMode.RIGHT)
  val negonly4 = IntegerFormat(4, NumberSign.NEGATIVE_ONLY, FillMode.LEFT)
  val negonly8 = IntegerFormat(8, NumberSign.NEGATIVE_ONLY, FillMode.ZEROES)
  
  behavior of "Integer negative only form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("0", negonly1) should be (Integer.valueOf(0))
    DemoSupport.parseString("9", negonly1) should be (Integer.valueOf(9))
    DemoSupport.parseString("22", negonly2) should be (Integer.valueOf(22))
    DemoSupport.parseString("-9", negonly2) should be (Integer.valueOf(-9))
    DemoSupport.parseString("2233", negonly4) should be (Integer.valueOf(2233))
    DemoSupport.parseString("-28 ", negonly4) should be (Integer.valueOf(-28))
    DemoSupport.parseString("-0002233", negonly8) should be (Integer.valueOf(-2233))
    DemoSupport.parseString("28223134", negonly8) should be (Integer.valueOf(28223134))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("T", negonly1) }
    intercept[LexicalException] { DemoSupport.parseString("t", negonly1) }
    intercept[LexicalException] { DemoSupport.parseString(" ", negonly1) }
    intercept[LexicalException] { DemoSupport.parseString("  1 ", negonly4) }
    intercept[LexicalException] { DemoSupport.parseString("true", negonly4) }
    intercept[LexicalException] { DemoSupport.parseString(" +28", negonly4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(0), negonly1) should be ("0")
    DemoSupport.writeString(Integer.valueOf(9), negonly1) should be ("9")
    DemoSupport.writeString(Integer.valueOf(22), negonly2) should be ("22")
    DemoSupport.writeString(Integer.valueOf(-9), negonly2) should be ("-9")
    DemoSupport.writeString(Integer.valueOf(2233), negonly4) should be ("2233")
    DemoSupport.writeString(Integer.valueOf(-28), negonly4) should be ("-28 ")
    DemoSupport.writeString(Integer.valueOf(-2233), negonly8) should be ("-0002233")
    DemoSupport.writeString(Integer.valueOf(28223134), negonly8) should be ("28223134")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString(jl.Boolean.TRUE, negonly1) }
    intercept[IOException] { DemoSupport.writeString("  ", negonly2) }
    intercept[IOException] { DemoSupport.writeString(null, negonly4) }
    intercept[IOException] { DemoSupport.writeString(Integer.valueOf(28223134), negonly4) }
  }
  
  val optional1 = IntegerFormat(1, NumberSign.OPTIONAL, FillMode.NONE)
  val optional2 = IntegerFormat(2, NumberSign.OPTIONAL, FillMode.RIGHT)
  val optional4 = IntegerFormat(4, NumberSign.OPTIONAL, FillMode.LEFT)
  val optional8 = IntegerFormat(8, NumberSign.OPTIONAL, FillMode.ZEROES)
  
  behavior of "Integer optional sign form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("0", optional1) should be (Integer.valueOf(0))
    DemoSupport.parseString("9", optional1) should be (Integer.valueOf(9))
    DemoSupport.parseString("+2", optional2) should be (Integer.valueOf(2))
    DemoSupport.parseString("-9", optional2) should be (Integer.valueOf(-9))
    DemoSupport.parseString("2233", optional4) should be (Integer.valueOf(2233))
    DemoSupport.parseString("-28 ", optional4) should be (Integer.valueOf(-28))
    DemoSupport.parseString("28  ", optional4) should be (Integer.valueOf(28))
    DemoSupport.parseString("+28 ", optional4) should be (Integer.valueOf(28))
    DemoSupport.parseString("-0002233", optional8) should be (Integer.valueOf(-2233))
    DemoSupport.parseString("+8223134", optional8) should be (Integer.valueOf(8223134))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("T", optional1) }
    intercept[LexicalException] { DemoSupport.parseString("t", optional1) }
    intercept[LexicalException] { DemoSupport.parseString(" ", optional1) }
    intercept[LexicalException] { DemoSupport.parseString("  1 ", optional4) }
    intercept[LexicalException] { DemoSupport.parseString("true", optional4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(0), optional1) should be ("0")
    DemoSupport.writeString(Integer.valueOf(9), optional1) should be ("9")
    DemoSupport.writeString(Integer.valueOf(22), optional2) should be ("22")
    DemoSupport.writeString(Integer.valueOf(-9), optional2) should be ("-9")
    DemoSupport.writeString(Integer.valueOf(2233), optional4) should be ("2233")
    DemoSupport.writeString(Integer.valueOf(-28), optional4) should be ("-28 ")
    DemoSupport.writeString(Integer.valueOf(-2233), optional8) should be ("-0002233")
    DemoSupport.writeString(Integer.valueOf(8223134), optional8) should be ("08223134")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString(jl.Boolean.TRUE, optional1) }
    intercept[IOException] { DemoSupport.writeString("  ", optional2) }
    intercept[IOException] { DemoSupport.writeString(null, optional4) }
    intercept[IOException] { DemoSupport.writeString(Integer.valueOf(28223134), optional4) }
  }
  
  val alwaysLeft2 = IntegerFormat(2, NumberSign.ALWAYS_LEFT, FillMode.RIGHT)
  val alwaysLeft4 = IntegerFormat(4, NumberSign.ALWAYS_LEFT, FillMode.LEFT)
  val alwaysLeft8 = IntegerFormat(8, NumberSign.ALWAYS_LEFT, FillMode.ZEROES)
  
  behavior of "Integer always left sign form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("+2", alwaysLeft2) should be (Integer.valueOf(2))
    DemoSupport.parseString("-9", alwaysLeft2) should be (Integer.valueOf(-9))
    DemoSupport.parseString("+233", alwaysLeft4) should be (Integer.valueOf(233))
    DemoSupport.parseString("-28 ", alwaysLeft4) should be (Integer.valueOf(-28))
    DemoSupport.parseString("+28 ", alwaysLeft4) should be (Integer.valueOf(28))
    DemoSupport.parseString("-0002233", alwaysLeft8) should be (Integer.valueOf(-2233))
    DemoSupport.parseString("+8223134", alwaysLeft8) should be (Integer.valueOf(8223134))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("T", alwaysLeft2) }
    intercept[LexicalException] { DemoSupport.parseString("t", alwaysLeft2) }
    intercept[LexicalException] { DemoSupport.parseString(" ", alwaysLeft2) }
    intercept[LexicalException] { DemoSupport.parseString("   1", alwaysLeft4) }
    intercept[LexicalException] { DemoSupport.parseString("true", alwaysLeft4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(2), alwaysLeft2) should be ("+2")
    DemoSupport.writeString(Integer.valueOf(-9), alwaysLeft2) should be ("-9")
    DemoSupport.writeString(Integer.valueOf(233), alwaysLeft4) should be ("+233")
    DemoSupport.writeString(Integer.valueOf(-28), alwaysLeft4) should be ("-28 ")
    DemoSupport.writeString(Integer.valueOf(-2233), alwaysLeft8) should be ("-0002233")
    DemoSupport.writeString(Integer.valueOf(823134), alwaysLeft8) should be ("+0823134")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString(jl.Boolean.TRUE, alwaysLeft2) }
    intercept[IOException] { DemoSupport.writeString("  ", alwaysLeft2) }
    intercept[IOException] { DemoSupport.writeString(null, alwaysLeft4) }
    intercept[IOException] { DemoSupport.writeString(Integer.valueOf(28223134), alwaysLeft8) }
  }
  
  val alwaysRight2 = IntegerFormat(2, NumberSign.ALWAYS_RIGHT, FillMode.RIGHT)
  val alwaysRight4 = IntegerFormat(4, NumberSign.ALWAYS_RIGHT, FillMode.LEFT)
  val alwaysRight8 = IntegerFormat(8, NumberSign.ALWAYS_RIGHT, FillMode.ZEROES)
  
  behavior of "Integer always right sign form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("2+", alwaysRight2) should be (Integer.valueOf(2))
    DemoSupport.parseString("9-", alwaysRight2) should be (Integer.valueOf(-9))
    DemoSupport.parseString("233+", alwaysRight4) should be (Integer.valueOf(233))
    DemoSupport.parseString("28- ", alwaysRight4) should be (Integer.valueOf(-28))
    DemoSupport.parseString("28+ ", alwaysRight4) should be (Integer.valueOf(28))
    DemoSupport.parseString("0002233-", alwaysRight8) should be (Integer.valueOf(-2233))
    DemoSupport.parseString("8223134+", alwaysRight8) should be (Integer.valueOf(8223134))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("T", alwaysRight2) }
    intercept[LexicalException] { DemoSupport.parseString("t", alwaysRight2) }
    intercept[LexicalException] { DemoSupport.parseString(" ", alwaysRight2) }
    intercept[LexicalException] { DemoSupport.parseString("   1", alwaysRight4) }
    intercept[LexicalException] { DemoSupport.parseString("true", alwaysRight4) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(2), alwaysRight2) should be ("2+")
    DemoSupport.writeString(Integer.valueOf(-9), alwaysRight2) should be ("9-")
    DemoSupport.writeString(Integer.valueOf(233), alwaysRight4) should be ("233+")
    DemoSupport.writeString(Integer.valueOf(-28), alwaysRight4) should be ("28- ")
    DemoSupport.writeString(Integer.valueOf(-2233), alwaysRight8) should be ("0002233-")
    DemoSupport.writeString(Integer.valueOf(823134), alwaysRight8) should be ("0823134+")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString(jl.Boolean.TRUE, alwaysRight2) }
    intercept[IOException] { DemoSupport.writeString("  ", alwaysRight2) }
    intercept[IOException] { DemoSupport.writeString(null, alwaysRight4) }
    intercept[IOException] { DemoSupport.writeString(Integer.valueOf(28223134), alwaysRight8) }
  }
  
  val pattern12Left = IntegerFormat(12, "###,###,###X", new ju.Locale("en"), FillMode.LEFT)
  val pattern12Right = IntegerFormat(12, "###,###,###X", new ju.Locale("en"), FillMode.RIGHT)
  
  behavior of "Integer pattern"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("1X          ", pattern12Left) should be (Integer.valueOf(1))
    DemoSupport.parseString("12,345X     ", pattern12Left) should be (Integer.valueOf(12345))
    DemoSupport.parseString("1,234,567X  ", pattern12Left) should be (Integer.valueOf(1234567))
    DemoSupport.parseString("          1X", pattern12Right) should be (Integer.valueOf(1))
    DemoSupport.parseString("     12,345X", pattern12Right) should be (Integer.valueOf(12345))
    DemoSupport.parseString("  1,234,567X", pattern12Right) should be (Integer.valueOf(1234567))
  }
  
  it should "throw exception on invalid input" in {
    intercept[jt.ParseException] { DemoSupport.parseString("1           ", pattern12Left) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(2), pattern12Left) should be ("2X          ")
    DemoSupport.writeString(Integer.valueOf(12345), pattern12Left) should be ("12,345X     ")
    DemoSupport.writeString(Integer.valueOf(1234567), pattern12Left) should be ("1,234,567X  ")
    DemoSupport.writeString(Integer.valueOf(2), pattern12Right) should be ("          2X")
    DemoSupport.writeString(Integer.valueOf(12345), pattern12Right) should be ("     12,345X")
    DemoSupport.writeString(Integer.valueOf(1234567), pattern12Right) should be ("  1,234,567X")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("  ", pattern12Left) }
  }
}