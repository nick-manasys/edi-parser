package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl, text => jt, util => ju }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.anypoint.df.edi.lexical.LexicalException
import com.anypoint.df.edi.lexical.TypeFormatConstants._

class StringFormatTest extends FlatSpec with Matchers {
  
  val pattern12Left = StringFormat(12, FillMode.LEFT)
  val pattern12Right = StringFormat(12, FillMode.RIGHT)
  val pattern12None = StringFormat(12, FillMode.NONE)
  
  behavior of "String pattern"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("1X          ", pattern12Left) should be ("1X")
    DemoSupport.parseString("12,345X     ", pattern12Left) should be ("12,345X")
    DemoSupport.parseString(" 1,234,567X ", pattern12Left) should be (" 1,234,567X")
    DemoSupport.parseString("          1X", pattern12Right) should be ("1X")
    DemoSupport.parseString("     12,345X", pattern12Right) should be ("12,345X")
    DemoSupport.parseString(" 1,234,567X ", pattern12Right) should be ("1,234,567X ")
    DemoSupport.parseString("1X          ", pattern12None) should be ("1X          ")
    DemoSupport.parseString("  12,345X   ", pattern12None) should be ("  12,345X   ")
    DemoSupport.parseString(" 1,234,567X ", pattern12None) should be (" 1,234,567X ")
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString("1X", pattern12Left) should be ("1X          ")
    DemoSupport.writeString("12,345X", pattern12Left) should be ("12,345X     ")
    DemoSupport.writeString(" 1,234,567X", pattern12Left) should be (" 1,234,567X ")
    DemoSupport.writeString(Integer.valueOf(123), pattern12Left) should be ("123         ")
    DemoSupport.writeString("1X", pattern12Right) should be ("          1X")
    DemoSupport.writeString("12,345X", pattern12Right) should be ("     12,345X")
    DemoSupport.writeString("1,234,567X ", pattern12Right) should be (" 1,234,567X ")
    DemoSupport.writeString(Integer.valueOf(123), pattern12Right) should be ("         123")
    DemoSupport.writeString("1X          ", pattern12None) should be ("1X          ")
    DemoSupport.writeString("  12,345X   ", pattern12None) should be ("  12,345X   ")
    DemoSupport.writeString(" 1,234,567X ", pattern12None) should be (" 1,234,567X ")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("01234567890", pattern12None) }
    intercept[IOException] { DemoSupport.writeString("0123456789012", pattern12None) }
  }
}