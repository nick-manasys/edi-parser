package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.anypoint.df.edi.lexical.LexicalException
import com.anypoint.df.edi.lexical.TypeFormatConstants._

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
    intercept[LexicalException] { DemoSupport.parseString("  28", unsigned4) }
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
  }
}