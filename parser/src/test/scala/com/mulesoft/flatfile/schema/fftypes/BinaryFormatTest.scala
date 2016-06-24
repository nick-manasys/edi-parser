package com.mulesoft.flatfile.schema.fftypes

import java.{ lang => jl }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.mulesoft.flatfile.lexical.LexicalException
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.EdiConstants

class BinaryFormatTest extends FlatSpec with Matchers {
  
  val unsigned2 = BinaryFormat(2, 4, 0, false)
  val unsigned4 = BinaryFormat(4, 8, 0, false)
  val unsigned8 = BinaryFormat(8, 12, 0, false)
  
  behavior of "Binary unsigned simple"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("\u0000\u0004", unsigned2, EdiConstants.ISO88591_CHARSET, true) should be (Integer.valueOf(4))
    DemoSupport.parseString("\u0004\u0004", unsigned2, EdiConstants.ISO88591_CHARSET, true) should be (Integer.valueOf(1028))
    DemoSupport.parseString("\u0000\u0000\u0000\u0004", unsigned4, EdiConstants.ISO88591_CHARSET, true) should be (Integer.valueOf(4))
    DemoSupport.parseString("\u0000\u0004\u0000\u0004", unsigned4, EdiConstants.ISO88591_CHARSET, true) should be (Integer.valueOf(262148))
    DemoSupport.parseString("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0004", unsigned8, EdiConstants.ISO88591_CHARSET, true) should be (Integer.valueOf(4))
    DemoSupport.parseString("\u0000\u0000\u0000\u0000\u0000\u0004\u0000\u0004", unsigned8, EdiConstants.ISO88591_CHARSET, true) should be (Integer.valueOf(262148))
    DemoSupport.parseString("\u0000\u0000\u0000\u0004\u0000\u0004\u0000\u0004", unsigned8, EdiConstants.ISO88591_CHARSET, true) should be (jl.Long.valueOf(17180131332L))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("T", unsigned2, EdiConstants.ISO88591_CHARSET, true) }
    intercept[LexicalException] { DemoSupport.parseString("t", unsigned2, EdiConstants.ISO88591_CHARSET, true) }
    intercept[LexicalException] { DemoSupport.parseString("\u7FFF\u00FF", unsigned2, EdiConstants.ISO88591_CHARSET, true) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(4), unsigned2) should be ("\u0000\u0004")
    DemoSupport.writeString(Integer.valueOf(1028), unsigned2) should be ("\u0004\u0004")
    DemoSupport.writeString(Integer.valueOf(4), unsigned4) should be ("\u0000\u0000\u0000\u0004")
    DemoSupport.writeString(Integer.valueOf(262148), unsigned4) should be ("\u0000\u0004\u0000\u0004")
    DemoSupport.writeString(Integer.valueOf(4), unsigned8) should be ("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0004")
    DemoSupport.writeString(Integer.valueOf(262148), unsigned8) should be ("\u0000\u0000\u0000\u0000\u0000\u0004\u0000\u0004")
    DemoSupport.writeString(jl.Long.valueOf(17180131332L), unsigned8) should be ("\u0000\u0000\u0000\u0004\u0000\u0004\u0000\u0004")
  }

}