package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.anypoint.df.edi.lexical.LexicalException
import com.anypoint.df.edi.lexical.TypeFormatConstants._

class BooleanFormatTest extends FlatSpec with Matchers {
  
  val digit1 = BooleanFormat(1, BooleanRepresentation.NUMBER, FillMode.NONE)
  val digit2 = BooleanFormat(2, BooleanRepresentation.NUMBER, FillMode.LEFT)
  val digit5 = BooleanFormat(5, BooleanRepresentation.NUMBER, FillMode.RIGHT)
  
  behavior of "BooleanFormat digit form"
  
  it should "parse single digit correctly" in {
    DemoSupport.parseString("0", digit1).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("1", digit1).asInstanceOf[Boolean] should be (true)
    DemoSupport.parseString("0 ", digit2).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("1 ", digit2).asInstanceOf[Boolean] should be (true)
    DemoSupport.parseString("    0", digit5).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("    1", digit5).asInstanceOf[Boolean] should be (true)
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("T", digit1) }
    intercept[LexicalException] { DemoSupport.parseString("t", digit1) }
    intercept[LexicalException] { DemoSupport.parseString(" ", digit1) }
    intercept[LexicalException] { DemoSupport.parseString("  1  ", digit5) }
    intercept[LexicalException] { DemoSupport.parseString("true ", digit5) }
    intercept[LexicalException] { DemoSupport.parseString("false", digit5) }
  }
  
  it should "write single digit correctly" in {
    DemoSupport.writeString(jl.Boolean.FALSE, digit1) should be ("0")
    DemoSupport.writeString(jl.Boolean.TRUE, digit1) should be ("1")
    DemoSupport.writeString(jl.Boolean.FALSE, digit2) should be ("0 ")
    DemoSupport.writeString(jl.Boolean.TRUE, digit2) should be ("1 ")
    DemoSupport.writeString(jl.Boolean.FALSE, digit5) should be ("    0")
    DemoSupport.writeString(jl.Boolean.TRUE, digit5) should be ("    1")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString(jl.Integer.valueOf(0), digit1) }
    intercept[IOException] { DemoSupport.writeString("  ", digit2) }
    intercept[IOException] { DemoSupport.writeString(null, digit5) }
  }
  
  val upper1 = BooleanFormat(1, BooleanRepresentation.ALPHA_UPPER, FillMode.NONE)
  val upper2 = BooleanFormat(2, BooleanRepresentation.ALPHA_UPPER, FillMode.LEFT)
  val upper5 = BooleanFormat(5, BooleanRepresentation.ALPHA_UPPER, FillMode.LEFT)
  val upper7 = BooleanFormat(7, BooleanRepresentation.ALPHA_UPPER, FillMode.RIGHT)
  
  behavior of "BooleanFormat upper form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("F", upper1).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("T", upper1).asInstanceOf[Boolean] should be (true)
    DemoSupport.parseString("F ", upper2).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("T ", upper2).asInstanceOf[Boolean] should be (true)
    DemoSupport.parseString("FALSE", upper5).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("TRUE ", upper5).asInstanceOf[Boolean] should be (true)
    DemoSupport.parseString("  FALSE", upper7).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("   TRUE", upper7).asInstanceOf[Boolean] should be (true)
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("0", upper1) }
    intercept[LexicalException] { DemoSupport.parseString("1", upper1) }
    intercept[LexicalException] { DemoSupport.parseString(" ", upper1) }
    intercept[LexicalException] { DemoSupport.parseString("  1  ", upper5) }
    intercept[LexicalException] { DemoSupport.parseString("true ", upper5) }
    intercept[LexicalException] { DemoSupport.parseString("false", upper5) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(jl.Boolean.FALSE, upper1) should be ("F")
    DemoSupport.writeString(jl.Boolean.TRUE, upper1) should be ("T")
    DemoSupport.writeString(jl.Boolean.FALSE, upper2) should be ("F ")
    DemoSupport.writeString(jl.Boolean.TRUE, upper2) should be ("T ")
    DemoSupport.writeString(jl.Boolean.FALSE, upper5) should be ("FALSE")
    DemoSupport.writeString(jl.Boolean.TRUE, upper5) should be ("TRUE ")
    DemoSupport.writeString(jl.Boolean.FALSE, upper7) should be ("  FALSE")
    DemoSupport.writeString(jl.Boolean.TRUE, upper7) should be ("   TRUE")
  }
  
  val lower1 = BooleanFormat(1, BooleanRepresentation.ALPHA_LOWER, FillMode.NONE)
  val lower2 = BooleanFormat(2, BooleanRepresentation.ALPHA_LOWER, FillMode.LEFT)
  val lower5 = BooleanFormat(5, BooleanRepresentation.ALPHA_LOWER, FillMode.LEFT)
  val lower7 = BooleanFormat(7, BooleanRepresentation.ALPHA_LOWER, FillMode.RIGHT)
  
  behavior of "BooleanFormat lower form"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("f", lower1).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("t", lower1).asInstanceOf[Boolean] should be (true)
    DemoSupport.parseString("f ", lower2).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("t ", lower2).asInstanceOf[Boolean] should be (true)
    DemoSupport.parseString("false", lower5).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("true ", lower5).asInstanceOf[Boolean] should be (true)
    DemoSupport.parseString("  false", lower7).asInstanceOf[Boolean] should be (false)
    DemoSupport.parseString("   true", lower7).asInstanceOf[Boolean] should be (true)
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { DemoSupport.parseString("0", lower1) }
    intercept[LexicalException] { DemoSupport.parseString("1", lower1) }
    intercept[LexicalException] { DemoSupport.parseString(" ", lower1) }
    intercept[LexicalException] { DemoSupport.parseString("  1  ", lower5) }
    intercept[LexicalException] { DemoSupport.parseString("TRUE ", lower5) }
    intercept[LexicalException] { DemoSupport.parseString("FALSE", lower5) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(jl.Boolean.FALSE, lower1) should be ("f")
    DemoSupport.writeString(jl.Boolean.TRUE, lower1) should be ("t")
    DemoSupport.writeString(jl.Boolean.FALSE, lower2) should be ("f ")
    DemoSupport.writeString(jl.Boolean.TRUE, lower2) should be ("t ")
    DemoSupport.writeString(jl.Boolean.FALSE, lower5) should be ("false")
    DemoSupport.writeString(jl.Boolean.TRUE, lower5) should be ("true ")
    DemoSupport.writeString(jl.Boolean.FALSE, lower7) should be ("  false")
    DemoSupport.writeString(jl.Boolean.TRUE, lower7) should be ("   true")
  }
}