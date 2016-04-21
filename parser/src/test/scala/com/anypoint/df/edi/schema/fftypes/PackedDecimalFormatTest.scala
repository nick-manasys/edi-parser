package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl, math => jm, text => jt, util => ju }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.anypoint.df.edi.lexical.{ EdiConstants, LexicalException, TypeFormat }
import com.anypoint.df.edi.lexical.TypeFormatConstants._

class PackedDecimalFormatTest extends FlatSpec with Matchers {
  
  val unsigned3 = PackedDecimalFormat(3, 0, false)
  val unsigned6 = PackedDecimalFormat(6, 0, false)
  val unsigned12 = PackedDecimalFormat(12, 0, false)
  
  def rawParse(data: String, format: TypeFormat) = DemoSupport.parseString(data, format, EdiConstants.ISO88591_CHARSET, true)
  
  behavior of "Packed decimal unsigned form"
  
  it should "parse input correctly" in {
    rawParse("\u0000\u0012\u003D", unsigned3) should be (Integer.valueOf(123))
    rawParse("\u0012\u0034\u005F", unsigned3) should be (Integer.valueOf(12345))
    rawParse("\u0000\u0000\u0012\u0034\u0056\u007F", unsigned6) should be (Integer.valueOf(1234567))
    rawParse("\u0012\u0034\u0056\u0078\u0090\u000F", unsigned6) should be (jl.Long.valueOf(12345678900L))
    rawParse("\u0000\u0000\u0000\u0000\u0000\u0000\u0012\u0034\u0056\u0078\u0090\u000F", unsigned12) should be (jl.Long.valueOf(12345678900L))
    rawParse("\u0012\u0034\u0056\u0078\u0090\u0012\u0034\u0056\u0078\u0090\u0012\u003F", unsigned12) should be (new jm.BigInteger("12345678901234567890123"))
  }
  
  it should "throw exception on invalid input" in {
    intercept[LexicalException] { rawParse("A^3", unsigned3) }
    intercept[LexicalException] { rawParse("\u0000\u0000\u001F\u0034\u0056\u007F", unsigned6) }
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(123), unsigned3) should be ("\u0000\u0012\u003F")
    DemoSupport.writeString(Integer.valueOf(12345), unsigned3) should be ("\u0012\u0034\u005F")
    DemoSupport.writeString(jl.Long.valueOf(12345678900L), unsigned3) should be ("\u0078\u0090\u000F")
    DemoSupport.writeString(Integer.valueOf(-12345), unsigned3) should be ("\u0012\u0034\u005F")
    DemoSupport.writeString(Integer.valueOf(1234567), unsigned6) should be ("\u0000\u0000\u0012\u0034\u0056\u007F")
    DemoSupport.writeString(jl.Long.valueOf(12345678900L), unsigned6) should be ("\u0012\u0034\u0056\u0078\u0090\u000F")
    DemoSupport.writeString(jl.Long.valueOf(12345678900L), unsigned12) should be ("\u0000\u0000\u0000\u0000\u0000\u0000\u0012\u0034\u0056\u0078\u0090\u000F")
    DemoSupport.writeString(new jm.BigInteger("12345678901234567890123"), unsigned12) should be ("\u0012\u0034\u0056\u0078\u0090\u0012\u0034\u0056\u0078\u0090\u0012\u003F")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("  ", unsigned3) }
  }
  
  val signed3 = PackedDecimalFormat(3, 0, true)
  val signed6 = PackedDecimalFormat(6, 0, true)
  val signed12 = PackedDecimalFormat(12, 0, true)
  
  behavior of "Packed decimal signed form"
  
  it should "parse input correctly" in {
    rawParse("\u0000\u0012\u003F", signed3) should be (Integer.valueOf(123))
    rawParse("\u0012\u0034\u005D", signed3) should be (Integer.valueOf(-12345))
    rawParse("\u0000\u0000\u0012\u0034\u0056\u007B", signed6) should be (Integer.valueOf(-1234567))
    rawParse("\u0012\u0034\u0056\u0078\u0090\u000F", signed6) should be (jl.Long.valueOf(12345678900L))
    rawParse("\u0000\u0000\u0000\u0000\u0000\u0000\u0012\u0034\u0056\u0078\u0090\u000D", signed12) should be (jl.Long.valueOf(-12345678900L))
    rawParse("\u0012\u0034\u0056\u0078\u0090\u0012\u0034\u0056\u0078\u0090\u0012\u003F", signed12) should be (new jm.BigInteger("12345678901234567890123"))
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(Integer.valueOf(-123), signed3) should be ("\u0000\u0012\u003D")
    DemoSupport.writeString(Integer.valueOf(12345), signed3) should be ("\u0012\u0034\u005F")
    DemoSupport.writeString(jl.Long.valueOf(-12345678900L), signed3) should be ("\u0078\u0090\u000D")
    DemoSupport.writeString(Integer.valueOf(1234567), signed6) should be ("\u0000\u0000\u0012\u0034\u0056\u007F")
    DemoSupport.writeString(jl.Long.valueOf(-12345678900L), signed6) should be ("\u0012\u0034\u0056\u0078\u0090\u000D")
    DemoSupport.writeString(jl.Long.valueOf(12345678900L), signed12) should be ("\u0000\u0000\u0000\u0000\u0000\u0000\u0012\u0034\u0056\u0078\u0090\u000F")
    DemoSupport.writeString(new jm.BigInteger("-12345678901234567890123"), signed12) should be ("\u0012\u0034\u0056\u0078\u0090\u0012\u0034\u0056\u0078\u0090\u0012\u003D")
  }
  
  val implied3 = PackedDecimalFormat(3, 2, true)
  val implied6 = PackedDecimalFormat(6, -2, true)
  val implied12 = PackedDecimalFormat(12, 6, true)
  
  behavior of "Packed decimal implied decimal form"
  
  it should "parse input correctly" in {
    rawParse("\u0000\u0012\u003F", implied3) should be (new jm.BigDecimal("1.23"))
    // note: .45 causes comparison error for next case
    rawParse("\u0012\u0032\u005D", implied3) should be (new jm.BigDecimal(-123.25))
    rawParse("\u0000\u0000\u0012\u0034\u0056\u007B", implied6) should be (Integer.valueOf(-123456700))
    rawParse("\u0012\u0034\u0056\u0078\u0090\u000F", implied6) should be (jl.Long.valueOf(1234567890000L))
    rawParse("\u0000\u0000\u0000\u0000\u0000\u0000\u0012\u0034\u0056\u0078\u0090\u000D", implied12) should be (new jm.BigDecimal("-12345.678900"))
    rawParse("\u0012\u0034\u0056\u0078\u0090\u0012\u0034\u0056\u0078\u0090\u0012\u003F", implied12) should be (new jm.BigDecimal("12345678901234567.890123"))
  }
  
  it should "write output correctly" in {
    DemoSupport.writeString(new jm.BigDecimal("-1.23"), implied3) should be ("\u0000\u0012\u003D")
    DemoSupport.writeString(Integer.valueOf(12345), implied3) should be ("\u0034\u0050\u000F")
    DemoSupport.writeString(Integer.valueOf(-123456789), implied3) should be ("\u0078\u0090\u000D")
    DemoSupport.writeString(Integer.valueOf(-123456700), implied6) should be ("\u0000\u0000\u0012\u0034\u0056\u007D")
    DemoSupport.writeString(jl.Long.valueOf(-1234567890000L), implied6) should be ("\u0012\u0034\u0056\u0078\u0090\u000D")
    DemoSupport.writeString(new jm.BigDecimal("12345.678900"), implied12) should be ("\u0000\u0000\u0000\u0000\u0000\u0000\u0012\u0034\u0056\u0078\u0090\u000F")
    DemoSupport.writeString(new jm.BigDecimal("-12345678901234567.890123"), implied12) should be ("\u0012\u0034\u0056\u0078\u0090\u0012\u0034\u0056\u0078\u0090\u0012\u003D")
  }
}