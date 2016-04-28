package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl, text => jt, util => ju }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import org.threeten.bp.{ LocalDateTime, OffsetDateTime, ZonedDateTime, ZoneId, ZoneOffset }

import com.anypoint.df.edi.lexical.LexicalException
import com.anypoint.df.edi.lexical.TypeFormatConstants._

class LocalDateTimeFormatTest extends FlatSpec with Matchers {
  
  val format12 = LocalDateTimeFormat(12, FillMode.LEFT)
  val format14 = LocalDateTimeFormat(14, FillMode.LEFT)
  
  behavior of "Local date time format"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("201602080102", format12) should be (LocalDateTime.of(2016, 2, 8, 1, 2))
    DemoSupport.parseString("20160208010203", format14) should be (LocalDateTime.of(2016, 2, 8, 1, 2, 3))
  }
  
  it should "write output correctly" in {
    val date1 = LocalDateTime.of(2016, 2, 8, 1, 2, 3, 456000000)
    DemoSupport.writeString(date1, format12) should be ("201602080102")
    DemoSupport.writeString(date1, format14) should be ("20160208010203")
    val date2 = ZonedDateTime.of(2016, 2, 8, 1, 2, 3, 456000000, ZoneId.of("Z"))
    DemoSupport.writeString(date2, format12) should be ("201602080102")
    DemoSupport.writeString(date2, format14) should be ("20160208010203")
    val date3 = OffsetDateTime.of(2016, 2, 8, 1, 2, 3, 456000000, ZoneOffset.UTC)
    DemoSupport.writeString(date3, format12) should be ("201602080102")
    DemoSupport.writeString(date3, format14) should be ("20160208010203")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("01234567890", format14) }
  }
}