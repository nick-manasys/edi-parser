package com.mulesoft.flatfile.schema.fftypes

import java.{ lang => jl, text => jt, util => ju }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import org.threeten.bp.{ LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime, ZoneId, ZoneOffset }

import com.mulesoft.flatfile.lexical.LexicalException
import com.mulesoft.flatfile.lexical.TypeFormatConstants._

class LocalTimeFormatTest extends FlatSpec with Matchers {
  
  val format2 = LocalTimeFormat(2, FillMode.LEFT)
  val format4 = LocalTimeFormat(4, FillMode.LEFT)
  val format6 = LocalTimeFormat(6, FillMode.LEFT)
  val format7 = LocalTimeFormat(7, FillMode.LEFT)
  val format9 = LocalTimeFormat(9, FillMode.LEFT)
  
  behavior of "Local time format"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("02", format2) should be (LocalTime.of(2, 0))
    DemoSupport.parseString("0203", format4) should be (LocalTime.of(2, 3))
    DemoSupport.parseString("020304", format6) should be (LocalTime.of(2, 3, 4))
    DemoSupport.parseString("0203045", format7) should be (LocalTime.of(2, 3, 4, 500000000))
    DemoSupport.parseString("020304567", format9) should be (LocalTime.of(2, 3, 4, 567000000))
  }
  
  it should "write output correctly" in {
    val date1 = LocalTime.of(1, 2, 3, 456000000)
    DemoSupport.writeString(date1, format2) should be ("01")
    DemoSupport.writeString(date1, format4) should be ("0102")
    DemoSupport.writeString(date1, format6) should be ("010203")
    DemoSupport.writeString(date1, format7) should be ("0102034")
    DemoSupport.writeString(date1, format9) should be ("010203456")
    val date2 = LocalDateTime.of(2016, 2, 8, 1, 2, 3, 456000000)
    DemoSupport.writeString(date2, format2) should be ("01")
    DemoSupport.writeString(date2, format4) should be ("0102")
    DemoSupport.writeString(date2, format6) should be ("010203")
    DemoSupport.writeString(date2, format9) should be ("010203456")
    val date3 = ZonedDateTime.of(2016, 2, 8, 1, 2, 3, 456000000, ZoneId.of("Z"))
    DemoSupport.writeString(date3, format2) should be ("01")
    DemoSupport.writeString(date3, format4) should be ("0102")
    DemoSupport.writeString(date3, format6) should be ("010203")
    DemoSupport.writeString(date3, format9) should be ("010203456")
    val date4 = OffsetDateTime.of(2016, 2, 8, 1, 2, 3, 456000000, ZoneOffset.UTC)
    DemoSupport.writeString(date4, format2) should be ("01")
    DemoSupport.writeString(date4, format4) should be ("0102")
    DemoSupport.writeString(date4, format6) should be ("010203")
    DemoSupport.writeString(date4, format9) should be ("010203456")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("01234567890", format4) }
  }
}