package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl, text => jt, util => ju }
import java.io.IOException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import org.threeten.bp.{ LocalDate, OffsetDateTime, ZonedDateTime, ZoneId, ZoneOffset }

import com.anypoint.df.edi.lexical.LexicalException
import com.anypoint.df.edi.lexical.TypeFormatConstants._

class LocalDateFormatTest extends FlatSpec with Matchers {
  
  val format4 = LocalDateFormat(4, FillMode.LEFT)
  val format6 = LocalDateFormat(6, FillMode.LEFT)
  val format8 = LocalDateFormat(8, FillMode.LEFT)
  
  behavior of "Local date format"
  
  it should "parse input correctly" in {
    DemoSupport.parseString("2016", format4) should be (LocalDate.of(2016, 1, 1))
    DemoSupport.parseString("201602", format6) should be (LocalDate.of(2016, 2, 1))
    DemoSupport.parseString("20160208", format8) should be (LocalDate.of(2016, 2, 8))
  }
  
  it should "write output correctly" in {
    val date1 = LocalDate.of(2016, 2, 8)
    DemoSupport.writeString(date1, format4) should be ("2016")
    DemoSupport.writeString(date1, format6) should be ("201602")
    DemoSupport.writeString(date1, format8) should be ("20160208")
    val date2 = ZonedDateTime.of(2016, 2, 8, 0, 0, 0, 0, ZoneId.of("Z"))
    DemoSupport.writeString(date2, format4) should be ("2016")
    DemoSupport.writeString(date2, format6) should be ("201602")
    DemoSupport.writeString(date2, format8) should be ("20160208")
    val date3 = OffsetDateTime.of(2016, 2, 8, 0, 0, 0, 0, ZoneOffset.UTC)
    DemoSupport.writeString(date3, format4) should be ("2016")
    DemoSupport.writeString(date3, format6) should be ("201602")
    DemoSupport.writeString(date3, format8) should be ("20160208")
  }
  
  it should "throw exception on invalid value" in {
    intercept[IOException] { DemoSupport.writeString("01234567890", format4) }
  }
}