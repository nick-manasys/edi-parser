package com.anypoint.df.edi.schema.fftypes

import java.{ util => ju }
import com.anypoint.df.edi.lexical.TypeFormat
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.schema.SchemaJavaDefs

trait FlatFileYaml {
  val fillKey = "justify"
  val numberSignKey = "sign"
  val numberPadKey = "justify"
  val boolReprKey = "represent"
  val formatKey = "format"
  val patternKey = "pattern"
  val localeKey = "locale"

  val defaultFill = StringSpaceFill.LEFT
  val defaultSign = NumberSign.OPTIONAL
  val defaultNumberPad = NumberPad.SPACE_LEFT
  val defaultBoolRepr = BooleanRepresentation.ALPHA_LOWER
}

trait FlatFileFormat extends SchemaJavaDefs with FlatFileYaml {
  type pairWriter = (String, Object) => Unit
  
  // utility methods for writing format parameters
  protected def writeFill(fill: StringSpaceFill, writer: pairWriter) = {
    if (fill != defaultFill) writer(fillKey, fill.name)
  }
  protected def writeSign(sign: NumberSign, writer: pairWriter) = {
    if (sign != defaultSign) writer(numberSignKey, sign.name)
  }
  protected def writeNumberPad(pad: NumberPad, writer: pairWriter) = {
    if (pad != defaultNumberPad) writer(numberPadKey, pad.name)
  }
  protected def writeBooleanRepresentation(repr: BooleanRepresentation, writer: pairWriter) = {
    if (repr != defaultBoolRepr) writer(boolReprKey, repr.name)
  }
  protected def writePattern(pattern: String, writer: pairWriter) = {
    writer(patternKey, pattern)
  }
  protected def writeLocale(locale: ju.Locale, writer: pairWriter) = {
    writer(localeKey, locale.getCountry)
  }
  
  def writeOptions(writer: pairWriter): Unit
}

trait FormatFactory extends SchemaJavaDefs with FlatFileYaml {

  // utility methods for retrieving format parameters
  protected def getFill(map: ValueMap): StringSpaceFill = {
    if (map != null && map.containsKey(fillKey)) {
      val code = getAsString(fillKey, map)
      StringSpaceFill.valueOf(code.toUpperCase)
    } else defaultFill
  }
  protected def getSign(map: ValueMap): NumberSign = {
    if (map != null && map.containsKey(numberSignKey)) {
      val code = getAsString(numberSignKey, map)
      NumberSign.valueOf(code.toUpperCase)
    } else defaultSign
  }
  protected def getNumberPad(map: ValueMap): NumberPad = {
    if (map != null && map.containsKey(numberPadKey)) {
      val code = getAsString(numberPadKey, map)
      NumberPad.valueOf(code.toUpperCase)
    } else defaultNumberPad
  }
  protected def getBooleanRepresentation(map: ValueMap): BooleanRepresentation = {
    if (map != null && map.containsKey(boolReprKey)) {
      val code = getAsString(boolReprKey, map)
      BooleanRepresentation.valueOf(code.toUpperCase)
    } else defaultBoolRepr
  }
  protected def getPattern(map: ValueMap): String = {
    getAsString(patternKey, map)
  }
  protected def getLocale(map: ValueMap): ju.Locale = {
    val country = getAsString(localeKey, map)
    new ju.Locale(country)
  }

  def readFormat(width: Int, map: ValueMap): TypeFormat
}

object FixedWidthFormats {
  val fixedFactories = Map[String, FormatFactory](BooleanFormat.code -> BooleanFormat,
    IntegerFormat.code -> IntegerFormat, LocalDateFormat.code -> LocalDateFormat,
    LocalDateTimeFormat.code -> LocalDateTimeFormat, LocalTimeFormat.code -> LocalTimeFormat,
    RealFormat.code -> RealFormat, StringFormat.code -> StringFormat)
}

object CopybookFormats {
  val copybookFactories = Map[String, FormatFactory](BooleanFormat.code -> BooleanFormat,
    IntegerFormat.code -> IntegerFormat, LocalDateFormat.code -> LocalDateFormat,
    LocalDateTimeFormat.code -> LocalDateTimeFormat, LocalTimeFormat.code -> LocalTimeFormat,
    RealFormat.code -> RealFormat, StringFormat.code -> StringFormat)
}