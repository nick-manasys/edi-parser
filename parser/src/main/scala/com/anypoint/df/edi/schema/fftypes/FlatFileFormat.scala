package com.anypoint.df.edi.schema.fftypes

import java.{ util => ju }
import com.anypoint.df.edi.lexical.TypeFormat
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.schema.SchemaJavaDefs

trait FlatFileYaml {
  val fillKey = "justify"
  val numberSignKey = "sign"
  val numberFillKey = "justify"
  val implicitKey = "implicit"
  val boolReprKey = "represent"
  val formatKey = "format"
  val patternKey = "pattern"
  val localeKey = "locale"
  val signedKey = "signed"

  val defaultFill = FillMode.LEFT
  val defaultSign = NumberSign.OPTIONAL
  val defaultNumberFill = FillMode.RIGHT
  val defaultBoolRepr = BooleanRepresentation.ALPHA_LOWER
}

trait FlatFileFormat extends SchemaJavaDefs with FlatFileYaml {
  type pairWriter = (String, Object) => Unit
  
  // utility methods for writing format parameters
  protected def writeFill(fill: FillMode, writer: pairWriter) = {
    if (fill != defaultFill) writer(fillKey, fill.name)
  }
  protected def writeSign(sign: NumberSign, writer: pairWriter) = {
    if (sign != defaultSign) writer(numberSignKey, sign.name)
  }
  protected def writeNumberFill(pad: FillMode, writer: pairWriter) = {
    if (pad != defaultNumberFill) writer(numberFillKey, pad.name)
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
  protected def getFill(map: ValueMap): FillMode = {
    if (map != null && map.containsKey(fillKey)) {
      val code = getAsString(fillKey, map)
      try {
          FillMode.valueOf(code.toUpperCase)
      } catch {
        case e: IllegalArgumentException => throw new IllegalArgumentException(s"Unknown value '$code' for $fillKey")
      }
    } else defaultFill
  }
  protected def getSign(map: ValueMap): NumberSign = {
    if (map != null && map.containsKey(numberSignKey)) {
      val code = getAsString(numberSignKey, map)
      try {
          NumberSign.valueOf(code.toUpperCase)
      } catch {
        case e: IllegalArgumentException => throw new IllegalArgumentException(s"Unknown value '$code' for $numberSignKey")
      }
    } else defaultSign
  }
  protected def getNumberFill(map: ValueMap): FillMode = {
    if (map != null && map.containsKey(fillKey)) {
      val code = getAsString(fillKey, map)
      try {
          FillMode.valueOf(code.toUpperCase)
      } catch {
        case e: IllegalArgumentException => throw new IllegalArgumentException(s"Unknown value '$code' for $fillKey")
      }
    } else defaultNumberFill
  }
  protected def getBooleanRepresentation(map: ValueMap): BooleanRepresentation = {
    if (map != null && map.containsKey(boolReprKey)) {
      val code = getAsString(boolReprKey, map)
      try {
          BooleanRepresentation.valueOf(code.toUpperCase)
      } catch {
        case e: IllegalArgumentException => throw new IllegalArgumentException(s"Unknown value '$code' for $boolReprKey")
      }
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
    DecimalFormat.code -> DecimalFormat, PackedDecimalFormat.code -> PackedDecimalFormat,
    StringFormat.code -> StringFormat)
}

object CopybookFormats {
  val copybookFactories = Map[String, FormatFactory](BooleanFormat.code -> BooleanFormat,
    IntegerFormat.code -> IntegerFormat, LocalDateFormat.code -> LocalDateFormat,
    LocalDateTimeFormat.code -> LocalDateTimeFormat, LocalTimeFormat.code -> LocalTimeFormat,
    DecimalFormat.code -> DecimalFormat, PackedDecimalFormat.code -> PackedDecimalFormat,
    StringFormat.code -> StringFormat)
}