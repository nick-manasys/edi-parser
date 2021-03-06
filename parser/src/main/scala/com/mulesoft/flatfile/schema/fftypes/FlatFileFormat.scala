package com.mulesoft.flatfile.schema.fftypes

import com.mulesoft.flatfile.lexical.TypeFormat
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.schema.SchemaJavaDefs
import java.{ lang => jl, util => ju }
import scala.annotation.tailrec

trait FlatFileYaml {
  val fillKey = "justify"
  val numberSignKey = "sign"
  val numberFillKey = "justify"
  val implicitKey = "implicit"
  val boolReprKey = "represent"
  val caseKey = "caseSensitive"
  val formatKey = "format"
  val patternKey = "pattern"
  val localeKey = "locale"
  val signedKey = "signed"
  val zonedKey = "zoned"
  val digitsKey = "digits"

  val defaultFill = FillMode.LEFT
  val defaultSign = NumberSign.OPTIONAL
  val defaultNumberFill = FillMode.RIGHT
  val defaultBoolRepr = "true|false"
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
  protected def writeBooleanRepresentation(t: String, f: String, writer: pairWriter) = {
    val repr = t + '|' + f
    if (repr != defaultBoolRepr) writer(boolReprKey, repr)
  }
  protected def writeCaseSensitive(sensitive: Boolean, writer: pairWriter) = {
    if (sensitive) writer(caseKey, "TRUE")
  }
  protected def writePattern(pattern: String, writer: pairWriter) = {
    writer(patternKey, pattern)
  }
  protected def writeLocale(locale: ju.Locale, writer: pairWriter) = {
    writer(localeKey, locale.getCountry)
  }
  protected def writeImplicit(impl: Int, writer: pairWriter) = {
    if (impl != 0) writer(implicitKey, Integer.valueOf(impl))
  }
  protected def writeSigned(signed: Boolean, writer: pairWriter) = {
    if (signed) writer(signedKey, "TRUE")
  }
  protected def writeZoned(zoned: Boolean, writer: pairWriter) = {
    if (zoned) writer(zonedKey, "TRUE")
  }
  protected def writeDigits(digits: Int, writer: pairWriter) = {
    writer(digitsKey, Integer.valueOf(digits))
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
  protected def getBooleanRepresentation(map: ValueMap): (String, String) = {
    val repr = if (map != null && map.containsKey(boolReprKey)) getAsString(boolReprKey, map) else defaultBoolRepr
    val split = repr.indexOf('|')
    if (split < 0) throw new IllegalArgumentException("Missing required '|' separator in boolean representation")
    (repr.take(split), repr.drop(split + 1))
  }
  protected def getCaseSensitive(map: ValueMap): Boolean = {
    map != null && map.containsKey(caseKey) && getAsString(caseKey, map).toUpperCase == "TRUE"
  }
  protected def getPattern(map: ValueMap): String = {
    getAsString(patternKey, map)
  }
  protected def getLocale(map: ValueMap): ju.Locale = {
    val country = getAsString(localeKey, map)
    new ju.Locale(country)
  }
  protected def getImplicit(map: ValueMap): Int = {
    if (map != null && map.containsKey(implicitKey)) getAsInt(implicitKey, map) else 0
  }
  protected def getSigned(map: ValueMap): Boolean = {
    map != null && map.containsKey(signedKey) && map.get(signedKey).asInstanceOf[jl.Boolean]
  }
  protected def getZoned(map: ValueMap): Boolean = {
    map != null && map.containsKey(zonedKey) && map.get(zonedKey).asInstanceOf[jl.Boolean]
  }
  protected def getDigits(map: ValueMap): Int = {
    if (map != null && map.containsKey(digitsKey)) getAsInt(digitsKey, map) else 0
  }

  def readFormat(width: Int, map: ValueMap): TypeFormat
}

object FixedWidthFormats {
  val fixedFactories = Map[String, FormatFactory](BinaryFormat.code -> BinaryFormat,
    BooleanFormat.code -> BooleanFormat, IntegerFormat.code -> IntegerFormat, LocalDateFormat.code -> LocalDateFormat,
    LocalDateTimeFormat.code -> LocalDateTimeFormat, LocalTimeFormat.code -> LocalTimeFormat,
    DecimalFormat.code -> DecimalFormat, PackedDecimalFormat.code -> PackedDecimalFormat,
    StringFormat.code -> StringFormat)
}

object CopybookFormats {
  val copybookFactories = Map[String, FormatFactory](BinaryFormat.code -> BinaryFormat,
    BooleanFormat.code -> BooleanFormat, IntegerFormat.code -> IntegerFormat, LocalDateFormat.code -> LocalDateFormat,
    LocalDateTimeFormat.code -> LocalDateTimeFormat, LocalTimeFormat.code -> LocalTimeFormat,
    DecimalFormat.code -> DecimalFormat, PackedDecimalFormat.code -> PackedDecimalFormat,
    StringFormat.code -> StringFormat)
}