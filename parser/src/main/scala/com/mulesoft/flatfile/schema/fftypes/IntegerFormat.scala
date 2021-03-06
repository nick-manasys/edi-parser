package com.mulesoft.flatfile.schema.fftypes

import spire.math.Number
import java.{ lang => jl, math => jm, text => jt }
import java.util.Locale
import com.mulesoft.flatfile.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.formats.{ NumberFormatBase, StringFormatBase }

object IntegerFormat extends FormatFactory {

  def code = "Integer"

  case class IntegerFormatImpl(width: Int, sign: NumberSign, fill: FillMode, zoned: Boolean)
    extends ZonedFormatBase(code, width, sign, fill, zoned) {
  
    override def genericType = GenericType.INTEGER

    override def parse(lexer: LexerBase) = {
      val neg = zoned && checkNegative(lexer.tokenBuilder)
      val digits = checkIntegerFormat(lexer)
      val value = convertSizedInteger(lexer, digits)
      if (neg) value match {
        case i: Integer       => Integer.valueOf(-i.intValue)
        case l: jl.Long       => jl.Long.valueOf(-l.longValue)
        case b: jm.BigInteger => b.negate
      } else value
    }

    override def write(value: Object, writer: WriterBase) = {
      writer.startToken
      value match {
        case n: Number =>
          if (n.canBeInt) writeIntegerValue(Integer.valueOf(n.toInt), writer)
          else if (n.canBeLong) writeIntegerValue(jl.Long.valueOf(n.toLong), writer)
          else writeIntegerValue(n.toBigInt.bigInteger, writer)
        case n: jl.Number =>
          writeIntegerValue(value, writer)
        case _ =>
          wrongType(value, writer)
          writePadded("0", false, writer)
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeSign(sign, writer)
      writeNumberFill(fill, writer)
      writeZoned(zoned, writer)
    }
  }

  case class IntegerPatternImpl(width: Int, pattern: String, locale: Locale, fill: FillMode)
    extends StringFormatBase(code, width, width, fill) with FlatFileFormat {
  
    override def genericType = GenericType.INTEGER

    private def buildFormat = new jt.DecimalFormat(pattern, new jt.DecimalFormatSymbols(locale))

    override def parseToken(lexer: LexerBase) = {
      val format = buildFormat
      format.setParseIntegerOnly(true)
      format.parse(lexer.token)
    }

    override def buildToken(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
          if (n.canBeInt) buildFormat.format(Integer.valueOf(n.toInt))
          else if (n.canBeLong) buildFormat.format(jl.Long.valueOf(n.toLong))
          else buildFormat.format(n.toBigInt.bigInteger)
        case n: jl.Number =>
          buildFormat.format(n)
        case _ =>
          wrongType(value, writer)
          ""
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writer(patternKey, pattern)
      writer(localeKey, locale.getCountry)
    }
  }

  def apply(width: Int, sign: NumberSign, fill: FillMode, zoned: Boolean): TypeFormat =
    IntegerFormatImpl(width, sign, fill, zoned)
  def apply(width: Int, sign: NumberSign, fill: FillMode): TypeFormat = apply(width, sign, fill, false)
  def apply(width: Int, pattern: String, locale: Locale, fill: FillMode): TypeFormat =
    IntegerPatternImpl(width, pattern, locale, fill)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      val locale = getLocale(map)
      val fill = getFill(map)
      apply(width, pattern, locale, fill)
    } else {
      val sign = getSign(map)
      val fill = getNumberFill(map)
      val zoned = getZoned(map)
      apply(width, sign, fill, zoned)
    }
  }
}