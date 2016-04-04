package com.anypoint.df.edi.schema.fftypes

import spire.math.Number

import java.{ lang => jl, text => jt }
import java.util.Locale

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.{ NumberFormatBase, TypeFormatBase }

object IntegerFormat extends FormatFactory {

  def code = "Integer"

  case class IntegerFormatImpl(width: Int, sign: NumberSign, fill: FillMode)
      extends NumberFormatBase(code, width, width, sign, true, fill) with FlatFileFormat {

    override def parse(lexer: LexerBase) = {
      val digits = checkIntegerFormat(lexer);
      convertSizedInteger(lexer, digits);
    }

    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
          writer.startToken
          if (n.canBeInt) writeIntegerValue(Integer.valueOf(n.toInt), writer)
          else if (n.canBeLong) writeIntegerValue(jl.Long.valueOf(n.toLong), writer)
          else writeIntegerValue(n.toBigInt.bigInteger, writer)
        case n: jl.Number =>
          writer.startToken
          writeIntegerValue(value, writer)
        case _ =>
          wrongType(value, writer)
          writer.writeToken("")
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeSign(sign, writer)
      writeNumberFill(fill, writer)
    }
  }

  case class IntegerPatternImpl(width: Int, pattern: String, locale: Locale)
      extends TypeFormatBase(code, width, width) with FlatFileFormat {

    private def buildFormat = new jt.DecimalFormat(pattern, new jt.DecimalFormatSymbols(locale))

    override def parse(lexer: LexerBase) = {
      val format = buildFormat
      format.setParseIntegerOnly(true)
      format.parse(lexer.token)
    }

    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
          if (n.canBeInt) writer.writeEscaped(buildFormat.format(Integer.valueOf(n.toInt)))
          else if (n.canBeLong) writer.writeEscaped(buildFormat.format(jl.Long.valueOf(n.toLong)))
          else writer.writeEscaped(buildFormat.format(n.toBigInt.bigInteger))
        case n: jl.Number =>
          writer.writeEscaped(buildFormat.format(n))
        case _ =>
          wrongType(value, writer)
          writer.writeToken("")
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writer(patternKey, pattern)
      writer(localeKey, locale.getCountry)
    }
  }

  def apply(width: Int, sign: NumberSign, fill: FillMode): TypeFormat = IntegerFormatImpl(width, sign, fill)
  def apply(width: Int, pattern: String, locale: Locale): TypeFormat = IntegerPatternImpl(width, pattern, locale)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      val locale = getLocale(map)
      apply(width, pattern, locale)
    } else {
      val sign = getSign(map)
      val fill = getNumberFill(map)
      apply(width, sign, fill)
    }
  }
}