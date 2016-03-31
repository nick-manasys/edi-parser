package com.anypoint.df.edi.schema.fftypes

import spire.math.Number

import java.text.{ DecimalFormat, DecimalFormatSymbols }
import java.util.Locale

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.{ NumberFormatBase, TypeFormatBase }

object IntegerFormat extends FormatFactory {

  def code = "Integer"

  case class IntegerFormatImpl(width: Int, sign: NumberSign, pad: NumberPad)
      extends NumberFormatBase(code, width, width, sign, true, pad) with FlatFileFormat {

    override def parse(lexer: LexerBase) = {
      val digits = checkIntegerFormat(lexer);
      convertSizedInteger(lexer, digits);
    }

    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
          writer.startToken
          writeIntegerValue(value, writer)
        case _ =>
          wrongType(value, writer)
          writer.writeToken("")
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeSign(sign, writer)
      writeNumberPad(pad, writer)
    }
  }

  case class IntegerPatternImpl(width: Int, pattern: String, locale: Locale)
      extends TypeFormatBase(code, width, width) with FlatFileFormat {

    private def buildFormat = new DecimalFormat(pattern, new DecimalFormatSymbols(locale))

    override def parse(lexer: LexerBase) = {
      val format = buildFormat
      format.setParseIntegerOnly(true)
      format.parse(lexer.token)
    }

    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
          writer.writeEscaped(buildFormat.format(value))
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

  def apply(width: Int, sign: NumberSign, pad: NumberPad): TypeFormat = IntegerFormatImpl(width, sign, pad)
  def apply(width: Int, pattern: String, locale: Locale): TypeFormat = IntegerPatternImpl(width, pattern, locale)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      val locale = getLocale(map)
      apply(width, pattern, locale)
    } else {
      val sign = getSign(map)
      val pad = getNumberPad(map)
      apply(width, sign, pad)
    }
  }
}
