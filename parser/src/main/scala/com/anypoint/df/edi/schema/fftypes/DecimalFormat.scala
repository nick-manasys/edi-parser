package com.anypoint.df.edi.schema.fftypes

import spire.math.Number

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.{ DecimalFormatBase, NumberFormatBase, TypeFormatBase }

import java.{ math => jm, text => jt }
import java.util.Locale

object DecimalFormat extends FormatFactory {
  
  def code = "Decimal"
  
  case class DecimalFormatImpl(width: Int, sign: NumberSign, fill: FillMode)
      extends DecimalFormatBase(code, width, width, sign, true, fill, true, false, false, false) with FlatFileFormat {
    
    override def parse(lexer: LexerBase) = convertDecimalValue(lexer)
    
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
      writeNumberFill(fill, writer)
    }
  }
  
  case class DecimalImplicitImpl(width: Int, impl: Int, sign: NumberSign, fill: FillMode)
      extends NumberFormatBase(code, width, width, sign, true, fill) with FlatFileFormat {
    
    override def parse(lexer: LexerBase) = {
        checkIntegerFormat(lexer);
        new jm.BigDecimal(new jm.BigInteger(lexer.token()), impl);
    }
    
    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
          writer.startToken
          value match {
            case d: jm.BigDecimal =>
              val adjusted = d.movePointRight(impl).setScale(impl, jm.RoundingMode.HALF_UP)
              writeBigInteger(adjusted.toBigIntegerExact, writer)
          }
          writeIntegerValue(value, writer)
        case _ =>
          wrongType(value, writer)
          writer.writeToken("")
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writer(implicitKey, Integer.valueOf(impl))
      writeSign(sign, writer)
      writeNumberFill(fill, writer)
    }
  }
  
  case class DecimalPatternImpl(width: Int, pattern: String, locale: Locale)
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
          writer.writeEscaped(buildFormat.format(value))
        case _ =>
          wrongType(value, writer)
          writer.writeToken("")
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writePattern(pattern, writer)
      writeLocale(locale, writer)
    }
  }
  
  def apply(width: Int, sign: NumberSign, fill: FillMode): TypeFormat = DecimalFormatImpl(width, sign, fill)
  def apply(width: Int, sign: NumberSign, impl: Int, fill: FillMode): TypeFormat = DecimalImplicitImpl(width, impl, sign, fill)
  def apply(width: Int, pattern: String, locale: Locale): TypeFormat = DecimalPatternImpl(width, pattern, locale)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      val locale = getLocale(map)
      apply(width, pattern, locale)
    } else if (map != null && map.containsKey(implicitKey)) {
      val impl = getRequiredInt(implicitKey, map)
      val sign = getSign(map)
      val fill = getNumberFill(map)
      apply(width, sign, impl, fill)
    } else {
      val sign = getSign(map)
      val fill = getNumberFill(map)
      apply(width, sign, fill)
    }
  }
}