package com.anypoint.df.edi.schema.fftypes

import spire.math.Number
import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.{ NumberFormatBase, TypeFormatBase }
import java.util.Locale

object RealFormat extends FormatFactory {
  
  def code = "Real"
  
  case class RealFormatImpl(width: Int, impl: Int, sign: NumberSign, pad: NumberPad, pattern: String)
      extends NumberFormatBase(code, width, width, sign, true, pad) with FlatFileFormat {
    
    override def parse(lexer: LexerBase) = null
    
    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
        case _ => wrongType(value, writer)
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeSign(sign, writer)
      writeNumberPad(pad, writer)
    }
  }
  
  case class RealPatternImpl(width: Int, pattern: String, locale: Locale)
      extends TypeFormatBase(code, width, width) with FlatFileFormat {
    
    override def parse(lexer: LexerBase) = null
    
    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
        case _ => wrongType(value, writer)
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writePattern(pattern, writer)
      writeLocale(locale, writer)
    }
  }
  
  def apply(width: Int, sign: NumberSign, pad: NumberPad): TypeFormat = RealFormatImpl(width, -1, sign, pad, null)
  def apply(width: Int, sign: NumberSign, impl: Int, pad: NumberPad): TypeFormat = RealFormatImpl(width, impl, sign, pad, null)
  def apply(width: Int, pattern: String, locale: Locale): TypeFormat = RealFormatImpl(width, -1, null, null, pattern)

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