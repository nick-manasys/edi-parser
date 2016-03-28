package com.anypoint.df.edi.schema.fftypes

import spire.math.Number

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.NumberFormatBase

object IntegerFormat extends FlatFileFormat {
  
  def code = "Integer"
  
  case class IntegerFormatImpl(width: Int, sign: NumberSign, pad: NumberPad, pattern: String)
      extends NumberFormatBase(code, width, width, sign, true, pad) {
    override def parse(lexer: LexerBase) = null
    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
        case _ => wrongType(value, writer)
      }
    }
  }
  
  def apply(width: Int, sign: NumberSign, pad: NumberPad): TypeFormat = IntegerFormatImpl(width, sign, pad, null)
  def apply(width: Int, pattern: String): TypeFormat = IntegerFormatImpl(width, null, null, pattern)
}
