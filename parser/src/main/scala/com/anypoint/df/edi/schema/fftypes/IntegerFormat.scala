package com.anypoint.df.edi.schema.fftypes

import spire.math.Number

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.NumberFormatBase

object IntegerFormat {
  
  case class IntegerFormatImpl(code: String, width: Int, sign: NumberSign, pad: NumberPad, format: String)
      extends NumberFormatBase(code, width, width, sign, true, pad) {
    override def parse(lexer: LexerBase) = null
    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
        case _ => wrongType(value, writer)
      }
    }
  }
  
  def apply(width: Int, sign: NumberSign, pad: NumberPad): TypeFormat = IntegerFormatImpl("Integer", width, sign, pad, null)
  def apply(width: Int, format: String): TypeFormat = IntegerFormatImpl("Integer", width, null, null, format)
}
