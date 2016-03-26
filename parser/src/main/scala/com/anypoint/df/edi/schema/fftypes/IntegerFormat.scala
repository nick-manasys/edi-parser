package com.anypoint.df.edi.schema.fftypes

import spire.math.Number

import com.anypoint.df.edi.lexical.{ LexerBase, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.NumberFormatBase

case class IntegerValue(code: String, width: Int) extends NumberFormatBase(code, width, width, NumberSign.NEGATIVE_ONLY, true, NumberPad.UNPADDED) {
  override def parse(lexer: LexerBase) = null
  override def write(value: Object, writer: WriterBase) = {
    value match {
      case n: Number =>
      case _ => wrongType(value, writer)
    }
  }
}