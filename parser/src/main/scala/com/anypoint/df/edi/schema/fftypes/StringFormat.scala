package com.anypoint.df.edi.schema.fftypes

import com.anypoint.df.edi.lexical.{ LexerBase, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

case class StringValue(code: String, width: Int, fill: StringSpaceFill) extends StringFormatBase(code, width, width, fill) {
  override def parseToken(lexer: LexerBase): Object = null
  override def buildToken(value: Object, writer: WriterBase): String = {
    value match {
      case n: Number =>
      case _ => wrongType(value, writer)
    }
    ""
  }
}