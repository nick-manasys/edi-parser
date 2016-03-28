package com.anypoint.df.edi.schema.fftypes

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object StringFormat extends FlatFileFormat {
  
  def code = "String"
  
  case class StringFormatImpl(width: Int, fill: StringSpaceFill)
      extends StringFormatBase(code, width, width, fill) {
    override def parseToken(lexer: LexerBase): Object = null
    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case n: Number =>
        case _ => wrongType(value, writer)
      }
      ""
    }
  }
  
  def apply(width: Int, fill: StringSpaceFill): TypeFormat = StringFormatImpl(width, fill)
}