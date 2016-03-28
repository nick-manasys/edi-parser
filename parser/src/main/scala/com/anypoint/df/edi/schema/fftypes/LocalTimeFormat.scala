package com.anypoint.df.edi.schema.fftypes

import org.threeten.bp.LocalTime

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object LocalTimeFormat {

  case class LocalTimeFormatImpl(code: String, width: Int, fill: StringSpaceFill, format: String)
      extends StringFormatBase(code, width, width, fill) {
    override def parseToken(lexer: LexerBase): Object = null
    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case t: LocalTime =>
        case _ => wrongType(value, writer)
      }
      ""
    }
  }

  def apply(width: Int, fill: StringSpaceFill): TypeFormat = LocalTimeFormatImpl("Time", width, fill, null)
  def apply(width: Int, fill: StringSpaceFill, format: String): TypeFormat = LocalTimeFormatImpl("Time", width, fill, format)
}
