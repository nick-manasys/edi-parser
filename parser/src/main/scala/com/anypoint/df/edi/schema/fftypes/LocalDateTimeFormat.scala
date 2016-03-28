package com.anypoint.df.edi.schema.fftypes

import org.threeten.bp.LocalDateTime

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object LocalDateTimeFormat {

  case class LocalDateTimeFormatImpl(code: String, width: Int, fill: StringSpaceFill, format: String)
      extends StringFormatBase(code, width, width, fill) {
    override def parseToken(lexer: LexerBase): Object = null
    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case d: LocalDateTime =>
        case _ => wrongType(value, writer)
      }
      ""
    }
  }

  def apply(width: Int, fill: StringSpaceFill): TypeFormat = LocalDateTimeFormatImpl("DateTime", width, fill, null)
  def apply(width: Int, fill: StringSpaceFill, format: String): TypeFormat = LocalDateTimeFormatImpl("DateTime", width, fill, format)
}
