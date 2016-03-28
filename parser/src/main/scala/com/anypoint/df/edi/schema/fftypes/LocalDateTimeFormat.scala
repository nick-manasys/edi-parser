package com.anypoint.df.edi.schema.fftypes

import org.threeten.bp.LocalDateTime

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object LocalDateTimeFormat extends FlatFileFormat {
  
  def code = "DateTime"

  case class LocalDateTimeFormatImpl(width: Int, fill: StringSpaceFill, pattern: String)
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

  def apply(width: Int, fill: StringSpaceFill): TypeFormat = LocalDateTimeFormatImpl(width, fill, null)
  def apply(width: Int, fill: StringSpaceFill, pattern: String): TypeFormat = LocalDateTimeFormatImpl(width, fill, pattern)
}
