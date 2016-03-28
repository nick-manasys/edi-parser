package com.anypoint.df.edi.schema.fftypes

import org.threeten.bp.LocalDate

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object LocalDateFormat extends FlatFileFormat {
  
  def code = "Date"

  case class LocalDateFormatImpl(width: Int, fill: StringSpaceFill, pattern: String)
      extends StringFormatBase(code, width, width, fill) {
    override def parseToken(lexer: LexerBase): Object = null
    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case d: LocalDate =>
        case _ => wrongType(value, writer)
      }
      ""
    }
  }

  def apply(width: Int, fill: StringSpaceFill): TypeFormat = LocalDateFormatImpl(width, fill, null)
  def apply(width: Int, fill: StringSpaceFill, pattern: String): TypeFormat = LocalDateFormatImpl(width, fill, pattern)
}
