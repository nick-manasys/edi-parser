package com.anypoint.df.edi.schema.fftypes

import org.threeten.bp.LocalDate
import com.anypoint.df.edi.lexical.{ LexerBase, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

case class LocalDateTimeType(code: String, width: Int, fill: StringSpaceFill, format: String) extends StringFormatBase(code, width, width, fill) {
  override def parseToken(lexer: LexerBase): Object = null
  override def buildToken(value: Object, writer: WriterBase): String = {
    value match {
      case d: LocalDate => 
      case _ => wrongType(value, writer)
    }
    ""
  }
}