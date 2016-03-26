package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl }
import com.anypoint.df.edi.lexical.{ LexerBase, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

case class BooleanValue(code: String, width: Int, format: BooleanFormat, fill: StringSpaceFill) extends StringFormatBase(code, width, width, fill) {
  override def parseToken(lexer: LexerBase): Object = {
    format match {
      case BooleanFormat.NUMBER =>
        verifyDigits(lexer)
        val token = lexer.token
        if (token == "0") false
        else if (token == "1") true
        else {
          
        }
      case _ =>
    }
    null
  }
  override def buildToken(value: Object, writer: WriterBase): String = {
    value match {
      case b: jl.Boolean =>
      case _ => wrongType(value, writer)
    }
    ""
  }
}