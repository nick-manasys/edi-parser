package com.anypoint.df.edi.schema.fftypes

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object StringFormat extends FormatFactory {

  def code = "String"

  case class StringFormatImpl(width: Int, fill: FillMode)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {

    override def parseToken(lexer: LexerBase): Object = {
      lexer.token
    }

    override def buildToken(value: Object, writer: WriterBase): String = {
      value.toString
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
    }
  }

  def apply(width: Int, fill: FillMode): TypeFormat = StringFormatImpl(width, fill)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val fill = getFill(map)
    apply(width, fill)
  }
}