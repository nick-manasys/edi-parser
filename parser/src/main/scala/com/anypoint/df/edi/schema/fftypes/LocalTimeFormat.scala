package com.anypoint.df.edi.schema.fftypes

import org.threeten.bp.LocalTime

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object LocalTimeFormat extends FormatFactory {

  def code = "Time"

  case class LocalTimeFormatImpl(width: Int, fill: StringSpaceFill, pattern: String)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {

    override def parseToken(lexer: LexerBase): Object = null

    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case t: LocalTime =>
        case _ => wrongType(value, writer)
      }
      ""
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
      if (pattern != null) writePattern(pattern, writer)
    }
  }

  def apply(width: Int, fill: StringSpaceFill): TypeFormat = LocalTimeFormatImpl(width, fill, null)
  def apply(width: Int, fill: StringSpaceFill, pattern: String): TypeFormat = LocalTimeFormatImpl(width, fill, pattern)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val fill = getFill(map)
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      apply(width, fill, pattern)
    } else apply(width, fill)
  }
}
