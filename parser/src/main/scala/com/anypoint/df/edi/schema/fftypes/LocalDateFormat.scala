package com.anypoint.df.edi.schema.fftypes

import org.threeten.bp.LocalDate
import org.threeten.bp.format.DateTimeFormatter

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object LocalDateFormat extends FormatFactory {

  def code = "Date"

  abstract class LocalDateBase(width: Int, fill: StringSpaceFill)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {

    val formatter: DateTimeFormatter

    override def parseToken(lexer: LexerBase): Object = LocalDate.parse(lexer.token, formatter)

    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case d: LocalDate => d.format(formatter)
        case _ =>
          wrongType(value, writer)
          ""
      }
    }
  }

  case class LocalDateFormatImpl(width: Int, fill: StringSpaceFill)
      extends LocalDateBase(width, fill) with FlatFileFormat {

    val formatter =
      if (width >= 8) DateTimeFormatter.BASIC_ISO_DATE
      else width match {
        case 4 => DateTimeFormatter.ofPattern("yyyy")
        case 6 => DateTimeFormatter.ofPattern("yyyyMM")
        case _ => throw new IllegalArgumentException(s"Width $width is invalid for Date (must be 4, 6, or >= 8)")
      }

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
    }
  }

  case class LocalDatePatternImpl(width: Int, fill: StringSpaceFill, pattern: String)
      extends LocalDateBase(width, fill) with FlatFileFormat {

    val formatter = DateTimeFormatter.ofPattern(pattern)

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
      writePattern(pattern, writer)
    }
  }

  def apply(width: Int, fill: StringSpaceFill): TypeFormat = LocalDateFormatImpl(width, fill)
  def apply(width: Int, fill: StringSpaceFill, pattern: String): TypeFormat = LocalDatePatternImpl(width, fill, pattern)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val fill = getFill(map)
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      apply(width, fill, pattern)
    } else apply(width, fill)
  }
}