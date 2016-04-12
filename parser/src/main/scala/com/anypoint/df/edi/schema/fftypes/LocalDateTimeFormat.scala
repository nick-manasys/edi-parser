package com.anypoint.df.edi.schema.fftypes


import java.util.Calendar

import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase
import com.anypoint.df.edi.lexical.{LexerBase, TypeFormat, WriterBase}
import org.threeten.bp.LocalDateTime
import org.threeten.bp.format.DateTimeFormatter

object LocalDateTimeFormat extends FormatFactory {

  def code = "DateTime"
  
  abstract class LocalDateTimeBase(width: Int, fill: FillMode)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {

    val formatter: DateTimeFormatter

    override def parseToken(lexer: LexerBase): Object = LocalDateTime.parse(lexer.token, formatter)

    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case d: LocalDateTime => d.format(formatter)
        case c: Calendar => LocalDateTime.of(c.get(Calendar.YEAR), c.get(Calendar.MONTH), c.get(Calendar.DAY_OF_MONTH), c.get(Calendar.HOUR), c.get(Calendar.MINUTE), c.get(Calendar.SECOND)).format(formatter)
        case _ =>
          wrongType(value, writer)
          ""
      }
    }
  }

  case class LocalDateTimeFormatImpl(width: Int, fill: FillMode)
      extends LocalDateTimeBase(width, fill) with FlatFileFormat {

    val formatter =
      if (width >= 17) DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS")
      else width match {
        case 12 => DateTimeFormatter.ofPattern("yyyyMMddHHmm")
        case 14 => DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
        case 15 => DateTimeFormatter.ofPattern("yyyyMMddHHmmssS")
        case 16 => DateTimeFormatter.ofPattern("yyyyMMddHHmmssSS")
        case _ => throw new IllegalArgumentException(s"Width $width is invalid for DateTime (must be 12 or >= 14)")
      }

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
    }
  }

  case class LocalDateTimePatternImpl(width: Int, fill: FillMode, pattern: String)
      extends LocalDateTimeBase(width, fill) with FlatFileFormat {

    val formatter = DateTimeFormatter.ofPattern(pattern)

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
      writePattern(pattern, writer)
    }
  }

  def apply(width: Int, fill: FillMode): TypeFormat = LocalDateTimeFormatImpl(width, fill)
  def apply(width: Int, fill: FillMode, pattern: String): TypeFormat = LocalDateTimePatternImpl(width, fill, pattern)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val fill = getFill(map)
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      apply(width, fill, pattern)
    } else apply(width, fill)
  }
}