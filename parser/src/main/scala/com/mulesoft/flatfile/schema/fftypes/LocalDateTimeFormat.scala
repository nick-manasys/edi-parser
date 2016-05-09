package com.mulesoft.flatfile.schema.fftypes

import java.util.Calendar

import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.formats.StringFormatBase
import com.mulesoft.flatfile.lexical.{ LexerBase, TypeFormat, WriterBase }
import org.threeten.bp.{ LocalDateTime, OffsetDateTime, ZonedDateTime }
import org.threeten.bp.format.DateTimeFormatter

object LocalDateTimeFormat extends FormatFactory {

  def code = "DateTime"
  
  abstract class LocalDateTimeBase(width: Int, fill: FillMode)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {

    val formatter: DateTimeFormatter
    
    override def parseToken(lexer: LexerBase): Object = LocalDateTime.parse(lexer.token, formatter)
    
    private def truncate(text: String) = if (text.length > width) text.substring(0, width) else text

    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case d: LocalDateTime => truncate(d.format(formatter))
        case o: OffsetDateTime => truncate(o.format(formatter))
        case z: ZonedDateTime => truncate(z.format(formatter))
        case c: Calendar =>
          truncate(LocalDateTime.of(c.get(Calendar.YEAR), c.get(Calendar.MONTH), c.get(Calendar.DAY_OF_MONTH),
            c.get(Calendar.HOUR), c.get(Calendar.MINUTE), c.get(Calendar.SECOND)).format(formatter))
        case _ =>
          wrongType(value, writer)
          ""
      }
    }
  }

  case class LocalDateTimeFormatImpl(width: Int, fill: FillMode)
      extends LocalDateTimeBase(width, fill) with FlatFileFormat {

    val formatter =
      width match {
        case 12 => DateTimeFormatter.ofPattern("yyyyMMddHHmm")
        case 14 => DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
        case _ => throw new IllegalArgumentException(s"Width $width is invalid for DateTime (must be 12 or 14)")
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