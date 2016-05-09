package com.mulesoft.flatfile.schema.fftypes

import org.threeten.bp.{ LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime }
import org.threeten.bp.format.DateTimeFormatter

import com.mulesoft.flatfile.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.formats.StringFormatBase

object LocalTimeFormat extends FormatFactory {

  def code = "Time"
  
  abstract class LocalTimeBase(width: Int, fill: FillMode)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {

    val formatter: DateTimeFormatter

    override def parseToken(lexer: LexerBase): Object = LocalTime.parse(lexer.token, formatter)
    
    private def truncate(text: String) = if (text.length > width) text.substring(0, width) else text

    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case t: LocalTime => truncate(t.format(formatter))
        case d: LocalDateTime => truncate(d.format(formatter))
        case o: OffsetDateTime => truncate(o.format(formatter))
        case o: OffsetTime => truncate(o.format(formatter))
        case z: ZonedDateTime => truncate(z.format(formatter))
        case _ =>
          wrongType(value, writer)
          ""
      }
    }
  }

  case class LocalTimeFormatImpl(width: Int, fill: FillMode)
      extends LocalTimeBase(width, fill) with FlatFileFormat {

    val formatter =
      if (width >= 9) DateTimeFormatter.ofPattern("HHmmssSSS")
      else width match {
        case 2 => DateTimeFormatter.ofPattern("HH")
        case 4 => DateTimeFormatter.ofPattern("HHmm")
        case 6 => DateTimeFormatter.ofPattern("HHmmss")
        case 7 => DateTimeFormatter.ofPattern("HHmmssS")
        case 8 => DateTimeFormatter.ofPattern("HHmmssSS")
        case _ => throw new IllegalArgumentException(s"Width $width is invalid for Time (must be 2, 4, or >= 6)")
      }

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
    }
  }

  case class LocalTimePatternImpl(width: Int, fill: FillMode, pattern: String)
      extends LocalTimeBase(width, fill) with FlatFileFormat {

    val formatter = DateTimeFormatter.ofPattern(pattern)

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
      writePattern(pattern, writer)
    }
  }

  def apply(width: Int, fill: FillMode): TypeFormat = LocalTimeFormatImpl(width, fill)
  def apply(width: Int, fill: FillMode, pattern: String): TypeFormat = LocalTimePatternImpl(width, fill, pattern)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val fill = getFill(map)
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      apply(width, fill, pattern)
    } else apply(width, fill)
  }
}
