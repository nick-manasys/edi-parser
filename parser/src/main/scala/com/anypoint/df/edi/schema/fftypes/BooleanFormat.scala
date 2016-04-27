package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl, util => ju }
import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object BooleanFormat extends FormatFactory {

  def code = "Boolean"

  case class BooleanFormatImpl(width: Int, t: String, f: String, fill: FillMode)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {
    
    val (useTrue, useFalse) =
      if (t.length <= width && f.length <= width) (t, f)
      else {
        val trimt = t.substring(0, math.min(t.length, width))
        val trimf = f.substring(0, math.min(f.length, width))
        (trimt, trimf)
      }

    override def parseToken(lexer: LexerBase): Object = {
      val token = lexer.token
      if (token == useTrue) jl.Boolean.TRUE
      else if (token == useFalse) jl.Boolean.FALSE
      else {
          invalidInput(lexer)
          jl.Boolean.FALSE
        }
    }

    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case b: jl.Boolean =>
          if (b.booleanValue) useTrue else useFalse
        case _ =>
          wrongType(value, writer)
          ""
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
      writeBooleanRepresentation(useTrue, useFalse, writer)
    }
  }

  def apply(width: Int, t: String, f: String, fill: FillMode): TypeFormat = BooleanFormatImpl(width, t, f, fill)
  def apply(width: Int, repr: BooleanRepresentation, fill: FillMode): TypeFormat = {
    repr match {
      case BooleanRepresentation.ALPHA_LOWER => apply(width, "true", "false", fill)
      case BooleanRepresentation.ALPHA_UPPER => apply(width, "TRUE", "FALSE", fill)
      case BooleanRepresentation.NUMBER => apply(width, "1", "0", fill)
    }
  }

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val (t, f) = getBooleanRepresentation(map)
    val fill = getFill(map)
    apply(width, t, f, fill)
  }
}