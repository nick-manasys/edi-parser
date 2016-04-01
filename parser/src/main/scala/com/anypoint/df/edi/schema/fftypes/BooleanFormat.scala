package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl, util => ju }
import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object BooleanFormat extends FormatFactory {

  def code = "Boolean"

  case class BooleanFormatImpl(width: Int, repr: BooleanRepresentation, fill: FillMode)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {

    override def parseToken(lexer: LexerBase): Object = {
      val token = lexer.token
      repr match {
        case BooleanRepresentation.NUMBER =>
          verifyDigits(lexer)
          if (token == "0") jl.Boolean.FALSE
          else if (token == "1") jl.Boolean.TRUE
          else {
            invalidInput(lexer)
            jl.Boolean.FALSE
          }
        case BooleanRepresentation.ALPHA_LOWER =>
          if (token == "t" || token == "true") jl.Boolean.TRUE
          else if (token == "f" || token == "false") jl.Boolean.FALSE
          else {
            invalidInput(lexer)
            jl.Boolean.FALSE
          }
        case BooleanRepresentation.ALPHA_UPPER =>
          if (token == "T" || token == "TRUE") jl.Boolean.TRUE
          else if (token == "F" || token == "FALSE") jl.Boolean.FALSE
          else {
            invalidInput(lexer)
            jl.Boolean.FALSE
          }
      }
    }

    private def trimSize(full: String) = {
      if (width < 5) full.substring(0, 1)
      else full
    }

    override def buildToken(value: Object, writer: WriterBase): String = {
      value match {
        case b: jl.Boolean =>
          repr match {
            case BooleanRepresentation.NUMBER => if (b.booleanValue) "1" else "0"
            case BooleanRepresentation.ALPHA_LOWER => trimSize(b.toString)
            case BooleanRepresentation.ALPHA_UPPER => trimSize(b.toString.toUpperCase)
          }
        case _ =>
          wrongType(value, writer)
          ""
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeFill(fill, writer)
      writeBooleanRepresentation(repr, writer)
    }
  }

  def apply(width: Int, repr: BooleanRepresentation, fill: FillMode): TypeFormat = BooleanFormatImpl(width, repr, fill)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val repr = getBooleanRepresentation(map)
    val fill = getFill(map)
    apply(width, repr, fill)
  }
}