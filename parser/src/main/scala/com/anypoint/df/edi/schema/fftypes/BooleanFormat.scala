package com.anypoint.df.edi.schema.fftypes

import java.{ lang => jl }

import com.anypoint.df.edi.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.TypeFormatConstants._
import com.anypoint.df.edi.lexical.formats.StringFormatBase

object BooleanFormat extends FlatFileFormat {
  
  def code = "Boolean"

  case class BooleanFormatImpl(width: Int, repr: BooleanRepresentation, fill: StringSpaceFill)
      extends StringFormatBase(code, width, width, fill) {
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
        case _ =>
          val upper = token.toUpperCase
          if (upper == "T" || upper == "TRUE") jl.Boolean.TRUE
          else if (upper == "F" || upper == "FALSE") jl.Boolean.FALSE
          else {
            invalidInput(lexer)
            jl.Boolean.FALSE
          }
      }
      null
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
  }

  def apply(width: Int, repr: BooleanRepresentation, fill: StringSpaceFill): TypeFormat = BooleanFormatImpl(width, repr, fill)
}