package com.mulesoft.flatfile.schema.fftypes

import java.{ lang => jl, util => ju }
import com.mulesoft.flatfile.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.formats.StringFormatBase

object BooleanFormat extends FormatFactory {

  def code = "Boolean"

  case class BooleanFormatImpl(width: Int, t: String, f: String, caseSensitive: Boolean, fill: FillMode)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {
    
    override def genericType = GenericType.BOOLEAN
    
    val (useTrue, useFalse) = {
      if (t.length <= width && f.length <= width) (t, f)
      else {
        val trimt = t.substring(0, math.min(t.length, width))
        val trimf = f.substring(0, math.min(f.length, width))
        (trimt, trimf)
      }
    }
    val (compTrue, compFalse) =
      if (caseSensitive) (useTrue, useFalse)
      else (useTrue.toUpperCase, useFalse.toUpperCase)

    override def parseToken(lexer: LexerBase): Object = {
      val token = if (caseSensitive) lexer.token else lexer.token.toUpperCase
      if (token == compTrue) jl.Boolean.TRUE
      else if (token == compFalse) jl.Boolean.FALSE
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
      writeCaseSensitive(caseSensitive, writer)
      writeBooleanRepresentation(useTrue, useFalse, writer)
    }
  }

  def apply(width: Int, t: String, f: String, caseSensitive: Boolean, fill: FillMode): TypeFormat =
    BooleanFormatImpl(width, t, f, caseSensitive, fill)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val (t, f) = getBooleanRepresentation(map)
    val cs = getCaseSensitive(map)
    val fill = getFill(map)
    apply(width, t, f, cs, fill)
  }
}