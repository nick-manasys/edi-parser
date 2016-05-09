package com.mulesoft.flatfile.schema.fftypes

import com.mulesoft.flatfile.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.formats.StringFormatBase

object StringFormat extends FormatFactory {

  import PictureHandling._

  def code = "String"

  val pictureExpander = new PictureExpander {
    val allowedChars = Set('A', 'X', '9', 'B', '/', '0')
    val dataChars = Set('A', 'X', '9')
  }

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

  def main(args: Array[String]) = {
    println(pictureExpander.expandPicture("9(5)A(3)"))
    println(pictureExpander.expandPicture("XXB(5)9"))
    println(pictureExpander.expandPicture("XX*"))
  }
}