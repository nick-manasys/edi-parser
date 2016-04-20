package com.anypoint.df.edi.schema.fftypes

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.Charset

import com.anypoint.df.edi.lexical.{ EdiConstants, FlatFileLexer, FlatFileWriter, TypeFormat }

object DemoSupport {

  val charset = Charset.forName("UTF-8")

  def parseString(text: String, format: TypeFormat) = {
    val lexer = new FlatFileLexer(new ByteArrayInputStream(text.getBytes(charset)), EdiConstants.ISO88591_CHARSET)
    lexer.load(format.maxLength)
    format.parse(lexer)
  }

  def writeString(value: Object, format: TypeFormat) = {
    val stream = new ByteArrayOutputStream
    val writer = new FlatFileWriter(stream, charset)
    format.write(value, writer)
    writer.close
    new String(stream.toByteArray, charset)
  }
}