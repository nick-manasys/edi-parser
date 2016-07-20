package com.mulesoft.flatfile.schema.fftypes

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.Charset

import com.mulesoft.flatfile.lexical.{ EdiConstants, FlatFileLexer, FlatFileWriter, TypeFormat }

object DemoSupport {

  def parseString(text: String, format: TypeFormat, encoding: Charset, raw: Boolean): Object = {
    val lexer =
      new FlatFileLexer(new ByteArrayInputStream(text.getBytes(encoding)), encoding, raw, true, false, false, -1)
    lexer.load(format.maxLength)
    format.parse(lexer)
  }

  def parseString(text: String, format: TypeFormat): Object =
    parseString(text, format, EdiConstants.ISO88591_CHARSET, false)

  def writeString(value: Object, format: TypeFormat, encoding: Charset): String = {
    val stream = new ByteArrayOutputStream
    val writer = new FlatFileWriter(stream, encoding, "\n")
    format.write(value, writer)
    writer.close
    new String(stream.toByteArray, encoding)
  }
  
  def writeString(value: Object, format: TypeFormat): String = writeString(value, format, EdiConstants.ISO88591_CHARSET)
}