package com.anypoint.df.edi.schema.fftypes

import com.anypoint.df.edi.lexical.{ FlatFileLexer, FlatFileWriter, LexerBase, TypeFormat, WriterBase }
import com.anypoint.df.edi.lexical.formats.{ TypeFormatBase }
import java.{ lang => jl, math => jm, text => jt }
import spire.math.Number

object PackedDecimalFormat extends FormatFactory {
  
  def code = "Packed"
  
  case class PackedDecimalImpl(width: Int, impl: Int, signed: Boolean) extends TypeFormatBase(code, width, width)
    with FlatFileFormat {
    
    val digits = width * 2 - 1
    
    def convertInteger(text: String) = {
      val bias = if (text.charAt(0).isDigit) 0 else 1
      val length = text.length - bias
      if (length < 10) Integer.valueOf(text)
      else if (length < 19) jl.Long.valueOf(text)
      else new jm.BigInteger(text)
    }
    
    def convertPacked(lexer: FlatFileLexer) = {
      val bytes = lexer.rawToken
      val builder = new StringBuilder(width * 2)
      (bytes(bytes.length - 1) & 0xF) match {
        case 0xB | 0xD => if (signed) builder.append('-')
        case _ =>
      }
      var nonzero = false
      (0 until digits) foreach { i =>
        val offset = i / 2
        val nibble = 
          if (i % 2 == 0) (bytes(offset) & 0xFF) >> 4
          else bytes(offset) & 0xF
        if (nibble > 0) nonzero = true
        if (nonzero) builder.append((nibble + '0').toChar)
      }
      try {
        if (impl == 0) convertInteger(builder.toString)
        else if (impl > 0) new jm.BigDecimal(new jm.BigInteger(builder.toString), impl)
        else {
          (0 until -impl) foreach { _ => builder.append('0') }
          convertInteger(builder.toString)
        }
      } catch {
        case e: NumberFormatException =>
          invalidInput(lexer)
          Integer.valueOf(0)
      }
    }
    
    override def parse(lexer: LexerBase) = {
      lexer match {
        case l: FlatFileLexer => convertPacked(l)
        case _ => throw new IllegalStateException("PackedDecimalFormat requires FlatFileLexer")
      }
    }
    
    def writePacked(number: String, adjust: Int, negate: Boolean, writer: FlatFileWriter): Unit = {
      def digitAt(i: Int) = if (i < 0 || i >= number.length) 0 else number.charAt(i) - '0'
      val bytes = new Array[Byte](width)
      val bias = number.length - digits + adjust
      (0 until digits) foreach { i =>
        val nibble = digitAt(i + bias)
        val offset = i / 2
        bytes(offset) =
          if (i % 2 == 0) (bytes(offset) + nibble << 4).toByte
          else (bytes(offset) + nibble).toByte
      }
      val sign = if (signed && negate) 0xD else 0xF
      bytes(width - 1) = (bytes(width - 1) + sign).toByte
      writer.writeRaw(bytes)
    }
    
    def convertToPacked(number: String, adjust: Int, writer: FlatFileWriter): Unit = {
      if (number.charAt(0) == '-') {
        writePacked(number.substring(1), adjust, true, writer);
      } else {
        writePacked(number, adjust, false, writer);
      }
    }
    
    def convertBigDecimal(decimal: jm.BigDecimal, writer: FlatFileWriter): Unit = {
      val adjusted = decimal.movePointRight(impl).setScale(impl, jm.RoundingMode.HALF_UP)
      convertToPacked(adjusted.toBigInteger.toString, 0, writer)
    }
    
    override def write(value: Object, writer: WriterBase) = {
      writer match {
        case w: FlatFileWriter =>
          writer.startToken
          value match {
            case n: Number =>
              if (n.isWhole) convertToPacked(n.toString, impl, w)
              else convertBigDecimal(n.toBigDecimal.bigDecimal, w)
            case d: jm.BigDecimal => convertBigDecimal(d, w)
            case n: jl.Number => convertToPacked(n.toString, impl, w)
            case _ =>
              wrongType(value, writer)
              convertToPacked("0", 0, w)
          }
        case _ => throw new IllegalStateException("PackedDecimalFormat requires FlatFileWriter")
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writer(implicitKey, Integer.valueOf(impl))
      writer(signedKey, jl.Boolean.valueOf(signed))
    }
  }
  
  def apply(width: Int, impl: Int, signed: Boolean): TypeFormat = PackedDecimalImpl(width, impl, signed)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val impl = if (map != null && map.containsKey(implicitKey)) getAsInt(implicitKey, map) else 0
    val signed = map != null && map.containsKey(signedKey)
    apply(width, impl, signed)
  }
}