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
    
    override def parse(lexer: LexerBase) = {
      lexer match {
        case l: FlatFileLexer =>
          val bytes = l.rawToken
          val builder = new StringBuilder(width * 2)
          (bytes(bytes.length - 1) & 0xF) match {
            case 0xB | 0xD => builder.append('-')
            case _ =>
          }
          (0 until digits) foreach { i =>
            val offset = i / 2
            val nibble = 
              if (i % 2 == 0) bytes(offset) >> 4
              else bytes(offset) & 0xF
            builder.append((nibble + '0').toChar)
          }
          val text = builder.toString
          if (digits < 10) Integer.valueOf(text)
          else if (digits < 20) jl.Long.valueOf(text)
          else new jm.BigInteger(text)
        case _ => throw new IllegalStateException("PackedDecimalFormat requires FlatFileLexer")
      }
    }
    
    def writePacked(number: String, negate: Boolean, writer: FlatFileWriter): Unit = {
      def digitAt(i: Int) = if (i < 0) 0 else number.charAt(i) - '0'
      val bytes = new Array[Byte](width)
      val bias = number.length - digits
      (0 until digits) foreach { i =>
        val nibble = digitAt(i + bias)
        val offset = i / 2
        bytes(offset) =
          if (i % 2 == 0) (bytes(offset) + nibble << 4).toByte
          else (bytes(offset) + nibble).toByte
      }
      val sign = (signed, negate) match {
        case (true, true) => 0xD
        case (true, false) => 0xC
        case _ => 0xF
      }
      bytes(width - 1) = (bytes(width - 1) + sign).toByte
      writer.writeRaw(bytes)
    }
    
    def convertToPacked(number: String, writer: FlatFileWriter): Unit = {
      if (number.charAt(0) == '-') {
        writePacked(number.substring(1), true, writer);
      } else {
        writePacked(number, false, writer);
      }
    }
    
    override def write(value: Object, writer: WriterBase) = {
      writer match {
        case w: FlatFileWriter =>
          writer.startToken
          value match {
            case n: Number => convertToPacked(n.toString, w)
            case n: jl.Number => convertToPacked(n.toString, w)
            case _ =>
              wrongType(value, writer)
              convertToPacked("0", w)
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