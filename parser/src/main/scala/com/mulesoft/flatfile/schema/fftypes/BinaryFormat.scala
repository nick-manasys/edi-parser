package com.mulesoft.flatfile.schema.fftypes

import com.mulesoft.flatfile.lexical.{ FlatFileLexer, FlatFileWriter, LexerBase, TypeFormat, WriterBase }
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.formats.TypeFormatBase
import java.{ lang => jl, math => jm, text => jt }
import spire.math.Number
import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition

object BinaryFormat extends FormatFactory {

  def code = "Binary"

  case class BinaryFormatImpl(width: Int, digits: Int, impl: Int, signed: Boolean)
    extends TypeFormatBase(code, width, width) with ImplicitDecimal with FlatFileFormat {

    import ImplicitDecimalConstants._

    override def genericType = if (impl == 0) GenericType.INTEGER else GenericType.REAL

    if (!isValid(impl)) throw new IllegalArgumentException(s"Implicit decimal position $impl is out of range")
    val maxDigits = width match {
      case 2 => 4
      case 4 => 9
      case 8 => 18
      case _ => throw new IllegalArgumentException(s"Binary format width $width invalid (must be 2, 4, or 8)")
    }
    if (digits > maxDigits) throw new IllegalArgumentException(s"Binary format digits $digits too large for width $width")

    override def parse(lexer: LexerBase) = {
      lexer match {
        case l: FlatFileLexer =>
          val bytes = l.rawToken
          if (width <= 4) {
            val value = jl.Integer.valueOf(bytes.foldLeft(0) { (i, b) => (i << 8) + b })
            val limit = intPowers(digits)
            if (value.intValue.abs > limit) {
              lexer.error(this, ErrorCondition.INVALID_FORMAT, "too many digits in input")
              jl.Integer.valueOf(value % limit)
            } else value
          } else {
            val value = jl.Long.valueOf(bytes.foldLeft(0L) { (l, b) => (l << 8) + b })
            val limit = if (digits < longBias) intPowers(digits) else longPowers(digits - longBias)
            if (value.longValue.abs > limit) {
              lexer.error(this, ErrorCondition.INVALID_FORMAT, "too many digits in input")
              jl.Long.valueOf(value % limit)
            } else value
          }
        case _ => throw new IllegalStateException("PackedDecimalFormat requires FlatFileLexer")
      }
    }

    override def write(value: Object, writer: WriterBase) = {
      writer match {
        case w: FlatFileWriter =>

          def writeBinary(value: Long) = {
            def byte(num: Int) = {
              if (num == 0) value.asInstanceOf[Byte]
              else (value >> num * 8).asInstanceOf[Byte]
            }
            val bytes = width match {
              case 2 => Array(byte(1), byte(0))
              case 4 => Array(byte(3), byte(2), byte(1), byte(0))
              case 8 => Array(byte(7), byte(6), byte(5), byte(4), byte(3), byte(2), byte(1), byte(0))
            }
            w.writeRaw(bytes)
          }

          writer.startToken
          val scaled = value match {
            case n: Number =>
              if (n.canBeLong) adjustInteger(impl, n.toLong)
              else if (n.isWhole) adjustInteger(impl, n.toBigInt.bigInteger)
              else adjustInteger(impl, n.toBigDecimal.bigDecimal)
            case i: jl.Integer    => adjustInteger(impl, i)
            case l: jl.Long       => adjustInteger(impl, l)
            case b: jm.BigInteger => adjustInteger(impl, b)
            case d: jm.BigDecimal => adjustInteger(impl, d)
            case _ =>
              wrongType(value, writer)
              0
          }
          scaled match {
            case i: jl.Integer =>
              if (digits < longBias) writeBinary(i % intPowers(digits))
              else writeBinary(i.intValue)
            case l: jl.Long =>
              val modulus = if (digits < longBias) intPowers(digits) else longPowers(digits - longBias)
              writeBinary(l % modulus)
            case b: jm.BigInteger =>
              val modulus = if (digits < longBias) intBigPowers(digits) else longBigPowers(digits - longBias)
              writeBinary(b.mod(modulus).longValue)
          }
        case _ => throw new IllegalStateException("BinaryFormat requires FlatFileWriter")
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeImplicit(impl, writer)
      writeSigned(signed, writer)
      writeDigits(digits, writer)
    }
  }

  def apply(width: Int, digits: Int, impl: Int, signed: Boolean): TypeFormat =
    BinaryFormatImpl(width, digits, impl, signed)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    val impl = getImplicit(map)
    val signed = getSigned(map)
    val digits = getDigits(map)
    apply(width, digits, impl, signed)
  }
}