package com.mulesoft.flatfile.schema.fftypes

import spire.math.Number
import com.mulesoft.flatfile.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.mulesoft.flatfile.lexical.ErrorHandler._
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.formats.{ NumberFormatBase, StringFormatBase }
import java.{ lang => jl, math => jm, text => jt }
import java.util.Locale
import scala.annotation.tailrec

object DecimalFormat extends FormatFactory {

  def code = "Decimal"

  val zonedPositiveMap = Map('}' -> '0', 'A' -> '1', 'B' -> '2', 'C' -> '3', 'D' -> '4', 'E' -> '5',
    'F' -> '6', 'G' -> '7', 'H' -> '8', 'I' -> '9')
  val zonedNegativeMap = Map('{' -> '0', 'J' -> '1', 'K' -> '2', 'L' -> '3', 'M' -> '4', 'N' -> '5',
    'O' -> '6', 'P' -> '7', 'Q' -> '8', 'R' -> '9')

  case class DecimalFormatImpl(width: Int, sign: NumberSign, fill: FillMode, zoned: Boolean)
      extends NumberFormatBase(code, width, width, if (zoned) NumberSign.UNSIGNED else sign, true, fill)
      with FlatFileFormat {

    private def checkNegative(builder: jl.StringBuilder) = {
      def checkAndChange(offset: Int) = {
        val char = builder.charAt(offset)
        zonedPositiveMap.get(char) match {
          case Some(c) =>
            builder.setCharAt(offset, c)
            false
          case _ =>
            zonedNegativeMap.get(char) match {
              case Some(c) =>
                builder.setCharAt(offset, c)
                true
              case _ =>
                false
            }
        }
      }
      sign match {
        case NumberSign.ALWAYS_RIGHT => checkAndChange(builder.length - 1)
        case NumberSign.ALWAYS_LEFT => checkAndChange(0)
        case _ => throw new IllegalStateException("Invalid sign placement for zoned decimal")
      }
    }

    override def parse(lexer: LexerBase) = {
      val neg = zoned && checkNegative(lexer.tokenBuilder)
      // TODO: set sign
      convertPlainDecimal(lexer)
    }

    override def write(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
          writer.startToken
          if (n.canBeInt) writeDecimalValue(Integer.valueOf(n.toInt), writer)
          else if (n.canBeLong) writeDecimalValue(jl.Long.valueOf(n.toLong), writer)
          else if (n.isWhole) writeDecimalValue(n.toBigInt.bigInteger, writer)
          else writeDecimalValue(n.toBigDecimal.bigDecimal, writer)
        case n: jl.Number =>
          writer.startToken
          writeDecimalValue(n, writer)
        case _ =>
          wrongType(value, writer)
          writer.writeToken("")
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeSign(sign, writer)
      writeNumberFill(fill, writer)
      writeZoned(zoned, writer)
    }
  }

  case class DecimalImplicitImpl(width: Int, impl: Int, sign: NumberSign, fill: FillMode, zoned: Boolean)
      extends NumberFormatBase(code, width, width, if (zoned) NumberSign.UNSIGNED else sign, true, fill) with FlatFileFormat {

    private def checkNegative(builder: jl.StringBuilder) = {
      def checkAndChange(offset: Int) = {
        val char = builder.charAt(offset)
        zonedPositiveMap.get(char) match {
          case Some(c) =>
            builder.setCharAt(offset, c)
            false
          case _ =>
            zonedNegativeMap.get(char) match {
              case Some(c) =>
                builder.setCharAt(offset, c)
                true
              case _ =>
                false
            }
        }
      }
      sign match {
        case NumberSign.ALWAYS_RIGHT => checkAndChange(builder.length - 1)
        case NumberSign.ALWAYS_LEFT => checkAndChange(0)
        case _ => throw new IllegalStateException("Invalid sign placement for zoned decimal")
      }
    }

    override def parse(lexer: LexerBase) = {
      val neg = zoned && checkNegative(lexer.tokenBuilder)
      checkIntegerFormat(lexer)
      val bd = new jm.BigDecimal(new jm.BigInteger(lexer.token()), impl)
      if (neg) bd.negate else bd
    }

    override def write(value: Object, writer: WriterBase) = {
      def writeImplicit(decimal: jm.BigDecimal) = {
        writer.startToken
        val adjusted = decimal.movePointRight(impl).setScale(impl, jm.RoundingMode.HALF_UP)
        writeBigInteger(adjusted.toBigIntegerExact, writer)
      }
      value match {
        case n: Number =>
          writeImplicit(n.toBigDecimal.bigDecimal)
        case d: jm.BigDecimal =>
          writeImplicit(d)
        case _ =>
          wrongType(value, writer)
          writer.writeToken("")
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writer(implicitKey, Integer.valueOf(impl))
      writeSign(sign, writer)
      writeNumberFill(fill, writer)
      writeZoned(zoned, writer)
    }
  }

  case class DecimalPatternImpl(width: Int, pattern: String, locale: Locale, fill: FillMode)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {

    private def buildFormat = new jt.DecimalFormat(pattern, new jt.DecimalFormatSymbols(locale))

    override def parseToken(lexer: LexerBase) = {
      val format = buildFormat
      format.setParseBigDecimal(true)
      format.parse(lexer.token)
    }

    override def buildToken(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
          writer.startToken
          if (n.canBeInt) buildFormat.format(Integer.valueOf(n.toInt))
          else if (n.canBeLong) buildFormat.format(jl.Long.valueOf(n.toLong))
          else if (n.isWhole) buildFormat.format(n.toBigInt.bigInteger)
          else buildFormat.format(n.toBigDecimal.bigDecimal)
        case n: jl.Number =>
          buildFormat.format(value)
        case _ =>
          wrongType(value, writer)
          ""
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writePattern(pattern, writer)
      writeLocale(locale, writer)
    }
  }

  def apply(width: Int, sign: NumberSign, fill: FillMode, zoned: Boolean): TypeFormat =
    DecimalFormatImpl(width, sign, fill, zoned)
  def apply(width: Int, sign: NumberSign, fill: FillMode): TypeFormat = apply(width, sign, fill, false)
  def apply(width: Int, sign: NumberSign, impl: Int, fill: FillMode, zoned: Boolean): TypeFormat =
    DecimalImplicitImpl(width, impl, sign, fill, zoned)
  def apply(width: Int, sign: NumberSign, impl: Int, fill: FillMode): TypeFormat =
    apply(width, sign, impl, fill, false)
  def apply(width: Int, pattern: String, locale: Locale, fill: FillMode): TypeFormat =
    DecimalPatternImpl(width, pattern, locale, fill)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      val locale = getLocale(map)
      val fill = getFill(map)
      apply(width, pattern, locale, fill)
    } else if (map != null && map.containsKey(implicitKey)) {
      val impl = getRequiredInt(implicitKey, map)
      val sign = getSign(map)
      val fill = getNumberFill(map)
      val zoned = getZoned(map)
      apply(width, sign, impl, fill, zoned)
    } else {
      val sign = getSign(map)
      val fill = getNumberFill(map)
      val zoned = getZoned(map)
      apply(width, sign, fill, zoned)
    }
  }
}