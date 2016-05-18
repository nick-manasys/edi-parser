package com.mulesoft.flatfile.schema.fftypes

import spire.math.Number
import java.{ lang => jl, math => jm, text => jt }
import java.util.Locale
import com.mulesoft.flatfile.lexical.{ LexerBase, TypeFormat, WriterBase }
import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.formats.{ NumberFormatBase, StringFormatBase }

object IntegerFormat extends FormatFactory {

  def code = "Integer"

  val zonedPositiveMap = Map('}' -> '0', 'A' -> '1', 'B' -> '2', 'C' -> '3', 'D' -> '4', 'E' -> '5',
    'F' -> '6', 'G' -> '7', 'H' -> '8', 'I' -> '9')
  val zonedNegativeMap = Map('{' -> '0', 'J' -> '1', 'K' -> '2', 'L' -> '3', 'M' -> '4', 'N' -> '5',
    'O' -> '6', 'P' -> '7', 'Q' -> '8', 'R' -> '9')

  case class IntegerFormatImpl(width: Int, sign: NumberSign, fill: FillMode, zoned: Boolean)
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
      val digits = checkIntegerFormat(lexer)
      val value = convertSizedInteger(lexer, digits)
      if (neg) value match {
        case i: Integer => Integer.valueOf(-i.intValue)
        case l: jl.Long => jl.Long.valueOf(-l.longValue)
        case b: jm.BigInteger => b.negate
      } else value
    }

    override def write(value: Object, writer: WriterBase) = {
      writer.startToken
      value match {
        case n: Number =>
          if (n.canBeInt) writeIntegerValue(Integer.valueOf(n.toInt), writer)
          else if (n.canBeLong) writeIntegerValue(jl.Long.valueOf(n.toLong), writer)
          else writeIntegerValue(n.toBigInt.bigInteger, writer)
        case n: jl.Number =>
          writeIntegerValue(value, writer)
        case _ =>
          wrongType(value, writer)
          writePadded("0", false, writer)
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writeSign(sign, writer)
      writeNumberFill(fill, writer)
      writeZoned(zoned, writer)
    }
  }

  case class IntegerPatternImpl(width: Int, pattern: String, locale: Locale, fill: FillMode)
      extends StringFormatBase(code, width, width, fill) with FlatFileFormat {

    private def buildFormat = new jt.DecimalFormat(pattern, new jt.DecimalFormatSymbols(locale))

    override def parseToken(lexer: LexerBase) = {
      val format = buildFormat
      format.setParseIntegerOnly(true)
      format.parse(lexer.token)
    }

    override def buildToken(value: Object, writer: WriterBase) = {
      value match {
        case n: Number =>
          if (n.canBeInt) buildFormat.format(Integer.valueOf(n.toInt))
          else if (n.canBeLong) buildFormat.format(jl.Long.valueOf(n.toLong))
          else buildFormat.format(n.toBigInt.bigInteger)
        case n: jl.Number =>
          buildFormat.format(n)
        case _ =>
          wrongType(value, writer)
          ""
      }
    }

    override def writeOptions(writer: pairWriter): Unit = {
      writer(patternKey, pattern)
      writer(localeKey, locale.getCountry)
    }
  }

  def apply(width: Int, sign: NumberSign, fill: FillMode, zoned: Boolean): TypeFormat =
    IntegerFormatImpl(width, sign, fill, zoned)
  def apply(width: Int, sign: NumberSign, fill: FillMode): TypeFormat = apply(width, sign, fill, false)
  def apply(width: Int, pattern: String, locale: Locale, fill: FillMode): TypeFormat =
    IntegerPatternImpl(width, pattern, locale, fill)

  override def readFormat(width: Int, map: ValueMap): TypeFormat = {
    if (map != null && map.containsKey(patternKey)) {
      val pattern = getPattern(map)
      val locale = getLocale(map)
      val fill = getFill(map)
      apply(width, pattern, locale, fill)
    } else {
      val sign = getSign(map)
      val fill = getNumberFill(map)
      val zoned = getZoned(map)
      apply(width, sign, fill, zoned)
    }
  }
}