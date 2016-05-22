package com.mulesoft.flatfile.schema.fftypes

import com.mulesoft.flatfile.lexical.TypeFormatConstants._
import com.mulesoft.flatfile.lexical.formats.NumberFormatBase
import java.{ lang => jl, math => jm }

abstract class ZonedFormatBase(code: String, width: Int, sign: NumberSign, fill: FillMode, zoned: Boolean)
    extends NumberFormatBase(code, width, width, if (zoned) NumberSign.UNSIGNED else sign, true, fill)
    with FlatFileFormat {

  def checkNegative(builder: jl.StringBuilder) = {
    def checkAndChange(offset: Int) = {
      val char = builder.charAt(offset)
      ZonedFormatBase.zonedPositiveMap.get(char) match {
        case Some(c) =>
          builder.setCharAt(offset, c)
          false
        case _ =>
          ZonedFormatBase.zonedNegativeMap.get(char) match {
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
}

object ZonedFormatBase {
  val zonedPositiveMap = Map('}' -> '0', 'A' -> '1', 'B' -> '2', 'C' -> '3', 'D' -> '4', 'E' -> '5',
    'F' -> '6', 'G' -> '7', 'H' -> '8', 'I' -> '9')
  val zonedNegativeMap = Map('{' -> '0', 'J' -> '1', 'K' -> '2', 'L' -> '3', 'M' -> '4', 'N' -> '5',
    'O' -> '6', 'P' -> '7', 'Q' -> '8', 'R' -> '9')
}