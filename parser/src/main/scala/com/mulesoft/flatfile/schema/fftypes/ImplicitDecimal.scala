package com.mulesoft.flatfile.schema.fftypes

import java.{ math => jm }

/** Support for formats with implicit decimal point. The decimal point position is given as the number of decimal digits
  * in the representation of a value (so a value of 1 means the implicit decimal point is between the rightmost and
  * next-to-rightmost digits of a value. Several adjustment methods are provided in order to work efficiently with
  * different value sizes.
  */
trait ImplicitDecimal {

  import ImplicitDecimalConstants._

  /** Check if implicit decimal point position is valid.
    * @param impl
    * @return
    */
  def isValid(impl: Int) = impl.abs < implicitLimit

  /** Adjust Int value as integer, truncating any fractional component.
    * @param impl
    * @param value
    * @return Int, Long, or jm.BigInteger value
    */
  def adjustInteger(impl: Int, value: Int) = {
    if (impl == 0) value
    else if (impl > 0) {
      if (impl < longBias) value / intPowers(impl)
      else 0
    } else {
      if (impl < longBias) value.asInstanceOf[Long] * intPowers(impl)
      else if (impl < bigBias) jm.BigInteger.valueOf(value).multiply(longBigPowers(impl - longBias))
      else jm.BigInteger.valueOf(value).multiply(bigBigPowers(impl - bigBias))
    }
  }

  /** Adjust Long value as integer, truncating any fractional component.
    * @param impl
    * @param value
    * @return Long or jm.BigInteger value
    */
  def adjustInteger(impl: Int, value: Long) = {
    if (impl == 0) value
    else if (impl > 0) {
      if (impl < longBias) value / intPowers(impl)
      else if (impl < bigBias) value / longPowers(impl - longBias)
      else 0
    } else {
      if (impl < bigBias) jm.BigInteger.valueOf(value).multiply(longBigPowers(impl - longBias))
      else jm.BigInteger.valueOf(value).multiply(bigBigPowers(impl - bigBias))
    }
  }

  /** Get scaling factor as a BigInteger.
    * @param power positive power of ten
    * @return
    */
  private def bigScale(power: Int): jm.BigInteger = {
    if (power < longBias) intBigPowers(power)
    else if (power < bigBias) longBigPowers(power - longBias)
    else bigBigPowers(power - bigBias)
  }

  /** Adjust BigInteger value as integer, truncating any fractional component.
    * @param impl
    * @param value
    * @return jm.BigInteger value
    */
  def adjustInteger(impl: Int, value: jm.BigInteger) = {
    if (impl == 0) value
    else {
      val scale = bigScale(impl.abs)
      if (impl > 0) value.divide(scale)
      else value.multiply(scale)
    }
  }

  /** Adjust BigDecimal value as integer, truncating any fractional component.
    * @param impl
    * @param value
    * @return jm.BigInteger value
    */
  def adjustInteger(impl: Int, value: jm.BigDecimal) = {
    if (impl == 0) value
    else value.setScale(impl).toBigInteger
  }

  /** Adjust Int value as decimal, with any fractional component retained..
    * @param impl
    * @param value
    * @return Int, jm.BigInteger or jm.BigDecimal value
    */
  def adjustDecimal(impl: Int, value: Int) = {
    if (impl == 0) value
    else if (impl > 0) {
      if (impl < longBias && value % intPowers(impl) == 0) value / intPowers(impl)
      else new jm.BigDecimal(jm.BigInteger.valueOf(value), impl)
    } else jm.BigInteger.valueOf(value).multiply(bigScale(-impl))
  }

  /** Adjust Long value as decimal, with any fractional component retained..
    * @param impl
    * @param value
    * @return Long, jm.BigInteger or jm.BigDecimal value
    */
  def adjustDecimal(impl: Int, value: Long) = {
    if (impl == 0) value
    else if (impl > 0) {
      if (impl < longBias && value % intPowers(impl) == 0) value / intPowers(impl)
      else if (impl >= longBias && impl < bigBias && value % longPowers(impl - longBias) == 0) value / longPowers(impl - longBias)
      else new jm.BigDecimal(jm.BigInteger.valueOf(value), impl)
    } else jm.BigInteger.valueOf(value).multiply(bigScale(-impl))
  }

  /** Adjust BigInteger value as decimal, with any fractional component retained..
    * @param impl
    * @param value
    * @return jm.BigInteger or jm.BigDecimal value
    */
  def adjustDecimal(impl: Int, value: jm.BigInteger) = {
    if (impl == 0) value
    else if (impl > 0) new jm.BigDecimal(value, impl)
    else value.multiply(bigScale(-impl))
  }

  /** Adjust BigDecimal value as decimal, with any fractional component retained..
    * @param impl
    * @param value
    * @return jm.BigDecimal value
    */
  def adjustDecimal(impl: Int, value: jm.BigDecimal) = {
    if (impl == 0) value
    else value.setScale(impl)
  }
}

object ImplicitDecimalConstants {

  // powers 0 to 9
  val intPowers = (1 to 9).foldLeft(List(1)) { (acc, _) => acc.head * 10 :: acc }.toArray
  val intBigPowers = intPowers.map { jm.BigInteger.valueOf(_) }

  // powers 10 to 19
  val longBias = intPowers.length
  val longPowers = (1 to 9).foldLeft(List(10000000000L)) { (acc, _) => acc.head * 10 :: acc }.toArray
  val longBigPowers = longPowers.map { jm.BigInteger.valueOf(_) }

  // powers 20 to 31
  private val big10 = jm.BigInteger.valueOf(10)
  private val bigBase = new jm.BigInteger("100000000000000000000")
  val bigBias = longBias + longPowers.length
  val bigBigPowers = (1 to 11).foldLeft(List(bigBase)) { (acc, _) => acc.head.multiply(big10) :: acc }.toArray

  val implicitLimit = bigBias + bigBigPowers.length
}