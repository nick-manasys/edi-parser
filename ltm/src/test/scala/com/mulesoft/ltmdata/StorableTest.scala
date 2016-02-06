package com.mulesoft.ltmdata

import scala.util.Random
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class StorableTest extends FlatSpec with Matchers {
  
  behavior of "StorableValueSeq"
  
  val integerSize = ItemType.valueSize(new Integer(0))
	val ctx = new StructureContext(integerSize * 10 + 1)
  
  it should "keep data in memory to maximum size" in {
    val seq = ctx.newValueSeq
    (1 to 10) foreach { i =>
      seq += Integer.valueOf(i)
      seq.memSize should be (i * integerSize)
    }
    val iterator = seq.iterator
    (1 to 10) foreach { i =>
      iterator.hasNext should be (true)
      val item = iterator.next
      item should be (Integer.valueOf(i))
    }
    iterator.hasNext should be (false)
  }
  
  it should "dump to file when maximum size exceeded" in {
    val seq = ctx.newValueSeq
    (1 to 10) foreach { i => seq += Integer.valueOf(i) }
    seq.memSize should be (10 * integerSize)
    seq += new Integer(11)
    seq.memSize should be (0)
    (12 to 20) foreach { i => seq += Integer.valueOf(i) }
    seq.memSize should be (0)
    val iterator = seq.iterator
    (1 to 20) foreach { i =>
      iterator.hasNext should be (true)
      val item = iterator.next
      item should be (Integer.valueOf(i))
    }
    iterator.hasNext should be (false)
  }
  
/*  var mapCount = 0
  
  val keyMinLen = 4
  val keyMaxLen = 20
  val mapMinKeys = 4
  val mapMaxKeys = 20
  val mapMinItemsExp = 0
  val mapMaxItemsExp = 10
  val mapChildMapProb = .1
  val mapChildListProb = .1
  val listMinItemsExp = 0
  val listMaxItemsExp = 10
  
	val random = new Random
  
  def randomRange(min: Int, max: Int) = min + random.nextInt(max - min)
  
  def randomLetter: Char = {
    val num = random.nextInt(52)
    if (num < 26) ('A' + num).toChar else ('a' + num - 26).toChar
  }
  
  def genName = {
    val length = randomRange(keyMinLen, keyMaxLen)
    (0 to length).map { _ => randomLetter }.toString
  }
  
  def genKeys = {
    val count = randomRange(mapMinKeys, mapMaxKeys)
    (0 to count).map { _ =>  genName }.toIndexedSeq
  }
  
  def genMap(depth: Int) = {
    val keys = genKeys
    val descriptor = MapDescriptor(mapCount, keys)
    mapCount += 1
    val map = ctx.newMap(descriptor)
  }
  
  def main(args: Array[String]): Unit = {
    val top = ctx.newMapSeq
  }	*/
}