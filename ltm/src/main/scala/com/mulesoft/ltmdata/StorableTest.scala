package com.mulesoft.ltmdata

import scala.util.Random

object StorableTest {
  
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
  
	val ctx = new StructureContext
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
    
  }
  
  def main(args: Array[String]): Unit = {
    val top = ctx.newSeq
    
  }
}