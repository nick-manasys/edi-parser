package com.mulesoft.ltmdata

import java.{ lang => jl, math => jm }
import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream }
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class StorableTest extends FlatSpec with Matchers {
  
  val integerSize = ItemType.valueSize(new Integer(0))
  val inMemoryCount = 10
  val outMemoryCount = 40
  val relPrimeMultiplier = 13
  
  val ctxValues = new StorableStructureContext(integerSize * inMemoryCount + StorableSeq.baseMemoryUse + 1)
  
  behavior of "StorableValueSeq"
  
  it should "keep data in memory to maximum size" in {
    val seq = ctxValues.newValueSeq
    (1 to inMemoryCount) foreach { i =>
      seq += Integer.valueOf(i)
      seq.memSize should be (StorableSeq.baseMemoryUse + i * integerSize)
    }
    val iterator = seq.iterator
    (1 to inMemoryCount) foreach { i =>
      iterator.hasNext should be (true)
      val item = iterator.next
      item should be (Integer.valueOf(i))
    }
    iterator.hasNext should be (false)
    (1 to inMemoryCount) foreach { i =>
      val item = seq(i - 1)
      item should be (Integer.valueOf(i))
    }
  }
  
  it should "dump to file when maximum size exceeded" in {
    val seq = ctxValues.newValueSeq
    (1 to inMemoryCount) foreach { i => seq += Integer.valueOf(i) }
    seq.memSize should be (StorableSeq.baseMemoryUse + inMemoryCount * integerSize)
    seq += new Integer(inMemoryCount + 1)
    seq.memSize should be (0)
    (inMemoryCount + 2 to outMemoryCount) foreach { i => seq += Integer.valueOf(i) }
    seq.memSize should be (0)
    val iterator = seq.iterator
    (1 to outMemoryCount) foreach { i =>
      iterator.hasNext should be (true)
      val item = iterator.next
      item should be (Integer.valueOf(i))
    }
    iterator.hasNext should be (false)
  }
  
  it should "support both sequential and random access to dumped values" in {
    val seq = ctxValues.newValueSeq
    (1 to outMemoryCount) foreach { i => seq += Integer.valueOf(i) }
    seq.switchToInput
    (1 to outMemoryCount) foreach { i =>
      val item = seq(i - 1)
      item should be (Integer.valueOf(i))
    }
    (1 to outMemoryCount) foreach { i =>
      val perm = ((i - 1) * relPrimeMultiplier) / outMemoryCount + 1
      val item = seq(perm - 1)
      item should be (Integer.valueOf(perm))
    }
  }
  
  val mapKeys = Array("string", "integer", "long", "bigInteger", "bigDecimal", "seq")
  
  private def fillMap(number: Int, map: StorableMap) = {
    map += ("string" -> number.toString)
    map += ("integer" -> Integer.valueOf(number))
    map += ("long" -> jl.Long.valueOf(number))
    map += ("bigInteger" -> jm.BigInteger.valueOf(number))
    map += ("bigDecimal" -> new jm.BigDecimal(number))
  }
  
  val baseNumber = 1000000
  val mapSize = {
    val ctx = new StorableStructureContext(1000000)
    val map = ctx.newMap(ctx.addDescriptor(mapKeys))
    fillMap(baseNumber, map)
    map.memSize
  }
  
  val mapCtxMemoryLimit = mapSize * inMemoryCount + 1
  val ctxMaps = new StorableStructureContext(mapCtxMemoryLimit)
  val mapDescriptor = ctxMaps.addDescriptor(mapKeys)
  
  behavior of "StorableMap"
  
  private def writeToBytes(map: StorableMap) = {
    val bos = new ByteArrayOutputStream
    val dos = new DataOutputStream(bos)
    map.write(dos)
    dos.close
    bos.toByteArray
  }
  
  it should "store and retrieve from stream" in {
    val map1 = ctxMaps.newMap(mapDescriptor)
    val bytes = writeToBytes(map1)
    val dis = new DataInputStream(new ByteArrayInputStream(bytes))
    val map2 = ctxMaps.newMap(mapDescriptor)
    map2.read(dis, ctxValues)
    map1 should be (map2)
  }
  
  it should "support nested sequences kept in memory" in {
    val map1 = ctxMaps.newMap(mapDescriptor)
    val seq = ctxValues.newValueSeq
    (1 to inMemoryCount) foreach { i => seq += Integer.valueOf(i) }
    map1 += ("seq" -> seq)
    val bytes = writeToBytes(map1)
    val dis = new DataInputStream(new ByteArrayInputStream(bytes))
    val map2 = ctxMaps.newMap(mapDescriptor)
    map2.read(dis, ctxValues)
    map1 should be (map2)
  }
  
  it should "support nested lists dumped to file" in {
    val map1 = ctxMaps.newMap(mapDescriptor)
    val seq = ctxValues.newValueSeq
    seq += jl.Long.valueOf(0)
    while (seq.memSize > 0) seq += jl.Long.valueOf(seq.memSize)
    map1 += ("seq" -> seq)
    val bytes = writeToBytes(map1)
    val dis = new DataInputStream(new ByteArrayInputStream(bytes))
    val map2 = ctxMaps.newMap(mapDescriptor)
    map2.read(dis, ctxValues)
    map1 should be (map2)
  }
  
  behavior of "StorableMapSeq"
  
  val adjustedInMemoryCount = ((mapCtxMemoryLimit - StorableSeq.baseMemoryUse) / mapSize).toInt
  
  it should "keep data in memory to maximum size" in {
    val seq = ctxMaps.newMapSeq
    val maps = (1 to adjustedInMemoryCount).map { i =>
      val map = ctxMaps.newMap(mapDescriptor)
      fillMap(baseNumber + i, map)
      seq += map
      seq.memSize should be (StorableSeq.baseMemoryUse + i * mapSize)
      map
    }.toArray
    val iterator = seq.iterator
    val valuePairs = iterator.zipWithIndex
    valuePairs foreach {
      case (map, i) => map should be (maps(i))
    }
    (1 to adjustedInMemoryCount) foreach { i =>
      val map = seq(i - 1)
      map should be (maps(i - 1))
    }
  }
  
  it should "dump to file when maximum size exceeded" in {
    val seq = ctxMaps.newMapSeq
    val maps = (1 to outMemoryCount).map { i =>
      val map = ctxMaps.newMap(mapDescriptor)
      fillMap(baseNumber + i, map)
      seq += map
      if (i <= adjustedInMemoryCount) seq.memSize should be (StorableSeq.baseMemoryUse + i * mapSize)
      else seq.memSize should be (0)
      map
    }.toArray
    val iterator = seq.iterator
    val valuePairs = iterator.zipWithIndex
    valuePairs foreach {
      case (map, i) => map should be (maps(i))
    }
    (1 to outMemoryCount) foreach { i =>
      val map = seq(i - 1)
      map should be (maps(i - 1))
    }
  }
  
  it should "support both sequential and random access to dumped values" in {
    val seq = ctxMaps.newMapSeq
    val maps = (1 to outMemoryCount).map { i =>
      val map = ctxMaps.newMap(mapDescriptor)
      fillMap(baseNumber + i, map)
      seq += map
      map
    }.toArray
    seq.switchToInput
    (1 to outMemoryCount) foreach { i =>
      val map = seq(i - 1)
      map should be (maps(i - 1))
    }
    (1 to outMemoryCount) foreach { i =>
      val perm = ((i - 1) * relPrimeMultiplier) / outMemoryCount + 1
      val map = seq(perm - 1)
      map should be (maps(perm - 1))
    }
  }
}