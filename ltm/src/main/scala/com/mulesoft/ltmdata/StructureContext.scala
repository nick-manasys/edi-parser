package com.mulesoft.ltmdata

import collection.{ mutable => scm }

import java.io.File

/** Structure context, used to track all map descriptors and assign indexes.
  */
class StructureContext(val maxMemory: Long) {

  val descriptors = scm.Buffer[MapDescriptor]()

  def newMap(desc: MapDescriptor) = new StorableMap(desc)

  def newValueSeq = new StorableValueSeq(this)
  
  def newMapSeq = new StorableMapSeq(this)

  def newOut = File.createTempFile("storedata", "sds")
  
  def getDescriptor(index: Int) = descriptors(index)
  
  def addDescriptor(keys: IndexedSeq[String]) = {
    val desc = MapDescriptor(descriptors.size, keys)
    descriptors += desc
    desc
  }
}

case class MapDescriptor(index: Int, keys: IndexedSeq[String]) {
  val keyNums = keys.zipWithIndex.toMap
}
