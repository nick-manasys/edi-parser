package com.mulesoft.ltmdata

import collection.{ mutable => scm }

import java.io.File
import java.{ lang => jl, util => ju }

/** Structure context, used to track all map descriptors and assign indexes.
  */
abstract class StructureContext {

  val descriptors = scm.Buffer[MapDescriptor]()
  
  val maximumMemory: Option[Long]

  def newMap(desc: MapDescriptor): ju.Map[String, Object]

  def newValueSeq: jl.Iterable[Object]
  
  def newMapSeq: jl.Iterable[_ <: ju.Map[String,Object]]

  def newOut: File
  
  def getDescriptor(index: Int) = descriptors(index)
  
  def addDescriptor(keys: IndexedSeq[String]) = {
    val desc = MapDescriptor(descriptors.size, keys)
    descriptors += desc
    desc
  }
}

class MemoryStructureContext extends StructureContext {
  
  val maximumMemory = None
  
  def newMap(desc: MapDescriptor) = new ju.HashMap[String, Object]

  def newValueSeq = new ju.ArrayList[Object]
  
  def newMapSeq = new ju.ArrayList[ju.Map[String, Object]]

  def newOut = File.createTempFile("storedata", "sds")
}

class StorableStructureContext(val maxMemory: Long) extends StructureContext {
  
  val maximumMemory = Some(maxMemory)

  def newMap(desc: MapDescriptor) = new StorableMap(desc)

  def newValueSeq = new StorableValueSeq(this)
  
  def newMapSeq = new StorableMapSeq(this)

  def newOut = File.createTempFile("storedata", "sds")
}

case class MapDescriptor(index: Int, keys: IndexedSeq[String]) {
  val keyNums = keys.zipWithIndex.toMap
}

object StructureContext {
  val workingContext = sys.props.get("com.mulesoft.ltmdata.memoryLimit") match {
    case Some(v) => new StorableStructureContext(v.toLong)
    case None => new MemoryStructureContext
  }
}