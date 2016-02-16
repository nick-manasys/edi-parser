package com.mulesoft.ltmdata

import collection.{ mutable => scm }

import java.io.File
import java.{ lang => jl, util => ju }

/** Storage context, used to track all map descriptors and assign indexes.
  */
abstract class StorageContext {
  
  var mapNumber = 0

  val maximumMemory: Option[Long]
  
  val emptyDescriptor: MapDescriptor

  def newMap(desc: MapDescriptor): ju.Map[String, Object]

  def newMap(keys: Array[String]): ju.Map[String, Object] = newMap(addDescriptor(keys))

  def newValueSeq: ju.List[Object]

  def newMapSeq: ju.List[ju.Map[String, Object]]
  
  def newOut: File

  def getDescriptor(index: Int): MapDescriptor

  def addDescriptor(keys: Array[String]): MapDescriptor

  def newMemoryResidentMap: ju.Map[String, Object] = {
    mapNumber += 1
    new MemoryResidentMap(mapNumber.toString).asInstanceOf[ju.Map[String, Object]]
  }

  def newMemoryResidentMap(from: ju.Map[String, Object]): ju.Map[String, Object] = {
    val map = newMemoryResidentMap
    map.putAll(from)
    map
  }
}

class MemoryStorageContext extends StorageContext {

  val maximumMemory = None
  
  def newMap(desc: MapDescriptor) = new ju.HashMap[String, Object].asInstanceOf[ju.Map[String, Object]]

  def newValueSeq = new ju.ArrayList[Object]

  def newMapSeq = new ju.ArrayList[ju.Map[String, Object]]

  def newOut = File.createTempFile("storedata", "sds")

  val emptyDescriptor = MapDescriptor(0, Array[String]())

  def getDescriptor(index: Int) = emptyDescriptor

  def addDescriptor(keys: Array[String]) = emptyDescriptor
}

class StorableStorageContext(val maxMemory: Long) extends StorageContext {

  val descriptorIndexes = scm.HashMap[Array[String], Int]()

  val descriptors = scm.Buffer[MapDescriptor]()

  val maximumMemory = Some(maxMemory)

  val emptyDescriptor = addDescriptor(Array[String]())

  def newMap(desc: MapDescriptor) = new StorableMap(desc).asInstanceOf[ju.Map[String, Object]]

  def newValueSeq = new StorableValueSeq(this)

  def newMapSeq = new StorableMapSeq(this).asInstanceOf[ju.List[ju.Map[String, Object]]]

  def newOut = File.createTempFile("storedata", "sds")

  def getDescriptor(index: Int) = descriptors(index)

  def addDescriptor(keys: Array[String]) = {
    val index = descriptorIndexes.getOrElseUpdate(keys, {
      val index = descriptors.size
      descriptors += MapDescriptor(index, keys)
      index
    })
    descriptors(index)
  }
}

trait MemoryResident {
  def memoryId: String
}
  
class MemoryResidentMap(id: String) extends ju.HashMap with MemoryResident {
  def memoryId = id
}

case class MapDescriptor(index: Int, keys: IndexedSeq[String]) {
  val keyNums = keys.zipWithIndex.toMap
}

object StorageContext {
  def workingContext = sys.props.get("com.mulesoft.ltmdata.memoryLimit") match {
    case Some(v) => new StorableStorageContext(v.toLong)
    case None => new MemoryStorageContext
  }
}