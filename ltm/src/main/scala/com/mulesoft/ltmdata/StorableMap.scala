
package com.mulesoft.ltmdata

import java.io.{ DataInput, DataOutput }
import java.{ lang => jl, util => ju }
import java.math.BigDecimal

import collection.{ mutable => scm }
import collection.JavaConverters._

/** Storable map for data. The storage format for a map is a leading count followed by key-value pairs. The key is given
  * by index in the descriptor key list. Indexes are one byte if the number of possible keys is 256 or less, two bytes
  * if more. Values are in standard form as defined by item type.
  */
class StorableMap(val descriptor: MapDescriptor) extends ju.AbstractMap[String, Object] with StorableStructure {
  
  import StorableMap._

  class SizeTrackingMap extends scm.HashMap[String, Object] {

      var memorySize = baseMemoryUse
      
      override def put(key: String, value: Object): Option[Object] = {
        val e = findOrAddEntry(key, value)
        memorySize += ItemType.valueSize(value) - (if (e eq null) 0 else ItemType.valueSize(e.value))
        if (e eq null) None
        else { val v = e.value; e.value = value; Some(v) }
      }

      override def remove(key: String): Option[Object] = {
        val e = removeEntry(key)
        if (e eq null) None
        else {
          memorySize -= ItemType.valueSize(e.value)
          Some(e.value)
        }
      }

      override def +=(kv: (String, Object)): this.type = {
        val e = findOrAddEntry(kv._1, kv._2)
        if (e eq null) memorySize += ItemType.valueSize(kv._2)
        else {
          memorySize += ItemType.valueSize(kv._2) - ItemType.valueSize(e.value)
          e.value = kv._2
        }
        this
      }
  }
  
  val baseMap = new SizeTrackingMap

  def memSize = baseMap.memorySize
  
  def get(key: String): Option[Object] = baseMap.get(key)
  
  def iterator: Iterator[(String, Object)] = baseMap.iterator
  
  def += (kv: (String, Object)) = baseMap += kv
  def -= (key: String) = baseMap -= key

  override def put(key: String, value: Object): Object = baseMap.put(key, value) match {
    case Some(v) => v
    case None => null
  }

  def remove(key: String): Object = baseMap.remove(key) match {
    case Some(v) => v
    case None => null
  }
  
  def entrySet(): ju.Set[ju.Map.Entry[String, Object]] = {
    baseMap.map { case (k, v) =>
      new ju.AbstractMap.SimpleEntry(k, v).asInstanceOf[ju.Map.Entry[String, Object]] }.toSet.asJava
  }

  def write(os: DataOutput): Unit = {
    val writeIndex = if (descriptor.keys.size <= 256) (n: Int) => os.writeByte(n) else (n: Int) => os.writeShort(n)
    os.writeShort(size)
    baseMap.foreach {
      case (k, v) => descriptor.keyNums.getOrElse(k, -1) match {
        case -1 => throw new IllegalStateException(s"Key $k not included in map descriptor")
        case n =>
          writeIndex(n)
          val cname = v.getClass.getName
          ItemType.byName(cname) match {
            case null => throw new IllegalStateException(s"Unsupported value type $cname in map (with key $k)")
            case t => {
              os.writeByte(t.index)
              t.write(v, os)
            }
          }
      }
    }
  }

  def read(is: DataInput, ctx: StructureContext): Unit = {
    val readIndex =
      if (descriptor.keys.size <= 256) () => is.readByte.toInt & 0xFF
      else () => is.readShort.toInt & 0xFFFF
    val size = is.readShort
    (0 until size) foreach { _ =>
      val index = readIndex()
      val typ = ItemType.types(is.readByte)
      put(descriptor.keys(index), typ.read(is, ctx))
    }
  }
}

object StorableMap {
  val baseMemoryUse: Long = 64
}