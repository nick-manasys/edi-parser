
package com.mulesoft.ltmdata

import java.io.{ DataInput, DataOutput }
import java.{ lang => jl }
import java.math.BigDecimal

import scala.collection.{ mutable => scm }

/** Storable map for data. The storage format for a map is a leading count followed by key-value pairs. The key is given
  * by index in the descriptor key list. Indexes are one byte if the number of possible keys is 256 or less, two bytes
  * if more. Values start with a one-byte type code, and generally use standard Java DataInput/DataOutput formats, with
  * UTF for Strings and BigDecimals written as an integer scale followed by a byte array with leading length byte.
  */
class StorableMap(val descriptor: MapDescriptor) extends scm.HashMap[String, Object] with StorableStructure {

  var memorySize: Long = 0

  def memSize = memorySize
  
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
    if (e eq null) memorySize += valueSize(kv._2)
    else {
      memorySize += valueSize(kv._2) - valueSize(e.value)
      e.value = kv._2
    }
    this
  }

  def write(os: DataOutput): Unit = {
    val writeIndex = if (descriptor.keys.size <= 256) (n: Int) => os.writeByte(n) else (n: Int) => os.writeShort(n)
    os.writeShort(size)
    foreach {
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
    (0 to size) foreach { _ =>
      val index = readIndex()
      val typ = ItemType.types(is.readByte)
      put(descriptor.keys(index), typ.read(is, ctx))
    }
  }
}