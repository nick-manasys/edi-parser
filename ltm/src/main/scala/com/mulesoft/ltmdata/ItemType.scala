
package com.mulesoft.ltmdata

import java.io.{ DataInput, DataOutputStream }
import java.{ lang => jl }
import java.math.{ BigDecimal, BigInteger }

import scala.collection.{ mutable => scm }

/** Type definitions for stored data. Each type implements the appropriate handling to read and write values.
  */
sealed abstract class ItemType(val index: Int, val clas: Class[_ <: Object]) {
  def write(v: Object, os: DataOutputStream): Unit
  def read(is: DataInput, ctx: StructureContext): Object
}
case object StringType extends ItemType(0, classOf[String]) {
  def write(v: Object, os: DataOutputStream) = os.writeUTF(v.asInstanceOf[String])
  def read(is: DataInput, ctx: StructureContext) = is.readUTF
}
case object IntegerType extends ItemType(1, classOf[Integer]) {
  def write(v: Object, os: DataOutputStream) = os.writeInt(v.asInstanceOf[Integer].intValue)
  def read(is: DataInput, ctx: StructureContext) = Integer.valueOf(is.readInt)
}
case object LongType extends ItemType(2, classOf[jl.Long]) {
  def write(v: Object, os: DataOutputStream) = os.writeLong(v.asInstanceOf[Long].intValue)
  def read(is: DataInput, ctx: StructureContext) = jl.Long.valueOf(is.readLong)
}
case object DecimalType extends ItemType(3, classOf[BigDecimal]) {
  def write(v: Object, os: DataOutputStream) = {
    val d = v.asInstanceOf[BigDecimal]
    val byts = d.toBigInteger.toByteArray
    if (byts.length > Short.MaxValue) throw new IllegalStateException("Maximum length exceeded for value")
    os.writeInt(d.scale)
    os.writeShort(byts.length.toShort)
    os.write(byts, 0, byts.length)
  }
  def read(is: DataInput, ctx: StructureContext) = {
    val scale = is.readInt
    val length = is.readShort
    val bytes = Array.fill[Byte](length)(0)
    is.readFully(bytes)
    new BigDecimal(new BigInteger(bytes), scale)
  }
}
case object BufferType extends ItemType(4, classOf[scm.Buffer[Object]]) {
  def write(v: Object, os: DataOutputStream) = {
    val buffer = v.asInstanceOf[scm.Buffer[Object]]
    os.writeInt(buffer.size)
    buffer.foreach { v =>
      {
        val cname = v.getClass.getName
        ItemType.byName(cname) match {
          case null => throw new IllegalStateException(s"Unsupported value type $cname in buffer")
          case t => {
            os.writeByte(t.index)
            t.write(v, os)
          }
        }
      }
    }
  }
  def read(is: DataInput, ctx: StructureContext) = {
    val size = is.readInt
    val buffer = new scm.ArrayBuffer[Object](size)
    (0 to size).foreach { _ =>
      val typ = ItemType.types(is.readByte)
      buffer += typ.read(is, ctx)
    }
    buffer
  }
}
case object MapType extends ItemType(5, classOf[StorableMap]) {
  def write(v: Object, os: DataOutputStream) = {
    val map = v.asInstanceOf[StorableMap]
    os.writeShort(map.descriptor.index)
    map.write(os)
  }
  def read(is: DataInput, ctx: StructureContext) = {
    val map = ctx.newMap(ctx.getDescriptor(is.readShort))
    map.read(is, ctx)
    map
  }
}
case object ValueSeqType extends ItemType(6, classOf[StorableValueSeq]) {
  def write(v: Object, os: DataOutputStream) = {
    val seq = v.asInstanceOf[StorableValueSeq]
    seq.write(os)
  }
  def read(is: DataInput, ctx: StructureContext) = {
    val seq = ctx.newValueSeq
    seq.read(is)
    seq
  }
}
case object MapSeqType extends ItemType(7, classOf[StorableMapSeq]) {
  def write(v: Object, os: DataOutputStream) = {
    val seq = v.asInstanceOf[StorableMapSeq]
    seq.write(os)
  }
  def read(is: DataInput, ctx: StructureContext) = {
    val seq = ctx.newMapSeq
    seq.read(is)
    seq
  }
}

object ItemType {
  val types = Array[ItemType](StringType, IntegerType, LongType, DecimalType, MapType, ValueSeqType, MapSeqType)
  val byName = types.foldLeft(Map[String, ItemType]())((map, typ) => map + (typ.clas.getName -> typ))

  def itemType(value: Object) = {
    val cname = value.getClass.getName
    ItemType.byName(cname) match {
      case null => throw new IllegalStateException(s"Unsupported value type $cname")
      case t => t
    }
  }

  def stringSize(s: String) = (39 + s.size) & -8

  def valueSize(value: Object): Long = value match {
    case s: String => stringSize(s)
    case i: Integer => 16
    case l: jl.Long => 24
    case d: BigDecimal => 32 + d.precision / 3
    case struct: StorableStructure => struct.memSize
  }
}