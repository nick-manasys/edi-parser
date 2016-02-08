
package com.mulesoft.ltmdata

import java.io.{ DataInput, DataOutput }
import java.{ lang => jl }
import java.math.{ BigDecimal, BigInteger }

import scala.collection.{ mutable => scm }

/** Type definitions for stored data. Each type implements the appropriate handling to read and write values. The stored
  * forms each start with a one-byte type code, and generally use standard Java DataInput/DataOutput formats, with
  * UTF for Strings, BigIntegers as a byte array with leading length byte, and BigDecimals as an integer scale followed
  * by a BigInteger representation.
  */
sealed abstract class ItemType(val index: Int, val clas: Class[_ <: Object]) {
  def write(v: Object, os: DataOutput): Unit
  def read(is: DataInput, ctx: StructureContext): Object
}
case object StringType extends ItemType(0, classOf[String]) {
  def write(v: Object, os: DataOutput) = os.writeUTF(v.asInstanceOf[String])
  def read(is: DataInput, ctx: StructureContext) = is.readUTF
}
case object IntegerType extends ItemType(1, classOf[Integer]) {
  def write(v: Object, os: DataOutput) = os.writeInt(v.asInstanceOf[Integer].intValue)
  def read(is: DataInput, ctx: StructureContext) = Integer.valueOf(is.readInt)
}
case object LongType extends ItemType(2, classOf[jl.Long]) {
  def write(v: Object, os: DataOutput) = os.writeLong(v.asInstanceOf[Long].intValue)
  def read(is: DataInput, ctx: StructureContext) = jl.Long.valueOf(is.readLong)
}
case object BigIntegerType extends ItemType(3, classOf[BigInteger]) {
  def write(v: Object, os: DataOutput) = {
    val b = v.asInstanceOf[BigInteger]
    val byts = b.toByteArray
    if (byts.length > Short.MaxValue) throw new IllegalStateException("Maximum length exceeded for value")
    os.writeShort(byts.length.toShort)
    os.write(byts, 0, byts.length)
  }
  def read(is: DataInput, ctx: StructureContext) = {
    val length = is.readShort
    val bytes = Array.fill[Byte](length)(0)
    is.readFully(bytes)
    new BigInteger(bytes)
  }
}
case object BigDecimalType extends ItemType(4, classOf[BigDecimal]) {
  def write(v: Object, os: DataOutput) = {
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
case object MapType extends ItemType(5, classOf[StorableMap]) {
  def write(v: Object, os: DataOutput) = {
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
  def write(v: Object, os: DataOutput) = {
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
  def write(v: Object, os: DataOutput) = {
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
  val types = Array[ItemType](StringType, IntegerType, LongType, BigIntegerType, BigDecimalType, MapType, ValueSeqType, MapSeqType)
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
    case b: BigInteger => 32 + b.bitLength / 8
    case d: BigDecimal => 32 + d.precision / 3
    case struct: StorableStructure => struct.memSize
  }

  def writeTyped(value: Object, os: DataOutput): Unit = {
    val itemType = ItemType.itemType(value)
    os.writeByte(itemType.index)
    itemType.write(value, os)
  }

  def readTyped(is: DataInput, ctx: StructureContext): Object = {
    val index = is.readByte
    val itemType = ItemType.types(index)
    itemType.read(is, ctx)
  }
}