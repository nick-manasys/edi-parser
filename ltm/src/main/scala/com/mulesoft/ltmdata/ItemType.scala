
package com.mulesoft.ltmdata

import java.io.{ DataInput, DataOutput }
import java.{ lang => jl }
import java.math.{ BigDecimal, BigInteger }
import java.util.{ GregorianCalendar, TimeZone }
import org.threeten.bp.{ LocalDate, LocalDateTime, LocalTime }
import scala.collection.{ mutable => scm }
import spire.math.Number

/** Type definitions for stored data. Each type implements the appropriate handling to read and write values. The stored
  * forms each start with a one-byte type code, and generally use standard Java DataInput/DataOutput formats, with
  * UTF for Strings, BigIntegers as a byte array with leading length byte, and BigDecimals as an integer scale followed
  * by a BigInteger representation.
  */
sealed abstract class ItemType(val clas: Class[_ <: Object]) {
  
  val index = ItemType.nextIndex
  ItemType.nextIndex += 1

  def write(v: Object, os: DataOutput): Unit
  def read(is: DataInput, ctx: StorageContext): Object
}
case object StringType extends ItemType(classOf[String]) {
  def write(v: Object, os: DataOutput) = os.writeUTF(v.asInstanceOf[String])
  def read(is: DataInput, ctx: StorageContext) = is.readUTF
}
case object IntegerType extends ItemType(classOf[Integer]) {
  def write(v: Object, os: DataOutput) = os.writeInt(v.asInstanceOf[Integer].intValue)
  def read(is: DataInput, ctx: StorageContext) = Integer.valueOf(is.readInt)
}
case object LongType extends ItemType(classOf[jl.Long]) {
  def write(v: Object, os: DataOutput) = os.writeLong(v.asInstanceOf[Long].intValue)
  def read(is: DataInput, ctx: StorageContext) = jl.Long.valueOf(is.readLong)
}
case object BigIntegerType extends ItemType(classOf[BigInteger]) {
  def write(v: Object, os: DataOutput) = {
    val b = v.asInstanceOf[BigInteger]
    val byts = b.toByteArray
    if (byts.length > Short.MaxValue) throw new IllegalStateException("Maximum length exceeded for value")
    os.writeShort(byts.length.toShort)
    os.write(byts, 0, byts.length)
  }
  def read(is: DataInput, ctx: StorageContext) = {
    val length = is.readShort
    val bytes = Array.fill[Byte](length)(0)
    is.readFully(bytes)
    new BigInteger(bytes)
  }
}
case object BigDecimalType extends ItemType(classOf[BigDecimal]) {
  def write(v: Object, os: DataOutput) = {
    val d = v.asInstanceOf[BigDecimal]
    val byts = d.toBigInteger.toByteArray
    if (byts.length > Short.MaxValue) throw new IllegalStateException("Maximum length exceeded for value")
    os.writeInt(d.scale)
    os.writeShort(byts.length.toShort)
    os.write(byts, 0, byts.length)
  }
  def read(is: DataInput, ctx: StorageContext) = {
    val scale = is.readInt
    val length = is.readShort
    val bytes = Array.fill[Byte](length)(0)
    is.readFully(bytes)
    new BigDecimal(new BigInteger(bytes), scale)
  }
}
case object NumberIntegerType extends ItemType(classOf[Number]) {
  def write(v: Object, os: DataOutput) = os.write(v.asInstanceOf[Number].toInt)
  def read(is: DataInput, ctx: StorageContext) = Number(is.readInt)
}
case object NumberLongType extends ItemType(classOf[Number]) {
  def write(v: Object, os: DataOutput) = os.writeLong(v.asInstanceOf[Number].toLong)
  def read(is: DataInput, ctx: StorageContext) = Number(is.readLong)
}
case object NumberBigIntegerType extends ItemType(classOf[Number]) {
  def write(v: Object, os: DataOutput) = BigIntegerType.write(v.asInstanceOf[Number].toBigInt, os)
  def read(is: DataInput, ctx: StorageContext) = Number(BigIntegerType.read(is, ctx))
}
case object NumberBigDecimalType extends ItemType(classOf[Number]) {
  def write(v: Object, os: DataOutput) = BigDecimalType.write(v.asInstanceOf[Number].toBigDecimal, os)
  def read(is: DataInput, ctx: StorageContext) = Number(BigDecimalType.read(is, ctx))
}
case object BooleanType extends ItemType(classOf[jl.Boolean]) {
  def write(v: Object, os: DataOutput) = os.writeBoolean(v.asInstanceOf[jl.Boolean])
  def read(is: DataInput, ctx: StorageContext) = jl.Boolean.valueOf(is.readBoolean)
}
case object GregorianCalendarType extends ItemType(classOf[GregorianCalendar]) {
  def write(v: Object, os: DataOutput) = {
    val cal = v.asInstanceOf[GregorianCalendar]
    os.writeUTF(cal.getTimeZone.getID)
    os.writeLong(cal.getTimeInMillis)
  }
  def read(is: DataInput, ctx: StorageContext) = {
    val zone = TimeZone.getTimeZone(is.readUTF)
    val cal = new GregorianCalendar(zone)
    cal.setTimeInMillis(is.readLong)
    cal
  }
}
case object LocalDateType extends ItemType(classOf[LocalDate]) {
  def write(v: Object, os: DataOutput) = {
    val date = v.asInstanceOf[LocalDate]
    os.writeShort(date.getYear)
    os.writeByte(date.getMonthValue)
    os.writeByte(date.getDayOfMonth)
  }
  def read(is: DataInput, ctx: StorageContext) = {
    val year = is.readShort
    val month = is.readByte
    val dayOfMonth = is.readByte
    LocalDate.of(year, month, dayOfMonth)
  }
}
case object LocalDateTimeType extends ItemType(classOf[LocalDateTime]) {
  def write(v: Object, os: DataOutput) = {
    val date = v.asInstanceOf[LocalDate]
    os.writeShort(date.getYear)
    os.writeByte(date.getMonthValue)
    os.writeByte(date.getDayOfMonth)
  }
  def read(is: DataInput, ctx: StorageContext) = {
    val year = is.readShort
    val month = is.readByte
    val dayOfMonth = is.readByte
    LocalDate.of(year, month, dayOfMonth)
  }
}
case object LocalTimeType extends ItemType(classOf[LocalTime]) {
  def write(v: Object, os: DataOutput) = {
    val date = v.asInstanceOf[LocalTime]
    os.writeByte(date.getHour)
    os.writeByte(date.getMinute)
    os.writeByte(date.getSecond)
    os.writeInt(date.getNano)
  }
  def read(is: DataInput, ctx: StorageContext) = {
    val hour = is.readByte
    val minute = is.readByte
    val second = is.readByte
    val nano = is.readInt
    LocalTime.of(hour, minute, second, nano)
  }
}
case object MapType extends ItemType(classOf[StorableMap]) {
  def write(v: Object, os: DataOutput) = {
    val map = v.asInstanceOf[StorableMap]
    os.writeShort(map.descriptor.index)
    map.write(os)
  }
  def read(is: DataInput, ctx: StorageContext) = {
    val map = ctx.newMap(ctx.getDescriptor(is.readShort)).asInstanceOf[StorableMap]
    map.read(is, ctx)
    map
  }
}
case object ValueSeqType extends ItemType(classOf[StorableValueSeq]) {
  def write(v: Object, os: DataOutput) = {
    val seq = v.asInstanceOf[StorableValueSeq]
    seq.write(os)
  }
  def read(is: DataInput, ctx: StorageContext) = {
    val seq = ctx.newValueSeq.asInstanceOf[StorableValueSeq]
    seq.read(is)
    seq
  }
}
case object MapSeqType extends ItemType(classOf[StorableMapSeq]) {
  def write(v: Object, os: DataOutput) = {
    val seq = v.asInstanceOf[StorableMapSeq]
    seq.write(os)
  }
  def read(is: DataInput, ctx: StorageContext) = {
    val seq = ctx.newMapSeq.asInstanceOf[StorableMapSeq]
    seq.read(is)
    seq
  }
}

object ItemType {

  var nextIndex = 0

  val types = Array[ItemType](StringType, IntegerType, LongType, BigIntegerType, BigDecimalType, NumberIntegerType, NumberLongType, NumberBigIntegerType, NumberBigDecimalType, BooleanType, GregorianCalendarType, LocalDateType, LocalDateTimeType, LocalTimeType, MapType, ValueSeqType, MapSeqType)
  val byName = types.foldLeft(Map[String, ItemType]())((map, typ) => map + (typ.clas.getName -> typ))
  (0 until types.length).foreach { i => if (types(i).index != i)  throw new IllegalStateException(s"Error in item type sequence at $i") }

  def itemType(value: Object): ItemType = {
    value match {
      case n: Number =>
        if (n.withinInt) NumberIntegerType
        else if (n.withinLong) NumberLongType
        else if (n.isWhole) NumberBigIntegerType
        else NumberBigDecimalType
      case _ =>
        val cname = value.getClass.getName
        ItemType.byName(cname) match {
          case null => throw new IllegalStateException(s"Unsupported value type $cname")
          case t => t
        }
    }
  }

  def stringSize(s: String) = (39 + s.size) & -8

  def valueSize(value: Object): Long = value match {
    case s: String => stringSize(s)
    case i: Integer => 16
    case l: jl.Long => 24
    case b: BigInteger => 40
    case d: BigDecimal => 56
    case c: GregorianCalendar => 32
    case d: LocalDate => 24
    case d: LocalDateTime => 32
    case t: LocalTime => 24
    case n: Number =>
      if (n.canBeInt) 24
      else if (n.canBeLong) 32
      else if (n.isWhole) 48
      else 64
    case struct: StorableStructure => struct.memSize
    case _: MemoryResident => 0
    case _ => throw new IllegalStateException(s"Unknown size for value of type ${value.getClass.getName}")
  }

  def writeTyped(value: Object, os: DataOutput): Unit = {
    val itemType = ItemType.itemType(value)
    os.writeByte(itemType.index)
    itemType.write(value, os)
  }

  def readTyped(is: DataInput, ctx: StorageContext): Object = {
    val index = is.readByte
    val itemType = ItemType.types(index)
    itemType.read(is, ctx)
  }
}