package com.mulesoft.ltmdata

import scala.collection.{ generic => scg, mutable => scm }

import java.io.{ BufferedOutputStream, DataInput, DataOutput, DataOutputStream, File, FileOutputStream, RandomAccessFile }

/** Storable buffer for simple data values (no maps or lists).
  */
class StorableBuffer[A](ctx: StructureContext)
  extends scm.AbstractBuffer[A]
  with scm.Buffer[A]
  with scg.GenericTraversableTemplate[A, StorableBuffer]
  with scm.BufferLike[A, StorableBuffer[A]]
  with scm.IndexedSeqOptimized[A, StorableBuffer[A]]
  with scm.Builder[A, StorableBuffer[A]]
  with scm.ResizableArray[A]
  with MemorySized {

  lazy val companionInstance = new StorableBufferFactory(ctx) 
  override def companion: scg.GenericCompanion[StorableBuffer] = companionInstance
  
  val maxMemory = 1024 * 1024
  var totalSize: Long = 0
  var storeFile: File = null
  var outStream: DataOutputStream = null
  var storedCount = 0
  var inStream: RandomAccessFile = null
  var inItem = 0

  def memSize = totalSize
  
  private def readMap = {
    val index = inStream.readShort
    val map = ctx.newMap(ctx.descriptors(index))
    map.read(inStream, ctx)
    inItem += 1
    map
  }
  
  private def writeMap(map: StorableMap) = {
      val start = outStream.size
      outStream.writeShort(map.descriptor.index)
      map.write(outStream)
      val size: Int = outStream.size - start
      outStream.write(size)
  }

  def clear() { reduceToSize(0) }

  override def sizeHint(len: Int) {
    if (len > size && len >= 1) {
      val newarray = new Array[AnyRef](len)
      scala.compat.Platform.arraycopy(array, 0, newarray, 0, size0)
      array = newarray
    }
  }

  override def +=(elem: A): this.type = {
    ensureSize(size0 + 1)
    array(size0) = elem.asInstanceOf[AnyRef]
    size0 += 1
    this
  }

  override def ++=(xs: TraversableOnce[A]): this.type = xs match {
    case v: scala.collection.IndexedSeqLike[_, _] =>
      val n = v.length
      ensureSize(size0 + n)
      v.copyToArray(array.asInstanceOf[scala.Array[Any]], size0, n)
      size0 += n
      this
    case _ =>
      super.++=(xs)
  }

  def +=:(elem: A): this.type = {
    ensureSize(size0 + 1)
    copy(0, 1, size0)
    array(0) = elem.asInstanceOf[AnyRef]
    size0 += 1
    this
  }

  override def ++=:(xs: TraversableOnce[A]): this.type = { insertAll(0, xs.toTraversable); this }

  def insertAll(n: Int, seq: Traversable[A]) {
    if (n < 0 || n > size0) throw new IndexOutOfBoundsException(n.toString)
    val len = seq.size
    val newSize = size0 + len
    ensureSize(newSize)
    copy(n, n + len, size0 - n)
    seq.copyToArray(array.asInstanceOf[Array[Any]], n)
    size0 = newSize
  }

  override def remove(n: Int, count: Int) {
    require(count >= 0, "removing negative number of elements")
    if (n < 0 || n > size0 - count) throw new IndexOutOfBoundsException(n.toString)
    copy(n + count, n, size0 - (n + count))
    reduceToSize(size0 - count)
  }

  def remove(n: Int): A = {
    val result = apply(n)
    remove(n, 1)
    result
  }

  def result: StorableBuffer[A] = this
  
  def finish = {
    if (storeFile != null) outStream.close
  }

  def write(os: DataOutput): Unit = {
    os.writeShort(length)
    if (storeFile == null) {
      os.writeBoolean(true)
      foreach { map => map.write(os) }
    } else {
      os.writeBoolean(false)
      os.writeUTF(storeFile.getCanonicalPath)
    }
  }

  def read(is: DataInput): Unit = {
    val length = is.readShort
    if (is.readBoolean) (0 to length).foreach { _ => buffer += MapType.read(is, ctx) }
    else {
      val path = is.readUTF
      storeFile = new File(path)
      inStream = new RandomAccessFile(storeFile, "r")
    }
  }
}

class StorableMapBuffer(ctx: StructureContext) extends StorableBuffer[scm.Map[String, Object]](ctx) {
  
}

class StorableValueBuffer(ctx: StructureContext) extends StorableBuffer[scm.Map[String, Object]](ctx) {
  
}

/** Factory class for the `StorableBuffer` class.
  *
  * $factoryInfo
  * @define coll storable buffer
  * @define Coll `StorableBuffer`
  */
class StorableBufferFactory(ctx: StructureContext) extends scg.SeqFactory[StorableBuffer] {
  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: scg.CanBuildFrom[Coll, A, StorableBuffer[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: scm.Builder[A, StorableBuffer[A]] = new StorableBuffer[A](ctx)
}