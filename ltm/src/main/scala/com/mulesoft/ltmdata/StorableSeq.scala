
package com.mulesoft.ltmdata

import java.{ util => ju }
import java.io.{ BufferedOutputStream, DataInput, DataOutput, DataOutputStream, File, FileOutputStream, RandomAccessFile }

import collection.AbstractIterator
import collection.{ mutable => scm }

/** Storable sequence of data maps or values. Each storable sequence is backed by a separate file. Once the decision is
  * made to start storing the data (as determined by the maxMemory size for the sequence) the storage file is created
  * and all current sequence data is written to the file and then discarded. As more values are added they are appended
  * directly to the file. It's possible that storing may take place at multiple levels, so that an ancestor of a
  * storable sequence will itself be storing to a file.
  * 
  * The storage format is optimized for sequential access, but does allow random access with a substantial performance
  * penalty. Positioning forward is done by just reading and discarding values. Positioning backward is done by
  * resetting input to the start of the store file and reading forward.
  */
abstract class StorableSeq[A <: Object](ctx: StructureContext) extends scm.Seq[A] with StorableStructure with Iterable[A] {

  import StorableSeq._
  
  trait StorageMode {
    def size: Int
    def memSize: Long
    def add(item: A): Unit
    def get(idx: Int): A
    def write(os: DataOutput): Unit
    def finishOutput: Unit
    def initInput: Unit
  }

  protected class MemoryStorage extends StorageMode {

    var buffer = new scm.ArrayBuffer[A]
    var totalSize = baseMemoryUse

    def size = buffer.size

    def memSize = totalSize

    def add(item: A) = {
      buffer += item
      val itemSize =
        if (item.isInstanceOf[StorableStructure]) item.asInstanceOf[StorableStructure].memSize
        else ItemType.valueSize(item)
      totalSize += itemSize
      if (totalSize >= ctx.maxMemory) switchToFile(buffer)
    }

    def get(idx: Int) = buffer(idx)

    def write(os: DataOutput): Unit = {
      os.writeInt(length)
      os.writeBoolean(true)
      buffer.foreach { item => writeItem(item, os) }
    }

    def finishOutput = {}
    def initInput = {}
  }

  protected abstract class BaseFileStorage(file: File, count: Int) extends StorageMode {
    var storedCount = count
    var outStream: DataOutputStream = null
    var inStream: RandomAccessFile = null

    def size = storedCount

    def memSize = 0

    def write(os: DataOutput): Unit = {
      os.writeInt(length)
      os.writeBoolean(false)
      os.writeUTF(file.getAbsolutePath)
    }

    def finishOutput = {
      if (outStream != null) {
        outStream.close
        outStream = null
      }
    }

    def initInput = {
      if (inStream == null) inStream = new RandomAccessFile(file, "r")
    }
  }

  protected class StorableSequenceIterator extends AbstractIterator[A] with ju.Iterator[A] {
    var index = 0

    switchToInput

    override def size = storageHandler.size
    def hasNext = index < size
    def next: A = {
      val ind = index
      index += 1
      apply(ind)
    }
  }

  var storageHandler: StorageMode = new MemoryStorage

  protected def switchToFile(seq: Seq[A]): Unit

  protected def writeItem(item: A, os: DataOutput): Unit

  override def size = storageHandler.size

  def memSize = storageHandler.memSize

  def add(item: A): Unit = storageHandler.add(item)
  def update(index: Int, item: A): Unit = {
    if (index == size) storageHandler.add(item)
    else throw new UnsupportedOperationException("Implement only supports appending, not modifying")
  }

  def +=(item: A): Unit = storageHandler.add(item)

  def apply(idx: Int): A = storageHandler.get(idx)

  def iterator: Iterator[A] = new StorableSequenceIterator

  def length = storageHandler.size

  def write(os: DataOutput) = storageHandler.write(os)

  def switchToInput = {
    storageHandler.finishOutput
    storageHandler.initInput
  }
}

class StorableMapSeq(ctx: StructureContext) extends StorableSeq[StorableMap](ctx) {

  protected class MapFileStorage(file: File, count: Int) extends BaseFileStorage(file, count) {

    def this(items: Seq[StorableMap], file: File) = {
      this(file, 0)
      outStream = new DataOutputStream(new FileOutputStream(file))
      items.foreach { add(_) }
    }

    var inItem = 0

    def readItem = {
      val index = inStream.readShort
      val map = ctx.newMap(ctx.descriptors(index))
      map.read(inStream, ctx)
      inItem += 1
      map
    }

    def add(map: StorableMap) = {
      if (outStream == null) throw new IllegalStateException("Cannot add to seq after finish")
      writeItem(map, outStream)
      storedCount += 1
    }

    def get(idx: Int) = {
      if (inItem > idx) {
        inStream.seek(0)
        inItem = 0
      }
      while (inItem < idx) readItem
      readItem
    }
  }

  protected def switchToFile(seq: Seq[StorableMap]) = {
    storageHandler = new MapFileStorage(seq, ctx.newOut)
  }

  protected def writeItem(map: StorableMap, os: DataOutput) = {
    os.writeShort(map.descriptor.index)
    map.write(os)
  }

  def read(is: DataInput) = {
    val length = is.readInt
    if (is.readBoolean) {
      (0 to length).foreach { _ => storageHandler.add(MapType.read(is, ctx)) }
    } else {
      val path = is.readUTF
      val fileStore = new MapFileStorage(new File(path), length)
      storageHandler = fileStore
      fileStore.initInput
    }
  }
}

class StorableValueSeq(ctx: StructureContext) extends StorableSeq[Object](ctx) {

  protected class ValueFileStorage(file: File, count: Int) extends BaseFileStorage(file, count) {

    def this(items: Seq[Object], file: File) = {
      this(file, 0)
      outStream = new DataOutputStream(new FileOutputStream(file))
      items.foreach { add(_) }
    }

    var inItem = 0

    def readItem(is: DataInput) = {
      val value = ItemType.readTyped(is, ctx)
      inItem += 1
      value
    }

    def add(value: Object) = {
      if (outStream == null) throw new IllegalStateException("Cannot add to seq after finish")
      writeItem(value, outStream)
      storedCount += 1
    }

    def get(idx: Int) = {
      if (inItem > idx) {
        inStream.seek(0)
        inItem = 0
      }
      while (inItem < idx) readItem(inStream)
      readItem(inStream)
    }
  }
  
  protected def writeItem(value: Object, os: DataOutput) = ItemType.writeTyped(value, os)

  protected def switchToFile(seq: Seq[Object]) = {
    storageHandler = new ValueFileStorage(seq, ctx.newOut)
  }

  def read(is: DataInput) = {
    val length = is.readInt
    if (is.readBoolean) {
      (1 to length).foreach { _ => storageHandler.add(ItemType.readTyped(is, ctx)) }
    } else {
      val path = is.readUTF
      val fileStore = new ValueFileStorage(new File(path), length)
      storageHandler = fileStore
      fileStore.initInput
    }
  }
}

object StorableSeq {
  val baseMemoryUse: Long = 64
}