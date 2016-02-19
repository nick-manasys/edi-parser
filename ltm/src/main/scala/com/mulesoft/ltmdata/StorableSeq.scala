
package com.mulesoft.ltmdata

import annotation.tailrec
import collection.AbstractIterator
import collection.{ mutable => scm }

import java.{ util => ju }
import java.io.{ BufferedOutputStream, DataInput, DataOutput, DataOutputStream, File, FileOutputStream, RandomAccessFile }

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
abstract class StorableSeq[A <: Object](ctx: StorageContext) extends ju.AbstractList[A] with StorableStructure {

  import StorableSeq._

  abstract class StorageMode extends scm.Seq[A] {
    def memSize: Long
    def add(item: A): Unit
    def get(idx: Int): A
    def write(os: DataOutput): Unit
    def finishOutput: Unit
    def initInput: Unit

    def update(index: Int, item: A): Unit = {
      if (index == size) storageHandler.add(item)
      else throw new UnsupportedOperationException("Implement only supports appending, not modifying")
    }

    def +=(item: A): Unit = add(item)

    def apply(idx: Int): A = get(idx)
    
    def length: Int = size

    override def iterator = new StorableSequenceIterator
  }

  protected class MemoryStorage extends StorageMode {

    var buffer = new scm.ArrayBuffer[A]
    var totalSize = baseMemoryUse

    override def size = buffer.size

    def memSize = totalSize

    def add(item: A) = {
      buffer += item
      ctx.maximumMemory match {
        case Some(l) =>
          val itemSize =
            if (item.isInstanceOf[StorableStructure]) item.asInstanceOf[StorableStructure].memSize
            else ItemType.valueSize(item)
          totalSize += itemSize
          if (totalSize >= l) switchToFile(buffer)
        case None =>
      }
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

    override def size = storedCount

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
      storageHandler.apply(ind)
    }
    def remove = throw new UnsupportedOperationException("Sequence is not modifiable")
  }

  var storageHandler: StorageMode = new MemoryStorage

  protected def switchToFile(seq: Seq[A]): Unit

  protected def writeItem(item: A, os: DataOutput): Unit

  def memSize = storageHandler.memSize

  override def size = storageHandler.size

  override def add(item: A): Boolean = {
    storageHandler.add(item)
    true
  }

  def +=(item: A): Unit = storageHandler.add(item)

  def apply(idx: Int): A = storageHandler.get(idx)

  //  override def indexOf(item: A): Int = {
  //    @tailrec
  //    def findr(index: Int): Int = {
  //      if (index > size) -1
  //      else if (storageHandler.get(index) == item) index
  //      else findr(index + 1)
  //    }
  //    findr(0)
  //  }

  def get(idx: Int) = storageHandler.get(idx)

  override def iterator = storageHandler.iterator
  
  override def equals(other: Any) =
    other.isInstanceOf[StorableSeq[A]] && storageHandler == other.asInstanceOf[StorableSeq[A]].storageHandler

  def length = storageHandler.size

  def write(os: DataOutput) = storageHandler.write(os)

  def switchToInput = {
    storageHandler.finishOutput
    storageHandler.initInput
  }
}

class StorableMapSeq(ctx: StorageContext) extends StorableSeq[StorableMap](ctx) {

  protected class MapFileStorage(file: File, count: Int) extends BaseFileStorage(file, count) {

    def this(items: Seq[StorableMap], file: File) = {
      this(file, 0)
      outStream = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
      items.foreach { add(_) }
    }

    var inItem = 0

    def readItem = {
      val index = inStream.readShort
      val map = ctx.newMap(ctx.getDescriptor(index)).asInstanceOf[StorableMap]
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
      (0 to length).foreach { _ => storageHandler.add(MapType.read(is, ctx).asInstanceOf[StorableMap]) }
    } else {
      val path = is.readUTF
      val fileStore = new MapFileStorage(new File(path), length)
      storageHandler = fileStore
      fileStore.initInput
    }
  }
}

class StorableValueSeq(ctx: StorageContext) extends StorableSeq[Object](ctx) {

  protected class ValueFileStorage(file: File, count: Int) extends BaseFileStorage(file, count) {

    def this(items: Seq[Object], file: File) = {
      this(file, 0)
      outStream = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
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