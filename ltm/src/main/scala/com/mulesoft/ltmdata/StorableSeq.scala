
package com.mulesoft.ltmdata

import collection.AbstractIterator
import collection.{ immutable => sci, mutable => scm }

import java.io.{ BufferedOutputStream, DataInput, DataOutput, DataOutputStream, File, FileOutputStream, RandomAccessFile }

/** Storable sequence of data maps. Each storable sequence is backed by a separate file. Once the decision is made to
  * start storing the data (as determined by the maxMemory size for the sequence) the storage file is created and all
  * current child maps are written to the file and then discarded. As more child maps are added they are appended to the
  * file and then discarded. It's possible that storing may take place at multiple levels, so that an ancestor of a
  * storable sequence will itself be storing to a file. No special format is used for the stored data, it just consists
  * of the concatenated data for all child maps. Reading back the child maps is optimized for sequential retrieval, with
  * relative position used to move to the desired child map.
  */
class StorableSeq[A <: Object](ctx: StructureContext) extends Seq[A] with StorableStructure {

  trait StorageMode {
    def size: Int
    def memSize: Long
    def add(item: A): Unit
    def get(idx: Int): A
    def write(os: DataOutput): Unit
    def finish: Unit
  }

  class MemoryStorage extends StorageMode {

    var buffer = new scm.ArrayBuffer[A]
    var totalSize: Long = 0

    def size = buffer.size
    
    def memSize = totalSize
    
    def add(item: A) = {
      buffer += item
      val itemSize =
        if (item.isInstanceOf[StorableStructure]) item.asInstanceOf[StorableStructure].memSize
        else ItemType.valueSize(item)
      totalSize += itemSize
      if (totalSize >= maxMemory) switchToFile(buffer)
    }
    
    def get(idx: Int) = buffer(idx)

    def write(os: DataOutput): Unit = {
      os.writeShort(length)
      os.writeBoolean(true)
      buffer.foreach { item => writeItem(item, os) }
    }

    def finish = {}
  }

  class BaseFileStorage(file: File) extends StorageMode {
    var storedCount = 0
    var outStream: DataOutputStream = null
    var inStream: RandomAccessFile = null

    def size = storedCount
    
    def memSize = 0

    def write(os: DataOutput): Unit = {
      os.writeShort(length)
      os.writeBoolean(false)
      os.writeUTF(file.getAbsolutePath)
    }

    def finish = outStream.close

    def initInput = {
      inStream = new RandomAccessFile(file, "r")
    }
  }

  class StorableSequenceIterator extends AbstractIterator[A] {
    var index = 0
    def hasNext = index < size
    def next: A = {
      val ind = index
      index += 1
      apply(ind)
    }
  }

  val maxMemory = 1024 * 1024

  var storageHandler: StorageMode = null

  def size = storageHandler.size

  def memSize = storageHandler.memSize
  
  def add(item: A): Unit = storageHandler.add(item)
  
  def +=(item: A): Unit = storageHandler.add(item)

  def switchToFile(seq: Seq[A]): Unit

  def writeItem(item: A, os: DataOutput): Unit

  def apply(idx: Int): A = storageHandler.get(idx)

  def iterator: Iterator[A] = new StorableSequenceIterator

  def length = storageHandler.size

  def write(os: DataOutput) = storageHandler.write(os)

  def finish = storageHandler.finish
}

class StorableMapSeq(ctx: StructureContext) extends StorableSeq[StorableMap](ctx) {

  class MapFileStorage(file: File) extends BaseFileStorage(file) {

    def this(items: Seq[StorableMap], file: File) = {
      this(file)
      items.foreach { writeItem(_) }
    }

    var inItem = 0

    def readItem = {
      val index = inStream.readShort
      val map = ctx.newMap(ctx.descriptors(index))
      map.read(inStream, ctx)
      inItem += 1
      map
    }

    def writeItem(map: StorableMap) = {
      val start = outStream.size
      outStream.writeShort(map.descriptor.index)
      map.write(outStream)
      val size: Int = outStream.size - start
      outStream.write(size)
      storedCount += 1
    }

    def add(map: StorableMap) = writeItem(map)

    def get(idx: Int) = {
      while (inItem > idx) {
        val size = inStream.readInt
        inStream.seek(inStream.getFilePointer - 4 - size)
        inItem -= 1
      }
      while (inItem < idx) readItem
      readItem
    }
  }

  def switchToFile(seq: Seq[StorableMap]) = {
    storageHandler = new MapFileStorage(seq, ctx.newOut)
  }

  def read(is: DataInput) = {
    val length = is.readShort
    if (is.readBoolean) {
      storageHandler = new MemoryStorage
      (0 to length).foreach { _ => storageHandler.add(MapType.read(is, ctx)) }
    } else {
      val path = is.readUTF
      val fileStore = new MapFileStorage(new File(path))
      storageHandler = fileStore
      fileStore.initInput
    }
  }
}

class StorableValueSeq(ctx: StructureContext) extends StorableSeq[Object](ctx) {

  class ValueFileStorage(file: File) extends BaseFileStorage(file) {

    def this(items: Seq[Object], file: File) = {
      this(file)
      items.foreach { writeItem(_) }
    }

    var inItem = 0

    def writeItem(value: Object) = {
      val itemType = ItemType.itemType(value)
      outStream.writeByte(itemType.index)
      itemType.write(value, outStream)
      storedCount += 1
    }

    def add(value: Object) = writeItem(value)

    def get(idx: Int) = {
      while (inItem > idx) {
        val size = inStream.readInt
        inStream.seek(inStream.getFilePointer - 4 - size)
        inItem -= 1
      }
      while (inItem < idx) readItem(inStream)
      readItem(inStream)
    }
  }

  def readItem(is: DataInput) = {
    val index = is.readByte
    val itemType = ItemType.types(index)
    itemType.read(is, ctx)
  }

  def switchToFile(seq: Seq[Object]) = {
    storageHandler = new ValueFileStorage(seq, ctx.newOut)
  }

  def read(is: DataInput) = {
    val length = is.readInt
    if (is.readBoolean) {
      storageHandler = new MemoryStorage
      (0 to length).foreach { _ => storageHandler.add(MapType.read(is, ctx)) }
    } else {
      val path = is.readUTF
      val fileStore = new ValueFileStorage(new File(path))
      storageHandler = fileStore
      fileStore.initInput
    }
  }
}