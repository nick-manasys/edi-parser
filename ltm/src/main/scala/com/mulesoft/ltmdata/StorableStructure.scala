
package com.mulesoft.ltmdata

import java.io.{ DataInput, DataOutput }

/** Data structure which has some knowledge of how much memory space it uses and can be persisted.
  */
trait StorableStructure {
  
  def memSize: Long

  def read(is: DataInput, ctx: StructureContext): Unit

  def write(os: DataOutput): Unit
}