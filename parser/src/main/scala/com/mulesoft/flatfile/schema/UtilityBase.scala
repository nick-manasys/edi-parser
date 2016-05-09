package com.mulesoft.flatfile.schema

/** Utility base trait for parsers and writers.
 */
trait UtilityBase extends SchemaJavaDefs {
  
  import scala.annotation.tailrec
  
  import EdiSchema._

  /** Get value from array, or null if past end. */
  def valueOrNull[T](index: Int, values: Array[T]): T = if (index < values.length) values(index) else null.asInstanceOf[T]

  /** Build array of string values matching the simple value components. */
  def getStrings(comps: List[SegmentComponent], data: ValueMap) = {
    @tailrec
    def getr(rem: List[SegmentComponent], acc: List[String]): List[String] = rem match {
      case (h: ElementComponent) :: t => getr(t, data.get(h.key).asInstanceOf[String] :: acc)
      case h :: t => getr(t, acc)
      case _ => acc.reverse
    }
    getr(comps, Nil).toArray
  }
}