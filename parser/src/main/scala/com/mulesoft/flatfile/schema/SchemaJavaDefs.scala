package com.mulesoft.flatfile.schema

import java.{ util => ju }

/** Definitions used for compatibility with Java code using schemas. */
trait SchemaJavaDefs {

  type ValueMap = ju.Map[String, Object]
  type ValueMapImpl = ju.HashMap[String, Object]
  type MapList = ju.Collection[ValueMap]
  type MapListImpl = ju.ArrayList[ValueMap]
  type SimpleList = ju.Collection[Object]
  type SimpleListImpl = ju.ArrayList[Object]
  type RealNumber = java.math.BigDecimal
  type IntegerNumber = Integer

  def getRequiredValue(key: String, map: ValueMap) =
    if (map containsKey (key)) map.get(key)
    else throw new IllegalArgumentException(s"missing required value '$key'")

  def getRequiredString(key: String, map: ValueMap): String = {
    val value = getRequiredValue(key, map)
    if (value.isInstanceOf[String]) value.asInstanceOf[String]
    else throw new IllegalArgumentException(s"not a string value '$key'")
  }

  /** Get child int value (error if not found). */
  def getRequiredInt(key: String, map: ValueMap): Int = map.get(key) match {
    case n: Integer => n
    case null => throw new IllegalArgumentException("Missing required integer value '" + key + '\'')
    case _ => throw new IllegalArgumentException("Value '" + key + "' is not an integer")
  }

  def getRequiredValueMap(key: String, map: ValueMap): ValueMap = {
    val value = getRequiredValue(key, map)
    if (value.isInstanceOf[ValueMap]) value.asInstanceOf[ValueMap]
    else throw new IllegalArgumentException(s"not a value map '$key'")
  }

  def getRequiredMapList(key: String, map: ValueMap): MapList = {
    val value = getRequiredValue(key, map)
    if (value.isInstanceOf[MapList]) value.asInstanceOf[MapList]
    else throw new IllegalArgumentException(s"not a map list '$key'")
  }

  def getAs[T <: Object](key: String, map: ValueMap): T = map.get(key).asInstanceOf[T]

  def getAsRequired[T <: Object](key: String, map: ValueMap): T = {
    val result = map.get(key).asInstanceOf[T]
    if (result eq null) throw new IllegalArgumentException(s"value $key not present in map")
    result
  }

  def getAsString(key: String, map: ValueMap) = getAs[String](key, map)

  def getAsInt(key: String, map: ValueMap) = getAs[Integer](key, map).intValue()

  def getAsMap(key: String, map: ValueMap) = getAs[ValueMap](key, map)
  
  def getStringOption(key: String, dflt: Option[String], map: ValueMap): Option[String] =
    if (map.containsKey(key)) Some(getAs[String](key, map)) else dflt
  
  def getStringOption(key: String, map: ValueMap): Option[String] = getStringOption(key, None, map)
  
  def getIntOption(key: String, dflt: Option[Int], map: ValueMap): Option[Int] =
    if (map.containsKey(key)) Some(getAsInt(key, map)) else dflt
  
  def getIntOption(key: String, map: ValueMap): Option[Int] = getIntOption(key, None, map)

  def getAs[T <: Object](key: String, dflt: => T, map: ValueMap): T =
    if (map.containsKey(key)) map.get(key).asInstanceOf[T]
    else dflt

  def getOrSet[T <: Object](key: String, dflt: => T, map: ValueMap): T =
    if (map.containsKey(key)) map.get(key).asInstanceOf[T]
    else {
      val inst = dflt
      map.put(key, inst)
      inst
    }

  def addToList[T](key: String, item: T, map: ValueMap) =
    if (map != null) {
      val list = getOrSet(key, new ju.ArrayList[T](), map)
      if (list.isEmpty || list.get(list.size - 1) != item) list.add(item)
    }

  def mergeToList[T](key: String, from: ValueMap, to: ValueMap) =
    if (from.containsKey(key)) {
      val iter = getAs[ju.List[T]](key, from).iterator
      while (iter.hasNext) addToList(key, iter.next, to)
    }

  def swap(key1: String, key2: String, map: ValueMap) =
    if (map.containsKey(key1)) {
      val temp = map get (key1)
      if (map.containsKey(key2)) map put (key1, map get (key2))
      map put (key2, temp)
    } else if (map.containsKey(key2)) {
      map put (key1, map get (key2))
      map remove (key2)
    }

  def move(key: String, map1: ValueMap, map2: ValueMap) =
    if (map1.containsKey(key)) {
      map2 put (key, map1.get(key))
      map1 remove (key)
    }

  def applyIfPresent[T](key: String, map: ValueMap, f: T => Unit) =
    if (map.containsKey(key)) f(map.get(key).asInstanceOf[T])

  def foreachMapInMap(map: ValueMap, f: ValueMap => Unit) = {
    val iter = map.values.iterator
    while (iter.hasNext) f(iter.next.asInstanceOf[ValueMap])
  }

  def copyIfPresent(key1: String, map1: ValueMap, key2: String, map2: ValueMap) =
    if (map1.containsKey(key1)) map2 put (key2, map1.get(key1))

  def foreachListInMap(map: ValueMap, f: MapList => Unit) = {
    val iter = map.values.iterator
    while (iter.hasNext) {
      f(iter.next.asInstanceOf[MapList])
    }
  }

  def foreachMapInList(list: MapList, f: ValueMap => Unit) = {
    val iter = list.iterator
    while (iter.hasNext) {
      f(iter.next.asInstanceOf[ValueMap])
    }
  }
}

object SchemaJavaValues {

  // value keys for configuration information in read root map (latest interchange only)
  val delimiterCharacters = "Delimiters"

  // value keys for root map
  val functionalAcksGenerated = "FunctionalAcksGenerated"
  val interchangeAcksGenerated = "InterchangeAcksGenerated"
  val interchangeAcksReceived = "InterchangeAcksReceived"
  val interchangeAcksToSend = "InterchangeAcksToSend"

  // value keys for envelope data maps in root and structure set maps
  val interchangeKey = "Interchange"
  val groupKey = "Group"

  // value keys for top-level structure map
  val structureId = "Id"
  val structureName = "Name"
  val structureHeading = "Heading"
  val structureDetail = "Detail"
  val structureSummary = "Summary"
  val structureSchema = "Structure"

  // value key used in multiple maps
  val errorListKey = "Errors"
  
  // value key used for simple structures
  val dataKey = "Data"
}