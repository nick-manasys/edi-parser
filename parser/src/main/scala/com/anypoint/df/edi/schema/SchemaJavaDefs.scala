package com.anypoint.df.edi.schema

/** Definitions used for compatibility with Java code using schemas. */
trait SchemaJavaDefs {

  type ValueMap = java.util.Map[String, Object]
  type ValueMapImpl = java.util.HashMap[String, Object]
  type MapList = java.util.List[ValueMap]
  type MapListImpl = java.util.ArrayList[ValueMap]
  type SimpleList = java.util.List[Object]
  type SimpleListImpl = java.util.ArrayList[Object]
  type RealNumber = java.math.BigDecimal
  type IntegerNumber = Integer

  def getRequiredValue(key: String, map: ValueMap) =
    if (map containsKey (key)) map.get(key)
    else throw new IllegalArgumentException(s"missing required value '$key'")
  
  def getRequiredString(key: String, map: ValueMap): String = {
    def value = getRequiredValue(key, map)
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
    def value = getRequiredValue(key, map)
    if (value.isInstanceOf[ValueMap]) value.asInstanceOf[ValueMap]
    else throw new IllegalArgumentException(s"not a value map '$key'")
  }
  
  def getRequiredMapList(key: String, map: ValueMap): MapList = {
    def value = getRequiredValue(key, map)
    if (value.isInstanceOf[MapList]) value.asInstanceOf[MapList]
    else throw new IllegalArgumentException(s"not a map list '$key'")
  }
  
  def getAs[T](key: String, map: ValueMap): T = map.get(key).asInstanceOf[T]
  
  def getAsString(key: String, map: ValueMap) = getAs[String](key, map)
  
  def getAsMap(key: String, map: ValueMap) = getAs[ValueMap](key, map)
  
  def getAs[T](key: String, dflt: T, map: ValueMap): T =
    if (map.containsKey(key)) map.get(key).asInstanceOf[T]
    else dflt
  
  def swap(key1: String, key2: String, map: ValueMap) =
    if (map.containsKey(key1)) {
      val temp = map get(key1)
      if (map.containsKey(key2)) map put(key1, map get(key2))
      map put(key2, temp)
    } else if (map.containsKey(key2)) {
      map put(key1, map get(key2))
      map remove(key2)
    }
  
  def move(key: String, map1: ValueMap, map2: ValueMap) =
    if (map1.containsKey(key)) {
      map2 put(key, map1.get(key))
      map1 remove(key)
    }
    
  def applyIfPresent[T](key: String, map: ValueMap, f: T => Unit) =
    if (map.containsKey(key)) f(map.get(key).asInstanceOf[T])
  
  def foreachMapInMap(map: ValueMap, f: ValueMap => Unit) = {
    val iter = map.values.iterator
    while (iter.hasNext) {
      f(iter.next.asInstanceOf[ValueMap])
    }
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

  // value keys for configuration
  val delimiterCharacters = "Delimiters"
  val characterEncoding = "Encoding"

  // value keys for parse output map
  val functionalAcksGenerated = "FunctionalAcksGenerated"
  val functionalAcksReceived = "FunctionalAcksReceived"
  val interchangeAcksGenerated = "InterchangeAcksGenerated"
  val interchangeAcksReceived = "InterchangeAcksReceived"
  val interchangeAcksToSend = "InterchangeAcksToSend"
  val transactionsMap = "Transactions"
  
  // value keys for envelope data maps
  val interchangeKey = "Interchange"
  val groupKey = "Group"
  val setKey = "Set"

  // value keys for top-level transaction parse result map
  val transactionId = "Id"
  val transactionName = "Name"
  val transactionHeading = "Heading"
  val transactionDetail = "Detail"
  val transactionSummary = "Summary"
}