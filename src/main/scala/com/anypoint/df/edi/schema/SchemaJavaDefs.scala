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
}

object SchemaJavaValues {

  // value keys for configuration
  val delimiterCharacters = "delimiters"
  val characterEncoding = "encoding"
  val partnerIdentifer = "partner interchange ID"
  val selfIdentifier = "self interchange ID"

  // value keys for transaction set
  val interchangeProperties = "Interchange"
  val transactionsMap = "Transactions"
    
  // value keys only used for output
  val groupProperties = "Group"
  val setProperties = "Set"
  val setIdentifier = "Identifier"

  // value keys for top-level transaction parse result map
  val transactionId = "id"
  val transactionName = "name"
  val transactionHeading = "heading"
  val transactionDetail = "detail"
  val transactionSummary = "summary"
  
}