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

  // value keys for top-level transaction parse result map
  val transactionId = "id"
  val transactionName = "name"
  val transactionHeading = "heading"
  val transactionDetail = "detail"
  val transactionSummary = "summary"

}