package com.anypoint.df.edi.schema

/** Definitions for constants in YAML schema representations.
  *
  * @author MuleSoft, Inc.
  */
trait YamlDefs {
  val idRefKey = "idRef"
  val nameKey = "name"
  val positionKey = "position"
  val usageKey = "usage"
  val valuesKey = "values"
  val typeKey = "type"
  val afterKey = "after"
  val countKey = "count"
  val itemsKey = "items"
  val loopIdKey = "loopId"
  val compositesKey = "composites"
  val idKey = "id"
  val rulesKey = "rules"
  val elementsKey = "elements"
  val minLengthKey = "minLength"
  val maxLengthKey = "maxLength"
  val segmentsKey = "segments"
  val transactionsKey = "transactions"
  val groupKey = "group"
  val headingKey = "heading"
  val detailKey = "detail"
  val summaryKey = "summary"
  val formKey = "form"
  val versionKey = "version"
  val importsKey = "imports"
  
  val trimKey = "trim"
  
  val anyRepeatsValue = ">1"
}