package com.mulesoft.flatfile.schema

/** Definitions for constants in YAML schema representations.
  *
  * @author MuleSoft, Inc.
  */
trait YamlDefs {
  val idRefKey = "idRef"
  val nameKey = "name"
  val positionKey = "position"
  val usageKey = "usage"
  val valueKey = "value"
  val valuesKey = "values"
  val typeKey = "type"
  val afterKey = "after"
  val countKey = "count"
  val itemsKey = "items"
  val wrapIdKey = "wrapId"
  val groupKey = "group"
  val endPositionKey = "endPosition"
  val groupIdKey = "groupId"
  val compositesKey = "composites"
  val idKey = "id"
  val tagValueKey = "tagValue"
  val rulesKey = "rules"
  val elementsKey = "elements"
  val lengthKey = "length"
  val minLengthKey = "minLength"
  val maxLengthKey = "maxLength"
  val segmentsKey = "segments"
  val structuresKey = "structures"
  val classKey = "class"
  val dataKey = "data"
  val headingKey = "heading"
  val detailKey = "detail"
  val summaryKey = "summary"
  val formKey = "form"
  val versionKey = "version"
  val importsKey = "imports"
  
  val trimKey = "trim"
  val groupIdRefKey = "groupIdRef"
  val wrapIdRefKey = "wrapIdRef"
  
  // deprecated
  val tagKey = "tag"
  val tagStartKey = "tagStart"
  val tagLengthKey = "tagLength"
  
  val anyRepeatsValue = ">1"
  val anyRepeatsAlt = "unbounded"
}