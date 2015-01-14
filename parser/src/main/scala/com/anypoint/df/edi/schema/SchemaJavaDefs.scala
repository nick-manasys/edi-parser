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

/** Entity identity information. Interchange qualifier and id are used for one direction of an interchange, while type
  * (if non-zero length) is matched against the interchange type in an incoming message. The application identifier is
  * used for the group header.
  */
case class IdentityInformation(interchangeQualifier: String, interchangeId: String, interchangeType: String,
  applicationIdentifier: String)

object SchemaJavaValues {

  // value keys for configuration
  val delimiterCharacters = "Delimiters"
  val characterEncoding = "Encoding"

  // value keys for parse output map
  val acknowledgments = "Acknowledgments"
  val transactionsMap = "Transactions"
  
  // value keys for group data
  val groupInterchange = "Interchange"

  // value keys for top-level transaction parse result map
  val transactionId = "Id"
  val transactionName = "Name"
  val transactionHeading = "Heading"
  val transactionDetail = "Detail"
  val transactionSummary = "Summary"
  val transactionSet = "Set"
  val transactionGroup = "Group"
  
  // value key for top-level transaction output map
  val transactionInterSelfQualId = "InterIdQualSelf"
  val transactionInterSelfId = "InterIdSelf"
  val transactionGroupSelfId = "GroupIdSelf"
  val transactionInterPartnerQualId = "InterIdQualPartner"
  val transactionInterPartnerId = "InterIdPartner"
  val transactionGroupPartnerId = "GroupIdPartner"
  val transactionImplConventionRef = "ImplConventionRef"
}