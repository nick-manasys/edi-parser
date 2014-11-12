package com.anypoint.df.edi.schema

/** Definitions for X12 EDI schemas. */
trait X12SchemaDefs {

  import com.anypoint.df.edi.lexical.X12Constants._
  import EdiSchema._
  import X12SchemaValues._

  val GSSegment = Segment("GS", "Functional group header", List[SegmentComponent](
    ElementComponent(Element("479", IdType, 2, 2), functionalIdentifierName, 1, MandatoryUsage, 1),
    ElementComponent(Element("142", AlphaNumericType, 2, 15), applicationSendersName, 2, MandatoryUsage, 1),
    ElementComponent(Element("124", AlphaNumericType, 2, 15), applicationReceiversName, 3, MandatoryUsage, 1),
    ElementComponent(Element("373", DateType, 8, 8), groupDateName, 4, MandatoryUsage, 1),
    ElementComponent(Element("337", TimeType, 4, 4), groupTimeName, 5, MandatoryUsage, 1),
    ElementComponent(Element("28", IntegerType, 1, 9), groupControlName, 6, MandatoryUsage, 1),
    ElementComponent(Element("455", IdType, 1, 2), responsibleAgencyName, 7, MandatoryUsage, 1),
    ElementComponent(Element("480", AlphaNumericType, 2, 12), versionIdentifierName, 8, MandatoryUsage, 1)))

  val GESegment = Segment("GE", "Functional group trailer", List[SegmentComponent](
    ElementComponent(Element("97", IntegerType, 1, 6), numberOfSetsName, 1, MandatoryUsage, 1),
    ElementComponent(Element("28", IntegerType, 1, 9), groupControlName, 2, MandatoryUsage, 1)))

  val STSegment = Segment("ST", "Transaction set header", List[SegmentComponent](
    ElementComponent(Element("143", IdType, 3, 3), transactionSetIdentifierName, 1, MandatoryUsage, 1),
    ElementComponent(Element("329", AlphaNumericType, 4, 9), transactionSetControlName, 2, MandatoryUsage, 1),
    ElementComponent(Element("1705", AlphaNumericType, 1, 35), implementationConventionName, 3, OptionalUsage, 1)))

  val SESegment = Segment("SE", "Transaction set trailer", List[SegmentComponent](
    ElementComponent(Element("96", IntegerType, 1, 10), numberOfSegmentsName, 1, MandatoryUsage, 1),
    ElementComponent(Element("329", AlphaNumericType, 4, 9), transactionSetControlName, 2, MandatoryUsage, 1)))
}

object X12SchemaValues {
    
    // group properties from GS segment
    val functionalIdentifierName = "Functional identifier code";
    val applicationSendersName = "Application sender's code";
    val applicationReceiversName = "Application receiver's code";
    val groupDateName = "Group date";
    val groupTimeName = "Group time";
    val groupControlName = "Group control number";
    val responsibleAgencyName = "Responsible agency code";
    val versionIdentifierName = "Version / release / industry identifier code";
    val functionalIdentifierKey = "Functional identifier code (01)";
    val applicationSendersKey = "Application sender's code (02)";
    val applicationReceiversKey = "Application receiver's code (03)";
    val groupDateKey = "Group date (04)";
    val groupTimeKey = "Group time (05)";
    val groupControlKey = "Group control number (06)";
    val responsibleAgencyKey = "Responsible agency code (07)";
    val versionIdentifierKey = "Version / release / industry identifier code (08)";
    
    // group properties from GE segment
    val numberOfSetsName = "Number of transaction sets included";
//    val groupControlName = "Group control number";
    val numberOfSetsKey = "Number of transaction sets included (01)";
    val groupControlEndKey = "Group control number (02)";
    
    // transaction set properties from ST segment
    val transactionSetIdentifierName = "Transaction set identifier code";
    val transactionSetControlName = "Transaction set control number";
    val implementationConventionName = "Implementation convention reference";
    val transactionSetIdentifierKey = "Transaction set identifier code (01)";
    val transactionSetControlKey = "Transaction set control number (02)";
    val implementationConventionKey = "Implementation convention reference (03)";
    
    // transaction set properties from SE segment
    val numberOfSegmentsName = "Number of included segments";
//    val transactionSetControlName = "Transaction set control number";
    val numberOfSegmentsKey = "Number of included segments (01)";
    val transactionSetControlEndKey = "Transaction set control number (02)";
  
}