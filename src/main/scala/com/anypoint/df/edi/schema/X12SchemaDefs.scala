package com.anypoint.df.edi.schema

/** Definitions for X12 EDI schemas. */
trait X12SchemaDefs {

  import com.anypoint.df.edi.lexical.X12Constants._
  import EdiSchema._
    
    // group properties from GS segment
    val functionalIdentifierCode = "Functional identifier code";
    val applicationSendersCode = "Application sender's code";
    val applicationReceiversCode = "Application receiver's code";
    val groupDate = "Group date";
    val groupTime = "Group time";
    val groupControlNumber = "Group control number";
    val responsibleAgencyCode = "Responsible agency code";
    val versionIdentifierCode = "Version / release / industry identifier code";
    
    // group properties from GE segment
    val numberOfTransactionSets = "Number of transaction sets included";
//    val groupControlNumber = "Group control number";
    
    // transaction set properties from ST segment
    val transactionSetIdentifierCode = "Transaction set identifier code";
    val transactionSetControlNumber = "Transaction set control number";
    val implementationConventionReference = "Implementation convention reference";
    
    // transaction set properties from SE segment
    val numberOfIncludedSegments = "Number of included segments";
//    val transactionSetControlNumber = "Transaction set control number";

  val GSSegment = Segment("GS", "Functional group header", List[SegmentComponent](
    ElementComponent(Element("479", IdType, 2, 2), functionalIdentifierCode, MandatoryUsage, 1),
    ElementComponent(Element("142", AlphaNumericType, 2, 15), applicationSendersCode, MandatoryUsage, 1),
    ElementComponent(Element("124", AlphaNumericType, 2, 15), applicationReceiversCode, MandatoryUsage, 1),
    ElementComponent(Element("373", DateType, 8, 8), groupDate, MandatoryUsage, 1),
    ElementComponent(Element("337", TimeType, 4, 8), groupTime, MandatoryUsage, 1),
    ElementComponent(Element("28", IntegerType, 1, 9), groupControlNumber, MandatoryUsage, 1),
    ElementComponent(Element("455", IdType, 1, 2), responsibleAgencyCode, MandatoryUsage, 1),
    ElementComponent(Element("480", AlphaNumericType, 2, 12), versionIdentifierCode, MandatoryUsage, 1)))

  val GESegment = Segment("GE", "Functional group trailer", List[SegmentComponent](
    ElementComponent(Element("97", IntegerType, 1, 6), numberOfTransactionSets, MandatoryUsage, 1),
    ElementComponent(Element("28", IntegerType, 1, 9), groupControlNumber, MandatoryUsage, 1)))

  val STSegment = Segment("ST", "Transaction set header", List[SegmentComponent](
    ElementComponent(Element("143", IdType, 3, 3), transactionSetIdentifierCode, MandatoryUsage, 1),
    ElementComponent(Element("329", AlphaNumericType, 4, 9), transactionSetControlNumber, MandatoryUsage, 1),
    ElementComponent(Element("1705", AlphaNumericType, 1, 35), implementationConventionReference, OptionalUsage, 1)))

  val SESegment = Segment("SE", "Transaction set trailer", List[SegmentComponent](
    ElementComponent(Element("96", IntegerType, 1, 10), numberOfIncludedSegments, MandatoryUsage, 1),
    ElementComponent(Element("329", AlphaNumericType, 4, 9), transactionSetControlNumber, MandatoryUsage, 1)))
}