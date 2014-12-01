package com.anypoint.df.edi.schema

/** Definitions for X12 EDI schemas. */
trait X12SchemaDefs {

  import EdiSchema._
  import X12SchemaValues._
  import com.anypoint.df.edi.lexical.X12Constants._
  import com.anypoint.df.edi.lexical.EdiConstants.DataType
  import com.anypoint.df.edi.lexical.EdiConstants.DataType._

  val GSSegment = Segment("GS", "Functional group header", List[SegmentComponent](
    ElementComponent(Element("479", ID, 2, 2), functionalIdentifierName, 1, MandatoryUsage, 1),
    ElementComponent(Element("142", ALPHANUMERIC, 2, 15), applicationSendersName, 2, MandatoryUsage, 1),
    ElementComponent(Element("124", ALPHANUMERIC, 2, 15), applicationReceiversName, 3, MandatoryUsage, 1),
    ElementComponent(Element("373", DATE, 8, 8), groupDateName, 4, MandatoryUsage, 1),
    ElementComponent(Element("337", TIME, 4, 4), groupTimeName, 5, MandatoryUsage, 1),
    ElementComponent(Element("28", INTEGER, 1, 9), groupControlName, 6, MandatoryUsage, 1),
    ElementComponent(Element("455", ID, 1, 2), responsibleAgencyName, 7, MandatoryUsage, 1),
    ElementComponent(Element("480", ALPHANUMERIC, 2, 12), versionIdentifierName, 8, MandatoryUsage, 1)))

  val GESegment = Segment("GE", "Functional group trailer", List[SegmentComponent](
    ElementComponent(Element("97", INTEGER, 1, 6), numberOfSetsName, 1, MandatoryUsage, 1),
    ElementComponent(Element("28", INTEGER, 1, 9), groupControlName, 2, MandatoryUsage, 1)))

  val STSegment = Segment("ST", "Transaction set header", List[SegmentComponent](
    ElementComponent(Element("143", ID, 3, 3), transactionSetIdentifierName, 1, MandatoryUsage, 1),
    ElementComponent(Element("329", ALPHANUMERIC, 4, 9), transactionSetControlName, 2, MandatoryUsage, 1),
    ElementComponent(Element("1705", ALPHANUMERIC, 1, 35), implementationConventionName, 3, OptionalUsage, 1)))

  val SESegment = Segment("SE", "Transaction set trailer", List[SegmentComponent](
    ElementComponent(Element("96", INTEGER, 1, 10), numberOfSegmentsName, 1, MandatoryUsage, 1),
    ElementComponent(Element("329", ALPHANUMERIC, 4, 9), transactionSetControlName, 2, MandatoryUsage, 1)))

  // 997 acknowledgment schema (generated code)
  val elem143 = Element("143", ID, 3, 3)
  val elem329 = Element("329", ALPHANUMERIC, 4, 9)
  val elem716 = Element("716", ID, 1, 3)
  val elem718 = Element("718", ID, 1, 3)
  val elem1705 = Element("1705", ALPHANUMERIC, 1, 35)
  val compC030 = Composite("C030", "Position in Segment", List[SegmentComponent](
    ElementComponent(Element("722", INTEGER, 1, 2), "Element Position in Segment", 1, MandatoryUsage, 1),
    ElementComponent(Element("1528", INTEGER, 1, 2), "Component Data Element Position in Composite", 2, OptionalUsage, 1),
    ElementComponent(Element("1686", INTEGER, 1, 4), "Repeating Data Element Position", 3, OptionalUsage, 1)))
  val segAK1 = Segment("AK1", "Functional Group Response Header", List[SegmentComponent](
    ElementComponent(Element("479", ID, 2, 2), "Functional Identifier Code", 1, MandatoryUsage, 1),
    ElementComponent(Element("28", INTEGER, 1, 9), "Group Control Number", 2, MandatoryUsage, 1),
    ElementComponent(Element("480", ALPHANUMERIC, 1, 12), "Version / Release / Industry Identifier Code", 3, OptionalUsage, 1)))
  val segAK2 = Segment("AK2", "Transaction Set Response Header", List[SegmentComponent](
    ElementComponent(elem143, "Transaction Set Identifier Code", 1, MandatoryUsage, 1),
    ElementComponent(elem329, "Transaction Set Control Number", 2, MandatoryUsage, 1),
    ElementComponent(elem1705, "Implementation Convention Reference", 3, OptionalUsage, 1)))
  val segAK3 = Segment("AK3", "Data Segment Note", List[SegmentComponent](
    ElementComponent(Element("721", ID, 2, 3), "Segment ID Code", 1, MandatoryUsage, 1),
    ElementComponent(Element("719", INTEGER, 1, 10), "Segment Position in Transaction Set", 2, MandatoryUsage, 1),
    ElementComponent(Element("447", ALPHANUMERIC, 1, 4), "Loop Identifier Code", 3, OptionalUsage, 1),
    ElementComponent(Element("720", ID, 1, 3), "Segment Syntax Error Code", 4, OptionalUsage, 1)))
  val segAK4 = Segment("AK4", "Data Element Note", List[SegmentComponent](
    CompositeComponent(compC030, "Position in Segment", 1, MandatoryUsage, 1),
    ElementComponent(Element("725", INTEGER, 1, 4), "Data Element Reference Number", 2, OptionalUsage, 1),
    ElementComponent(Element("723", ID, 1, 3), "Data Element Syntax Error Code", 3, MandatoryUsage, 1),
    ElementComponent(Element("724", ALPHANUMERIC, 1, 99), "Copy of Bad Data Element", 4, OptionalUsage, 1)))
  val segAK5 = Segment("AK5", "Transaction Set Response Trailer", List[SegmentComponent](
    ElementComponent(Element("717", ID, 1, 1), "Transaction Set Acknowledgment Code", 1, MandatoryUsage, 1),
    ElementComponent(elem718, "Transaction Set Syntax Error Code", 2, OptionalUsage, 1),
    ElementComponent(elem718, "Transaction Set Syntax Error Code", 3, OptionalUsage, 1),
    ElementComponent(elem718, "Transaction Set Syntax Error Code", 4, OptionalUsage, 1),
    ElementComponent(elem718, "Transaction Set Syntax Error Code", 5, OptionalUsage, 1),
    ElementComponent(elem718, "Transaction Set Syntax Error Code", 6, OptionalUsage, 1)))
  val segAK9 = Segment("AK9", "Functional Group Response Trailer", List[SegmentComponent](
    ElementComponent(Element("715", ID, 1, 1), "Functional Group Acknowledge Code", 1, MandatoryUsage, 1),
    ElementComponent(Element("97", INTEGER, 1, 6), "Number of Transaction Sets Included", 2, MandatoryUsage, 1),
    ElementComponent(Element("123", INTEGER, 1, 6), "Number of Received Transaction Sets", 3, MandatoryUsage, 1),
    ElementComponent(Element("2", INTEGER, 1, 6), "Number of Accepted Transaction Sets", 4, MandatoryUsage, 1),
    ElementComponent(elem716, "Functional Group Syntax Error Code", 5, OptionalUsage, 1),
    ElementComponent(elem716, "Functional Group Syntax Error Code", 6, OptionalUsage, 1),
    ElementComponent(elem716, "Functional Group Syntax Error Code", 7, OptionalUsage, 1),
    ElementComponent(elem716, "Functional Group Syntax Error Code", 8, OptionalUsage, 1),
    ElementComponent(elem716, "Functional Group Syntax Error Code", 9, OptionalUsage, 1)))
  val segSE = Segment("SE", "Transaction Set Trailer", List[SegmentComponent](
    ElementComponent(Element("96", INTEGER, 1, 10), "Number of Included Segments", 1, MandatoryUsage, 1),
    ElementComponent(elem329, "Transaction Set Control Number", 2, MandatoryUsage, 1)))
  val segST = Segment("ST", "Transaction Set Header", List[SegmentComponent](
    ElementComponent(elem143, "Transaction Set Identifier Code", 1, MandatoryUsage, 1),
    ElementComponent(elem329, "Transaction Set Control Number", 2, MandatoryUsage, 1),
    ElementComponent(elem1705, "Implementation Convention Reference", 3, OptionalUsage, 1)))

  val trans997 = Transaction("997", "Functional Acknowledgment", "FA", List[TransactionComponent](
    ReferenceComponent(segST, MandatoryUsage, 1), ReferenceComponent(segAK1, MandatoryUsage, 1),
    GroupComponent("AK2", OptionalUsage, -1, List[TransactionComponent](
      ReferenceComponent(segAK2, OptionalUsage, 1),
      GroupComponent("AK3", OptionalUsage, -1, List[TransactionComponent](
        ReferenceComponent(segAK3, OptionalUsage, 1), ReferenceComponent(segAK4, OptionalUsage, 99))),
      ReferenceComponent(segAK5, MandatoryUsage, 1))), ReferenceComponent(segAK9, MandatoryUsage, 1),
    ReferenceComponent(segSE, MandatoryUsage, 1)), List[TransactionComponent](), List[TransactionComponent]())
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