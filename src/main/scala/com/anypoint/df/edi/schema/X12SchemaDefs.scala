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

}

object X12Acknowledgment {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.X12Constants._
  import com.anypoint.df.edi.lexical.EdiConstants.DataType
  import com.anypoint.df.edi.lexical.EdiConstants.DataType._
  
  /** Transaction syntax error codes (X12 718 element codes). */
  sealed abstract class TransactionSyntaxError(val code: Int)
  case object NotSupportedTransaction extends TransactionSyntaxError(1)
  case object MissingTrailerTransaction extends TransactionSyntaxError(2)
  case object ControlNumberMismatch extends TransactionSyntaxError(3)
  case object WrongSegmentCount extends TransactionSyntaxError(4)
  case object SegmentsInError extends TransactionSyntaxError(5)
  case object BadTransactionSetId extends TransactionSyntaxError(6)
  case object BadTransactionSetControl extends TransactionSyntaxError(7)
  case object AuthenticationKeyUnknown extends TransactionSyntaxError(8)
  case object EncryptionKeyUnknown extends TransactionSyntaxError(9)
  case object ServiceNotAvailable extends TransactionSyntaxError(10)
  case object UnknownSecurityRecipient extends TransactionSyntaxError(11)
  case object IncorrectMessageLength extends TransactionSyntaxError(12)
  case object MessageAuthenticationFailed extends TransactionSyntaxError(13)
  case object UnknownSecurityOriginator extends TransactionSyntaxError(15)
  case object DecryptionSyntaxError extends TransactionSyntaxError(16)
  case object SecurityNotSupported extends TransactionSyntaxError(17)
  case object SetNotInGroup extends TransactionSyntaxError(18)
  case object InvalidImplementationConvention extends TransactionSyntaxError(23)
  case object MissingS3ESecurityEndSegment extends TransactionSyntaxError(24)
  case object MissingS3ESecurityStartSegment extends TransactionSyntaxError(25)
  case object MissingS4ESecurityEndSegment extends TransactionSyntaxError(26)
  case object MissingS4ESecurityStartSegment extends TransactionSyntaxError(27)

  /** Transaction set acknowledgment codes (X12 717 element codes). */
  sealed abstract class TransactionAcknowledgmentCode(val code: String)
  case object AcceptedTransaction extends TransactionAcknowledgmentCode("A")
  case object AcceptedWithErrorsTransaction extends TransactionAcknowledgmentCode("E")
  case object AuthenticationFailedTransaction extends TransactionAcknowledgmentCode("M")
  case object RejectedTransaction extends TransactionAcknowledgmentCode("R")
  case object ValidityFailedTransaction extends TransactionAcknowledgmentCode("W")
  case object DecryptionBadTransaction extends TransactionAcknowledgmentCode("X")

  /** Segment syntax error codes (X12 720 element codes). */
  sealed abstract class SegmentSyntaxError(val code: Int)
  case object UnrecognizedSegment extends SegmentSyntaxError(1)
  case object UnexpectedSegment extends SegmentSyntaxError(2)
  case object MissingMandatorySegment extends SegmentSyntaxError(3)
  case object TooManyLoops extends SegmentSyntaxError(4)
  case object TooManyOccurs extends SegmentSyntaxError(5)
  case object NotInTransactionSegment extends SegmentSyntaxError(6)
  case object OutOfOrderSegment extends SegmentSyntaxError(7)
  case object DataErrorsSegment extends SegmentSyntaxError(8)

  /** Information for a segment error (used to generate X12 AK3 segment). */
  case class SegmentError(val id: String, val position: Int, val loopId: Option[String],
    val error: Option[SegmentSyntaxError])

  /** Data element syntax error codes (X12 723 element codes). */
  sealed abstract class ElementSyntaxError(val code: Int, val text: String)
  case object MissingRequiredElement extends ElementSyntaxError(1, "missing required element")
  case object MissingConditionalElement extends ElementSyntaxError(2, "missing conditional element")
  case object TooManyElements extends ElementSyntaxError(3, "too many elements")
  case object DataTooShort extends ElementSyntaxError(4, "data value too short")
  case object DataTooLong extends ElementSyntaxError(5, "data value too long")
  case object InvalidCharacter extends ElementSyntaxError(6, "invalid character in data value")
  case object InvalidCodeValue extends ElementSyntaxError(7, "invalid code value")
  case object InvalidDate extends ElementSyntaxError(8, "invalid date")
  case object InvalidTime extends ElementSyntaxError(9, "invalid time")
  case object ExclusionConditionViolated extends ElementSyntaxError(10, "exclusion condition violated")
  case object TooManyRepititions extends ElementSyntaxError(11, "too many repetitions")
  case object TooManyComponents extends ElementSyntaxError(12, "too many components")
  
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