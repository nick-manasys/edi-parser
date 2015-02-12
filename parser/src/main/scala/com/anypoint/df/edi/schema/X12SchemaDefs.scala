package com.anypoint.df.edi.schema

/** Definitions for X12 EDI schemas. */
trait X12SchemaDefs {

  import EdiSchema._
  import X12SchemaValues._
  import com.anypoint.df.edi.lexical.X12Constants._
  import com.anypoint.df.edi.lexical.EdiConstants.DataType
  import com.anypoint.df.edi.lexical.EdiConstants.DataType._

  val GSSegment = Segment("GS", "Functional group header", List[SegmentComponent](
    ElementComponent(Element("479", "", ID, 2, 2), Some(functionalIdentifierName), "GS01", 1, MandatoryUsage, 1),
    ElementComponent(Element("142", "", ALPHANUMERIC, 2, 15), Some(applicationSendersName), "GS02", 2, MandatoryUsage, 1),
    ElementComponent(Element("124", "", ALPHANUMERIC, 2, 15), Some(applicationReceiversName), "GS03", 3, MandatoryUsage, 1),
    ElementComponent(Element("373", "", DATE, 8, 8), Some(groupDateName), "GS04", 4, MandatoryUsage, 1),
    ElementComponent(Element("337", "", TIME, 4, 4), Some(groupTimeName), "GS05", 5, MandatoryUsage, 1),
    ElementComponent(Element("28", "", INTEGER, 1, 9), Some(groupControlName), "GS06", 6, MandatoryUsage, 1),
    ElementComponent(Element("455", "", ID, 1, 2), Some(responsibleAgencyName), "GS07", 7, MandatoryUsage, 1),
    ElementComponent(Element("480", "", ALPHANUMERIC, 2, 12), Some(versionIdentifierName), "GS08", 8, MandatoryUsage, 1)), Nil)

  val GESegment = Segment("GE", "Functional group trailer", List[SegmentComponent](
    ElementComponent(Element("97", "", INTEGER, 1, 6), Some(numberOfSetsName), "GE01", 1, MandatoryUsage, 1),
    ElementComponent(Element("28", "", INTEGER, 1, 9), Some(groupControlName), "GE02", 2, MandatoryUsage, 1)), Nil)

  val STSegment = Segment("ST", "Transaction set header", List[SegmentComponent](
    ElementComponent(Element("143", "", ID, 3, 3), Some(transactionSetIdentifierName), "ST01", 1, MandatoryUsage, 1),
    ElementComponent(Element("329", "", ALPHANUMERIC, 4, 9), Some(transactionSetControlName), "ST02", 2, MandatoryUsage, 1),
    ElementComponent(Element("1705", "", ALPHANUMERIC, 1, 35), Some(implementationConventionName), "ST03", 3, OptionalUsage, 1)), Nil)

  val SESegment = Segment("SE", "Transaction set trailer", List[SegmentComponent](
    ElementComponent(Element("96", "", INTEGER, 1, 10), Some(numberOfSegmentsName), "SE01", 1, MandatoryUsage, 1),
    ElementComponent(Element("329", "", ALPHANUMERIC, 4, 9), Some(transactionSetControlName), "SE02", 2, MandatoryUsage, 1)), Nil)
}

object X12Acknowledgment {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.X12Constants._
  import com.anypoint.df.edi.lexical.EdiConstants.DataType
  import com.anypoint.df.edi.lexical.EdiConstants.DataType._

  /** Functional group syntax error codes (X12 716 element codes). */
  sealed abstract class GroupSyntaxError(val code: Int)
  case object NotSupportedGroup extends GroupSyntaxError(1)
  case object NotSupportedGroupVersion extends GroupSyntaxError(2)
  case object MissingGroupTrailer extends GroupSyntaxError(3)
  case object GroupControlNumberMismatch extends GroupSyntaxError(4)
  case object GroupTransactionCountError extends GroupSyntaxError(5)
  case object GroupControlNumberError extends GroupSyntaxError(6)
  case object GroupControlNumberNotUnique extends GroupSyntaxError(19)

  /** Functional group acknowledgment codes (X12 717 element codes). */
  sealed abstract class GroupAcknowledgmentCode(val code: String)
  case object AcceptedGroup extends GroupAcknowledgmentCode("A")
  case object AcceptedWithErrorsGroup extends GroupAcknowledgmentCode("E")
  case object PartiallyAcceptedGroup extends GroupAcknowledgmentCode("P")
  case object RejectedGroup extends GroupAcknowledgmentCode("R")

  /** Transaction syntax error codes (X12 718 element codes). */
  sealed abstract class TransactionSyntaxError(val code: Int)
  case object NotSupportedTransaction extends TransactionSyntaxError(1)
  case object MissingTrailerTransaction extends TransactionSyntaxError(2)
  case object ControlNumberMismatch extends TransactionSyntaxError(3)
  case object WrongSegmentCount extends TransactionSyntaxError(4)
  case object SegmentsInError extends TransactionSyntaxError(5)
  case object BadTransactionSetId extends TransactionSyntaxError(6)
  case object BadTransactionSetControl extends TransactionSyntaxError(7)
  case object SetNotInGroup extends TransactionSyntaxError(18)
  case object InvalidImplementationConvention extends TransactionSyntaxError(23)

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
  val elem143 = Element("143", "Transaction Set Identifier Code", ID, 3, 3)
  val elem329 = Element("329", "Transaction Set Control Number", ALPHANUMERIC, 4, 9)
  val elem716 = Element("716", "Functional Group Syntax Error Code", ID, 1, 3)
  val elem718 = Element("718", "Transaction Set Syntax Error Code", ID, 1, 3)
  val elem1705 = Element("1705", "Implementation Convention Reference", ALPHANUMERIC, 1, 35)
  val segAK1 = Segment("AK1", "Functional Group Response Header", List[SegmentComponent](
    ElementComponent(Element("479", "Functional Identifier Code", ID, 2, 2), None, "AK101", 1, MandatoryUsage, 1),
    ElementComponent(Element("28", "Group Control Number", INTEGER, 1, 9), None, "AK102", 2, MandatoryUsage, 1),
    ElementComponent(Element("480", "Version / Release / Industry Identifier Code", ALPHANUMERIC, 1, 12), None, "AK103", 3, OptionalUsage, 1)), Nil)
  val segAK2 = Segment("AK2", "Transaction Set Response Header", List[SegmentComponent](
    ElementComponent(elem143, None, "AK201", 1, MandatoryUsage, 1),
    ElementComponent(elem329, None, "AK202", 2, MandatoryUsage, 1),
    ElementComponent(elem1705, None, "AK203", 3, OptionalUsage, 1)), Nil)
  val segAK3 = Segment("AK3", "Data Segment Note", List[SegmentComponent](
    ElementComponent(Element("721", "Segment ID Code", ID, 2, 3), None, "AK301", 1, MandatoryUsage, 1),
    ElementComponent(Element("719", "Segment Position in Transaction Set", INTEGER, 1, 10), None, "AK302", 2, MandatoryUsage, 1),
    ElementComponent(Element("447", "Loop Identifier Code", ALPHANUMERIC, 1, 4), None, "AK303", 3, OptionalUsage, 1),
    ElementComponent(Element("720", "Segment Syntax Error Code", ID, 1, 3), None, "AK304", 4, OptionalUsage, 1)), Nil)
  val segAK4 = Segment("AK4", "Data Element Note", List[SegmentComponent](
    CompositeComponent(Composite("C030", "Position in Segment", List[SegmentComponent](
      ElementComponent(Element("722", "Element Position in Segment", INTEGER, 1, 2), None, "AK40101", 1, MandatoryUsage, 1),
      ElementComponent(Element("1528", "Component Data Element Position in Composite", INTEGER, 1, 2), None, "AK40102", 2, OptionalUsage, 1),
      ElementComponent(Element("1686", "Repeating Data Element Position", INTEGER, 1, 4), None, "AK40103", 3, OptionalUsage, 1)), Nil), None, "AK401", 1, MandatoryUsage, 1),
    ElementComponent(Element("725", "Data Element Reference Number", INTEGER, 1, 4), None, "AK402", 2, OptionalUsage, 1),
    ElementComponent(Element("723", "Data Element Syntax Error Code", ID, 1, 3), None, "AK403", 3, MandatoryUsage, 1),
    ElementComponent(Element("724", "Copy of Bad Data Element", ALPHANUMERIC, 1, 99), None, "AK404", 4, OptionalUsage, 1)), Nil)
  val segAK4compC030 = segAK4.components.head match {
    case comp: CompositeComponent => comp.composite
    case _ => throw new IllegalStateException("first component of segment AK4 must be a composite")
  }
  val segAK5 = Segment("AK5", "Transaction Set Response Trailer", List[SegmentComponent](
    ElementComponent(Element("717", "Transaction Set Acknowledgment Code", ID, 1, 1), None, "AK501", 1, MandatoryUsage, 1),
    ElementComponent(elem718, None, "AK502", 2, OptionalUsage, 1),
    ElementComponent(elem718, None, "AK503", 3, OptionalUsage, 1),
    ElementComponent(elem718, None, "AK504", 4, OptionalUsage, 1),
    ElementComponent(elem718, None, "AK505", 5, OptionalUsage, 1),
    ElementComponent(elem718, None, "AK506", 6, OptionalUsage, 1)), Nil)
  val segAK9 = Segment("AK9", "Functional Group Response Trailer", List[SegmentComponent](
    ElementComponent(Element("715", "Functional Group Acknowledge Code", ID, 1, 1), None, "AK901", 1, MandatoryUsage, 1),
    ElementComponent(Element("97", "Number of Transaction Sets Included", INTEGER, 1, 6), None, "AK902", 2, MandatoryUsage, 1),
    ElementComponent(Element("123", "Number of Received Transaction Sets", INTEGER, 1, 6), None, "AK903", 3, MandatoryUsage, 1),
    ElementComponent(Element("2", "Number of Accepted Transaction Sets", INTEGER, 1, 6), None, "AK904", 4, MandatoryUsage, 1),
    ElementComponent(elem716, None, "AK905", 5, OptionalUsage, 1),
    ElementComponent(elem716, None, "AK906", 6, OptionalUsage, 1),
    ElementComponent(elem716, None, "AK907", 7, OptionalUsage, 1),
    ElementComponent(elem716, None, "AK908", 8, OptionalUsage, 1),
    ElementComponent(elem716, None, "AK909", 9, OptionalUsage, 1)), Nil)
  val segSE = Segment("SE", "Transaction Set Trailer", List[SegmentComponent](
    ElementComponent(Element("96", "Number of Included Segments", INTEGER, 1, 10), None, "SE01", 1, MandatoryUsage, 1),
    ElementComponent(elem329, None, "SE02", 2, MandatoryUsage, 1)), Nil)
  val segST = Segment("ST", "Transaction Set Header", List[SegmentComponent](
    ElementComponent(elem143, None, "ST01", 1, MandatoryUsage, 1),
    ElementComponent(elem329, None, "ST02", 2, MandatoryUsage, 1),
    ElementComponent(elem1705, None, "ST03", 3, OptionalUsage, 1)), Nil)

  val trans997 = Transaction("997", "Functional Acknowledgment", "FA", List[TransactionComponent](
    ReferenceComponent(segST, SegmentPosition(0, "0100"), MandatoryUsage, 1),
    ReferenceComponent(segAK1, SegmentPosition(0, "0200"), MandatoryUsage, 1),
    GroupComponent("AK2", OptionalUsage, -1, List[TransactionComponent](
      ReferenceComponent(segAK2, SegmentPosition(0, "0300"), OptionalUsage, 1),
      GroupComponent("AK3", OptionalUsage, -1, List[TransactionComponent](
        ReferenceComponent(segAK3, SegmentPosition(0, "0400"), OptionalUsage, 1),
        ReferenceComponent(segAK4, SegmentPosition(0, "0500"), OptionalUsage, 99)), None, Nil),
      ReferenceComponent(segAK5, SegmentPosition(0, "0600"), MandatoryUsage, 1)), None, Nil),
      ReferenceComponent(segAK9, SegmentPosition(0, "0700"), MandatoryUsage, 1),
    ReferenceComponent(segSE, SegmentPosition(0, "0800"), MandatoryUsage, 1)),
    List[TransactionComponent](), List[TransactionComponent]())
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
  val functionalIdentifierKey = "GS01";
  val applicationSendersKey = "GS02";
  val applicationReceiversKey = "GS03";
  val groupDateKey = "GS04";
  val groupTimeKey = "GS05";
  val groupControlKey = "GS06";
  val responsibleAgencyKey = "GS07";
  val versionIdentifierKey = "GS08";

  // group properties from GE segment
  val numberOfSetsName = "Number of transaction sets included";
  //    val groupControlName = "Group control number";
  val numberOfSetsKey = "GE01";
  val groupControlEndKey = "GE02";

  // transaction set properties from ST segment
  val transactionSetIdentifierName = "Transaction set identifier code";
  val transactionSetControlName = "Transaction set control number";
  val implementationConventionName = "Implementation convention reference";
  val transactionSetIdentifierKey = "ST01";
  val transactionSetControlKey = "ST02";
  val implementationConventionKey = "ST03";

  // transaction set properties from SE segment
  val numberOfSegmentsName = "Number of included segments";
  //    val transactionSetControlName = "Transaction set control number";
  val numberOfSegmentsKey = "SE01";
  val transactionSetControlEndKey = "SE02";

}