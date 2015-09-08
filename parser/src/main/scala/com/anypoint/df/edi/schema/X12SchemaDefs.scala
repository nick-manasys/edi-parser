package com.anypoint.df.edi.schema

/** Definitions for X12 EDI schemas. */
object X12SchemaDefs {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.X12Constants._
  import com.anypoint.df.edi.lexical.EdiConstants.DataType
  import com.anypoint.df.edi.lexical.EdiConstants.DataType._

  /** Key for map of transaction version maps in root map of send or receive. */
  val transactionsMap = "TransactionSets"

  /** Key for transaction set header information in transaction data map. */
  val setKey = "SetHeader"

  val elemI05 = Element("I05", "Interchange ID Qualifier", ID, 2, 2)
  val elemI12 = Element("I12", "Interchange Control Number", INTEGER, 9, 9)

  val ISASegment = Segment("ISA", "Interchange Control Header", List[SegmentComponent](
    ElementComponent(Element("I01", "Authorization Information Qualifier", ID, 2, 2), None, "ISA01", 1, MandatoryUsage, 1),
    ElementComponent(Element("I02", "Authorization Information", ALPHANUMERIC, 10, 10), None, "ISA02", 2, MandatoryUsage, 1),
    ElementComponent(Element("I03", "Security Information Qualifier", ID, 2, 2), None, "ISA03", 3, MandatoryUsage, 1),
    ElementComponent(Element("I04", "Security Information", ALPHANUMERIC, 10, 10), None, "ISA04", 4, MandatoryUsage, 1),
    ElementComponent(elemI05, None, "ISA05", 5, MandatoryUsage, 1),
    ElementComponent(Element("I06", "Interchange Sender ID", ALPHANUMERIC, 15, 15), None, "ISA06", 6, MandatoryUsage, 1),
    ElementComponent(elemI05, None, "ISA07", 7, MandatoryUsage, 1),
    ElementComponent(Element("I07", "Interchange Receiver ID", ALPHANUMERIC, 15, 15), None, "ISA08", 8, MandatoryUsage, 1),
    ElementComponent(Element("I08", "Interchange Date", DATE, 6, 6), None, "ISA09", 9, MandatoryUsage, 1),
    ElementComponent(Element("I09", "Interchange Time", TIME, 4, 4), None, "ISA10", 10, MandatoryUsage, 1),
    ElementComponent(Element("I65", "Repetition Separator", ALPHANUMERIC, 1, 1), None, "ISA11", 11, MandatoryUsage, 1),
    ElementComponent(Element("I11", "Interchange Control Version Number", ID, 5, 5), None, "ISA12", 12, MandatoryUsage, 1),
    ElementComponent(elemI12, None, "ISA13", 13, MandatoryUsage, 1),
    ElementComponent(Element("I13", "Acknowledgment Requested", ID, 1, 1), None, "ISA14", 14, MandatoryUsage, 1),
    ElementComponent(Element("I14", "Interchange Usage Indicator", ID, 1, 1), None, "ISA15", 15, MandatoryUsage, 1),
    ElementComponent(Element("I15", "Component Element Separator", ALPHANUMERIC, 1, 1), None, "ISA16", 16, MandatoryUsage, 1)), Nil)

  val IEASegment = Segment("IEA", "Interchange Control Trailer", List[SegmentComponent](
    ElementComponent(Element("I16", "Number of Included Functional Groups", INTEGER, 1, 5), None, "IEA01", 1, MandatoryUsage, 1),
    ElementComponent(elemI12, None, "IEA02", 2, MandatoryUsage, 1)), Nil)

  // group properties from GS segment
  val functionalIdentifierName = "Functional identifier code"
  val applicationSendersName = "Application sender's code"
  val applicationReceiversName = "Application receiver's code"
  val groupDateName = "Group date"
  val groupTimeName = "Group time"
  val groupControlName = "Group control number"
  val responsibleAgencyName = "Responsible agency code"
  val versionIdentifierName = "Version / release / industry identifier code"

  // group properties from GE segment
  val numberOfSetsName = "Number of transaction sets included"

  val GSSegment = Segment("GS", "Functional group header", List[SegmentComponent](
    ElementComponent(Element("479", "", ID, 2, 2), Some(functionalIdentifierName), "GS01", 1, MandatoryUsage, 1),
    ElementComponent(Element("142", "", ALPHANUMERIC, 2, 15), Some(applicationSendersName), "GS02", 2, MandatoryUsage, 1),
    ElementComponent(Element("124", "", ALPHANUMERIC, 2, 15), Some(applicationReceiversName), "GS03", 3, MandatoryUsage, 1),
    ElementComponent(Element("373", "", DATE, 8, 8), Some(groupDateName), "GS04", 4, MandatoryUsage, 1),
    ElementComponent(Element("337", "", TIME, 4, 8), Some(groupTimeName), "GS05", 5, MandatoryUsage, 1),
    ElementComponent(Element("28", "", INTEGER, 1, 9), Some(groupControlName), "GS06", 6, MandatoryUsage, 1),
    ElementComponent(Element("455", "", ID, 1, 2), Some(responsibleAgencyName), "GS07", 7, MandatoryUsage, 1),
    ElementComponent(Element("480", "", ALPHANUMERIC, 1, 12), Some(versionIdentifierName), "GS08", 8, MandatoryUsage, 1)), Nil)

  val GESegment = Segment("GE", "Functional group trailer", List[SegmentComponent](
    ElementComponent(Element("97", "", INTEGER, 1, 6), Some(numberOfSetsName), "GE01", 1, MandatoryUsage, 1),
    ElementComponent(Element("28", "", INTEGER, 1, 9), Some(groupControlName), "GE02", 2, MandatoryUsage, 1)), Nil)

  // transaction set properties from ST segment
  val transactionSetIdentifierName = "Transaction set identifier code"
  val transactionSetControlName = "Transaction set control number"
  val implementationConventionName = "Implementation convention reference"

  // transaction set properties from SE segment
  val numberOfSegmentsName = "Number of included segments"

  val STSegment = Segment("ST", "Transaction set header", List[SegmentComponent](
    ElementComponent(Element("143", "", ID, 3, 3), Some(transactionSetIdentifierName), "ST01", 1, MandatoryUsage, 1),
    ElementComponent(Element("329", "", ALPHANUMERIC, 4, 9), Some(transactionSetControlName), "ST02", 2, MandatoryUsage, 1),
    ElementComponent(Element("1705", "", ALPHANUMERIC, 1, 35), Some(implementationConventionName), "ST03", 3, OptionalUsage, 1)), Nil)

  val SESegment = Segment("SE", "Transaction set trailer", List[SegmentComponent](
    ElementComponent(Element("96", "", INTEGER, 1, 10), Some(numberOfSegmentsName), "SE01", 1, MandatoryUsage, 1),
    ElementComponent(Element("329", "", ALPHANUMERIC, 4, 9), Some(transactionSetControlName), "SE02", 2, MandatoryUsage, 1)), Nil)

  val InterchangeStartSegment = ISASegment.ident
  val InterchangeEndSegment = IEASegment.ident

  // value keys for ISA segment
  val interAuthQualKey = ISASegment.components(0).key
  val interAuthInfoKey = ISASegment.components(1).key
  val interSecurityQualKey = ISASegment.components(2).key
  val interSecurityInfoKey = ISASegment.components(3).key
  val interSenderQualKey = ISASegment.components(4).key
  val interSenderInfoKey = ISASegment.components(5).key
  val interReceiverQualKey = ISASegment.components(6).key
  val interReceiverInfoKey = ISASegment.components(7).key
  val interDateKey = ISASegment.components(8).key
  val interTimeKey = ISASegment.components(9).key
  val interRepSepKey = ISASegment.components(10).key
  val interControlVersionKey = ISASegment.components(11).key
  val interControlNumberHeaderKey = ISASegment.components(12).key
  val interAckRequestedKey = ISASegment.components(13).key
  val interUsageIndicatorKey = ISASegment.components(14).key
  val interCompSepKey = ISASegment.components(15).key

  // value keys for GS segment
  val interNumberGroupsIncludedKey = IEASegment.components(0).key
  val interControlNumberTrailerKey = IEASegment.components(1).key

  // value keys for GS segment
  val groupFunctionalIdentifierKey = GSSegment.components(0).key
  val groupApplicationSenderKey = GSSegment.components(1).key
  val groupApplicationReceiverKey = GSSegment.components(2).key
  val groupDateKey = GSSegment.components(3).key
  val groupTimeKey = GSSegment.components(4).key
  val groupControlNumberHeaderKey = GSSegment.components(5).key
  val groupResponsibleAgencyKey = GSSegment.components(6).key
  val groupVersionReleaseIndustryKey = GSSegment.components(7).key

  // value keys for GE segment
  val groupNumberSetsIncludedKey = GESegment.components(0).key
  val groupControlNumberTrailerKey = GESegment.components(1).key

  // value keys for ST segment
  val setIdentifierCodeKey = STSegment.components(0).key
  val setControlNumberHeaderKey = STSegment.components(1).key
  val setImplementationConventionKey = STSegment.components(2).key

  // value keys for SE segment
  val setNumberSegmentsIncludedKey = SESegment.components(0).key
  val setControlNumberTrailerKey = SESegment.components(1).key
}

object X12Acknowledgment {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.X12Constants._
  import com.anypoint.df.edi.lexical.EdiConstants.DataType
  import com.anypoint.df.edi.lexical.EdiConstants.DataType._

  trait Coded[K] {
    val code: K
  }

  def instanceMap[K, V <: Coded[K]](values: V*): Map[K, V] =
    values.toList.foldLeft(Map[K, V]())((acc, value) => acc + (value.code -> value))

  /** Interchange acknowledgment codes (TA1 I17 element codes). */
  sealed case class InterchangeAcknowledgmentCode(val code: String) extends Coded[String]
  val AcknowledgedNoErrors = InterchangeAcknowledgmentCode("A")
  val AcknowledgedWithErrors = InterchangeAcknowledgmentCode("E")
  val AcknowledgedRejected = InterchangeAcknowledgmentCode("R")
  val InterchangeAcknowledgmentCodes = instanceMap[String, InterchangeAcknowledgmentCode](AcknowledgedNoErrors,
    AcknowledgedWithErrors, AcknowledgedRejected)

  /** Interchange note codes (TA1 I18 element codes). */
  sealed case class InterchangeNoteCode(val code: String, val text: String) extends Coded[String]
  val InterchangeNoError = InterchangeNoteCode("000", "No error")
  val InterchangeControlNumberMismatch = InterchangeNoteCode("001", "The Interchange Control Number in the Header and Trailer Do Not Match")
  val InterchangeUnsupportedStandard = InterchangeNoteCode("002", "This Standard as Noted in the Control Standards Identifier is Not Supported")
  val InterchangeUnsupportedVersion = InterchangeNoteCode("003", "This Version of the Controls is Not Supported")
  val InterchangeSegmentTerminator = InterchangeNoteCode("004", "The Segment Terminator is Invalid")
  val InterchangeSenderIdQual = InterchangeNoteCode("005", "Invalid Interchange ID Qualifier for Sender")
  val InterchangeSenderId = InterchangeNoteCode("006", "Invalid Interchange Sender ID")
  val InterchangeReceiverIdQual = InterchangeNoteCode("007", "Invalid Interchange ID Qualifier for Receiver")
  val InterchangeReceiverId = InterchangeNoteCode("008", "Invalid Interchange Receiver ID")
  val InterchangeUnknownReceiverId = InterchangeNoteCode("009", "Unknown Interchange Receiver ID")
  val InterchangeAuthorizationInfoQual = InterchangeNoteCode("010", "Invalid Authorization Information Qualifier Value")
  val InterchangeAuthorizationInfo = InterchangeNoteCode("011", "Invalid Authorization Information Value")
  val InterchangeSecurityInfoQual = InterchangeNoteCode("012", "Invalid Security Information Qualifier Value")
  val InterchangeSecurityInfo = InterchangeNoteCode("013", "Invalid Security Information Value")
  val InterchangeInvalidDate = InterchangeNoteCode("014", "Invalid Interchange Date Value")
  val InterchangeInvalidTime = InterchangeNoteCode("015", "Invalid Interchange Time Value")
  val InterchangeInvalidStandardsIdentifier = InterchangeNoteCode("016", "Invalid Interchange Standards Identifier Value")
  val InterchangeInvalidVersionId = InterchangeNoteCode("017", "Invalid Interchange Version ID Value")
  val InterchangeInvalidControlNumber = InterchangeNoteCode("018", "Invalid Interchange Control Number Value")
  val InterchangeInvalidAcknowledgmentRequested = InterchangeNoteCode("019", "Invalid Acknowledgment Requested Value")
  val InterchangeInvalidTestIndicator = InterchangeNoteCode("020", "Invalid Test Indicator Value")
  val InterchangeInvalidGroupCount = InterchangeNoteCode("021", "Invalid Number of Included Groups Value")
  val InterchangeInvalidControlStructure = InterchangeNoteCode("022", "Invalid Control Structure")
  val InterchangeEndOfFile = InterchangeNoteCode("023", "Improper (Premature) End-of-File (Transmission)")
  val InterchangeInvalidContent = InterchangeNoteCode("024", "Invalid Interchange Content (e.g., Invalid GS Segment)")
  val InterchangeDuplicateNumber = InterchangeNoteCode("025", "Duplicate Interchange Control Number")
  val InterchangeDataElementSeparator = InterchangeNoteCode("026", "Invalid Data Element Separator")
  val InterchangeComponentElementSeparator = InterchangeNoteCode("027", "Invalid Component Element Separator")
  val InterchangeNoteCodes = instanceMap[String, InterchangeNoteCode](InterchangeNoError,
    InterchangeControlNumberMismatch, InterchangeUnsupportedStandard, InterchangeUnsupportedVersion,
    InterchangeSegmentTerminator, InterchangeSenderIdQual, InterchangeSenderId, InterchangeReceiverIdQual,
    InterchangeReceiverId, InterchangeUnknownReceiverId, InterchangeInvalidDate, InterchangeInvalidTime,
    InterchangeInvalidStandardsIdentifier, InterchangeInvalidVersionId, InterchangeInvalidControlNumber,
    InterchangeInvalidAcknowledgmentRequested, InterchangeInvalidTestIndicator, InterchangeInvalidGroupCount,
    InterchangeInvalidControlStructure, InterchangeEndOfFile, InterchangeInvalidContent, InterchangeDuplicateNumber,
    InterchangeDataElementSeparator, InterchangeComponentElementSeparator)

  /** Associate lexer start status codes from ISA segment with interchange note codes. */
  import com.anypoint.df.edi.lexical.X12Lexer.InterchangeStartStatus
  val LexerStartStatusInterchangeNote = Map(
    (InterchangeStartStatus.AUTHORIZATION_QUALIFIER_ERROR -> InterchangeAuthorizationInfoQual),
    (InterchangeStartStatus.AUTHORIZATION_INFO_ERROR -> InterchangeAuthorizationInfo),
    (InterchangeStartStatus.SECURITY_QUALIFIER_ERROR -> InterchangeSecurityInfoQual),
    (InterchangeStartStatus.SECURITY_INFO_ERROR -> InterchangeSecurityInfo),
    (InterchangeStartStatus.SENDER_ID_QUALIFIER_ERROR -> InterchangeSenderIdQual),
    (InterchangeStartStatus.SENDER_ID_ERROR -> InterchangeSenderId),
    (InterchangeStartStatus.RECEIVER_ID_QUALIFIER_ERROR -> InterchangeReceiverIdQual),
    (InterchangeStartStatus.RECEIVER_ID_ERROR -> InterchangeReceiverId),
    (InterchangeStartStatus.INTERCHANGE_DATE_ERROR -> InterchangeInvalidDate),
    (InterchangeStartStatus.INTERCHANGE_TIME_ERROR -> InterchangeInvalidTime),
    (InterchangeStartStatus.VERSION_ID_ERROR -> InterchangeInvalidVersionId),
    (InterchangeStartStatus.INTER_CONTROL_ERROR -> InterchangeInvalidControlNumber),
    (InterchangeStartStatus.ACK_REQUESTED_ERROR -> InterchangeInvalidAcknowledgmentRequested),
    (InterchangeStartStatus.TEST_INDICATOR_ERROR -> InterchangeInvalidTestIndicator))

  /** Associate lexer end status codes from IEA segment with interchange note codes. */
  import com.anypoint.df.edi.lexical.X12Lexer.InterchangeEndStatus
  val LexerEndStatusInterchangeNote = Map(
    (InterchangeEndStatus.CONTROL_NUMBER_ERROR -> InterchangeControlNumberMismatch),
    (InterchangeEndStatus.GROUP_COUNT_ERROR -> InterchangeInvalidGroupCount))

  /** Functional group syntax error codes (X12 716 element codes). */
  sealed case class GroupSyntaxError(val code: String, val text: String) extends Coded[String]
  val NotSupportedGroup = GroupSyntaxError("1", "Functional Group Not Supported")
  val NotSupportedGroupVersion = GroupSyntaxError("2", "Functional Group Version Not Supported")
  val MissingGroupTrailer = GroupSyntaxError("3", "Functional Group Trailer Missing")
  val GroupControlNumberMismatch = GroupSyntaxError("4", "Group Control Number in the Functional Group Header and Trailer Do Not Agree")
  val GroupTransactionCountError = GroupSyntaxError("5", "Number of Included Transaction Sets Does Not Match Actual Count")
  val GroupControlNumberError = GroupSyntaxError("6", "Group Control Number Violates Syntax")
  val GroupControlNumberNotUnique = GroupSyntaxError("19", "Functional Group Control Number not Unique within Interchange")
  val GroupSyntaxErrors = instanceMap[String, GroupSyntaxError](NotSupportedGroup, NotSupportedGroupVersion,
    MissingGroupTrailer, GroupControlNumberMismatch, GroupTransactionCountError, GroupControlNumberError,
    GroupControlNumberNotUnique)

  /** Functional group acknowledgment codes (X12 717 element codes). */
  sealed case class GroupAcknowledgmentCode(val code: String, val text: String) extends Coded[String]
  val AcceptedGroup = GroupAcknowledgmentCode("A", "Accepted")
  val AcceptedWithErrorsGroup = GroupAcknowledgmentCode("E", "Accepted, But Errors Were Noted")
  val PartiallyAcceptedGroup = GroupAcknowledgmentCode("P", "Partially Accepted, At Least One Transaction Set Was Rejected")
  val RejectedGroup = GroupAcknowledgmentCode("R", "Rejected")
  val GroupAcknowledgmentCodes = instanceMap[String, GroupAcknowledgmentCode](AcceptedGroup, AcceptedWithErrorsGroup,
    PartiallyAcceptedGroup, RejectedGroup)

  /** Transaction set syntax error codes (X12 718 element codes). */
  sealed case class TransactionSyntaxError(val code: String, val text: String) extends Coded[String]
  val NotSupportedTransaction = TransactionSyntaxError("1", "Transaction Set Not Supported")
  val MissingTrailerTransaction = TransactionSyntaxError("2", "Transaction Set Trailer Missing")
  val ControlNumberMismatch = TransactionSyntaxError("3", "Transaction Set Control Number in Header and Trailer Do Not Match")
  val WrongSegmentCount = TransactionSyntaxError("4", "Number of Included Segments Does Not Match Actual Count")
  val SegmentsInError = TransactionSyntaxError("5", "One or More Segments in Error")
  val BadTransactionSetId = TransactionSyntaxError("6", "Missing or Invalid Transaction Set Identifier")
  val BadTransactionSetControl = TransactionSyntaxError("7", "Missing or Invalid Transaction Set Control Number")
  val SetNotInGroup = TransactionSyntaxError("18", "Transaction Set not in Functional Group")
  val InvalidImplementationConvention = TransactionSyntaxError("23", "Invalid Transaction Set Implementation Convention Reference")
  val TransactionSyntaxErrors = instanceMap[String, TransactionSyntaxError](NotSupportedTransaction,
    MissingTrailerTransaction, ControlNumberMismatch, WrongSegmentCount, SegmentsInError, BadTransactionSetId,
    BadTransactionSetControl, SetNotInGroup, InvalidImplementationConvention)

  /** Transaction set acknowledgment codes (X12 717 element codes). */
  sealed case class TransactionAcknowledgmentCode(val code: String, val text: String) extends Coded[String]
  val AcceptedTransaction = TransactionAcknowledgmentCode("A", "Accepted")
  val AcceptedWithErrorsTransaction = TransactionAcknowledgmentCode("E", "Accepted But Errors Were Noted")
  val AuthenticationFailedTransaction = TransactionAcknowledgmentCode("M", "Rejected, Message Authentication Code (MAC) Failed")
  val RejectedTransaction = TransactionAcknowledgmentCode("R", "Rejected")
  val ValidityFailedTransaction = TransactionAcknowledgmentCode("W", "Rejected, Assurance Failed Validity Tests")
  val DecryptionBadTransaction = TransactionAcknowledgmentCode("X", "Rejected, Content After Decryption Could Not Be Analyzed")
  val TransactionAcknowledgmentCodes = instanceMap[String, TransactionAcknowledgmentCode](AcceptedTransaction,
    AcceptedWithErrorsTransaction, AuthenticationFailedTransaction, RejectedTransaction, ValidityFailedTransaction,
    DecryptionBadTransaction)

  /** Segment syntax error codes (X12 720 element codes). */
  sealed case class SegmentSyntaxError(val code: String, val text: String) extends Coded[String]
  val UnrecognizedSegment = SegmentSyntaxError("1", "Unrecognized segment ID")
  val UnexpectedSegment = SegmentSyntaxError("2", "Unexpected segment")
  val MissingMandatorySegment = SegmentSyntaxError("3", "Mandatory segment missing")
  val TooManyLoops = SegmentSyntaxError("4", "Loop Occurs Over Maximum Times")
  val TooManyOccurs = SegmentSyntaxError("5", "Segment Exceeds Maximum Use")
  val NotInTransactionSegment = SegmentSyntaxError("6", "Segment Not in Defined Transaction Set")
  val OutOfOrderSegment = SegmentSyntaxError("7", "Segment Not in Proper Sequence")
  val DataErrorsSegment = SegmentSyntaxError("8", "Segment Has Data Element Errors")
  val SegmentSyntaxErrors = instanceMap[String, SegmentSyntaxError](UnrecognizedSegment, UnexpectedSegment,
    MissingMandatorySegment, TooManyLoops, TooManyOccurs, NotInTransactionSegment, OutOfOrderSegment, DataErrorsSegment)

  /** Information for a segment error (used to generate X12 AK3 segment). */
  case class SegmentError(val id: String, val position: Int, val loopId: Option[String],
    val error: Option[SegmentSyntaxError])

  /** Data element syntax error codes (X12 723 element codes). */
  sealed case class ElementSyntaxError(val code: String, val text: String) extends Coded[String]
  val MissingRequiredElement = ElementSyntaxError("1", "Mandatory data element missing")
  val MissingConditionalElement = ElementSyntaxError("2", "Conditional required data element missing")
  val TooManyElements = ElementSyntaxError("3", "Too many data elements (more data elements than defined for the segment)")
  val DataTooShort = ElementSyntaxError("4", "Data element too short")
  val DataTooLong = ElementSyntaxError("5", "Data element too long")
  val InvalidCharacter = ElementSyntaxError("6", "Invalid character in data element")
  val InvalidCodeValue = ElementSyntaxError("7", "Invalid code value")
  val InvalidDate = ElementSyntaxError("8", "Invalid Date")
  val InvalidTime = ElementSyntaxError("9", "Invalid Time")
  val ExclusionConditionViolated = ElementSyntaxError("10", "Exclusion Condition Violated")
  val TooManyRepititions = ElementSyntaxError("11", "Too Many Repetitions (more repetitions than defined for the segment)")
  val TooManyComponents = ElementSyntaxError("12", "Too Many Components (more components than defined for the element)")
  val ElementSyntaxErrors = instanceMap[String, ElementSyntaxError](MissingRequiredElement, MissingConditionalElement,
    TooManyElements, DataTooShort, DataTooLong, InvalidCharacter, InvalidCodeValue, InvalidDate, InvalidTime,
    ExclusionConditionViolated, TooManyRepititions, TooManyComponents)

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
      ElementComponent(Element("1686", "Repeating Data Element Position", INTEGER, 1, 4), None, "AK40103", 3, OptionalUsage, 1)), Nil, 0), None, "AK401", 1, MandatoryUsage, 1),
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

  val groupAK3 = GroupComponent("AK3", OptionalUsage, -1, StructureSequence(true, List[StructureComponent](
    ReferenceComponent(segAK3, SegmentPosition(0, "0400"), OptionalUsage, 1),
    ReferenceComponent(segAK4, SegmentPosition(0, "0500"), OptionalUsage, 99))), None, Nil)
  val groupAK2_997 = GroupComponent("AK2", OptionalUsage, -1, StructureSequence(true, List[StructureComponent](
    ReferenceComponent(segAK2, SegmentPosition(0, "0300"), OptionalUsage, 1),
    groupAK3,
    ReferenceComponent(segAK5, SegmentPosition(0, "0600"), MandatoryUsage, 1))), None, Nil)
  val funcAckVersion = EdiSchemaVersion(EdiSchema.X12, "005010")
  val trans997 = Structure("997", "Functional Acknowledgment", Some("FA"), Some(StructureSequence(false, List[StructureComponent](
    ReferenceComponent(segST, SegmentPosition(0, "0100"), MandatoryUsage, 1),
    ReferenceComponent(segAK1, SegmentPosition(0, "0200"), MandatoryUsage, 1),
    groupAK2_997,
    ReferenceComponent(segAK9, SegmentPosition(0, "0700"), MandatoryUsage, 1),
    ReferenceComponent(segSE, SegmentPosition(0, "0800"), MandatoryUsage, 1)))),
    None, None, funcAckVersion)

  // 999 acknowledgment schema (generated code, eliminated duplicates from 997)
  val elem447 = Element("447", "Loop Identifier Code", ALPHANUMERIC, 1, 4)
  val elem618 = Element("618", "Implementation Transaction Set Syntax Error Code", ID, 1, 3)
  val elem719 = Element("719", "Segment Position in Transaction Set", INTEGER, 1, 10)
  val elem721 = Element("721", "Segment ID Code", ID, 2, 3)
  val compC030 = Composite("C030", "Position in Segment", List[SegmentComponent](
    ElementComponent(Element("722", "Element Position in Segment", INTEGER, 1, 2), None, "IK40101", 1, MandatoryUsage, 1),
    ElementComponent(Element("1528", "Component Data Element Position in Composite", INTEGER, 1, 2), None, "IK40102", 2, OptionalUsage, 1),
    ElementComponent(Element("1686", "Repeating Data Element Position", INTEGER, 1, 4), None, "IK40103", 3, OptionalUsage, 1)), Nil, 0)
  val compC998 = Composite("C998", "Context Identification", List[SegmentComponent](
    ElementComponent(Element("9999", "Context Name", ALPHANUMERIC, 1, 35), None, "C99801", 1, MandatoryUsage, 1),
    ElementComponent(Element("9998", "Context Reference", ALPHANUMERIC, 1, 35), None, "C99802", 2, OptionalUsage, 1)), Nil, 0)
  val compC999 = Composite("C999", "Reference in Segment", List[SegmentComponent](
    ElementComponent(Element("725", "Data Element Reference Number", INTEGER, 1, 4), None, "CTX0601", 1, MandatoryUsage, 1),
    ElementComponent(Element("725", "Data Element Reference Number", INTEGER, 1, 4), None, "CTX0602", 2, OptionalUsage, 1)), Nil, 0)
  val segCTX = Segment("CTX", "Context", List[SegmentComponent](
    CompositeComponent(compC998, Some("Context Identification"), "CTX01", 1, MandatoryUsage, 10),
    ElementComponent(elem721, None, "CTX02", 2, OptionalUsage, 1),
    ElementComponent(elem719, None, "CTX03", 3, OptionalUsage, 1),
    ElementComponent(elem447, None, "CTX04", 4, OptionalUsage, 1),
    CompositeComponent(compC030.rewrite("CTX05", convertEdiForm("X12")), Some("Position in Segment"), "CTX05", 5, OptionalUsage, 1),
    CompositeComponent(compC999.rewrite("CTX06", convertEdiForm("X12")), Some("Reference in Segment"), "CTX06", 6, OptionalUsage, 1)), Nil)
  val segIK3 = Segment("IK3", "Implementation Data Segment Note", List[SegmentComponent](
    ElementComponent(elem721, None, "IK301", 1, MandatoryUsage, 1),
    ElementComponent(elem719, None, "IK302", 2, MandatoryUsage, 1),
    ElementComponent(elem447, None, "IK303", 3, OptionalUsage, 1),
    ElementComponent(Element("620", "Implementation Segment Syntax Error Code", ID, 1, 3), None, "IK304", 4, OptionalUsage, 1)), Nil)
  val segIK4 = Segment("IK4", "Implementation Data Element Note", List[SegmentComponent](
    CompositeComponent(compC030.rewrite("IK401", convertEdiForm("X12")), Some("Position in Segment"), "IK401", 1, MandatoryUsage, 1),
    ElementComponent(Element("725", "Data Element Reference Number", INTEGER, 1, 4), None, "IK402", 2, OptionalUsage, 1),
    ElementComponent(Element("621", "Implementation Data Element Syntax Error Code", ID, 1, 3), None, "IK403", 3, MandatoryUsage, 1),
    ElementComponent(Element("724", "Copy of Bad Data Element", ALPHANUMERIC, 1, 99), None, "IK404", 4, OptionalUsage, 1)), Nil)
  val segIK5 = Segment("IK5", "Implementation Transaction Set Response Trailer", List[SegmentComponent](
    ElementComponent(Element("717", "Transaction Set Acknowledgment Code", ID, 1, 1), None, "IK501", 1, MandatoryUsage, 1),
    ElementComponent(elem618, None, "IK502", 2, OptionalUsage, 1),
    ElementComponent(elem618, None, "IK503", 3, OptionalUsage, 1),
    ElementComponent(elem618, None, "IK504", 4, OptionalUsage, 1),
    ElementComponent(elem618, None, "IK505", 5, OptionalUsage, 1),
    ElementComponent(elem618, None, "IK506", 6, OptionalUsage, 1)), Nil)
    
  val groupIK4 = GroupComponent("IK4", OptionalUsage, -1, StructureSequence(true, List[StructureComponent](
    ReferenceComponent(segIK4, SegmentPosition(0, "0600"), OptionalUsage, 1),
    ReferenceComponent(segCTX, SegmentPosition(0, "0700"), OptionalUsage, 10))), None, Nil)
  val groupIK3 = GroupComponent("IK3", OptionalUsage, -1, StructureSequence(true, List[StructureComponent](
    ReferenceComponent(segIK3, SegmentPosition(0, "0400"), OptionalUsage, 1),
    ReferenceComponent(segCTX, SegmentPosition(0, "0500"), OptionalUsage, 10),
    groupIK4)), None, Nil)
  val groupAK2_999 = GroupComponent("AK2", OptionalUsage, -1, StructureSequence(true, List[StructureComponent](
    ReferenceComponent(segAK2, SegmentPosition(0, "0300"), OptionalUsage, 1),
    groupIK3,
    ReferenceComponent(segIK5, SegmentPosition(0, "0800"), MandatoryUsage, 1))), None, Nil)
  val trans999 = Structure("999", "Implementation Acknowledgment", Some("FA"), Some(StructureSequence(false, List[StructureComponent](
    ReferenceComponent(segST, SegmentPosition(0, "0100"), MandatoryUsage, 1),
    ReferenceComponent(segAK1, SegmentPosition(0, "0200"), MandatoryUsage, 1),
    groupAK2_999,
    ReferenceComponent(segAK9, SegmentPosition(0, "0900"), MandatoryUsage, 1),
    ReferenceComponent(segSE, SegmentPosition(0, "1000"), MandatoryUsage, 1)))),
    None, None, funcAckVersion)

  // random access arrays of keys
  val segAK9Comps = segAK9.components.toArray
  val segIK5Comps = segIK5.components.toArray
  val segAK5Comps = segAK5.components.toArray
  val segIK4Comps = segIK4.components.toArray
  val segAK4Comps = segAK4.components.toArray
  val segIK3Comps = segIK3.components.toArray
  val segAK3Comps = segAK3.components.toArray
  val segAK2Comps = segAK2.components.toArray
  val segAK1Comps = segAK1.components.toArray
  val segCTXComps = segCTX.components.toArray
  val groupIK4Keys = groupIK4.seq.items map { comp => comp.key } toArray
  val groupIK3Keys = groupIK3.seq.items map { comp => comp.key } toArray
  val groupAK3Keys = groupAK3.seq.items map { comp => comp.key } toArray
  val groupAK2_997Keys = groupAK2_997.seq.items map { comp => comp.key } toArray
  val groupAK2_999Keys = groupAK2_999.seq.items map { comp => comp.key } toArray
  val trans997Keys = trans997.heading.get.items map { comp => comp.key } toArray
  val trans999Keys = trans999.heading.get.items map { comp => comp.key } toArray
  
  // access methods to select appropriate model
  def groupXK2Keys(generate999: Boolean) = if (generate999) groupAK2_999Keys else groupAK2_997Keys
  def groupXK3Keys(generate999: Boolean) = if (generate999) groupIK3Keys else groupAK3Keys
  def segXK3Comps(generate999: Boolean) = if (generate999) segIK3Comps else segAK3Comps
  def segXK4Comps(generate999: Boolean) = if (generate999) segIK4Comps else segAK4Comps
  def segXK5Comps(generate999: Boolean) = if (generate999) segIK5Comps else segAK5Comps
  def ackTransKeys(generate999: Boolean) = if (generate999) trans999Keys else trans997Keys

  // TA1 acknowledgment data (generated code)
  val segTA1 = Segment("TA1", "Interchange Acknowledgment", List[SegmentComponent](
    ElementComponent(Element("I12", "Interchange Control Number", INTEGER, 9, 9), None, "TA101", 1, MandatoryUsage, 1),
    ElementComponent(Element("I08", "Interchange Date", DATE, 6, 6), None, "TA102", 2, MandatoryUsage, 1),
    ElementComponent(Element("I09", "Interchange Time", TIME, 4, 4), None, "TA103", 3, MandatoryUsage, 1),
    ElementComponent(Element("I17", "Interchange Acknowledgment Code", ID, 1, 1), None, "TA104", 4, MandatoryUsage, 1),
    ElementComponent(Element("I18", "Interchange Note Code", ID, 3, 3), None, "TA105", 5, MandatoryUsage, 1)), Nil)
}