package com.mulesoft.flatfile.schema

/** Base control definitions for HL7 EDI schemas. */
object HL7SchemaDefs {
  import EdiSchema._

  import com.mulesoft.flatfile.lexical.EdifactConstants.SyntaxVersion
  import com.mulesoft.flatfile.lexical.HL7Support

  val mshKey = "MSH"
  val errsKey = "ERR"

  val elemDTM = Element("DTM", "Date/Time", HL7Support.buildType("DTM", 4, 64))
  val elemID = Element("ST", "Coded Value for HL7 Defined Tables", HL7Support.buildType("ST", 1, 199))
  val elemIS = Element("ST", "Coded Value for User-Defined Tables", HL7Support.buildType("ST", 1, 20))
  val elemNM = Element("NM", "Numeric", HL7Support.buildType("NM", 1, 16))
  val elemST = Element("ST", "String Data", HL7Support.buildType("ST", 1, 199))
  val elemTX = Element("TX", "Text Data", HL7Support.buildType("ST", 1, 2000))
  val elemVaries = Element("varies", "varies", HL7Support.buildType("ST", 0, 0))
  val compCE = Composite("CE", "Coded Element", List[SegmentComponent](
    ElementComponent(elemST, None, "CE-01", 1, OptionalUsage, 1),
    ElementComponent(elemST, None, "CE-02", 2, OptionalUsage, 1),
    ElementComponent(elemID, None, "CE-03", 3, OptionalUsage, 1),
    ElementComponent(elemST, None, "CE-04", 4, OptionalUsage, 1),
    ElementComponent(elemST, None, "CE-05", 5, OptionalUsage, 1),
    ElementComponent(elemID, None, "CE-06", 6, OptionalUsage, 1)), Nil, 0)
  val compCWE = Composite("CWE", "Coded with Exceptions", List[SegmentComponent](
    ElementComponent(elemST, None, "CWE-01", 1, OptionalUsage, 1),
    ElementComponent(elemST, None, "CWE-02", 2, OptionalUsage, 1),
    ElementComponent(elemID, None, "CWE-03", 3, OptionalUsage, 1),
    ElementComponent(elemST, None, "CWE-04", 4, OptionalUsage, 1),
    ElementComponent(elemST, None, "CWE-05", 5, OptionalUsage, 1),
    ElementComponent(elemID, None, "CWE-06", 6, OptionalUsage, 1),
    ElementComponent(elemST, None, "CWE-07", 7, ConditionalUsage, 1),
    ElementComponent(elemST, None, "CWE-08", 8, OptionalUsage, 1),
    ElementComponent(elemST, None, "CWE-09", 9, OptionalUsage, 1)), Nil, 0)
  val compEI = Composite("EI", "Entity Identifier", List[SegmentComponent](
    ElementComponent(elemST, None, "MSH-21-01", 1, OptionalUsage, 1),
    ElementComponent(elemIS, None, "MSH-21-02", 2, OptionalUsage, 1),
    ElementComponent(elemST, None, "MSH-21-03", 3, ConditionalUsage, 1),
    ElementComponent(elemID, None, "MSH-21-04", 4, ConditionalUsage, 1)), Nil, 0)
  val compERL = Composite("ERL", "Error Location", List[SegmentComponent](
    ElementComponent(elemST, None, "ERR-02-01", 1, MandatoryUsage, 1),
    ElementComponent(elemNM, None, "ERR-02-02", 2, MandatoryUsage, 1),
    ElementComponent(elemNM, None, "ERR-02-03", 3, OptionalUsage, 1),
    ElementComponent(elemNM, None, "ERR-02-04", 4, OptionalUsage, 1),
    ElementComponent(elemNM, None, "ERR-02-05", 5, OptionalUsage, 1),
    ElementComponent(elemNM, None, "ERR-02-06", 6, OptionalUsage, 1)), Nil, 0)
  val compHD = Composite("HD", "Hierarchic Designator", List[SegmentComponent](
    ElementComponent(elemIS, None, "HD-01", 1, OptionalUsage, 1),
    ElementComponent(elemST, None, "HD-02", 2, ConditionalUsage, 1),
    ElementComponent(elemID, None, "HD-03", 3, ConditionalUsage, 1)), Nil, 0)
  val compMSG = Composite("MSG", "Message Type", List[SegmentComponent](
    ElementComponent(elemID, None, "MSH-09-01", 1, MandatoryUsage, 1),
    ElementComponent(elemID, None, "MSH-09-02", 2, MandatoryUsage, 1),
    ElementComponent(elemID, None, "MSH-09-03", 3, MandatoryUsage, 1)), Nil, 0)
  val compPT = Composite("PT", "Processing Type", List[SegmentComponent](
    ElementComponent(elemID, None, "MSH-11-01", 1, OptionalUsage, 1),
    ElementComponent(elemID, None, "MSH-11-02", 2, OptionalUsage, 1)), Nil, 0)
  val compTS = Composite("TS", "Time Stamp", List[SegmentComponent](
    ElementComponent(elemDTM, None, "TS-01", 1, MandatoryUsage, 1),
    ElementComponent(elemID, None, "TS-02", 2, OptionalUsage, 1)), Nil, 0)
  val compXTN = Composite("XTN", "Extended Telecommunication Number", List[SegmentComponent](
    ElementComponent(elemST, None, "ERR-12-01", 1, OptionalUsage, 1),
    ElementComponent(elemID, None, "ERR-12-02", 2, OptionalUsage, 1),
    ElementComponent(elemID, None, "ERR-12-03", 3, OptionalUsage, 1),
    ElementComponent(elemST, None, "ERR-12-04", 4, OptionalUsage, 1),
    ElementComponent(elemNM, None, "ERR-12-05", 5, OptionalUsage, 1),
    ElementComponent(elemNM, None, "ERR-12-06", 6, OptionalUsage, 1),
    ElementComponent(elemNM, None, "ERR-12-07", 7, OptionalUsage, 1),
    ElementComponent(elemNM, None, "ERR-12-08", 8, OptionalUsage, 1),
    ElementComponent(elemST, None, "ERR-12-09", 9, OptionalUsage, 1),
    ElementComponent(elemST, None, "ERR-12-10", 10, OptionalUsage, 1),
    ElementComponent(elemST, None, "ERR-12-11", 11, OptionalUsage, 1),
    ElementComponent(elemST, None, "ERR-12-12", 12, ConditionalUsage, 1)), Nil, 0)
  val compELD = Composite("ELD", "Error Location and Description", List[SegmentComponent](
    ElementComponent(elemST, None, "ERR-01-01", 1, OptionalUsage, 1),
    ElementComponent(elemNM, None, "ERR-01-02", 2, OptionalUsage, 1),
    ElementComponent(elemNM, None, "ERR-01-03", 3, OptionalUsage, 1),
    CompositeComponent(compCE.rewrite("ERR-01-04", convertEdiForm("HL7")), Some("Coded Element"), "ERR-01-04", 4, OptionalUsage, 1)), Nil, 0)
  val compVID = Composite("VID", "Version Identifier", List[SegmentComponent](
    ElementComponent(elemID, None, "MSH-12-01", 1, OptionalUsage, 1),
    CompositeComponent(compCE.rewrite("MSH-12-02", convertEdiForm("HL7")), Some("Coded Element"), "MSH-12-02", 2, OptionalUsage, 1),
    CompositeComponent(compCE.rewrite("MSH-12-03", convertEdiForm("HL7")), Some("Coded Element"), "MSH-12-03", 3, OptionalUsage, 1)), Nil, 0)
  val compXON = Composite("XON", "Extended Composite Name and Identification Number for Organizations", List[SegmentComponent](
    ElementComponent(elemST, None, "SFT-01-01", 1, OptionalUsage, 1),
    ElementComponent(elemIS, None, "SFT-01-02", 2, OptionalUsage, 1),
    ElementComponent(elemNM, None, "SFT-01-03", 3, OptionalUsage, 1),
    ElementComponent(elemNM, None, "SFT-01-04", 4, OptionalUsage, 1),
    ElementComponent(elemID, None, "SFT-01-05", 5, OptionalUsage, 1),
    CompositeComponent(compHD.rewrite("SFT-01-06", convertEdiForm("HL7")), Some("Hierarchic Designator"), "SFT-01-06", 6, OptionalUsage, 1),
    ElementComponent(elemID, None, "SFT-01-07", 7, OptionalUsage, 1),
    CompositeComponent(compHD.rewrite("SFT-01-08", convertEdiForm("HL7")), Some("Hierarchic Designator"), "SFT-01-08", 8, OptionalUsage, 1),
    ElementComponent(elemID, None, "SFT-01-09", 9, OptionalUsage, 1),
    ElementComponent(elemST, None, "SFT-01-10", 10, OptionalUsage, 1)), Nil, 0)
  val segERR = new Segment("ERR", "Error", List[SegmentComponent](
    CompositeComponent(compELD, Some("Error Location and Description"), "ERR-01", 1, OptionalUsage, 1),
    CompositeComponent(compERL, Some("Error Location"), "ERR-02", 2, OptionalUsage, 1),
    CompositeComponent(compCWE.rewrite("ERR-03", convertEdiForm("HL7")), Some("Coded with Exceptions"), "ERR-03", 3, MandatoryUsage, 1),
    ElementComponent(elemID, None, "ERR-04", 4, MandatoryUsage, 1),
    CompositeComponent(compCWE.rewrite("ERR-05", convertEdiForm("HL7")), Some("Coded with Exceptions"), "ERR-05", 5, OptionalUsage, 1),
    ElementComponent(elemST, None, "ERR-06", 6, OptionalUsage, 1),
    ElementComponent(elemTX, None, "ERR-07", 7, OptionalUsage, 1),
    ElementComponent(elemTX, None, "ERR-08", 8, OptionalUsage, 1),
    ElementComponent(elemIS, None, "ERR-09", 9, OptionalUsage, 1),
    CompositeComponent(compCWE.rewrite("ERR-10", convertEdiForm("HL7")), Some("Coded with Exceptions"), "ERR-10", 10, OptionalUsage, 1),
    CompositeComponent(compCWE.rewrite("ERR-11", convertEdiForm("HL7")), Some("Coded with Exceptions"), "ERR-11", 11, OptionalUsage, 1),
    CompositeComponent(compXTN, Some("Extended Telecommunication Number"), "ERR-12", 12, OptionalUsage, 1)), Nil)
  val segMSA = new Segment("MSA", "Message Acknowledgment", List[SegmentComponent](
    ElementComponent(elemID, None, "MSA-01", 1, MandatoryUsage, 1),
    ElementComponent(elemST, None, "MSA-02", 2, MandatoryUsage, 1),
    ElementComponent(elemST, None, "MSA-03", 3, OptionalUsage, 1),
    ElementComponent(elemNM, None, "MSA-04", 4, OptionalUsage, 1),
    ElementComponent(elemVaries, None, "MSA-05", 5, UnusedUsage, 1),
    CompositeComponent(compCE.rewrite("MSA-06", convertEdiForm("HL7")), Some("Coded Element"), "MSA-06", 6, OptionalUsage, 1)), Nil)
  val segMSH = new Segment("MSH", "Message Header", List[SegmentComponent](
    //    ElementComponent(elemST, None, "MSH-01", 1, MandatoryUsage, 1), (removed since not really a field)
    ElementComponent(elemST, None, "MSH-02", 2, MandatoryUsage, 1),
    CompositeComponent(compHD.rewrite("MSH-03", convertEdiForm("HL7")), Some("Hierarchic Designator"), "MSH-03", 3, OptionalUsage, 1),
    CompositeComponent(compHD.rewrite("MSH-04", convertEdiForm("HL7")), Some("Hierarchic Designator"), "MSH-04", 4, OptionalUsage, 1),
    CompositeComponent(compHD.rewrite("MSH-05", convertEdiForm("HL7")), Some("Hierarchic Designator"), "MSH-05", 5, OptionalUsage, 1),
    CompositeComponent(compHD.rewrite("MSH-06", convertEdiForm("HL7")), Some("Hierarchic Designator"), "MSH-06", 6, OptionalUsage, 1),
    CompositeComponent(compTS.rewrite("MSH-07", convertEdiForm("HL7")), Some("Time Stamp"), "MSH-07", 7, MandatoryUsage, 1),
    ElementComponent(elemST, None, "MSH-08", 8, OptionalUsage, 1),
    CompositeComponent(compMSG, Some("Message Type"), "MSH-09", 9, MandatoryUsage, 1),
    ElementComponent(elemST, None, "MSH-10", 10, MandatoryUsage, 1),
    CompositeComponent(compPT, Some("Processing Type"), "MSH-11", 11, MandatoryUsage, 1),
    CompositeComponent(compVID, Some("Version Identifier"), "MSH-12", 12, MandatoryUsage, 1),
    ElementComponent(elemNM, None, "MSH-13", 13, OptionalUsage, 1),
    ElementComponent(elemST, None, "MSH-14", 14, OptionalUsage, 1),
    ElementComponent(elemID, None, "MSH-15", 15, OptionalUsage, 1),
    ElementComponent(elemID, None, "MSH-16", 16, OptionalUsage, 1),
    ElementComponent(elemID, None, "MSH-17", 17, OptionalUsage, 1),
    ElementComponent(elemID, None, "MSH-18", 18, OptionalUsage, 1),
    CompositeComponent(compCE.rewrite("MSH-19", convertEdiForm("HL7")), Some("Coded Element"), "MSH-19", 19, OptionalUsage, 1),
    ElementComponent(elemID, None, "MSH-20", 20, OptionalUsage, 1),
    CompositeComponent(compEI, Some("Entity Identifier"), "MSH-21", 21, OptionalUsage, 1)), Nil)
  val segSFT = new Segment("SFT", "Software Segment", List[SegmentComponent](
    CompositeComponent(compXON, Some("Extended Composite Name and Identification Number for Organizations"), "SFT-01", 1, MandatoryUsage, 1),
    ElementComponent(elemST, None, "SFT-02", 2, MandatoryUsage, 1),
    ElementComponent(elemST, None, "SFT-03", 3, MandatoryUsage, 1),
    ElementComponent(elemST, None, "SFT-04", 4, MandatoryUsage, 1),
    ElementComponent(elemTX, None, "SFT-05", 5, OptionalUsage, 1),
    CompositeComponent(compTS.rewrite("SFT-06", convertEdiForm("HL7")), Some("Time Stamp"), "SFT-06", 6, OptionalUsage, 1)), Nil)

  val ackVersion = EdiSchemaVersion(EdiSchema.HL7, null)
  val transACK = Structure("ACK", "ACK", None, Some(StructureSequence(false, List[StructureComponent](
    ReferenceComponent(segMSH, new DefinedPosition(0, "01"), MandatoryUsage, 1), ReferenceComponent(segSFT, new DefinedPosition(0, "02"), OptionalUsage, -1),
    ReferenceComponent(segMSA, new DefinedPosition(0, "03"), MandatoryUsage, 1), ReferenceComponent(segERR, new DefinedPosition(0, "04"), OptionalUsage, -1)))),
    None, None, ackVersion)

  val mshEncodingCharsKey = segMSH.components(0).key
  val mshControlKey = segMSH.components(8).key
  val mshStructureKey = segMSH.components(7).asInstanceOf[CompositeComponent].composite.components(2).key
  val mshVersionKey = segMSH.components(10).asInstanceOf[CompositeComponent].composite.components(0).key
  val mshDateTimeKey = segMSH.components(5).asInstanceOf[CompositeComponent].composite.components(0).key
  val mshSendingApplication = segMSH.components(1).asInstanceOf[CompositeComponent].composite
  val mshSendingFacility = segMSH.components(2).asInstanceOf[CompositeComponent].composite
  val mshReceivingApplication = segMSH.components(3).asInstanceOf[CompositeComponent].composite
  val mshReceivingFacility = segMSH.components(4).asInstanceOf[CompositeComponent].composite
}

object HL7Identity extends UtilityBase {

  import EdiSchema.Composite

  case class HierarchicDesignator(namespace: String, ident: String, idType: String)

  /** Interchange identity information used by HL7. All values present are matched against received interchanges.
    * @param application
    * @param facility
    */
  case class HL7IdentityInformation(application: HierarchicDesignator, facility: HierarchicDesignator)

  def buildHierarchicDesignator(strings: Array[String]) =
    HierarchicDesignator(valueOrNull(0, strings), valueOrNull(1, strings), valueOrNull(2, strings))

  def buildIdentityInformation(app: Composite, facility: Composite, map: ValueMap) = {
    val apphd = buildHierarchicDesignator(getStrings(app.components, map))
    val fachd = buildHierarchicDesignator(getStrings(facility.components, map))
    HL7IdentityInformation(apphd, fachd)
  }
}

object HL7Acknowledgment {

  trait Coded[K] {
    val code: K
  }

  def instanceMap[K, V <: Coded[K]](values: V*): Map[K, V] =
    values.toList.foldLeft(Map[K, V]())((acc, value) => acc + (value.code -> value))

  /** Acknowledgment codes (MSA-1 values). */
  sealed abstract class AcknowledgmentCode(val code: String, val text: String) extends Coded[String]
  case object AcknowledgedApplicationAccept extends AcknowledgmentCode("AA", "Application Accept")
  case object AcknowledgedApplicationError extends AcknowledgmentCode("AE", "Application Error")
  case object AcknowledgedApplicationReject extends AcknowledgmentCode("AR", "Application Reject")
  case object AcknowledgedCommitAccept extends AcknowledgmentCode("CA", "Commit Accept")
  case object AcknowledgedCommitError extends AcknowledgmentCode("CE", "Commit Error")
  case object AcknowledgedCommitReject extends AcknowledgmentCode("CR", "Commit Reject")
  val AcknowledgmentCodes = instanceMap[String, AcknowledgmentCode](AcknowledgedApplicationAccept,
    AcknowledgedApplicationError, AcknowledgedApplicationReject, AcknowledgedCommitAccept, AcknowledgedCommitError,
    AcknowledgedCommitReject)

  /** Error codes (ERR-3 values). */
  sealed abstract class ErrorCode(val code: String, val text: String) extends Coded[String]
  case object ErrorMessageAccepted extends ErrorCode("0", "Message accepted")
  case object ErrorSegmentSequence extends ErrorCode("100", "Segment sequence error")
  case object ErrorRequiredFieldMissing extends ErrorCode("101", "Required field missing")
  case object ErrorDataType extends ErrorCode("102", "Data type error")
  case object ErrorTableValue extends ErrorCode("103", "Table value not found")
  case object ErrorMessageType extends ErrorCode("200", "Unsupported message type")
  case object ErrorEventCode extends ErrorCode("201", "Unsupported event code")
  case object ErrorProcessingId extends ErrorCode("202", "Unsupported processing id")
  case object ErrorVersionId extends ErrorCode("203", "Unsupported version id")
  case object ErrorUnknownKey extends ErrorCode("204", "Unknown key identifier")
  case object ErrorDuplicateKey extends ErrorCode("205", "Duplicate key identifier")
  case object ErrorRecordLocked extends ErrorCode("206", "Application record locked ")
  case object ErrorApplicationError extends ErrorCode("207", "Application internal error")
  val ErrorCodes = instanceMap[String, ErrorCode](ErrorMessageAccepted, ErrorSegmentSequence, ErrorSegmentSequence,
    ErrorRequiredFieldMissing, ErrorDataType, ErrorTableValue, ErrorMessageType, ErrorEventCode, ErrorProcessingId,
    ErrorVersionId, ErrorUnknownKey, ErrorDuplicateKey, ErrorRecordLocked, ErrorApplicationError)

  /** Severity codes (ERR-4 values). */
  sealed abstract class SeverityCode(val code: String, val text: String) extends Coded[String]
  case object SeverityWarning extends SeverityCode("W", "Warning")
  case object SeverityInformation extends SeverityCode("I", "Information")
  case object SeverityError extends SeverityCode("E", "Error")
  val SeverityCodes = instanceMap[String, SeverityCode](SeverityWarning, SeverityInformation, SeverityError)
}