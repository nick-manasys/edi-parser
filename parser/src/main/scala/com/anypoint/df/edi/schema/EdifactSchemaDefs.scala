package com.anypoint.df.edi.schema

/** Base control definitions for EDIFACT EDI schemas. */
object EdifactSchemaDefs {
  import EdiSchema._
  import com.anypoint.df.edi.lexical.EdifactConstants.SyntaxVersion
  
  // transaction set properties
  val implementationConventionReference = "ImplementationConventionOverride"
  
  val interchangeStartSegment = "UNB"
  val interchangeEndSegment = "UNZ"

  val interHeadSyntaxIdentKey = "UNB0101"
  val interHeadSyntaxVersionKey = "UNB0102"
  val interHeadSyntaxCodeKey = "UNB0103"
  val interHeadSyntaxEncodingKey = "UNB0104"
  val interHeadSenderIdentKey = "UNB0201"
  val interHeadSenderQualKey = "UNB0202"
  val interHeadRecipientIdentKey = "UNB0301"
  val interHeadRecipientQualKey = "UNB0302"
  val interHeadDateKey = "UNB0401"
  val interHeadTimeKey = "UNB0402"
  val interHeadReferenceKey = "UNB05"
  val interHeadApplicationKey = "UNB07"
  val interHeadPriorityKey = "UNB08"
  val interHeadAckreqKey = "UNB09"
  val interHeadAgreementKey = "UNB10"
  val interHeadTestKey = "UNB11"

  val interTrailCountKey = "UNZ01"
  val interTrailReferenceKey = "UNZ02"

  val groupHeadMessageGroupKey = "UNG01"
  val groupHeadSenderIdentKey = "UNG0201"
  val groupHeadSenderQualKey = "UNG0202"
  val groupHeadRecipientIdentKey = "UNG0301"
  val groupHeadRecipientQualKey = "UNG0302"
  val groupHeadDateKey = "UNG0401"
  val groupHeadTimeKey = "UNG0402"
  val groupHeadReferenceKey = "UNG05"
  val groupHeadAgencyKey = "UNG06"
  val groupHeadMessageVersionKey = "UNG0701"
  val groupHeadMessageReleaseKey = "UNG0702"
  val groupHeadMessageAssignedKey = "UNG0703"

  val groupTrailCountKey = "UNE01"
  val groupTrailReferenceKey = "UNE02"

  val msgHeadReferenceKey = "UNH01"
  val msgHeadMessageTypeKey = "UNH0201"
  val msgHeadMessageVersionKey = "UNH0202"
  val msgHeadMessageReleaseKey = "UNH0203"
  val msgHeadMessageAgencyKey = "UNH0204"
  val msgHeadMessageAssignedKey = "UNH0205"
  val msgHeadMessageDirectoryKey = "UNH0206"
  val msgHeadMessageSubfunctionKey = "UNH0207"
  val msgHeadCommonAccessKey = "UNH03"
  val msgHeadStatusSequenceKey = "UNH0401"
  val msgHeadStatusFirstLastKey = "UNH0402"
  val msgHeadSubsetIdentKey = "UNH0501"
  val msgHeadSubsetVersionKey = "UNH0502"
  val msgHeadSubsetReleaseKey = "UNH0503"
  val msgHeadSubsetAgencyKey = "UNH0504"
  val msgHeadImplIdentKey = "UNH0601"
  val msgHeadImplVersionKey = "UNH0602"
  val msgHeadImplReleaseKey = "UNH0603"
  val msgHeadImplAgencyKey = "UNH0604"
  val msgHeadScenarioIdentKey = "UNH0701"
  val msgHeadScenarioVersionKey = "UNH0702"
  val msgHeadScenarioReleaseKey = "UNH0703"
  val msgHeadScenarioAgencyKey = "UNH0704"

  val msgTrailCountKey = "UNT01"
  val msgTrailReferenceKey = "UNT02"
  
  val sectionControlIdent = "UNS01"

  val versions = Map((SyntaxVersion.VERSION3 -> ControlV3Defs), (SyntaxVersion.VERSION4 -> ControlV4Defs))
}

/** Version-dependent control definitions for EDIFACT EDI schemas. */
trait EdifactVersionDefs {
  import EdiSchema._

  val segUNB: Segment
  val segUNE: Segment
  val segUNH: Segment
  val segUNT: Segment
  val segUNG: Segment
  val segUNS: Segment
  val segUNZ: Segment
  val segUCI: Segment
  val segUCF: Segment
  val segUCM: Segment
  val segUCS: Segment
  val segUCD: Segment

  val CONTRLsg1: GroupComponent
  val CONTRLsg2: GroupComponent
  val CONTRLsg3: GroupComponent
  val CONTRLsg4: GroupComponent
  val CONTRLsg5: GroupComponent
  val transCONTRL: Transaction

  val interHeadSyntax: Composite
  val interHeadSender: Composite
  val interHeadRecipient: Composite
}

object ControlV3Defs extends EdifactVersionDefs {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.EdiConstants.DataType
  import com.anypoint.df.edi.lexical.EdiConstants.DataType._

  // control segments schema (generated code, with hand modifications for versions and conversion to date/time/integer data types)
  val elem0020 = Element("0020", "Interchange control reference", ALPHANUMERIC, 0, 14)
  val elem0048 = Element("0048", "Functional group reference number", ALPHANUMERIC, 0, 14)
  val compS001 = Composite("S001", "SYNTAX IDENTIFIER", List[SegmentComponent](
    ElementComponent(Element("0001", "SYNTAX IDENTIFIER", ALPHA, 4, 4), Some("Syntax identifier"), "UNB0101", 10, MandatoryUsage, 1),
    ElementComponent(Element("0002", "SYNTAX VERSION NUMBER", ALPHANUMERIC, 1, 1), Some("Syntax version number"), "UNB0102", 20, MandatoryUsage, 1)), Nil)
  val compS002 = Composite("S002", "INTERCHANGE SENDER", List[SegmentComponent](
    ElementComponent(Element("0004", "Sender identification", ALPHANUMERIC, 0, 35), None, "UNB0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0007", "Identification code qualifier", ALPHANUMERIC, 0, 4), None, "UNB0202", 20, ConditionalUsage, 1),
    ElementComponent(Element("0014", "Routing address", ALPHANUMERIC, 0, 14), Some("Routing address"), "UNB0203", 30, ConditionalUsage, 1)), Nil)
  val compS003 = Composite("S003", "INTERCHANGE RECIPIENT", List[SegmentComponent](
    ElementComponent(Element("0010", "Recipient identification", ALPHANUMERIC, 0, 35), None, "UNB0301", 10, MandatoryUsage, 1),
    ElementComponent(Element("0007", "Identification code qualifier", ALPHANUMERIC, 0, 4), None, "UNB0302", 20, ConditionalUsage, 1),
    ElementComponent(Element("0014", "Routing address", ALPHANUMERIC, 0, 14), Some("Routing address"), "UNB0303", 30, ConditionalUsage, 1)), Nil)
  val compS004_0 = Composite("S004", "DATE AND TIME OF PREPARATION", List[SegmentComponent](
    ElementComponent(Element("0017", "DATE", INTEGER, 6, 6), Some("Date"), "UNB0401", 10, MandatoryUsage, 1),
    ElementComponent(Element("0019", "TIME", INTEGER, 4, 4), Some("Time"), "UNB0402", 20, MandatoryUsage, 1)), Nil)
  val compS004_1 = Composite("S004", "DATE AND TIME OF PREPARATION", List[SegmentComponent](
    ElementComponent(Element("0017", "DATE", INTEGER, 6, 6), Some("Date"), "UNG0401", 10, MandatoryUsage, 1),
    ElementComponent(Element("0019", "TIME", INTEGER, 4, 4), Some("Time"), "UNG0402", 20, MandatoryUsage, 1)), Nil)
  val compS005 = Composite("S005", "RECIPIENT REFERENCE/PASSWORD DETAILS", List[SegmentComponent](
    ElementComponent(Element("0022", "RECIPIENT REFERENCE/PASSWORD", ALPHANUMERIC, 0, 14), Some("Recipient reference/password"), "UNB0601", 10, MandatoryUsage, 1),
    ElementComponent(Element("0025", "RECIPIENT REFERENCE/PASSWORD QUALIFIER", ALPHANUMERIC, 2, 2), Some("Recipient reference/password qualifier"), "UNB0602", 20, ConditionalUsage, 1)), Nil)
  val compS006 = Composite("S006", "APPLICATION SENDER'S IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0040", "Application sender identification", ALPHANUMERIC, 0, 35), Some("Sender identification"), "UNG0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0007", "Identification code qualifier", ALPHANUMERIC, 0, 4), Some("Sender identification qualifier"), "UNG0202", 20, ConditionalUsage, 1)), Nil)
  val compS007 = Composite("S007", "APPLICATION RECIPIENTS IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0044", "Application recipient's identification", ALPHANUMERIC, 0, 35), Some("Recipient's identification"), "UNG0301", 10, MandatoryUsage, 1),
    ElementComponent(Element("0007", "Identification code qualifier", ALPHANUMERIC, 0, 4), Some("Recipients identification qualifier"), "UNG0302", 20, ConditionalUsage, 1)), Nil)
  val compS008 = Composite("S008", "MESSAGE VERSION", List[SegmentComponent](
    ElementComponent(Element("0052", "Message version number", ALPHANUMERIC, 0, 3), None, "UNG0701", 10, MandatoryUsage, 1),
    ElementComponent(Element("0054", "Message release number", ALPHANUMERIC, 0, 3), None, "UNG0702", 20, MandatoryUsage, 1),
    ElementComponent(Element("0057", "Association assigned code", ALPHANUMERIC, 0, 6), None, "UNG0703", 30, ConditionalUsage, 1)), Nil)
  val segUNB = Segment("UNB", "INTERCHANGE HEADER", List[SegmentComponent](
    CompositeComponent(compS001, Some("SYNTAX IDENTIFIER"), "UNB01", 10, MandatoryUsage, 1),
    CompositeComponent(compS002, Some("INTERCHANGE SENDER"), "UNB02", 20, MandatoryUsage, 1),
    CompositeComponent(compS003, Some("INTERCHANGE RECIPIENT"), "UNB03", 30, MandatoryUsage, 1),
    CompositeComponent(compS004_0, Some("DATE AND TIME OF PREPARATION"), "UNB04", 40, MandatoryUsage, 1),
    ElementComponent(elem0020, Some("INTERCHANGE CONTROL REFERENCE"), "UNB05", 50, MandatoryUsage, 1),
    CompositeComponent(compS005, Some("RECIPIENT REFERENCE/PASSWORD DETAILS"), "UNB06", 60, ConditionalUsage, 1),
    ElementComponent(Element("0026", "APPLICATION REFERENCE", ALPHANUMERIC, 0, 14), None, "UNB07", 70, ConditionalUsage, 1),
    ElementComponent(Element("0029", "PROCESSING PRIORITY CODE", ALPHA, 1, 1), None, "UNB08", 80, ConditionalUsage, 1),
    ElementComponent(Element("0031", "ACKNOWLEDGEMENT REQUEST", INTEGER, 1, 1), None, "UNB09", 90, ConditionalUsage, 1),
    ElementComponent(Element("0032", "COMMUNICATIONS AGREEMENT IDENTIFIER", ALPHANUMERIC, 0, 35), None, "UNB10", 100, ConditionalUsage, 1),
    ElementComponent(Element("0035", "TEST INDICATOR", INTEGER, 1, 1), None, "UNB11", 110, ConditionalUsage, 1)), Nil)
  val segUNE = Segment("UNE", "GROUP TRAILER", List[SegmentComponent](
    ElementComponent(Element("0060", "Group Control Count", INTEGER, 0, 6), Some("GROUP CONTROL COUNT"), "UNE01", 10, MandatoryUsage, 1),
    ElementComponent(elem0048, Some("GROUP REFERENCE NUMBER"), "UNE02", 20, MandatoryUsage, 1)), Nil)
  val segUNG = Segment("UNG", "GROUP HEADER", List[SegmentComponent](
    ElementComponent(Element("0038", "MESSAGE GROUP IDENTIFICATION", ALPHANUMERIC, 0, 6), None, "UNG01", 10, ConditionalUsage, 1),
    CompositeComponent(compS006, Some("APPLICATION SENDER IDENTIFICATION"), "UNG02", 20, ConditionalUsage, 1),
    CompositeComponent(compS007, Some("APPLICATION RECIPIENT IDENTIFICATION"), "UNG03", 30, ConditionalUsage, 1),
    CompositeComponent(compS004_1, Some("DATE AND TIME OF PREPARATION"), "UNG04", 40, ConditionalUsage, 1),
    ElementComponent(elem0048, Some("GROUP REFERENCE NUMBER"), "UNG05", 50, MandatoryUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("CONTROLLING AGENCY, CODED"), "UNG06", 60, ConditionalUsage, 1),
    CompositeComponent(compS008, Some("MESSAGE VERSION"), "UNG07", 70, ConditionalUsage, 1),
    ElementComponent(Element("0058", "APPLICATION PASSWORD", ALPHANUMERIC, 0, 14), None, "UNG08", 80, ConditionalUsage, 1)), Nil)
  val segUNS = Segment("UNS", "SECTION CONTROL", List[SegmentComponent](
    ElementComponent(Element("0081", "SECTION IDENTIFICATION", ALPHA, 1, 1), None, "UNS01", 10, MandatoryUsage, 1)), Nil)
  val segUNZ = Segment("UNZ", "INTERCHANGE TRAILER", List[SegmentComponent](
    ElementComponent(Element("0036", "INTERCHANGE CONTROL COUNT", INTEGER, 0, 6), None, "UNZ01", 10, MandatoryUsage, 1),
    ElementComponent(elem0020, Some("INTERCHANGE CONTROL REFERENCE"), "UNZ02", 20, MandatoryUsage, 1)), Nil)

  // defined values
  val interHeadSyntax = compS001
  val interHeadSender = compS002
  val interHeadRecipient = compS003

  // v4 CONTRL acknowledgment schema (generated code, with modifications to separate out the groups within CONTRL)
  val elem0013 = Element("0013", "Service segment tag, coded", ALPHA, 3, 3)
  val elem0062 = Element("0062", "Message reference number", ALPHANUMERIC, 0, 14)
  val elem0083 = Element("0083", "Action, coded", ALPHANUMERIC, 0, 3)
  val elem0085 = Element("0085", "Syntax error, coded", ALPHANUMERIC, 0, 3)
  val elem0138 = Element("0138", "SECURITY SEGMENT POSITION", NUMBER, 0, 6)
  val elem0534 = Element("0534", "SECURITY REFERENCE NUMBER", ALPHANUMERIC, 0, 14)

  val compS009_0 = Composite("S009", "MESSAGE IDENTIFIER", List[SegmentComponent](
    ElementComponent(Element("0065", "Message type", ALPHANUMERIC, 0, 6), None, "UCM0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0052", "Message version number", ALPHANUMERIC, 0, 3), None, "UCM0202", 20, MandatoryUsage, 1),
    ElementComponent(Element("0054", "Message release number", ALPHANUMERIC, 0, 3), None, "UCM0203", 30, MandatoryUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UCM0204", 40, MandatoryUsage, 1),
    ElementComponent(Element("0057", "Association assigned code", ALPHANUMERIC, 0, 6), None, "UCM0205", 50, ConditionalUsage, 1)), Nil)
  val compS009_1 = Composite("S009", "MESSAGE IDENTIFIER", List[SegmentComponent](
    ElementComponent(Element("0065", "Message type", ALPHANUMERIC, 0, 6), None, "UNH0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0052", "Message version number", ALPHANUMERIC, 0, 3), None, "UNH0202", 20, MandatoryUsage, 1),
    ElementComponent(Element("0054", "Message release number", ALPHANUMERIC, 0, 3), None, "UNH0203", 30, MandatoryUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UNH0204", 40, MandatoryUsage, 1),
    ElementComponent(Element("0057", "Association assigned code", ALPHANUMERIC, 0, 6), None, "UNH0205", 50, ConditionalUsage, 1)), Nil)
  val compS010 = Composite("S010", "STATUS OF THE TRANSFER", List[SegmentComponent](
    ElementComponent(Element("0070", "Sequence of transfers", INTEGER, 0, 2), None, "UNH0401", 10, MandatoryUsage, 1),
    ElementComponent(Element("0073", "First and last transfer", ALPHA, 1, 1), None, "UNH0402", 20, ConditionalUsage, 1)), Nil)
  val compS011_0 = Composite("S011", "DATA ELEMENT IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0098", "Erroneous data element position in segment.", INTEGER, 0, 3), Some("Erroneous data element position in segment"), "UCF0701", 10, MandatoryUsage, 1),
    ElementComponent(Element("0104", "Erroneous component data element position", INTEGER, 0, 3), None, "UCF0702", 20, ConditionalUsage, 1),
    ElementComponent(Element("0136", "ERRONEOUS DATA ELEMENT OCCURRENCE", INTEGER, 0, 6), Some("Erroneous data element occurrence"), "UCF0703", 30, ConditionalUsage, 1)), Nil)
  val compS011_1 = Composite("S011", "DATA ELEMENT IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0098", "Erroneous data element position in segment.", INTEGER, 0, 3), Some("Erroneous data element position in segment"), "UCM0601", 10, MandatoryUsage, 1),
    ElementComponent(Element("0104", "Erroneous component data element position", INTEGER, 0, 3), None, "UCM0602", 20, ConditionalUsage, 1),
    ElementComponent(Element("0136", "ERRONEOUS DATA ELEMENT OCCURRENCE", INTEGER, 0, 6), Some("Erroneous data element occurrence"), "UCM0603", 30, ConditionalUsage, 1)), Nil)
  val compS011_2 = Composite("S011", "DATA ELEMENT IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0098", "Erroneous data element position in segment.", INTEGER, 0, 3), Some("Erroneous data element position in segment"), "UCD0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0104", "Erroneous component data element position", INTEGER, 0, 3), None, "UCD0202", 20, ConditionalUsage, 1),
    ElementComponent(Element("0136", "ERRONEOUS DATA ELEMENT OCCURRENCE", INTEGER, 0, 6), Some("Erroneous data element occurrence"), "UCD0203", 30, ConditionalUsage, 1)), Nil)
  val compS011_3 = Composite("S011", "DATA ELEMENT IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0098", "Erroneous data element position in segment.", INTEGER, 0, 3), Some("Erroneous data element position in segment"), "UCI0701", 10, MandatoryUsage, 1),
    ElementComponent(Element("0104", "Erroneous component data element position", INTEGER, 0, 3), None, "UCI0702", 20, ConditionalUsage, 1),
    ElementComponent(Element("0136", "ERRONEOUS DATA ELEMENT OCCURRENCE", INTEGER, 0, 6), Some("Erroneous data element occurrence"), "UCI0703", 30, ConditionalUsage, 1)), Nil)
  val compS016 = Composite("S016", "MESSAGE SUBSET IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0115", "MESSAGE SUBSET IDENTIFICATION", ALPHANUMERIC, 0, 14), Some("Message subset identification"), "UNH0501", 10, MandatoryUsage, 1),
    ElementComponent(Element("0116", "MESSAGE SUBSET VERSION NUMBER", ALPHANUMERIC, 0, 3), Some("Message subset version number"), "UNH0502", 20, ConditionalUsage, 1),
    ElementComponent(Element("0118", "MESSAGE SUBSET RELEASE NUMBER", ALPHANUMERIC, 0, 3), Some("Message subset release number"), "UNH0503", 30, ConditionalUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UNH0504", 40, ConditionalUsage, 1)), Nil)
  val compS017 = Composite("S017", "MESSAGE IMPLEMENTATION GUIDELINE IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0121", "MESSAGE IMPLEMENTATION GUIDELINE IDENTIFICATION", ALPHANUMERIC, 0, 14), Some("Message implementation guideline ident"), "UNH0601", 10, MandatoryUsage, 1),
    ElementComponent(Element("0122", "MESSAGE IMPLEMENTATION GUIDELINE VERSION NUMBER", ALPHANUMERIC, 0, 3), Some("Message implementation guideline version"), "UNH0602", 20, ConditionalUsage, 1),
    ElementComponent(Element("0124", "MESSAGE IMPLEMENTATION GUIDELINE RELEASE NUMBER", ALPHANUMERIC, 0, 3), Some("Message implementation guideline release"), "UNH0603", 30, ConditionalUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UNH0604", 40, ConditionalUsage, 1)), Nil)
  val compS018 = Composite("S018", "SCENARIO IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0127", "SCENARIO IDENTIFICATION", ALPHANUMERIC, 0, 14), Some("Scenario identification"), "UNH0701", 10, MandatoryUsage, 1),
    ElementComponent(Element("0128", "SCENARIO VERSION NUMBER", ALPHANUMERIC, 0, 3), Some("Scenario version number"), "UNH0702", 20, ConditionalUsage, 1),
    ElementComponent(Element("0130", "SCENARIO RELEASE NUMBER", ALPHANUMERIC, 0, 3), Some("Scenario release number"), "UNH0703", 30, ConditionalUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UNH0704", 40, ConditionalUsage, 1)), Nil)
  val compS020 = Composite("S020", "REFERENCE IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0813", "REFERENCE QUALIFIER", ALPHANUMERIC, 0, 3), Some("Reference qualifier"), "S02001", 10, MandatoryUsage, 1),
    ElementComponent(Element("0802", "REFERENCE IDENTIFICATION NUMBER", ALPHANUMERIC, 0, 35), Some("Reference identification number"), "S02002", 20, MandatoryUsage, 1)), Nil)
  val segUCD = Segment("UCD", "DATA ELEMENT ERROR INDICATION", List[SegmentComponent](
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCD01", 10, MandatoryUsage, 1),
    CompositeComponent(compS011_2, Some("DATA ELEMENT IDENTIFICATION"), "UCD02", 20, MandatoryUsage, 1)), Nil)
  val segUCF = Segment("UCF", "FUNCTIONAL GROUP RESPONSE", List[SegmentComponent](
    ElementComponent(Element("0048", "Functional group reference number", ALPHANUMERIC, 0, 14), Some("FUNCTIONAL GROUP REFERENCE NUMBER"), "UCF01", 10, MandatoryUsage, 1),
    CompositeComponent(compS006, Some("APPLICATION SENDER'S IDENTIFICATION"), "UCF02", 20, MandatoryUsage, 1),
    CompositeComponent(compS007, Some("APPLICATION RECIPIENTS IDENTIFICATION"), "UCF03", 30, MandatoryUsage, 1),
    ElementComponent(elem0083, Some("ACTION, CODED"), "UCF04", 40, MandatoryUsage, 1),
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCF05", 50, ConditionalUsage, 1),
    ElementComponent(Element("0135", "SERVICE SEGMENT TAG, CODED", ALPHANUMERIC, 0, 3), None, "UCF06", 60, ConditionalUsage, 1),
    CompositeComponent(compS011_0, Some("DATA ELEMENT IDENTIFICATION"), "UCF07", 70, ConditionalUsage, 1)), Nil)
  val segUCI = Segment("UCI", "INTERCHANGE RESPONSE", List[SegmentComponent](
    ElementComponent(Element("0020", "Interchange control reference", ALPHANUMERIC, 0, 14), Some("INTERCHANGE CONTROL REFERENCE"), "UCI01", 10, MandatoryUsage, 1),
    CompositeComponent(compS002, Some("INTERCHANGE SENDER"), "UCI02", 20, MandatoryUsage, 1),
    CompositeComponent(compS003, Some("INTERCHANGE RECIPIENT"), "UCI03", 30, MandatoryUsage, 1),
    ElementComponent(elem0083, Some("ACTION, CODED"), "UCI04", 40, MandatoryUsage, 1),
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCI05", 50, ConditionalUsage, 1),
    ElementComponent(elem0013, Some("SERVICE SEGMENT TAG, CODED"), "UCI06", 60, ConditionalUsage, 1),
    CompositeComponent(compS011_3, Some("DATA ELEMENT IDENTIFICATION"), "UCI07", 70, ConditionalUsage, 1)), Nil)
  val segUCM = Segment("UCM", "MESSAGE/PACKAGE RESPONSE", List[SegmentComponent](
    ElementComponent(elem0062, Some("MESSAGE REFERENCE NUMBER"), "UCM01", 10, ConditionalUsage, 1),
    CompositeComponent(compS009_0, Some("MESSAGE IDENTIFIER"), "UCM02", 20, ConditionalUsage, 1),
    ElementComponent(elem0083, Some("ACTION, CODED"), "UCM03", 30, MandatoryUsage, 1),
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCM04", 40, ConditionalUsage, 1),
    ElementComponent(elem0013, Some("SERVICE SEGMENT TAG, CODED"), "UCM05", 50, ConditionalUsage, 1),
    CompositeComponent(compS011_1, Some("DATA ELEMENT IDENTIFICATION"), "UCM06", 60, ConditionalUsage, 1)), Nil)
  val segUCS = Segment("UCS", "SEGMENT ERROR INDICATION", List[SegmentComponent](
    ElementComponent(Element("0096", "Segment position in message", INTEGER, 0, 6), Some("SEGMENT POSITION IN MESSAGE BODY"), "UCS01", 10, MandatoryUsage, 1),
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCS02", 20, ConditionalUsage, 1)), Nil)
  val segUNH = Segment("UNH", "MESSAGE HEADER", List[SegmentComponent](
    ElementComponent(elem0062, Some("MESSAGE REFERENCE NUMBER"), "UNH01", 10, MandatoryUsage, 1),
    CompositeComponent(compS009_1, Some("MESSAGE IDENTIFIER"), "UNH02", 20, MandatoryUsage, 1),
    ElementComponent(Element("0068", "Common access reference", ALPHANUMERIC, 0, 35), Some("COMMON ACCESS REFERENCE"), "UNH03", 30, ConditionalUsage, 1),
    CompositeComponent(compS010, Some("STATUS OF THE TRANSFER"), "UNH04", 40, ConditionalUsage, 1)), Nil)
  val segUNT = Segment("UNT", "MESSAGE TRAILER", List[SegmentComponent](
    ElementComponent(Element("0074", "Number of segments in a message", INTEGER, 0, 6), Some("NUMBER OF SEGMENTS IN A MESSAGE"), "UNT01", 10, MandatoryUsage, 1),
    ElementComponent(elem0062, Some("MESSAGE REFERENCE NUMBER"), "UNT02", 20, MandatoryUsage, 1)), Nil)

  val CONTRLsg5 = GroupComponent("Segment group 5", ConditionalUsage, 999, List[TransactionComponent](
    ReferenceComponent(segUCS, SegmentPosition(0, "0130"), MandatoryUsage, 1), ReferenceComponent(segUCD, SegmentPosition(0, "0140"), ConditionalUsage, 99)), None, Nil)
  val CONTRLsg4 = GroupComponent("Segment group 4", ConditionalUsage, 999999, List[TransactionComponent](
    ReferenceComponent(segUCM, SegmentPosition(0, "0110"), MandatoryUsage, 1),
    CONTRLsg5), None, Nil)
  val CONTRLsg3 = GroupComponent("Segment group 3", ConditionalUsage, 999999, List[TransactionComponent](
    ReferenceComponent(segUCF, SegmentPosition(0, "0090"), MandatoryUsage, 1),
    CONTRLsg4), None, Nil)
  val CONTRLsg2 = GroupComponent("Segment group 2", ConditionalUsage, 999, List[TransactionComponent](
    ReferenceComponent(segUCS, SegmentPosition(0, "0060"), MandatoryUsage, 1), ReferenceComponent(segUCD, SegmentPosition(0, "0070"), ConditionalUsage, 99)), None, Nil)
  val CONTRLsg1 = GroupComponent("Segment group 1", ConditionalUsage, 999999, List[TransactionComponent](
    ReferenceComponent(segUCM, SegmentPosition(0, "0040"), MandatoryUsage, 1),
    CONTRLsg2), None, Nil)

  val transCONTRL = Transaction("CONTRL", "Application error and acknowledgement message", None,
    List[TransactionComponent](
      ReferenceComponent(segUNH, SegmentPosition(0, "0010"), MandatoryUsage, 1),
      ReferenceComponent(segUCI, SegmentPosition(0, "0020"), MandatoryUsage, 1),
      CONTRLsg1,
      CONTRLsg3,
      ReferenceComponent(segUNT, SegmentPosition(0, "0150"), MandatoryUsage, 1)),
    List[TransactionComponent](), List[TransactionComponent]())
}

object ControlV4Defs extends EdifactVersionDefs {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.EdiConstants.DataType
  import com.anypoint.df.edi.lexical.EdiConstants.DataType._

  // control segments schema (generated code, with hand modifications for versions and conversion to date/time/integer data types)
  val elem0020 = Element("0020", "Interchange control reference", ALPHANUMERIC, 0, 14)
  val elem0048 = Element("0048", "Functional group reference number", ALPHANUMERIC, 0, 14)
  val compS001 = Composite("S001", "SYNTAX IDENTIFIER", List[SegmentComponent](
    ElementComponent(Element("0001", "SYNTAX IDENTIFIER", ALPHA, 4, 4), Some("Syntax identifier"), "UNB0101", 10, MandatoryUsage, 1),
    ElementComponent(Element("0002", "SYNTAX VERSION NUMBER", ALPHANUMERIC, 1, 1), Some("Syntax version number"), "UNB0102", 20, MandatoryUsage, 1),
    ElementComponent(Element("0080", "SERVICE CODE LIST DIRECTORY VERSION NUMBER", ALPHANUMERIC, 0, 6), Some("Service code list directory version number"), "UNB0103", 30, ConditionalUsage, 1),
    ElementComponent(Element("0133", "CHARACTER ENCODING, CODED", ALPHANUMERIC, 0, 3), Some("Character encoding, coded"), "UNB0104", 40, ConditionalUsage, 1)), Nil)
  val compS002 = Composite("S002", "INTERCHANGE SENDER", List[SegmentComponent](
    ElementComponent(Element("0004", "Sender identification", ALPHANUMERIC, 0, 35), None, "UNB0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0007", "Identification code qualifier", ALPHANUMERIC, 0, 4), None, "UNB0202", 20, ConditionalUsage, 1),
    ElementComponent(Element("0008", "INTERCHANGE SENDER INTERNAL IDENTIFICATION", ALPHANUMERIC, 0, 35), Some("Interchange sender internal identification"), "UNB0203", 30, ConditionalUsage, 1),
    ElementComponent(Element("0042", "INTERCHANGE SENDER INTERNAL SUB-IDENTIFICATION", ALPHANUMERIC, 0, 35), Some("Interchange sender internal sub-identification"), "UNB0204", 40, ConditionalUsage, 1)), Nil)
  val compS003 = Composite("S003", "INTERCHANGE RECIPIENT", List[SegmentComponent](
    ElementComponent(Element("0010", "Recipient identification", ALPHANUMERIC, 0, 35), None, "UNB0301", 10, MandatoryUsage, 1),
    ElementComponent(Element("0007", "Identification code qualifier", ALPHANUMERIC, 0, 4), None, "UNB0302", 20, ConditionalUsage, 1),
    ElementComponent(Element("0014", "INTERCHANGE RECIPIENT INTERNAL IDENTIFICATION", ALPHANUMERIC, 0, 35), Some("Interchange recipient internal identification"), "UNB0303", 30, ConditionalUsage, 1),
    ElementComponent(Element("0046", "INTERCHANGE RECIPIENT INTERNAL SUB-IDENTIFICATION", ALPHANUMERIC, 0, 35), Some("Interchange recipient internal sub-identification"), "UNB0304", 40, ConditionalUsage, 1)), Nil)
  val compS004_0 = Composite("S004", "DATE AND TIME OF PREPARATION", List[SegmentComponent](
    ElementComponent(Element("0017", "DATE", INTEGER, 8, 8), Some("Date"), "UNB0401", 10, MandatoryUsage, 1),
    ElementComponent(Element("0019", "TIME", INTEGER, 4, 4), Some("Time"), "UNB0402", 20, MandatoryUsage, 1)), Nil)
  val compS004_1 = Composite("S004", "DATE AND TIME OF PREPARATION", List[SegmentComponent](
    ElementComponent(Element("0017", "DATE", INTEGER, 8, 8), Some("Date"), "UNG0401", 10, MandatoryUsage, 1),
    ElementComponent(Element("0019", "TIME", INTEGER, 4, 4), Some("Time"), "UNG0402", 20, MandatoryUsage, 1)), Nil)
  val compS005 = Composite("S005", "RECIPIENT REFERENCE/PASSWORD DETAILS", List[SegmentComponent](
    ElementComponent(Element("0022", "RECIPIENT REFERENCE/PASSWORD", ALPHANUMERIC, 0, 14), Some("Recipient reference/password"), "UNB0601", 10, MandatoryUsage, 1),
    ElementComponent(Element("0025", "RECIPIENT REFERENCE/PASSWORD QUALIFIER", ALPHANUMERIC, 2, 2), Some("Recipient reference/password qualifier"), "UNB0602", 20, ConditionalUsage, 1)), Nil)
  val compS006 = Composite("S006", "APPLICATION SENDER'S IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0040", "Application sender identification", ALPHANUMERIC, 0, 35), Some("Sender identification"), "UNG0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0007", "Identification code qualifier", ALPHANUMERIC, 0, 4), Some("Sender identification qualifier"), "UNG0202", 20, ConditionalUsage, 1)), Nil)
  val compS007 = Composite("S007", "APPLICATION RECIPIENTS IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0044", "Application recipient's identification", ALPHANUMERIC, 0, 35), Some("Recipient's identification"), "UNG0301", 10, MandatoryUsage, 1),
    ElementComponent(Element("0007", "Identification code qualifier", ALPHANUMERIC, 0, 4), Some("Recipients identification qualifier"), "UNG0302", 20, ConditionalUsage, 1)), Nil)
  val compS008 = Composite("S008", "MESSAGE VERSION", List[SegmentComponent](
    ElementComponent(Element("0052", "Message version number", ALPHANUMERIC, 0, 3), None, "UNG0701", 10, MandatoryUsage, 1),
    ElementComponent(Element("0054", "Message release number", ALPHANUMERIC, 0, 3), None, "UNG0702", 20, MandatoryUsage, 1),
    ElementComponent(Element("0057", "Association assigned code", ALPHANUMERIC, 0, 6), None, "UNG0703", 30, ConditionalUsage, 1)), Nil)
  val segUNB = Segment("UNB", "INTERCHANGE HEADER", List[SegmentComponent](
    CompositeComponent(compS001, Some("SYNTAX IDENTIFIER"), "UNB01", 10, MandatoryUsage, 1),
    CompositeComponent(compS002, Some("INTERCHANGE SENDER"), "UNB02", 20, MandatoryUsage, 1),
    CompositeComponent(compS003, Some("INTERCHANGE RECIPIENT"), "UNB03", 30, MandatoryUsage, 1),
    CompositeComponent(compS004_0, Some("DATE AND TIME OF PREPARATION"), "UNB04", 40, MandatoryUsage, 1),
    ElementComponent(elem0020, Some("INTERCHANGE CONTROL REFERENCE"), "UNB05", 50, MandatoryUsage, 1),
    CompositeComponent(compS005, Some("RECIPIENT REFERENCE/PASSWORD DETAILS"), "UNB06", 60, ConditionalUsage, 1),
    ElementComponent(Element("0026", "APPLICATION REFERENCE", ALPHANUMERIC, 0, 14), None, "UNB07", 70, ConditionalUsage, 1),
    ElementComponent(Element("0029", "PROCESSING PRIORITY CODE", ALPHA, 1, 1), None, "UNB08", 80, ConditionalUsage, 1),
    ElementComponent(Element("0031", "ACKNOWLEDGEMENT REQUEST", INTEGER, 1, 1), None, "UNB09", 90, ConditionalUsage, 1),
    ElementComponent(Element("0032", "INTERCHANGE AGREEMENT IDENTIFIER", ALPHANUMERIC, 0, 35), None, "UNB10", 100, ConditionalUsage, 1),
    ElementComponent(Element("0035", "TEST INDICATOR", INTEGER, 1, 1), None, "UNB11", 110, ConditionalUsage, 1)), Nil)
  val segUNE = Segment("UNE", "GROUP TRAILER", List[SegmentComponent](
    ElementComponent(Element("0060", "Group Control Count", INTEGER, 0, 6), Some("GROUP CONTROL COUNT"), "UNE01", 10, MandatoryUsage, 1),
    ElementComponent(elem0048, Some("GROUP REFERENCE NUMBER"), "UNE02", 20, MandatoryUsage, 1)), Nil)
  val segUNG = Segment("UNG", "GROUP HEADER", List[SegmentComponent](
    ElementComponent(Element("0038", "MESSAGE GROUP IDENTIFICATION", ALPHANUMERIC, 0, 6), None, "UNG01", 10, ConditionalUsage, 1),
    CompositeComponent(compS006, Some("APPLICATION SENDER IDENTIFICATION"), "UNG02", 20, ConditionalUsage, 1),
    CompositeComponent(compS007, Some("APPLICATION RECIPIENT IDENTIFICATION"), "UNG03", 30, ConditionalUsage, 1),
    CompositeComponent(compS004_1, Some("DATE AND TIME OF PREPARATION"), "UNG04", 40, ConditionalUsage, 1),
    ElementComponent(elem0048, Some("GROUP REFERENCE NUMBER"), "UNG05", 50, MandatoryUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("CONTROLLING AGENCY, CODED"), "UNG06", 60, ConditionalUsage, 1),
    CompositeComponent(compS008, Some("MESSAGE VERSION"), "UNG07", 70, ConditionalUsage, 1),
    ElementComponent(Element("0058", "APPLICATION PASSWORD", ALPHANUMERIC, 0, 14), None, "UNG08", 80, ConditionalUsage, 1)), Nil)
  val segUNS = Segment("UNS", "SECTION CONTROL", List[SegmentComponent](
    ElementComponent(Element("0081", "SECTION IDENTIFICATION", ALPHA, 1, 1), None, "UNS01", 10, MandatoryUsage, 1)), Nil)
  val segUNZ = Segment("UNZ", "INTERCHANGE TRAILER", List[SegmentComponent](
    ElementComponent(Element("0036", "INTERCHANGE CONTROL COUNT", INTEGER, 0, 6), None, "UNZ01", 10, MandatoryUsage, 1),
    ElementComponent(elem0020, Some("INTERCHANGE CONTROL REFERENCE"), "UNZ02", 20, MandatoryUsage, 1)), Nil)

  // defined values
  val interHeadSyntax = compS001
  val interHeadSender = compS002
  val interHeadRecipient = compS003

  // v4 CONTRL acknowledgment schema (generated code, with modifications to separate out the groups within CONTRL)
  val elem0013 = Element("0013", "Service segment tag, coded", ALPHA, 3, 3)
  val elem0062 = Element("0062", "Message reference number", ALPHANUMERIC, 0, 14)
  val elem0083 = Element("0083", "Action, coded", ALPHANUMERIC, 0, 3)
  val elem0085 = Element("0085", "Syntax error, coded", ALPHANUMERIC, 0, 3)
  val elem0138 = Element("0138", "SECURITY SEGMENT POSITION", INTEGER, 0, 6)
  val elem0534 = Element("0534", "SECURITY REFERENCE NUMBER", ALPHANUMERIC, 0, 14)

  val compS009_0 = Composite("S009", "MESSAGE IDENTIFIER", List[SegmentComponent](
    ElementComponent(Element("0065", "Message type", ALPHANUMERIC, 0, 6), None, "UCM0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0052", "Message version number", ALPHANUMERIC, 0, 3), None, "UCM0202", 20, MandatoryUsage, 1),
    ElementComponent(Element("0054", "Message release number", ALPHANUMERIC, 0, 3), None, "UCM0203", 30, MandatoryUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UCM0204", 40, MandatoryUsage, 1),
    ElementComponent(Element("0057", "Association assigned code", ALPHANUMERIC, 0, 6), None, "UCM0205", 50, ConditionalUsage, 1),
    ElementComponent(Element("0110", "CODE LIST DIRECTORY VERSION NUMBER", ALPHANUMERIC, 0, 6), Some("Code list directory version number"), "UCM0206", 60, ConditionalUsage, 1),
    ElementComponent(Element("0113", "MESSAGE TYPE SUB-FUNCTION IDENTIFICATION", ALPHANUMERIC, 0, 6), Some("Message type sub-function identification"), "UCM0207", 70, ConditionalUsage, 1)), Nil)
  val compS009_1 = Composite("S009", "MESSAGE IDENTIFIER", List[SegmentComponent](
    ElementComponent(Element("0065", "Message type", ALPHANUMERIC, 0, 6), None, "UNH0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0052", "Message version number", ALPHANUMERIC, 0, 3), None, "UNH0202", 20, MandatoryUsage, 1),
    ElementComponent(Element("0054", "Message release number", ALPHANUMERIC, 0, 3), None, "UNH0203", 30, MandatoryUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UNH0204", 40, MandatoryUsage, 1),
    ElementComponent(Element("0057", "Association assigned code", ALPHANUMERIC, 0, 6), None, "UNH0205", 50, ConditionalUsage, 1),
    ElementComponent(Element("0110", "CODE LIST DIRECTORY VERSION NUMBER", ALPHANUMERIC, 0, 6), Some("Code list directory version number"), "UNH0206", 60, ConditionalUsage, 1),
    ElementComponent(Element("0113", "MESSAGE TYPE SUB-FUNCTION IDENTIFICATION", ALPHANUMERIC, 0, 6), Some("Message type sub-function identification"), "UNH0207", 70, ConditionalUsage, 1)), Nil)
  val compS010 = Composite("S010", "STATUS OF THE TRANSFER", List[SegmentComponent](
    ElementComponent(Element("0070", "Sequence of transfers", INTEGER, 0, 2), None, "UNH0401", 10, MandatoryUsage, 1),
    ElementComponent(Element("0073", "First and last transfer", ALPHA, 1, 1), None, "UNH0402", 20, ConditionalUsage, 1)), Nil)
  val compS011_0 = Composite("S011", "DATA ELEMENT IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0098", "Erroneous data element position in segment.", INTEGER, 0, 3), Some("Erroneous data element position in segment"), "UCF0701", 10, MandatoryUsage, 1),
    ElementComponent(Element("0104", "Erroneous component data element position", INTEGER, 0, 3), None, "UCF0702", 20, ConditionalUsage, 1),
    ElementComponent(Element("0136", "ERRONEOUS DATA ELEMENT OCCURRENCE", INTEGER, 0, 6), Some("Erroneous data element occurrence"), "UCF0703", 30, ConditionalUsage, 1)), Nil)
  val compS011_1 = Composite("S011", "DATA ELEMENT IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0098", "Erroneous data element position in segment.", INTEGER, 0, 3), Some("Erroneous data element position in segment"), "UCM0601", 10, MandatoryUsage, 1),
    ElementComponent(Element("0104", "Erroneous component data element position", INTEGER, 0, 3), None, "UCM0602", 20, ConditionalUsage, 1),
    ElementComponent(Element("0136", "ERRONEOUS DATA ELEMENT OCCURRENCE", INTEGER, 0, 6), Some("Erroneous data element occurrence"), "UCM0603", 30, ConditionalUsage, 1)), Nil)
  val compS011_2 = Composite("S011", "DATA ELEMENT IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0098", "Erroneous data element position in segment.", INTEGER, 0, 3), Some("Erroneous data element position in segment"), "UCD0201", 10, MandatoryUsage, 1),
    ElementComponent(Element("0104", "Erroneous component data element position", INTEGER, 0, 3), None, "UCD0202", 20, ConditionalUsage, 1),
    ElementComponent(Element("0136", "ERRONEOUS DATA ELEMENT OCCURRENCE", INTEGER, 0, 6), Some("Erroneous data element occurrence"), "UCD0203", 30, ConditionalUsage, 1)), Nil)
  val compS011_3 = Composite("S011", "DATA ELEMENT IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0098", "Erroneous data element position in segment.", INTEGER, 0, 3), Some("Erroneous data element position in segment"), "UCI0701", 10, MandatoryUsage, 1),
    ElementComponent(Element("0104", "Erroneous component data element position", INTEGER, 0, 3), None, "UCI0702", 20, ConditionalUsage, 1),
    ElementComponent(Element("0136", "ERRONEOUS DATA ELEMENT OCCURRENCE", INTEGER, 0, 6), Some("Erroneous data element occurrence"), "UCI0703", 30, ConditionalUsage, 1)), Nil)
  val compS016 = Composite("S016", "MESSAGE SUBSET IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0115", "MESSAGE SUBSET IDENTIFICATION", ALPHANUMERIC, 0, 14), Some("Message subset identification"), "UNH0501", 10, MandatoryUsage, 1),
    ElementComponent(Element("0116", "MESSAGE SUBSET VERSION NUMBER", ALPHANUMERIC, 0, 3), Some("Message subset version number"), "UNH0502", 20, ConditionalUsage, 1),
    ElementComponent(Element("0118", "MESSAGE SUBSET RELEASE NUMBER", ALPHANUMERIC, 0, 3), Some("Message subset release number"), "UNH0503", 30, ConditionalUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UNH0504", 40, ConditionalUsage, 1)), Nil)
  val compS017 = Composite("S017", "MESSAGE IMPLEMENTATION GUIDELINE IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0121", "MESSAGE IMPLEMENTATION GUIDELINE IDENTIFICATION", ALPHANUMERIC, 0, 14), Some("Message implementation guideline ident"), "UNH0601", 10, MandatoryUsage, 1),
    ElementComponent(Element("0122", "MESSAGE IMPLEMENTATION GUIDELINE VERSION NUMBER", ALPHANUMERIC, 0, 3), Some("Message implementation guideline version"), "UNH0602", 20, ConditionalUsage, 1),
    ElementComponent(Element("0124", "MESSAGE IMPLEMENTATION GUIDELINE RELEASE NUMBER", ALPHANUMERIC, 0, 3), Some("Message implementation guideline release"), "UNH0603", 30, ConditionalUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UNH0604", 40, ConditionalUsage, 1)), Nil)
  val compS018 = Composite("S018", "SCENARIO IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0127", "SCENARIO IDENTIFICATION", ALPHANUMERIC, 0, 14), Some("Scenario identification"), "UNH0701", 10, MandatoryUsage, 1),
    ElementComponent(Element("0128", "SCENARIO VERSION NUMBER", ALPHANUMERIC, 0, 3), Some("Scenario version number"), "UNH0702", 20, ConditionalUsage, 1),
    ElementComponent(Element("0130", "SCENARIO RELEASE NUMBER", ALPHANUMERIC, 0, 3), Some("Scenario release number"), "UNH0703", 30, ConditionalUsage, 1),
    ElementComponent(Element("0051", "Controlling agency", ALPHANUMERIC, 0, 2), Some("Controlling agency, coded"), "UNH0704", 40, ConditionalUsage, 1)), Nil)
  val compS020 = Composite("S020", "REFERENCE IDENTIFICATION", List[SegmentComponent](
    ElementComponent(Element("0813", "REFERENCE QUALIFIER", ALPHANUMERIC, 0, 3), Some("Reference qualifier"), "S02001", 10, MandatoryUsage, 1),
    ElementComponent(Element("0802", "REFERENCE IDENTIFICATION NUMBER", ALPHANUMERIC, 0, 35), Some("Reference identification number"), "S02002", 20, MandatoryUsage, 1)), Nil)
  val segUCD = Segment("UCD", "DATA ELEMENT ERROR INDICATION", List[SegmentComponent](
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCD01", 10, MandatoryUsage, 1),
    CompositeComponent(compS011_2, Some("DATA ELEMENT IDENTIFICATION"), "UCD02", 20, MandatoryUsage, 1)), Nil)
  val segUCF = Segment("UCF", "FUNCTIONAL GROUP RESPONSE", List[SegmentComponent](
    ElementComponent(Element("0048", "Functional group reference number", ALPHANUMERIC, 0, 14), Some("FUNCTIONAL GROUP REFERENCE NUMBER"), "UCF01", 10, MandatoryUsage, 1),
    CompositeComponent(compS006, Some("APPLICATION SENDER'S IDENTIFICATION"), "UCF02", 20, MandatoryUsage, 1),
    CompositeComponent(compS007, Some("APPLICATION RECIPIENTS IDENTIFICATION"), "UCF03", 30, MandatoryUsage, 1),
    ElementComponent(elem0083, Some("ACTION, CODED"), "UCF04", 40, MandatoryUsage, 1),
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCF05", 50, ConditionalUsage, 1),
    ElementComponent(Element("0135", "SERVICE SEGMENT TAG, CODED", ALPHANUMERIC, 0, 3), None, "UCF06", 60, ConditionalUsage, 1),
    CompositeComponent(compS011_0, Some("DATA ELEMENT IDENTIFICATION"), "UCF07", 70, ConditionalUsage, 1),
    ElementComponent(elem0534, None, "UCF08", 80, ConditionalUsage, 1),
    ElementComponent(elem0138, None, "UCF09", 90, ConditionalUsage, 1)), Nil)
  val segUCI = Segment("UCI", "INTERCHANGE RESPONSE", List[SegmentComponent](
    ElementComponent(Element("0020", "Interchange control reference", ALPHANUMERIC, 0, 14), Some("INTERCHANGE CONTROL REFERENCE"), "UCI01", 10, MandatoryUsage, 1),
    CompositeComponent(compS002, Some("INTERCHANGE SENDER"), "UCI02", 20, MandatoryUsage, 1),
    CompositeComponent(compS003, Some("INTERCHANGE RECIPIENT"), "UCI03", 30, MandatoryUsage, 1),
    ElementComponent(elem0083, Some("ACTION, CODED"), "UCI04", 40, MandatoryUsage, 1),
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCI05", 50, ConditionalUsage, 1),
    ElementComponent(elem0013, Some("SERVICE SEGMENT TAG, CODED"), "UCI06", 60, ConditionalUsage, 1),
    CompositeComponent(compS011_3, Some("DATA ELEMENT IDENTIFICATION"), "UCI07", 70, ConditionalUsage, 1),
    ElementComponent(elem0534, None, "UCI08", 80, ConditionalUsage, 1),
    ElementComponent(elem0138, None, "UCI09", 90, ConditionalUsage, 1)), Nil)
  val segUCM = Segment("UCM", "MESSAGE/PACKAGE RESPONSE", List[SegmentComponent](
    ElementComponent(elem0062, Some("MESSAGE REFERENCE NUMBER"), "UCM01", 10, ConditionalUsage, 1),
    CompositeComponent(compS009_0, Some("MESSAGE IDENTIFIER"), "UCM02", 20, ConditionalUsage, 1),
    ElementComponent(elem0083, Some("ACTION, CODED"), "UCM03", 30, MandatoryUsage, 1),
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCM04", 40, ConditionalUsage, 1),
    ElementComponent(elem0013, Some("SERVICE SEGMENT TAG, CODED"), "UCM05", 50, ConditionalUsage, 1),
    CompositeComponent(compS011_1, Some("DATA ELEMENT IDENTIFICATION"), "UCM06", 60, ConditionalUsage, 1),
    ElementComponent(Element("0800", "PACKAGE REFERENCE NUMBER", ALPHANUMERIC, 0, 35), None, "UCM07", 70, ConditionalUsage, 1),
    CompositeComponent(compS020, Some("REFERENCE IDENTIFICATION"), "UCM08", 80, ConditionalUsage, 99),
    ElementComponent(elem0534, None, "UCM09", 90, ConditionalUsage, 1),
    ElementComponent(elem0138, None, "UCM10", 100, ConditionalUsage, 1)), Nil)
  val segUCS = Segment("UCS", "SEGMENT ERROR INDICATION", List[SegmentComponent](
    ElementComponent(Element("0096", "Segment position in message", INTEGER, 0, 6), Some("SEGMENT POSITION IN MESSAGE BODY"), "UCS01", 10, MandatoryUsage, 1),
    ElementComponent(elem0085, Some("SYNTAX ERROR, CODED"), "UCS02", 20, ConditionalUsage, 1)), Nil)
  val segUNH = Segment("UNH", "MESSAGE HEADER", List[SegmentComponent](
    ElementComponent(elem0062, Some("MESSAGE REFERENCE NUMBER"), "UNH01", 10, MandatoryUsage, 1),
    CompositeComponent(compS009_1, Some("MESSAGE IDENTIFIER"), "UNH02", 20, MandatoryUsage, 1),
    ElementComponent(Element("0068", "Common access reference", ALPHANUMERIC, 0, 35), Some("COMMON ACCESS REFERENCE"), "UNH03", 30, ConditionalUsage, 1),
    CompositeComponent(compS010, Some("STATUS OF THE TRANSFER"), "UNH04", 40, ConditionalUsage, 1),
    CompositeComponent(compS016, Some("MESSAGE SUBSET IDENTIFICATION"), "UNH05", 50, ConditionalUsage, 1),
    CompositeComponent(compS017, Some("MESSAGE IMPLEMENTATION GUIDELINE IDENTIFICATION"), "UNH06", 60, ConditionalUsage, 1),
    CompositeComponent(compS018, Some("SCENARIO IDENTIFICATION"), "UNH07", 70, ConditionalUsage, 1)), Nil)
  val segUNT = Segment("UNT", "MESSAGE TRAILER", List[SegmentComponent](
    ElementComponent(Element("0074", "Number of segments in a message", INTEGER, 0, 6), Some("NUMBER OF SEGMENTS IN A MESSAGE"), "UNT01", 10, MandatoryUsage, 1),
    ElementComponent(elem0062, Some("MESSAGE REFERENCE NUMBER"), "UNT02", 20, MandatoryUsage, 1)), Nil)

  val CONTRLsg5 = GroupComponent("Segment group 5", ConditionalUsage, 999, List[TransactionComponent](
    ReferenceComponent(segUCS, SegmentPosition(0, "0130"), MandatoryUsage, 1), ReferenceComponent(segUCD, SegmentPosition(0, "0140"), ConditionalUsage, 99)), None, Nil)
  val CONTRLsg4 = GroupComponent("Segment group 4", ConditionalUsage, 999999, List[TransactionComponent](
    ReferenceComponent(segUCM, SegmentPosition(0, "0110"), MandatoryUsage, 1),
    CONTRLsg5), None, Nil)
  val CONTRLsg3 = GroupComponent("Segment group 3", ConditionalUsage, 999999, List[TransactionComponent](
    ReferenceComponent(segUCF, SegmentPosition(0, "0090"), MandatoryUsage, 1),
    CONTRLsg4), None, Nil)
  val CONTRLsg2 = GroupComponent("Segment group 2", ConditionalUsage, 999, List[TransactionComponent](
    ReferenceComponent(segUCS, SegmentPosition(0, "0060"), MandatoryUsage, 1), ReferenceComponent(segUCD, SegmentPosition(0, "0070"), ConditionalUsage, 99)), None, Nil)
  val CONTRLsg1 = GroupComponent("Segment group 1", ConditionalUsage, 999999, List[TransactionComponent](
    ReferenceComponent(segUCM, SegmentPosition(0, "0040"), MandatoryUsage, 1),
    CONTRLsg2), None, Nil)

  val transCONTRL = Transaction("CONTRL", "Application error and acknowledgement message", None,
    List[TransactionComponent](
      ReferenceComponent(segUNH, SegmentPosition(0, "0010"), MandatoryUsage, 1),
      ReferenceComponent(segUCI, SegmentPosition(0, "0020"), MandatoryUsage, 1),
      CONTRLsg1,
      CONTRLsg3,
      ReferenceComponent(segUNT, SegmentPosition(0, "0150"), MandatoryUsage, 1)),
    List[TransactionComponent](), List[TransactionComponent]())
}

object EdifactAcknowledgment {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.X12Constants._
  import com.anypoint.df.edi.lexical.EdiConstants.DataType
  import com.anypoint.df.edi.lexical.EdiConstants.DataType._

  trait Coded[K] {
    val code: K
  }

  def instanceMap[K, V <: Coded[K]](values: V*): Map[K, V] =
    values.toList.foldLeft(Map[K, V]())((acc, value) => acc + (value.code -> value))

  /** Acknowledgment action codes (0083 element codes). */
  sealed abstract class AcknowledgmentActionCode(val code: String, val text: String) extends Coded[String]
  case object AcknowledgedAllLevels extends AcknowledgmentActionCode("1", "This level and all lower levels acknowledged (deprecated)")
  case object AcknowledgedWithErrors extends AcknowledgmentActionCode("2", "This level and all lower levels acknowledged, with errors reported (deprecated)")
  case object AcknowledgedWithRejections extends AcknowledgmentActionCode("3", "One or more rejected at next lower level (deprecated)")
  case object AcknowledgedRejected extends AcknowledgmentActionCode("4", "This level and all lower levels rejected")
  case object AcknowledgedUnbUnzAccepted extends AcknowledgmentActionCode("5", "UNB/UNZ accepted (deprecated)")
  case object AcknowledgedUnbUnzRejected extends AcknowledgmentActionCode("6", "UNB/UNZ rejected (deprecated)")
  case object AcknowledgedLevel extends AcknowledgmentActionCode("7", "This level acknowledged, next lower level acknowledged if not explicitly rejected")
  case object AcknowledgedInterchangeReceived extends AcknowledgmentActionCode("8", "Interchange received")
  val AcknowledgmentActionCodes = instanceMap[String, AcknowledgmentActionCode](AcknowledgedRejected,
    AcknowledgedLevel, AcknowledgedInterchangeReceived)

  /** Syntax error codes (0085 element codes). */
  sealed abstract class SyntaxError(val code: String, val text: String) extends Coded[String]
  case object NotSupportedVersion extends SyntaxError("2", "Syntax version or level not supported")
  case object NotActualRecipient extends SyntaxError("7", "Interchange recipient not actual recipient")
  case object InvalidSimpleValue extends SyntaxError("12", "Invalid value for element, composite, or component")
  case object MissingRequiredValue extends SyntaxError("13", "Missing required service or user segment, data element, composite data element or component data element")
  case object ValuePositionNotSupported extends SyntaxError("14", "Unsupported value in position for data element, composite data element or component data element")
  case object NotSupportedInPosition extends SyntaxError("15", "Unsupported position for segment type, data element, composite data element or component data element")
  case object TooManyConstituents extends SyntaxError("16", "Segment or composite contained too many data elements")
  case object NoAgreementForValue extends SyntaxError("17", "No agreement allowing interchange, functional group, or message with value")
  case object UnspecifiedError extends SyntaxError("18", "Error with nature not reported")
  case object InvalidDecimalNotation extends SyntaxError("19", "Invalid decimal notation")
  case object CharacterInvalidAsServiceCharacter extends SyntaxError("20", "Character invalid as service character")
  case object InvalidSyntaxCharacter extends SyntaxError("21", "Invalid character(s) for specified syntax level")
  case object InvalidServiceCharacter extends SyntaxError("22", "Character(s) invalid as service character")
  case object UnknownInterchangeSender extends SyntaxError("23", "Unknown interchange sender")
  case object MessageTooOld extends SyntaxError("24", "Interchange or functional group is older than configured limit")
  case object TestIndicatorNotSupported extends SyntaxError("25", "Test processing cannot be performed for interchange, functional group, or message")
  case object DuplicateDetected extends SyntaxError("26", "Duplicate of interchange, functional group, or message")
  case object SecurityFunctionNotSupported extends SyntaxError("27", "Security function is not supported")
  case object ControlReferenceMismatch extends SyntaxError("28", "Control reference in UNB/UNG/UNH does not match UNZ/UNE/UNT")
  case object ControlCountMismatch extends SyntaxError("29", "Control count does not match number of instances received")
  case object FunctionalGroupsMessageMix extends SyntaxError("30", "Individual messages and functional groups have been mixed at the same level in the interchange")
  case object MultipleMessageTypesGroup extends SyntaxError("31", "Different message types in a functional group")
  case object LowerLevelEmpty extends SyntaxError("32", "Interchange did not contain any message or functional groups, or functional group did not contain any messages")
  case object InvalidOccurrence extends SyntaxError("33", "Invalid segment or data element in interchange, between messages, or between functional groups")
  case object NestingIndicationNotAllowed extends SyntaxError("34", "Explicit nesting used in message where not allowed")
  case object TooManySegmentRepetitions extends SyntaxError("35", "Segment repeated too many times")
  case object TooManyGroupRepetitions extends SyntaxError("36", "Segment group repeated too many times")
  case object InvalidCharacterType extends SyntaxError("37", "Numeric character(s) in alpha element, or alpha character(s) in numeric element")
  case object MissingDigitBeforeDecimal extends SyntaxError("38", "Missing digit in front of decimal sign")
  case object ElementTooLong extends SyntaxError("39", "Data element too long")
  case object ElementTooShort extends SyntaxError("40", "Data element too short")
  case object PermanentCommunicationError extends SyntaxError("41", "Permanent error reported by communication network for interchange")
  case object TemporaryCommunicationError extends SyntaxError("42", "Temporary error reported by communication network for interchange")
  case object UnknownInterchangeRecipient extends SyntaxError("43", "Unknown interchange recipient")
  val SyntaxErrors = instanceMap[String, SyntaxError](NotSupportedVersion, NotActualRecipient, InvalidSimpleValue,
    MissingRequiredValue, ValuePositionNotSupported, NotSupportedInPosition, TooManyConstituents, NoAgreementForValue,
    UnspecifiedError, InvalidDecimalNotation, CharacterInvalidAsServiceCharacter, InvalidSyntaxCharacter,
    InvalidServiceCharacter, UnknownInterchangeSender, MessageTooOld, TestIndicatorNotSupported, DuplicateDetected,
    SecurityFunctionNotSupported, ControlReferenceMismatch, ControlCountMismatch, FunctionalGroupsMessageMix,
    MultipleMessageTypesGroup, LowerLevelEmpty, InvalidOccurrence, NestingIndicationNotAllowed,
    TooManySegmentRepetitions, TooManyGroupRepetitions, InvalidCharacterType, MissingDigitBeforeDecimal,
    ElementTooLong, ElementTooShort, PermanentCommunicationError, TemporaryCommunicationError,
    UnknownInterchangeRecipient)

}