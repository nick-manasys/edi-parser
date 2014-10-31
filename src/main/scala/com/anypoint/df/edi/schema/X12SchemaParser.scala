package com.anypoint.df.edi.schema

import java.io.InputStream
import com.anypoint.df.edi.parser.ParserBase
import com.anypoint.df.edi.parser.ParserBase.ItemType
import com.anypoint.df.edi.parser.ParserBase.ItemType._
import com.anypoint.df.edi.parser.X12Parser
import scala.util.Success
import java.io.IOException

/** Parser for X12 EDI documents.
  */
class X12SchemaParser(in: InputStream, sc: EdiSchema) extends SchemaParser(new X12Parser(in), sc) {

  import com.anypoint.df.edi.parser.X12Constants._
  import EdiSchema._

  val GSSegment = Segment("GS", "Functional group header", List[SegmentComponent](
    ElementComponent(Element("479", IdType, 2, 2), FUNCTIONAL_IDENTIFIER_CODE, MandatoryUsage, 1),
    ElementComponent(Element("142", AlphaNumericType, 2, 15), APPLICATION_SENDERS_CODE, MandatoryUsage, 1),
    ElementComponent(Element("124", AlphaNumericType, 2, 15), APPLICATION_RECEIVERS_CODE, MandatoryUsage, 1),
    ElementComponent(Element("373", DateType, 8, 8), GROUP_DATE, MandatoryUsage, 1),
    ElementComponent(Element("337", TimeType, 4, 8), GROUP_TIME, MandatoryUsage, 1),
    ElementComponent(Element("28", IntegerType, 1, 9), GROUP_CONTROL_NUMBER, MandatoryUsage, 1),
    ElementComponent(Element("455", IdType, 1, 2), RESPONSIBLE_AGENCY_CODE, MandatoryUsage, 1),
    ElementComponent(Element("480", AlphaNumericType, 2, 12), VERSION_IDENTIFIER_CODE, MandatoryUsage, 1)))

  val GESegment = Segment("GE", "Functional group trailer", List[SegmentComponent](
    ElementComponent(Element("97", IntegerType, 1, 6), NUMBER_OF_TRANSACTION_SETS, MandatoryUsage, 1),
    ElementComponent(Element("28", IntegerType, 1, 9), GROUP_CONTROL_NUMBER, MandatoryUsage, 1)))

  val STSegment = Segment("ST", "Transaction set header", List[SegmentComponent](
    ElementComponent(Element("143", IdType, 3, 3), TRANSACTION_SET_IDENTIFIER_CODE, MandatoryUsage, 1),
    ElementComponent(Element("329", AlphaNumericType, 4, 9), TRANSACTION_SET_CONTROL_NUMBER, MandatoryUsage, 1),
    ElementComponent(Element("1705", AlphaNumericType, 1, 35), IMPLEMENTATION_CONVENTION_REFERENCE, OptionalUsage, 1)))

  val SESegment = Segment("SE", "Transaction set trailer", List[SegmentComponent](
    ElementComponent(Element("96", IntegerType, 1, 10), NUMBER_OF_INCLUDED_SEGMENTS, MandatoryUsage, 1),
    ElementComponent(Element("329", AlphaNumericType, 4, 9), TRANSACTION_SET_CONTROL_NUMBER, MandatoryUsage, 1)))

  def init() = baseParser.init(new ValueMapImpl())

  /** Parse start of a functional group. */
  def openGroup() =
    if (checkSegment(GSSegment)) parseSegment(GSSegment)
    else throw new IOException("missing required GS segment")

  /** Check if at functional group close segment. */
  def isGroupClose() = checkSegment(GESegment)

  /** Parse close of a functional group. */
  def closeGroup(props: ValueMap) = parseSegment(GESegment)

  /** Parse start of a transaction set. */
  def openSet() =
    if (checkSegment(STSegment)) {
      val values = parseSegment(STSegment)
      (values.get(TRANSACTION_SET_IDENTIFIER_CODE).asInstanceOf[String], values)
    } else throw new IOException("missing required ST segment")

  /** Check if at transaction set close segment. */
  def isSetClose() = checkSegment(SESegment)

  /** Parse close of a transaction set. */
  def closeSet(props: ValueMap) = parseSegment(SESegment)
}