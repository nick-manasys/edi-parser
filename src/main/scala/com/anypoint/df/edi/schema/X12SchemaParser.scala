package com.anypoint.df.edi.schema

import java.io.InputStream
import java.io.IOException

import scala.util.Success

import com.anypoint.df.edi.lexical.EdiConstants.ItemType
import com.anypoint.df.edi.lexical.EdiConstants.ItemType._
import com.anypoint.df.edi.lexical.LexerBase
import com.anypoint.df.edi.lexical.X12Lexer

/** Parser for X12 EDI documents.
  */
class X12SchemaParser(in: InputStream, sc: EdiSchema) extends SchemaParser(new X12Lexer(in), sc) {

  import com.anypoint.df.edi.lexical.X12Constants._
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

  def init() = lexer.init(new ValueMapImpl())

  /** Parse start of a functional group. */
  def openGroup() =
    if (checkSegment(GSSegment)) parseSegment(GSSegment)
    else throw new IllegalStateException("missing required GS segment")

  /** Check if at functional group close segment. */
  def isGroupClose() = checkSegment(GESegment)

  /** Parse close of a functional group. */
  def closeGroup(props: ValueMap) = {
    if (checkSegment(GESegment)) {
      val endprops = parseSegment(GESegment)
      if (props.get(GROUP_CONTROL_NUMBER) != endprops.get(GROUP_CONTROL_NUMBER)) {
        throw new IllegalStateException("group control number in trailer does not match header")
      }
    } else throw new IllegalStateException("not positioned at GE segment")
  }

  /** Parse start of a transaction set. */
  def openSet() =
    if (checkSegment(STSegment)) {
      val values = parseSegment(STSegment)
      (values.get(TRANSACTION_SET_IDENTIFIER_CODE).asInstanceOf[String], values)
    } else throw new IllegalStateException("missing required ST segment")

  /** Check if at transaction set close segment. */
  def isSetClose() = checkSegment(SESegment)

  /** Parse close of a transaction set. */
  def closeSet(props: ValueMap) = {
    if (checkSegment(SESegment)) {
      val endprops = parseSegment(SESegment)
      if (props.get(TRANSACTION_SET_CONTROL_NUMBER) != endprops.get(TRANSACTION_SET_CONTROL_NUMBER)) {
        throw new IllegalStateException("transaction set control number in trailer does not match header")
      }
    } else throw new IllegalStateException("not positioned at SE segment")
  }
}