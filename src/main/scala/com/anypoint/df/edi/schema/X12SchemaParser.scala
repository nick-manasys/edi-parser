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
class X12SchemaParser(in: InputStream, sc: EdiSchema) extends SchemaParser(new X12Lexer(in), sc) with X12SchemaDefs {
  
  import EdiSchema._
  import com.anypoint.df.edi.lexical.X12Constants._
  import X12SchemaValues._

  def init() = lexer.init(new ValueMapImpl())
  
  def term(props: ValueMap) = lexer.term(props)

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
      if (props.get(groupControlKey) != endprops.get(groupControlEndKey)) {
        throw new IllegalStateException("group control number in trailer does not match header")
      }
    } else throw new IllegalStateException("not positioned at GE segment")
  }

  /** Parse start of a transaction set. */
  def openSet() =
    if (checkSegment(STSegment)) {
      val values = parseSegment(STSegment)
      (values.get(transactionSetIdentifierKey).asInstanceOf[String], values)
    } else throw new IllegalStateException("missing required ST segment")

  /** Check if at transaction set close segment. */
  def isSetClose() = checkSegment(SESegment)

  /** Parse close of a transaction set. */
  def closeSet(props: ValueMap) = {
    if (checkSegment(SESegment)) {
      val endprops = parseSegment(SESegment)
      if (props.get(transactionSetControlKey) != endprops.get(transactionSetControlEndKey)) {
        throw new IllegalStateException("transaction set control number in trailer does not match header")
      }
    } else throw new IllegalStateException("not positioned at SE segment")
  }
  
  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == "ST" || segment.ident == "SE"
}