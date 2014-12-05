package com.anypoint.df.edi.schema

import com.anypoint.df.edi.lexical.X12Writer
import java.io.OutputStream
import java.nio.charset.Charset
import java.util.Calendar
import java.util.GregorianCalendar

/** Writer for X12 EDI documents.
  */
class X12SchemaWriter(out: OutputStream, sc: EdiSchema) extends SchemaWriter(new X12Writer, sc) with X12SchemaDefs {

  import com.anypoint.df.edi.lexical.X12Constants._
  import EdiSchema._
  import X12SchemaValues._

  var setCount = 0
  var setSegmentBase = 0

  /** Initialize writer and output interchange header segment(s). */
  def init(delims: String, encoding: String, props: ValueMap) = {
    if (delims.size < 4 || delims.size > 5) throw new IllegalArgumentException("delimiter string must be 4 or 5 characters")
    val repsep = if (delims(2) == 'U') -1 else delims(2)
    writer.asInstanceOf[X12Writer].configureX12(out, Charset.forName(encoding), delims(0), delims(1),
      repsep, delims(3))
    writer.init(props)
  }

  /** Output interchange trailer segment(s) and finish with stream. */
  def term(props: ValueMap) = {
    writer.term(props)
  }

  /** Write start of a functional group. */
  def openGroup(functId: String, props: ValueMap) = {
    val modprops = new ValueMapImpl(props)
    modprops put(functionalIdentifierKey, functId)
    val calendar = new GregorianCalendar
    modprops put(groupDateKey, calendar)
    val time = Integer.valueOf((calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000)
    modprops put(groupTimeKey, time)
    writeSegment(modprops, GSSegment)
    setCount = 0
  }

  /** Write close of a functional group. */
  def closeGroup(props: ValueMap) = {
    val modprops = new ValueMapImpl(props)
    modprops put(numberOfSetsKey, Integer.valueOf(setCount))
    modprops put(groupControlEndKey, props.get(groupControlKey))
    writeSegment(modprops, GESegment)
  }

  /** Write start of a transaction set. */
  def openSet(ident: String, props: ValueMap) = {
    val modprops = new ValueMapImpl(props)
    modprops put(transactionSetIdentifierKey, ident)
    setSegmentBase = writer.getSegmentCount
    writeSegment(props, STSegment)
  }

  /** Write close of a transaction set. */
  def closeSet(props: ValueMap) = {
    val modprops = new ValueMapImpl(props)
    modprops put(numberOfSegmentsKey, Integer.valueOf(writer.getSegmentCount - setSegmentBase + 1))
    modprops put(transactionSetControlEndKey, props.get(transactionSetControlKey))
    writeSegment(modprops, SESegment)
    setCount += 1
  }

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == "ST" || segment.ident == "SE"
}