package com.anypoint.df.edi.schema

import java.io.OutputStream
import java.nio.charset.Charset
import java.util.Calendar
import java.util.GregorianCalendar
import scala.collection.JavaConversions
import scala.util.Try
import com.anypoint.df.edi.lexical.X12Writer

/** Writer for X12 EDI documents.
  */
class X12SchemaWriter(out: OutputStream, sc: EdiSchema)
  extends SchemaWriter(new X12Writer, sc.merge(X12Acknowledgment.trans997)) with X12SchemaDefs {

  import com.anypoint.df.edi.lexical.X12Constants._
  import EdiSchema._
  import SchemaJavaValues._
  import X12SchemaValues._

  var setCount = 0
  var setSegmentBase = 0
  var inGroup = false

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

  /** Write start of a functional group (modifies passed in map). */
  def openGroup(functId: String, props: ValueMap) = {
    props put (functionalIdentifierKey, functId)
    val calendar = new GregorianCalendar
    props put (groupDateKey, calendar)
    val time = Integer.valueOf((calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000)
    props put (groupTimeKey, time)
    writeSegment(props, GSSegment)
    setCount = 0
  }

  /** Write close of a functional group (modifies passed in map). */
  def closeGroup(props: ValueMap) = {
    props put (numberOfSetsKey, Integer.valueOf(setCount))
    props put (groupControlEndKey, props.get(groupControlKey))
    writeSegment(props, GESegment)
  }

  /** Write start of a transaction set (modifies passed in map). */
  def openSet(ident: String, props: ValueMap) = {
    props put (transactionSetIdentifierKey, ident)
    setSegmentBase = writer.getSegmentCount
    writeSegment(props, STSegment)
  }

  /** Write close of a transaction set (modifies passed in map). */
  def closeSet(props: ValueMap) = {
    props put (numberOfSegmentsKey, Integer.valueOf(writer.getSegmentCount - setSegmentBase + 1))
    props put (transactionSetControlEndKey, props.get(transactionSetControlKey))
    writeSegment(props, SESegment)
    setCount += 1
  }

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == "ST" || segment.ident == "SE"

  /** Write the output message. */
  def write(map: ValueMap, basenum: Int) = Try({
    var internum = basenum
    val transMap = getRequiredValueMap(transactionsMap, map)
    val transactions = JavaConversions.mapAsScalaMap(transMap).values.flatMap(value =>
      JavaConversions.iterableAsScalaIterable(value.asInstanceOf[MapList]))
    val interchanges = transactions.groupBy(transdata => (
      getRequiredString(transactionInterSelfQualId, transdata),
      getRequiredString(transactionInterSelfId, transdata),
      getRequiredString(transactionInterPartnerQualId, transdata),
      getRequiredString(transactionInterPartnerId, transdata)))
    // TODO: handle multiple interchanges correctly
    val (selfQual, selfId, partnerQual, partnerId) = interchanges.keys.head
    val interProps = new ValueMapImpl
    interProps put (INTER_CONTROL, Integer valueOf (internum))
    interProps put (RECEIVER_ID_QUALIFIER, partnerQual)
    interProps put (RECEIVER_ID, partnerId)
    interProps put (SENDER_ID_QUALIFIER, selfQual)
    interProps put (SENDER_ID, selfId)
    // temporary fixed value, until we can evaluate older forms
    interProps put (VERSION_ID, "00501")
    init(getRequiredString(delimiterCharacters, map), getRequiredString(characterEncoding, map), interProps)
    interchanges foreach {
      case (_, interlist) => {
        val groups = interlist.groupBy(transdata => {
          val ident = getRequiredString(transactionId, transdata)
          val transdef = schema.transactions(ident)
          val selfid = getRequiredString(transactionGroupSelfId, transdata)
          val partnerid = getRequiredString(transactionGroupPartnerId, transdata)
          (selfid, partnerid, transdef group)
        })
        var groupnum = 1
        groups foreach {
          case ((selfGroup, partnerGroup, groupCode), grouplist) => {
            val groupProps = new ValueMapImpl
            groupProps put (applicationSendersKey, selfGroup)
            groupProps put (applicationReceiversKey, partnerGroup)
            groupProps put (groupControlKey, Integer valueOf (groupnum))
            // TODO: get these from the schema
            groupProps put (responsibleAgencyKey, "X")
            groupProps put (versionIdentifierKey, schema.version)
            openGroup(groupCode, groupProps)
            writer.countGroup
            var setnum = 1
            grouplist foreach (transaction => {
              val ident = getRequiredString(transactionId, transaction)
              val setProps = new ValueMapImpl
              setProps put (transactionSetControlKey, setnum toString)
              if (transaction containsKey (transactionImplConventionRef)) setProps put (implementationConventionKey,
                transaction get (transactionImplConventionRef))
              openSet(ident, setProps)
              writeTransaction(transaction, schema transactions (ident))
              closeSet(setProps)
              setnum += 1
            })
            closeGroup(groupProps)
            groupnum += 1
          }
        }
        term(interProps)
        internum += 1
      }
    }
    internum
  })
}