package com.anypoint.df.edi.schema

import java.io.OutputStream
import java.nio.charset.Charset
import java.util.Calendar
import java.util.GregorianCalendar
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.util.Try
import com.anypoint.df.edi.lexical.{ WriteException, EdifactWriter, WriterBase }
import com.anypoint.df.edi.lexical.EdifactConstants._

/** Configuration parameters for EDIFACT schema writer.
  */
case class EdifactWriterConfig(val syntax: SyntaxIdentifier, val version: SyntaxVersion, val subChar: Int,
  val decimalMark: Char, val charSet: Charset, val delims: String, val suffix: String) {
  if (delims.size != 0 && delims.size != 5) throw new IllegalArgumentException("delimiter string must be empty or 5 characters")
}

/** Provider interface for control numbers. */
trait EdifactNumberProvider {
  def contextToken(senderId: String, senderQual: String, receiverId: String, receiverQual: String): String
  def nextInterchange(context: String): String
  def nextGroup(context: String, senderId: String, senderQual: String, receiverId: String, receiverQual: String): String
  def nextMessage(context: String, msgType: String, msgVersion: String, msgRelease: String, agency: String): String
}

/** Writer for EDIFACT EDI documents.
  */
case class EdifactSchemaWriter(out: OutputStream, sc: EdiSchema, numprov: EdifactNumberProvider, config: EdifactWriterConfig)
  extends SchemaWriter(new EdifactWriter(out, config.charSet, config.version, config.syntax, config.delims,
    config.suffix, config.subChar, config.decimalMark), sc) {

  import EdiSchema._
  import SchemaJavaValues._
  import EdifactSchemaDefs._

  val schemaDefs = versions(config.version)

  case class Message(val ident: String, val index: Int, val selfId: Option[String], val partnerId: Option[String],
    val data: ValueMap) {
    override def toString = s"message $ident at index $index"
  }

  var setCount = 0
  var setSegmentBase = 0
  var inGroup = false

  def logAndThrow(text: String, cause: Option[Throwable]) = {
    logger error text
    cause match {
      case Some(e) => throw new WriteException(text, e)
      case _ => throw new WriteException(text)
    }
  }

  /** Output interchange header segment(s). */
  def init(props: ValueMap) = {
    props put (interHeadSyntaxIdentKey, config.syntax.code)
    props put (interHeadSyntaxVersionKey, config.version.code)
    // TODO: set character encoding for v4, if it doesn't match syntax
    writer.init(props)
    writeSegment(props, schemaDefs.segUNB)
  }

  /** Output interchange trailer segment(s). */
  def term(props: ValueMap) = {
    writeSegment(props, schemaDefs.segUNZ)
    writer.term(props)
  }

  /** Write start of a functional group (modifies passed in map). */
  def openGroup(functId: String, props: ValueMap) = {
    props put (groupHeadReferenceKey, functId)
    writeSegment(props, schemaDefs.segUNG)
    setCount = 0
  }

  /** Write close of a functional group (modifies passed in map). */
  def closeGroup(props: ValueMap) = {
    props put (groupTrailCountKey, Integer.valueOf(setCount))
    props put (groupTrailReferenceKey, props.get(groupHeadReferenceKey))
    writeSegment(props, schemaDefs.segUNE)
  }

  /** Write start of a transaction set (modifies passed in map). */
  def openSet(ident: String, props: ValueMap) = {
    props put (msgHeadReferenceKey, ident)
    setSegmentBase = writer.getSegmentCount
    writeSegment(props, schemaDefs.segUNH)
  }

  /** Write close of a transaction set (modifies passed in map). */
  def closeSet(props: ValueMap) = {
    props put (msgTrailCountKey, Integer.valueOf(writer.getSegmentCount - setSegmentBase + 1))
    props put (msgTrailReferenceKey, props.get(msgHeadReferenceKey))
    writeSegment(props, schemaDefs.segUNT)
  }

  /** Check if an envelope segment (handled directly, outside of transaction). */
  def isEnvelopeSegment(segment: Segment) = segment.ident == "UNH" || segment.ident == "UNT"

  /** Group messages into lists for one or more interchanges based on the self and partner identification
    * information. This also verifies that the data for each message matches the message type of the containing
    * list, and saves information in the message to refer back to the position in the input data. Note that the
    * list of message for each interchange is in reverse order relative to the original order of the message.
    * @param rootMap message root
    * @return List(((interSelfQual, interSelf, interPartQual, interPart, interUsage), List(Message))
    */
  def messageInterchanges(rootMap: ValueMap) = {
    def optionalString(value: String) = if (value == null) None else Some(value)
    def optionalInt(value: Integer) = if (value == null) None else Some(value.intValue)
    val interDflt = getRequiredValueMap(interchangeKey, rootMap)
    val groupDflt = getAsMap(groupKey, rootMap)
    val transMap = getRequiredValueMap(messagesMap, rootMap)
    def getInterchangeString(key: String, specific: ValueMap) =
      if (specific != null && specific.containsKey(key)) getAsString(key, specific)
      else getAsString(key, interDflt)
    def getInterchangeInt(key: String, specific: ValueMap) =
      if (specific != null && specific.containsKey(key)) getAs[Integer](key, specific)
      else getAs[Integer](key, interDflt)
    def getGroupString(key: String, specific: ValueMap) =
      if (specific != null && specific.containsKey(key)) getAsString(key, specific)
      else if (groupDflt == null) null
      else getAsString(key, groupDflt)
    def tupleKey(transet: Message) = try {
      val specific = getAsMap(interchangeKey, transet.data)
      (optionalString(getInterchangeString(interHeadSenderQualKey, specific)),
        getInterchangeString(interHeadSenderIdentKey, specific),
        optionalString(getInterchangeString(interHeadRecipientQualKey, specific)),
        getInterchangeString(interHeadRecipientIdentKey, specific),
        optionalString(getInterchangeString(interHeadApplicationKey, specific)),
        optionalString(getInterchangeString(interHeadPriorityKey, specific)),
        optionalInt(getInterchangeInt(interHeadAckreqKey, specific)),
        optionalString(getInterchangeString(interHeadAgreementKey, specific)),
        optionalInt(getInterchangeInt(interHeadTestKey, specific)))
    } catch {
      case e: IllegalArgumentException => logAndThrow(s"$transet ${e.getMessage}", None)
    }
    val scalaTrans = transMap.asScala
    val result = scalaTrans.foldLeft(TreeMap[(Option[String], String, Option[String], String, Option[String], Option[String], Option[Int], Option[String], Option[Int]), List[Message]]()) {
      case (acc, (transnum, transets)) => {
        val transbuff = transets.asInstanceOf[MapList].asScala
        val sequence = (0 until transbuff.size) map { i =>
          val transdata = transbuff(i)
          try {
            val transid = getRequiredString(transactionId, transdata)
            if (transid != transnum) throw new IllegalArgumentException(s"$transactionId value '$transid'' does not match containing type")
            val specific = getAsMap(interchangeKey, transdata)
            val selfid = optionalString(getGroupString(groupHeadSenderIdentKey, specific))
            val partnerid = optionalString(getGroupString(groupHeadRecipientIdentKey, specific))
            Message(transnum, i, selfid, partnerid, transdata)
          } catch {
            case e: IllegalArgumentException => logAndThrow(s"transaction $transnum at index $i ${e.getMessage}", None)
          }
        }
        sequence.foldLeft(acc) ((acc, tset) => {
          val key = tupleKey(tset)
          val list = acc get (key) match {
            case Some(l) => l
            case None => Nil
          }
          acc + (key -> (tset :: list))
        })
      }
    }
    result map { case (tupkey, translist) => (tupkey, translist reverse) } toList
  }

  /** Stores list of string values matching the simple value components. */
  def setStrings(values: List[String], comps: List[SegmentComponent], data: ValueMap) = {
    @tailrec
    def setr(vals: List[String], rem: List[SegmentComponent]): Unit = vals match {
      case h :: t => if (rem.nonEmpty) {
        if (h != null && h.size > 0) data put (rem.head.key, h)
        setr(t, rem.tail)
      }
      case _ =>
    }
    setr(values, comps)
  }

  /** Write the output message. */
  def write(map: ValueMap) = Try({
    val interchanges = messageInterchanges(map)
    if (interchanges.isEmpty) throw new WriteException("no transactions to be sent")
    interchanges foreach {
      case ((selfQual, selfId, partnerQual, partnerId, appRef, priorityCode, ackReq, interAgree, useIndicator), interlist) => {
        val interProps = new ValueMapImpl
        val context = numprov contextToken (selfId, selfQual.getOrElse(null), partnerId, partnerQual.getOrElse(null))
        val interref = numprov nextInterchange (context)
        interProps put (interHeadReferenceKey, interref)
        setStrings(List(config.syntax.code, config.version.code), schemaDefs.unbSyntax.components, interProps)
        setStrings(List(partnerId, partnerQual.getOrElse(null)), schemaDefs.unbSender.components, interProps)
        setStrings(List(selfId, selfQual.getOrElse(null)), schemaDefs.unbRecipient.components, interProps)
        if (!map.containsKey(interHeadDateKey)) {
          val calendar = new GregorianCalendar
          val yearnum = calendar.get(Calendar.YEAR)
          val basedate = calendar.get(Calendar.DAY_OF_MONTH) * 100 + calendar.get(Calendar.MONTH) + 1
          val datetime = schemaDefs.segUNB.components(3).asInstanceOf[CompositeComponent]
          val dateelem = datetime.composite.components(0).asInstanceOf[ElementComponent].element
          val date = if (dateelem.maxLength == 8) basedate * 10000 + yearnum else basedate * 100 + yearnum % 100
          interProps put (interHeadDateKey, Integer.valueOf(date))
        }
        if (!map.containsKey(interHeadTimeKey)) {
          val calendar = new GregorianCalendar
          val time = Integer.valueOf(calendar.get(Calendar.HOUR_OF_DAY) * 60 + calendar.get(Calendar.MINUTE))
          interProps put (interHeadTimeKey, time)
        }
        if (appRef.isDefined) interProps put (interHeadApplicationKey, appRef.get)
        if (priorityCode.isDefined) interProps put (interHeadPriorityKey, priorityCode.get)
        if (ackReq.isDefined) interProps put (interHeadAckreqKey, Integer.valueOf(ackReq.get))
        if (interAgree.isDefined) interProps put (interHeadAgreementKey, interAgree.get)
        if (useIndicator.isDefined) interProps put (interHeadTestKey, Integer.valueOf(useIndicator.get))
        if (appRef.isDefined) interProps put (interHeadApplicationKey, appRef.get)
        if (appRef.isDefined) interProps put (interHeadApplicationKey, appRef.get)
        if (appRef.isDefined) interProps put (interHeadApplicationKey, appRef.get)
        if (appRef.isDefined) interProps put (interHeadApplicationKey, appRef.get)
        init(interProps)
        setCount = 0
        val groups = interlist.groupBy(transet => {
          val transdef = schema.transactions(transet.ident)
          (transet selfId, transet partnerId, transdef.group.getOrElse(""))
        })
        groups foreach {
          case ((selfGroup, partnerGroup, groupCode), grouplist) => {
            grouplist foreach (transet => try {
              val transdata = transet.data
              val setProps =
                if (transdata.containsKey(messageHeaderKey)) new ValueMapImpl(getRequiredValueMap(messageHeaderKey, transdata))
                else new ValueMapImpl
              setProps put (msgHeadReferenceKey, numprov.nextMessage(context, transet.ident, "D", schema.version, "UN"))
              setProps put (msgHeadMessageTypeKey, transet.ident)
              val version = schema.version.toUpperCase
              setProps put (msgHeadMessageVersionKey, version.substring(0, 1))
              setProps put (msgHeadMessageReleaseKey, version.substring(1))
              if (!setProps.containsKey(msgHeadMessageAgencyKey)) setProps put (msgHeadMessageAgencyKey, "UN")
              openSet(transet ident, setProps)
              writeTransaction(transdata, schema transactions (transet ident))
              closeSet(setProps)
              setCount += 1
            } catch {
              case e@(_: IllegalArgumentException | _: WriteException) => {
                logAndThrow(s"transaction ${transet ident} at index ${transet index} ${e.getMessage}", Some(e))
              }
            })
          }
        }
        val termprops = new ValueMapImpl
        termprops put (interTrailReferenceKey, interref)
        termprops put (interTrailCountKey, Integer.valueOf(setCount))
        term(termprops)
      }
    }
    writer close
  })
}