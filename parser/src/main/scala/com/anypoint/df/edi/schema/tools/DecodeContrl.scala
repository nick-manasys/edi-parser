package com.anypoint.df.edi.schema.tools

import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.EdifactConstants._
import com.anypoint.df.edi.schema.{ SchemaJavaDefs, EdiSchema, EdifactSchemaDefs, EdifactVersionDefs }
import com.anypoint.df.edi.schema.SchemaJavaValues._
import com.anypoint.df.edi.schema.EdifactAcknowledgment._
import com.anypoint.df.edi.schema.EdifactIdentityInformation
import com.anypoint.df.edi.schema.EdifactSchemaParser
import com.anypoint.df.edi.schema.EdifactParserConfig
import scala.util.Failure
import scala.util.Success
import java.io.ByteArrayInputStream
import com.anypoint.df.edi.schema.ControlV3Defs

/** EDIFACT CONTRL functional acknowledgment decoder. */
object DecodeContrl extends SchemaJavaDefs {

  import EdifactSchemaDefs._
  import EdiSchema._

  /** Build identity composite description. */
  def buildIdentity(comp: Composite, map: ValueMap) = {
      val comps = comp.components
    val builder = new StringBuilder
    builder ++= getRequiredString(comps.head.key, map)
    val rest = comps.tail
    if (rest.exists { comp => map.containsKey(comp.key) }) {
      builder ++= " ["
      rest.foreach { comp =>
        if (map.containsKey(comp.key)) {
          builder ++= s"${comp.key}=${getRequiredString(comp.key, map)}"
        }
      }
      builder ++= "]"
    }
    builder toString
  }
  
  /** Build identity description for segment component (which must be an identity composite). */
  def buildIdentity(comp: SegmentComponent, map: ValueMap): String =
    buildIdentity(comp.asInstanceOf[CompositeComponent].composite, map)

  /** Build data element identification for segment component (which must be a data element composite). */
  def buildOptionalDataElement(ccomp: SegmentComponent, map: ValueMap) =
    if (map.containsKey(ccomp.asInstanceOf[CompositeComponent].composite.components.head.key)) {
      val comps = ccomp.asInstanceOf[CompositeComponent].composite.components
      val builder = new StringBuilder
      builder ++= s" element #${getRequiredString(comps.head.key, map)}"
      val t1 = comps.tail
      if (map.containsKey(t1.head.key)) {
        builder ++= s" component ${getRequiredString(t1.head.key, map)}"
        val t2 = comps.tail
        if (map.containsKey(t2.head.key)) {
          builder ++= s" repeat ${getRequiredString(t2.head.key, map)}"
        }
      }
      builder toString
    } else ""

  /** Build message identifier for segment component (which must be a message identifier composite). */
  def buildMessageIdentifier(ccomp: SegmentComponent, map: ValueMap) = {
      val comps = ccomp.asInstanceOf[CompositeComponent].composite.components
      s"(${getRequiredString(comps(0).key, map)} ${getRequiredString(comps(1).key, map)}${getRequiredString(comps(2).key, map)} ${getRequiredString(comps(3).key, map)})"
    }
  
  /** Build description of UCM/UCS+UCD group. */
  def buildMessageGroup(schemaDefs: EdifactVersionDefs, ucmgroup: GroupComponent, grpmap: ValueMap) = {
      val ucmmap = getRequiredValueMap(ucmgroup.items(0).key, grpmap)
      val ucmcomps = schemaDefs.segUCM.components
      val builder = new StringBuilder
      builder ++= s" Message #${getRequiredString(ucmcomps(0).key, ucmmap)} ${buildMessageIdentifier(ucmcomps(1), ucmmap)}\n"
      if (grpmap.containsKey(ucmgroup.items(1).key)) {
        val grpcomps = ucmgroup.items(1).asInstanceOf[GroupComponent].items
        foreachMapInList(getRequiredMapList(ucmgroup.items(1).key, grpmap), map => {
          val ucsmap = getRequiredValueMap(ucmgroup.items(0).key, grpmap)
        })
      }
      builder toString
    }

  def decode(rootmap: ValueMap) = {
    val intermap = getRequiredValueMap(interchangeKey, rootmap)
    val schemaDefs = versions(EDIFACT_VERSIONS.get(getRequiredString(interHeadSyntaxVersionKey, intermap)))
    if (getRequiredString(transactionId, rootmap) != schemaDefs.transCONTRL.ident) throw new IllegalArgumentException("Not a CONTRL message")
    val builder = new StringBuilder
    builder ++= s"CONTRL sender ${buildIdentity(schemaDefs.interHeadSender, intermap)}\n"
    builder ++= s"CONTRL receipient ${buildIdentity(schemaDefs.interHeadRecipient, intermap)}\n"
    val headmap = getRequiredValueMap(transactionHeading, rootmap)
    val msgcomps = schemaDefs.transCONTRL.heading.toArray
    
    // interpret the required UCI segment
    val ucimap = getRequiredValueMap(msgcomps(1).key, headmap)
    val ucicomps = schemaDefs.segUCI.components
    builder ++= s"Interchange control reference ${getRequiredString(ucicomps(0).key, ucimap)}\n"
    builder ++= s"Interchange sender ${buildIdentity(ucicomps(1).asInstanceOf[CompositeComponent], ucimap)}\n"
    builder ++= s"Interchange receipient ${buildIdentity(ucicomps(2).asInstanceOf[CompositeComponent], ucimap)}\n"
    builder ++= s"Interchange action: ${AcknowledgmentActionCodes(getRequiredString(ucicomps(3).key, ucimap))}\n"
    if (ucimap.containsKey(ucicomps(4).key)) {
      builder ++= s"Syntax error ${SyntaxErrors(getRequiredString(ucicomps(4).key, ucimap)).text}"
      if (ucimap.containsKey(ucicomps(5).key)) {
        builder ++= s" on segment ${getRequiredString(ucicomps(5).key, ucimap)}${buildOptionalDataElement(ucicomps(6), ucimap)}"
      } else if (ucimap.containsKey(ucicomps(7).key)) {
        builder ++= s" security reference ${getRequiredString(ucicomps(7).key, ucimap)} at position ${getAs[BigDecimal](ucicomps(8).key, ucimap).toString}"
      }
      builder ++= "\n"
    }
    
    // interpret Group 1, if present
    if (headmap.containsKey(msgcomps(2).key)) {
      val ucmgroup = msgcomps(2).asInstanceOf[GroupComponent]
      foreachMapInList(getRequiredMapList(msgcomps(2).key, headmap), map => builder ++= buildMessageGroup(schemaDefs, ucmgroup, map))
    }
    
    // interpret Group 3, if present
    if (headmap.containsKey(msgcomps(3).key)) {
      
    }
    builder ++= "\n"
    builder toString
  }

  val testMsg = s"""UNB+UNOC:3+INTTRA:ZZZ+RECIPIENT:ZZZ+030205:2003+50'
UNH+50+CONTRL:D:99B:UN'
UCI+665+SENDER+INTTRA+7'
UCM+318+IFTMIN:D:99B:UN+4'
UCS+4'
UCD+12+3'
UNT+6+50'
UNZ+1+50'"""

  def main(args: Array[String]): Unit = {
    val config = EdifactParserConfig(true, true, true, true, true, true, true, -1,
      ASCII_CHARSET, Array[EdifactIdentityInformation](), Array[EdifactIdentityInformation]())
    val is = new ByteArrayInputStream(testMsg.getBytes)
    val schema = EdiSchema(EdiSchema.EdiFact, "D01A", Map[String, EdiSchema.Element](), Map[String, EdiSchema.Composite](),
      Map[String, EdiSchema.Segment](), Map[String, EdiSchema.Transaction]()).merge(ControlV3Defs.transCONTRL)
    val parser = EdifactSchemaParser(is, schema, new DefaultEdifactNumberValidator, config)
    parser.parse match {
      case Success(root) => println(decode(getRequiredMapList("CONTRL", getRequiredValueMap(transactionsMap, root)).get(0)))
      case Failure(e) => throw e
    }
  }
}