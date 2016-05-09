package com.mulesoft.flatfile.schema.tools

import com.mulesoft.flatfile.lexical.X12Constants._
import com.mulesoft.flatfile.schema.{ SchemaJavaDefs, X12SchemaDefs, EdiSchema }
import com.mulesoft.flatfile.schema.SchemaJavaValues._
import com.mulesoft.flatfile.schema.X12Acknowledgment._

/** X12 997 functional acknowledgment decoder. This is set up for received 997, with the interchange information linked
 * from the transaction map.
  */
object Decode997 extends SchemaJavaDefs {
  
  import X12SchemaDefs._

  def decode(root: ValueMap) = {
    if (getRequiredString(structureId, root) != trans997.ident) throw new IllegalArgumentException("Not a 997 transaction set")
    val builder = new StringBuilder
    val inter = getRequiredValueMap(interchangeKey, root)
    builder ++= s"From ${getRequiredString(SENDER_ID_QUALIFIER, inter)}:${getRequiredString(SENDER_ID, inter)}\n"
    builder ++= s"To ${getRequiredString(RECEIVER_ID_QUALIFIER, inter)}:${getRequiredString(RECEIVER_ID, inter)}\n"
    val ackhead = getRequiredValueMap(structureHeading, root)
    val ak1data = getRequiredValueMap(trans997.headingKeys(1), ackhead)
    builder ++= s"Acknowledged group code ${getRequiredString(segAK1Comps(0) key, ak1data)} with control number ${getRequiredInt(segAK1Comps(1) key, ak1data)}"
    applyIfPresent[String](segAK1Comps(2) key, ak1data, value => builder ++= s", version $value")
    builder ++= "\n"
    applyIfPresent[MapList](groupAK2_997 key, ackhead, list =>
      foreachMapInList(list, { map => {
          val ak2data = getRequiredValueMap(groupAK2_997.keys(0), map)
          builder ++= s" Transaction ${getRequiredString(segAK2Comps(0) key, ak2data)} with control number ${getRequiredString(segAK2Comps(1) key, ak2data)}"
          applyIfPresent[String](segAK2Comps(2) key, ak2data, value => builder ++= s", implementation reference $value")
          builder ++= "\n"
          applyIfPresent[MapList](groupAK3 key, map, list =>
            foreachMapInList(list, { map => {
              val ak3data = getRequiredValueMap(groupAK3.keys(0), map)
              builder ++= s"  Segment ${getRequiredString(segAK3Comps(0) key, ak3data)} at position ${getRequiredInt(segAK3Comps(1) key, ak3data)}"
              applyIfPresent[String](segAK3Comps(2) key, ak3data, value => builder ++= s" (loop $value)")
              applyIfPresent[String](segAK3Comps(3) key, ak3data, value => builder ++= s" has syntax error ${SegmentSyntaxErrors(value).text}")
              builder ++= "\n"
              applyIfPresent[MapList](groupAK3.keys(1), map, list =>
                foreachMapInList(list, { ak4data => {
                  val comps = segAK4Comps(0).asInstanceOf[EdiSchema.CompositeComponent].composite.components
                  builder ++= s"   Element ${ak4data.get(comps(0) key)}"
                  applyIfPresent[Integer](comps(1) key, ak4data, value => builder ++= s", position $value")
                  applyIfPresent[Integer](comps(2) key, ak4data, value => builder ++= s", repetition $value")
                  applyIfPresent[Integer](segAK4Comps(1) key, ak4data, value => builder ++= s" (reference $value)")
                  builder ++= s" error  ${ElementSyntaxErrors(getRequiredString(segAK4Comps(2) key, ak4data)).text}"
                  applyIfPresent[Integer](segAK4Comps(3) key, ak4data, value => builder ++= s" (data '$value'')")
                  builder ++= "\n"
                }}))
            }}))
          val ak5data = getRequiredValueMap(groupAK2_997.keys(2), map)
          builder ++= s" Transaction ${TransactionAcknowledgmentCodes(getRequiredString(segAK5Comps(0) key, ak5data)).text}\n"
          applyIfPresent[String](segAK5Comps(1) key, ak5data, value => {
            builder ++= " Error codes: "
            (1 to 5) foreach (i => applyIfPresent[String](segAK5Comps(i).key, ak5data,
              value => builder ++= s" ${TransactionSyntaxErrors(value).text}"))
            builder ++= "\n"
          })
        }}))
    val ak9data = getRequiredValueMap(trans997.headingKeys(3), ackhead)
    builder ++= s"Group result: ${GroupAcknowledgmentCodes(getRequiredString(segAK9Comps(0) key, ak9data)).text}, contained ${getRequiredInt(segAK9Comps(1) key, ak9data)} transaction set(s) with ${getRequiredInt(segAK9Comps(2) key, ak9data)} received and ${getRequiredInt(segAK9Comps(3) key, ak9data)} accepted\n"
    applyIfPresent[String](segAK9Comps(4) key, ak9data, value => {
      builder ++= " Error codes: "
      (4 to 8) foreach (i => applyIfPresent[String](segAK9Comps(i).key, ak9data,
        value => builder ++= s" ${GroupSyntaxErrors(value).text}"))
      builder ++= "\n"
    })
    builder toString
  }
}