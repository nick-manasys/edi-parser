package com.anypoint.df.edi.schema.tools

import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.schema.{ SchemaJavaDefs, X12SchemaDefs, EdiSchema }
import com.anypoint.df.edi.schema.SchemaJavaValues._
import com.anypoint.df.edi.schema.X12Acknowledgment._

/** X12 997 functional acknowledgment decoder. This is set up for received 997, with the interchange information linked
 * from the transaction map.
  */
object Decode997 extends SchemaJavaDefs {
  
  import X12SchemaDefs._

  def decode(root: ValueMap) = {
    if (getRequiredString(transactionId, root) != trans997.ident) throw new IllegalArgumentException("Not a 997 transaction")
    val builder = new StringBuilder
    val inter = getRequiredValueMap(interchangeKey, root)
    builder ++= s"From ${getRequiredString(SENDER_ID_QUALIFIER, inter)}:${getRequiredString(SENDER_ID, inter)}\n"
    builder ++= s"To ${getRequiredString(RECEIVER_ID_QUALIFIER, inter)}:${getRequiredString(RECEIVER_ID, inter)}\n"
    val ackhead = getRequiredValueMap(transactionHeading, root)
    val ak1data = getRequiredValueMap(trans997Keys(1), ackhead)
    builder ++= s"Acknowledged group code ${getRequiredString(segAK1.components(0) key, ak1data)} with control number ${getRequiredInt(segAK1.components(1) key, ak1data)}"
    applyIfPresent[String](segAK1.components(2) key, ak1data, value => builder ++= s", version $value")
    builder ++= "\n"
    applyIfPresent[MapList](groupAK2 key, ackhead, list =>
      foreachMapInList(list, { map => {
          val ak2data = getRequiredValueMap(groupAK2Keys(0), map)
          builder ++= s" Transaction ${getRequiredString(segAK2.components(0) key, ak2data)} with control number ${getRequiredString(segAK2.components(1) key, ak2data)}"
          applyIfPresent[String](segAK2.components(2) key, ak2data, value => builder ++= s", implementation reference $value")
          builder ++= "\n"
          applyIfPresent[MapList](groupAK3 key, map, list =>
            foreachMapInList(list, { map => {
              val ak3data = getRequiredValueMap(groupAK3Keys(0), map)
              builder ++= s"  Segment ${getRequiredString(segAK3.components(0) key, ak3data)} at position ${getRequiredInt(segAK3.components(1) key, ak3data)}"
              applyIfPresent[String](segAK3.components(2) key, ak3data, value => builder ++= s" (loop $value)")
              applyIfPresent[String](segAK3.components(3) key, ak3data, value => builder ++= s" has syntax error ${SegmentSyntaxErrors(value)}")
              builder ++= "\n"
              applyIfPresent[MapList](groupAK3Keys(1), map, list =>
                foreachMapInList(list, { ak4data => {
                  val comps = segAK4.components(0).asInstanceOf[EdiSchema.CompositeComponent].composite.components
                  builder ++= s"   Element ${ak4data.get(comps(0) key)}"
                  applyIfPresent[Integer](comps(1) key, ak4data, value => builder ++= s", position $value")
                  applyIfPresent[Integer](comps(2) key, ak4data, value => builder ++= s", repetition $value")
                  applyIfPresent[Integer](segAK4.components(1) key, ak4data, value => builder ++= s" (reference $value)")
                  builder ++= s" error  ${ElementSyntaxErrors(getRequiredString(segAK4.components(2) key, ak4data))}"
                  applyIfPresent[Integer](segAK4.components(3) key, ak4data, value => builder ++= s" (data '$value'')")
                  builder ++= "\n"
                }}))
            }}))
          val ak5data = getRequiredValueMap(groupAK2Keys(2), map)
          builder ++= s" Transaction ${TransactionAcknowledgmentCodes(getRequiredString(segAK5.components(0) key, ak5data))}\n"
          applyIfPresent[String](segAK5.components(1) key, ak5data, value => {
            builder ++= " Error codes: "
            (1 to 5) foreach (i => applyIfPresent[String](segAK5.components(i).key, ak5data,
              value => builder ++= s" ${TransactionSyntaxErrors(value)}"))
            builder ++= "\n"
          })
        }}))
    val ak9data = getRequiredValueMap(trans997Keys(3), ackhead)
    builder ++= s"Group result: ${GroupAcknowledgmentCodes(getRequiredString(segAK9.components(0) key, ak9data))}, contained ${getRequiredInt(segAK9.components(1) key, ak9data)} transaction set(s) with ${getRequiredInt(segAK9.components(2) key, ak9data)} received and ${getRequiredInt(segAK9.components(3) key, ak9data)} accepted\n"
    applyIfPresent[String](segAK9.components(4) key, ak9data, value => {
      builder ++= " Error codes: "
      (4 to 8) foreach (i => applyIfPresent[String](segAK9.components(i).key, ak9data,
        value => builder ++= s" ${GroupSyntaxErrors(value)}"))
      builder ++= "\n"
    })
    builder toString
  }
}