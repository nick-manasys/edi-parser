package com.anypoint.df.edi.schema.tools

import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.schema.{ SchemaJavaDefs, X12SchemaDefs, EdiSchema }
import com.anypoint.df.edi.schema.EdiSchema._
import com.anypoint.df.edi.schema.SchemaJavaValues._
import com.anypoint.df.edi.schema.X12Acknowledgment._

/** X12 999 functional acknowledgment decoder. This is set up for received 999, with the interchange information linked
  * from the transaction map.
  * TODO: refactor common code with Decode997 into base class.
  */
object Decode999 extends SchemaJavaDefs {

  import X12SchemaDefs._

  def decode(root: ValueMap) = {
    if (getRequiredString(transactionId, root) != trans999.ident) throw new IllegalArgumentException("Not a 999 transaction")
    val builder = new StringBuilder
    def decodeC030(comp: SegmentComponent, map: ValueMap) = {
      val comps = comp.asInstanceOf[EdiSchema.CompositeComponent].composite.components
      applyIfPresent[Integer](comps(0) key, map, value => builder ++= s" Element $value")
      applyIfPresent[Integer](comps(1) key, map, value => builder ++= s", position $value")
      applyIfPresent[Integer](comps(2) key, map, value => builder ++= s", repetition $value")
    }
    def decodeCTX(list: MapList) =
      if (list != null) foreachMapInList(list, { map =>
        {
          val c998Comps = segCTXComps(0).asInstanceOf[CompositeComponent].composite.components
          builder ++= s"   Context ${map get (c998Comps(0) key)}"
          applyIfPresent[String](c998Comps(1) key, map, value => builder ++= s" ($value)")
          applyIfPresent[String](segCTXComps(1) key, map, value => builder ++= s" Segment ${value}")
          applyIfPresent[String](segCTXComps(2) key, map, value => builder ++= s" at position ${value}")
          applyIfPresent[String](segCTXComps(3) key, map, value => builder ++= s" (loop $value)")
          decodeC030(segCTXComps(4), map)
          val c999Comps = segCTXComps(5).asInstanceOf[CompositeComponent].composite.components
          applyIfPresent[String](c999Comps(0) key, map, value => builder ++= s" component ${value}")
          applyIfPresent[String](c999Comps(1) key, map, value => builder ++= s":$value")
          builder ++= "\n"
        }
      })
    val inter = getRequiredValueMap(interchangeKey, root)
    builder ++= s"From ${getRequiredString(SENDER_ID_QUALIFIER, inter)}:${getRequiredString(SENDER_ID, inter)}\n"
    builder ++= s"To ${getRequiredString(RECEIVER_ID_QUALIFIER, inter)}:${getRequiredString(RECEIVER_ID, inter)}\n"
    val ackhead = getRequiredValueMap(transactionHeading, root)
    val ak1data = getRequiredValueMap(trans997Keys(1), ackhead)
    builder ++= s"Acknowledged group code ${getRequiredString(segAK1Comps(0) key, ak1data)} with control number ${getRequiredInt(segAK1.components(1) key, ak1data)}"
    applyIfPresent[String](segAK1Comps(2) key, ak1data, value => builder ++= s", version $value")
    builder ++= "\n"
    applyIfPresent[MapList](groupAK2_999 key, ackhead, list =>
      foreachMapInList(list, { map =>
        {
          val ak2data = getRequiredValueMap(groupAK2_999Keys(0), map)
          builder ++= s" Transaction ${getRequiredString(segAK2Comps(0) key, ak2data)} with control number ${getRequiredString(segAK2.components(1) key, ak2data)}"
          applyIfPresent[String](segAK2Comps(2) key, ak2data, value => builder ++= s", implementation reference $value")
          builder ++= "\n"
          applyIfPresent[MapList](groupIK3 key, map, list =>
            foreachMapInList(list, { map =>
              {
                val ik3data = getRequiredValueMap(groupIK3Keys(0), map)
                builder ++= s"  Segment ${getRequiredString(segIK3Comps(0) key, ik3data)} at position ${getRequiredInt(segIK3.components(1) key, ik3data)}"
                applyIfPresent[String](segIK3Comps(2) key, ik3data, value => builder ++= s" (loop $value)")
                applyIfPresent[String](segIK3Comps(3) key, ik3data, value => builder ++= s" has syntax error ${SegmentSyntaxErrors(value)}")
                builder ++= "\n"
                applyIfPresent[MapList](groupIK3Keys(1), map, list =>
                  foreachMapInList(list, { ik4data =>
                    {
                      builder ++= "  "
                      decodeC030(segIK4Comps(0), ik4data)
                      applyIfPresent[Integer](segIK4Comps(1) key, ik4data, value => builder ++= s" (reference $value)")
                      builder ++= s" error  ${ElementSyntaxErrors(getRequiredString(segIK4Comps(2) key, ik4data))}"
                      applyIfPresent[Integer](segIK4Comps(3) key, ik4data, value => builder ++= s" (data '$value'')")
                      builder ++= "\n"
                    }
                  }))
              }
            }))
          val ik5data = getRequiredValueMap(groupAK2_999Keys(2), map)
          builder ++= s" Transaction ${TransactionAcknowledgmentCodes(getRequiredString(segIK5Comps(0) key, ik5data))}\n"
          applyIfPresent[String](segIK5Comps(1) key, ik5data, value => {
            builder ++= " Error codes: "
            (1 to 5) foreach (i => applyIfPresent[String](segIK5Comps(i).key, ik5data,
              value => builder ++= s" ${TransactionSyntaxErrors(value)}"))
            builder ++= "\n"
          })
        }
      }))
    val ak9data = getRequiredValueMap(trans999Keys(3), ackhead)
    builder ++= s"Group result: ${GroupAcknowledgmentCodes(getRequiredString(segAK9Comps(0) key, ak9data))}, contained ${getRequiredInt(segAK9.components(1) key, ak9data)} transaction set(s) with ${getRequiredInt(segAK9.components(2) key, ak9data)} received and ${getRequiredInt(segAK9.components(3) key, ak9data)} accepted\n"
    applyIfPresent[String](segAK9Comps(4) key, ak9data, value => {
      builder ++= " Error codes: "
      (4 to 8) foreach (i => applyIfPresent[String](segAK9Comps(i).key, ak9data,
        value => builder ++= s" ${GroupSyntaxErrors(value)}"))
      builder ++= "\n"
    })
    builder toString
  }
}