package com.anypoint.df.edi.schema

import java.io.InputStreamReader
import java.io.StringReader
import java.io.StringWriter
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.io.Source
import java.io.ByteArrayInputStream
import java.util.GregorianCalendar
import scala.util.Success
import scala.collection.JavaConverters._
import java.util.Calendar
import java.io.ByteArrayOutputStream
import java.io.FileInputStream
import java.io.File
import org.scalactic.Equality
import scala.annotation.tailrec

class ParserErrorTests extends FlatSpec with Matchers with SchemaJavaDefs {

  import EdiSchema._
  import com.anypoint.df.edi.lexical.X12Constants._
  import SchemaJavaValues._

  implicit val mapEq = new Equality[ValueMap] {
    def areEqual(jmap: ValueMap, b: Any) = b match {
      case smap: Map[String, Any] =>
        if (jmap.size == smap.size) smap.forall {
          case (key, sval) => jmap.containsKey(key) && (jmap.get(key) match {
            case cmap: ValueMap => areEqual(cmap, sval)
            case jlist: MapList => jlist.asScala.toList == sval
            case jval => jval == sval
          })
        } else false
    }
  }

  val DATETIME = "090604*1205"
  val ISA = "ISA*00*ABC       *00*DEF       *01*013227180      *ZZ*IJDIECAFOX     *" + DATETIME + "*U*00401*000001244*0*P*>~"
  val GS = "GS*PO*006927180*IAIYUCAFOO*20080604*1205*168*X*004010~"
  val ST = "ST*850*000000176~"
  val IEA = "IEA*1*000001244~"

  def buildGE(count: Int) = s"GE*$count*168~"

  def buildSE(count: Int) = s"SE*$count*000000176~"

  behavior of "X12SchemaParser"
  
  it should "report simple data errors in 997 acknowledgment" in {
    
  }
}