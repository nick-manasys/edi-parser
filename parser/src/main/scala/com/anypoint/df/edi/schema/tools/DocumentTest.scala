package com.anypoint.df.edi.schema.tools

import java.io.{ ByteArrayOutputStream, File, FileInputStream, InputStream, InputStreamReader }
import java.{ util => ju }

import scala.collection.mutable.Set
import scala.util.{ Failure, Success }

import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.lexical.EdifactConstants._
import com.anypoint.df.edi.schema._
import com.anypoint.df.edi.schema.EdifactSchemaDefs._

class DefaultX12NumberProvider extends X12NumberProvider {
  var interNum = 0
  var groupNum = 0
  var setNum = 0
  def interchangIdentifier(senderQual: String, senderId: String, receiverQual: String, receiverId: String) = ""
  def nextInterchange(interchange: String) = {
    interNum += 1
    interNum
  }
  def nextGroup(interchange: String, senderCode: String, receiverCode: String) = {
    groupNum += 1
    groupNum
  }
  def nextSet(interchange: String, senderCode: String, receiverCode: String) = {
    setNum += 1
    setNum.toString
  }
}

class DefaultX12EnvelopeHandler(config: X12ParserConfig, schema: EdiSchema) extends X12EnvelopeHandler with SchemaJavaDefs {

  import X12Acknowledgment._
  import X12SchemaDefs._

  var groupNums = Set[Int]()
  var setNums = Set[String]()
  var groupCode = ""

  /** Handle ISA segment data, returning either an InterchangeNoteCode (if there's a problem that prevents processing of
    * the interchange) or the parser configuration to be used for reading the interchange.
    */
  def handleIsa(map: ju.Map[String, Object]) = {
    groupNums = Set[Int]()
    config
  }

  /** Handle GS segment data, returning either an GroupSyntaxError (if there's a problem that prevents processing of
    * the group) or null.
    */
  def handleGs(map: ju.Map[String, Object]) = {
    setNums = Set[String]()
    groupCode = getRequiredString(groupFunctionalIdentifierKey, map)
    val unique = groupNums.add(getRequiredInt(groupControlNumberHeaderKey, map))
    if (!unique) GroupControlNumberNotUnique else null
  }

  /** Handle ST segment data, returning either a StructureSyntaxError (if there's a problem that prevents processing of
    * the transaction set) or the transaction schema definition for parsing and validating the transaction set data.
    */
  def handleSt(map: ju.Map[String, Object]) = {
    val unique = setNums.add(getRequiredString(setControlNumberHeaderKey, map))
    if (!unique) X12Acknowledgment.BadTransactionSetControl
    else {
      val setId = getAsString(setIdentifierCodeKey, map)
      val structure = schema.structures.get(setId) match {
        case Some(structure) => structure
        case None => setId match {
          case "997" => trans997
          case "999" => trans999
          case _ => null
        }
      }
      if (structure == null) NotSupportedTransaction
      else if (structure.group.get == groupCode) structure else SetNotInGroup
    }
  }
}

class DefaultEdifactNumberProvider extends EdifactNumberProvider {
  var interNum = 0
  var groupNum = 0
  var setNum = 0
  def contextToken(senderQual: String, senderId: String, receiverQual: String, receiverId: String) = ""
  def nextInterchange(context: String) = {
    interNum += 1
    interNum.toString
  }
  def nextGroup(context: String, senderQual: String, senderId: String, receiverQual: String, receiverId: String) = {
    groupNum += 1
    groupNum.toString
  }
  def nextMessage(context: String, msgType: String, msgVersion: String, msgRelease: String, agency: String) = {
    setNum += 1
    setNum.toString
  }
}

class DefaultEdifactNumberValidator extends EdifactNumberValidator {
  var groupRefs = Set[String]()
  var setRefs = Set[String]()
  def contextToken(senderId: String, senderCode: String, senderInternal: String, senderInternalSub: String,
    receiverId: String, receiverCode: String, receiverInternal: String, receiverInternalSub: String) = ""
  def validateInterchange(controlRef: String, context: String) = {
    groupRefs = Set[String]()
    true
  }
  def validateGroup(groupRef: String, senderId: String, senderCode: String, receiverId: String, receiverCode: String,
    context: String) = {
    setRefs = Set[String]()
    groupRefs.add(groupRef)
  }
  def validateMessage(msgRef: String, msgType: String, msgVersion: String, msgRelease: String, agencyCode: String,
    associationCode: String, directoryVersion: String, subFunction: String, context: String) = setRefs.add(msgRef)
}

class DefaultHL7NumberProvider extends HL7NumberProvider {
  var msgNum = 0
  def nextMessage(sender: HL7Identity.HL7IdentityInformation, receiver: HL7Identity.HL7IdentityInformation) = {
    msgNum += 1
    msgNum.toString
  }
}

class DefaultHL7NumberValidator extends HL7NumberValidator {
  import HL7Identity._
  var msgNums = Set[String]()
  def validateMessage(sender: HL7IdentityInformation, receiver: HL7IdentityInformation, control: String) =
    msgNums.add(control)
}

sealed abstract class DocumentTest(val schema: EdiSchema) extends SchemaJavaDefs {

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found. */
  def parse(is: InputStream): ValueMap

  /** Prepare input message for output, reversing the sender and receiver values. */
  def prepareOutput(inmap: ValueMap): Unit

  /** Regenerate parsed document by writing map to output. This should give a document identical to the input, except
    * for date/times and line endings following segment terminators.
    */
  def printDoc(map: ValueMap): String

  /** Write functional acknowledgement information from parse as output document. */
  def printAck(map: ValueMap): String
}

case class DocumentTestX12(es: EdiSchema, config: X12ParserConfig) extends DocumentTest(es) {

  def this(sch: EdiSchema, ack999: Boolean) = this(sch, X12ParserConfig(true, true, true, true, true, true, true, true,
    ack999, -1, CharacterRestriction.EXTENDED))

  import com.anypoint.df.edi.schema.SchemaJavaValues._
  import com.anypoint.df.edi.schema.X12Acknowledgment._
  import com.anypoint.df.edi.schema.X12SchemaDefs._

  val transactionListKey = s"v${es.ediVersion.version}"

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found. */
  def parse(is: InputStream): ValueMap = {
    val parser = new X12InterchangeParser(is, ASCII_CHARSET, new DefaultX12EnvelopeHandler(config, schema))
    parser.parse match {
      case Success(x) => x
      case Failure(e) => throw e
    }
  }

  /** Prepare input message for output, reversing the sender and receiver values. */
  def prepareOutput(map: ValueMap) = {
    val group = getRequiredValueMap(groupKey, map)
    val inter = getRequiredValueMap(interchangeKey, map)
    swap(SENDER_ID_QUALIFIER, RECEIVER_ID_QUALIFIER, inter)
    swap(SENDER_ID, RECEIVER_ID, inter)
    swap(groupApplicationSenderKey, groupApplicationReceiverKey, group)
  }

  /** Regenerate parsed document by writing map to output. This should give a document identical to the input, except
    * for date/times and line endings following segment terminators.
    */
  def printDoc(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    if (!map.containsKey(interchangeKey)) map put (interchangeKey, new ValueMapImpl)
    val config = X12WriterConfig(CharacterRestriction.EXTENDED, -1, ASCII_CHARSET, getRequiredString(delimiterCharacters, map), null)
    val writer = X12SchemaWriter(os, new DefaultX12NumberProvider, config)
    val transacts = getRequiredValueMap(transactionListKey, map)
    writer.write(map).get
    os.toString
  }

  /** Write 997/999 acknowledgement information from parse as output document. */
  def printAck(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val wconfig = X12WriterConfig(CharacterRestriction.EXTENDED, -1, ASCII_CHARSET, getRequiredString(delimiterCharacters, map), null)
    val writer = X12SchemaWriter(os, new DefaultX12NumberProvider, wconfig)
    val outmap = new ValueMapImpl(map)
    val structures = new ValueMapImpl
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    val ackcode = if (config generate999) "999" else "997"
    structures put (ackcode, acks)
    outmap put (transactionListKey, structures)
    if (!outmap.containsKey(interchangeKey)) outmap put (interchangeKey, new ValueMapImpl)
    writer.write(outmap).get
    os.toString
  }

  /** Write interchange acknowledgement information from parse as output. */
  def printInterchangeAcks(delims: String, list: MapList) = {
    val os = new ByteArrayOutputStream
    val config = X12WriterConfig(CharacterRestriction.EXTENDED, -1, ASCII_CHARSET, delims, null)
    val writer = X12SchemaWriter(os, new DefaultX12NumberProvider, config)
    foreachMapInList(list, map => writer.writeSegment(map, segTA1))
    writer.close
    os.close
    os.toString
  }
}

case class DocumentTestEdifact(es: EdiSchema, config: EdifactParserConfig) extends DocumentTest(es) {

  def this(sch: EdiSchema) = this(sch, EdifactParserConfig(true, true, true, true, true, true, true, false, -1, null,
    Array[EdifactIdentityInformation](), Array[EdifactIdentityInformation]()))

  import com.anypoint.df.edi.schema.EdifactAcknowledgment._
  import com.anypoint.df.edi.schema.SchemaJavaValues._

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found. */
  override def parse(is: InputStream): ValueMap = {
    val parser = EdifactSchemaParser(is, schema, new DefaultEdifactNumberValidator, config)
    parser.parse match {
      case Success(x) => x
      case Failure(e) => throw e
    }
  }

  /** Prepare input message for output, reversing the sender and receiver values. */
  def prepareOutput(map: ValueMap) = {
    val inter = getRequiredValueMap(interchangeKey, map)
    swap(interHeadSenderQualKey, interHeadRecipientQualKey, inter)
    swap(interHeadSenderIdentKey, interHeadRecipientIdentKey, inter)
    if (map.containsKey(groupKey)) {
      val group = getRequiredValueMap(groupKey, map)
      swap(groupHeadSenderQualKey, groupHeadRecipientQualKey, inter)
      swap(groupHeadSenderIdentKey, groupHeadRecipientIdentKey, group)
    }
  }

  /** Regenerate parsed document by writing map to output. This should give a document identical to the input, except
    * for date/times and line endings following segment terminators.
    */
  override def printDoc(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val inter = getRequiredValueMap(interchangeKey, map)
    val syntax = EDIFACT_CHARSETS.get(getAs(unbSyntax.components(0).key, "UNOA", inter))
    val version = EDIFACT_VERSIONS.get(getAs(unbSyntax.components(1).key, "4", inter))
    val config = EdifactWriterConfig(syntax, version, false, -1, '.', ASCII_CHARSET,
      getAs(delimiterCharacters, null, map), "", false)
    val writer = EdifactSchemaWriter(os, schema.merge(contrlMsg(version)), new DefaultEdifactNumberProvider, config)
    val transacts = getRequiredValueMap(messagesMap, map)
    writer.write(map).get
    os.toString
  }

  /** Write CONTRL acknowledgement information from parse as output document. */
  override def printAck(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val version = EDIFACT_VERSIONS.get(getAs(unbSyntax.components(1).key, "4", getAsMap(interchangeKey, map)))
    val config = EdifactWriterConfig(LEVELA, SyntaxVersion.VERSION3, false, -1, '.', ASCII_CHARSET,
      getAs(delimiterCharacters, "", map), "", false)
    val writer = EdifactSchemaWriter(os, schema.merge(contrlMsg(version)), new DefaultEdifactNumberProvider, config)
    val outmap = new ValueMapImpl
    outmap put (interchangeKey, map.get(interchangeKey))
    val structures = new ValueMapImpl
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    structures put ("CONTRL", acks)
    outmap put (messagesMap, structures)
    writer.write(outmap).get
    os.toString
  }
}

object DocumentTest {

  /** Reads a schema and base schema path, then parses one or more documents using that schema, reporting if any errors
    * are found.
    */
  def main(args: Array[String]): Unit = {
    val schemaFile = new File(args(0))
    val schema = new YamlReader().loadYaml(new InputStreamReader(new FileInputStream(schemaFile)), Array(args(1)))
    val parse = schema.ediVersion.ediForm match {
      case EdiSchema.X12 => new DocumentTestX12(schema, false)
      case EdiSchema.EdiFact => new DocumentTestEdifact(schema)
    }
    val examples = args.toList.tail.tail
    examples.map (path => {
      val is = new FileInputStream(new File(path))
      val result = parse.parse(is)
      println(s"Parsed document $path, acknowledgment is:")
      println(parse.printAck(result))
    })
    println("all documents parsed successfully")
  }
}