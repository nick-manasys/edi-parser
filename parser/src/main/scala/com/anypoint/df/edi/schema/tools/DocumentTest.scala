package com.anypoint.df.edi.schema.tools

import java.io.{ ByteArrayOutputStream, File, FileInputStream, InputStream, InputStreamReader }
import java.{ util => ju }

import scala.collection.mutable.Set
import scala.util.{ Failure, Success }

import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.schema._

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

class DefaultX12EnvelopeHandler(config: X12ParserConfig, schema: EdiSchema) extends X12EnvelopeHandler
    with SchemaJavaDefs {

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
    if (unique) null else GroupControlNumberNotUnique
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

class DefaultEdifactEnvelopeHandler(config: EdifactParserConfig, schema: EdiSchema) extends EdifactEnvelopeHandler
    with SchemaJavaDefs {

  import EdifactAcknowledgment._
  import EdifactSchemaDefs._

  var groupRefs = Set[String]()
  var msgRefs = Set[String]()

  /** Handle UNB segment data, returning either a SyntaxError (if there's a problem that prevents processing of the
    * interchange) or the parser configuration to be used for the interchange.
    */
  def handleUnb(map: ju.Map[String, Object]) = {
    groupRefs = Set[String]()
    config
  }

  /** Handle UNG segment data, returning either an SyntaxError (if there's a problem that prevents processing of the
    * group) or null.
    */
  def handleUng(map: ju.Map[String, Object]) = {
    val groupRef = getRequiredString(groupHeadReferenceKey, map)
    msgRefs = Set[String]()
    if (groupRefs.add(groupRef)) null else DuplicateDetected
  }

  /** Handle UNH segment data, returning either a SyntaxError (if there's a problem that prevents processing of the
    * message) or the message schema definition for parsing and validating the message data.
    */
  def handleUnh(map: ju.Map[String, Object]) = {
    val msgRef = getRequiredString(msgHeadReferenceKey, map)
    val unique = msgRefs.add(msgRef)
    if (unique) {
      val msgType = getAsString(msgHeadMessageTypeKey, map)
      val structure = schema.structures.get(msgType) match {
        case Some(structure) => structure
        case None => if (msgType == "CONTRL") transCONTRLv4 else null
      }
      if (structure == null) NoAgreementForValue
      else structure
    } else DuplicateDetected
  }
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

  /** Parse input message, reporting if any errors are found. */
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
  
  import com.anypoint.df.edi.lexical.X12Constants._

  def this(sch: EdiSchema, ack999: Boolean) = this(sch, X12ParserConfig(true, true, true, true, true, true, true, true,
    ack999, -1, com.anypoint.df.edi.lexical.X12Constants.CharacterRestriction.EXTENDED))

  import com.anypoint.df.edi.schema.SchemaJavaValues._
  import com.anypoint.df.edi.schema.X12Acknowledgment._
  import com.anypoint.df.edi.schema.X12SchemaDefs._

  /** Parse input message, reporting if any errors are found. */
  def parse(is: InputStream): ValueMap = {
    val parser = new X12InterchangeParser(is, ASCII_CHARSET, new DefaultX12EnvelopeHandler(config, es))
    parser.parse match {
      case Success(x) => x
      case Failure(e) => throw e
    }
  }

  /** Prepare input message for output, reversing the sender and receiver values. */
  def prepareOutput(map: ValueMap) = {
    val inter = getAsMap(interchangeKey, map)
    if (inter != null) {
        swap(SENDER_ID_QUALIFIER, RECEIVER_ID_QUALIFIER, inter)
        swap(SENDER_ID, RECEIVER_ID, inter)
    }
    val group = getAsMap(groupKey, map)
    if (group != null) swap(groupApplicationSenderKey, groupApplicationReceiverKey, group)
  }

  /** Regenerate parsed document by writing map to output. This should give a document identical to the input, except
    * for date/times and line endings following segment terminators.
    */
  def printDoc(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    if (!map.containsKey(interchangeKey)) map put (interchangeKey, new ValueMapImpl)
    val config = X12WriterConfig(CharacterRestriction.EXTENDED, -1, ASCII_CHARSET, getRequiredString(delimiterCharacters, map), null)
    val writer = X12SchemaWriter(os, new DefaultX12NumberProvider, config)
    writer.write(map).get
    os.toString
  }

  /** Write 997/999 acknowledgement information from parse as output document. */
  def printAck(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    if (acks.size > 0) {
      val wconfig = X12WriterConfig(CharacterRestriction.EXTENDED, -1, ASCII_CHARSET,
        getRequiredString(delimiterCharacters, map), null)
      val writer = X12SchemaWriter(os, new DefaultX12NumberProvider, wconfig)
      val outmap = new ValueMapImpl(map)
      val versions = new ValueMapImpl
      val ackcode = if (config generate999) "999" else "997"
      val structures = new ValueMapImpl
      val group = getAs(groupKey, getRequiredValueMap(groupKey, acks.get(0)), map)
      val verkey = schema.ediVersion.ediForm.versionKey(getRequiredString(groupVersionReleaseIndustryKey, group).take(6))
      versions put (verkey, structures)
      structures put (ackcode, acks)
      outmap put (transactionsMap, versions)
      if (!outmap.containsKey(interchangeKey)) outmap put (interchangeKey, new ValueMapImpl)
      writer.write(outmap).get
    }
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
  
  import com.anypoint.df.edi.lexical.EdifactConstants._
  import com.anypoint.df.edi.schema.EdifactSchemaDefs._

  def this(sch: EdiSchema) = this(sch, EdifactParserConfig(true, true, true, true, true, true, true, false, -1))

  import com.anypoint.df.edi.schema.EdifactAcknowledgment._
  import com.anypoint.df.edi.schema.SchemaJavaValues._

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found. */
  override def parse(is: InputStream): ValueMap = {
    val parser = new EdifactInterchangeParser(is, null, new DefaultEdifactEnvelopeHandler(config, schema))
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