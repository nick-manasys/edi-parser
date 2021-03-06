package com.mulesoft.flatfile.schema.tools

import java.io.{ ByteArrayOutputStream, File, FileInputStream, InputStream, InputStreamReader }
import java.{ util => ju }
import scala.collection.mutable.Set
import scala.util.{ Failure, Success }
import com.mulesoft.flatfile.lexical.EdiConstants._
import com.mulesoft.flatfile.schema._
import java.io.StringWriter

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
    * the interchange) or the parser configuration to be used for reading the interchange (or <code>null</code> if
    * default parser configuration to be used).
    */
  def handleIsa(map: ju.Map[String, Object]) = {
    groupNums = Set[Int]()
    null
  }

  /** Handle GS segment data, returning either an GroupSyntaxError (if there's a problem that prevents processing of
    * the group) or the parser configuration to be used for reading the interchange (or <code>null</code> if to use the
    * parser configuration previously set).
    */
  def handleGs(map: ju.Map[String, Object]) = {
    setNums = Set[String]()
    groupCode = getRequiredString(groupFunctionalIdentifierKey, map)
    val unique = groupNums.add(getRequiredInt(groupControlNumberHeaderKey, map))
    if (unique) config else GroupControlNumberNotUnique
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
    * interchange) or the parser configuration to be used for the interchange (or <code>null</code> if default parser
    * configuration to be used).
    */
  def handleUnb(map: ju.Map[String, Object]) = {
    groupRefs = Set[String]()
    null
  }

  /** Handle UNG segment data, returning either an SyntaxError (if there's a problem that prevents processing of the
    * group) or null.
    */
  def handleUng(map: ju.Map[String, Object]) = {
    val groupRef = getRequiredString(groupHeadReferenceKey, map)
    msgRefs = Set[String]()
    if (groupRefs.add(groupRef)) null else EdifactHandlerError(DuplicateDetected, DuplicateDetected.text)
  }

  /** Handle UNH segment data, returning either a SyntaxError (if there's a problem that prevents processing of the
    * message) or the structure configuration with message schema definition for parsing and validating the message
    * data and optional parser configuration.
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
      if (structure == null || (getAsString(msgHeadMessageVersionKey, map) +
        getAsString(msgHeadMessageReleaseKey, map)).toLowerCase() != schema.ediVersion.version) NoAgreementForValue
      else EdifactStructureConfig(structure, config)
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

class DefaultHL7EnvelopeHandler(structure: EdiSchema.Structure) extends HL7EnvelopeHandler {
  def handleMsh(map: ju.Map[String, Object]) = structure
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
  
  import com.mulesoft.flatfile.lexical.X12Constants._

  def this(sch: EdiSchema, ack999: Boolean) = this(sch, X12ParserConfig(true, true, true, true, true, true, true, true,
    ack999, -1, com.mulesoft.flatfile.lexical.X12Constants.CharacterRestriction.EXTENDED))

  import com.mulesoft.flatfile.schema.SchemaJavaValues._
  import com.mulesoft.flatfile.schema.X12Acknowledgment._
  import com.mulesoft.flatfile.schema.X12SchemaDefs._

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
    val config = X12WriterConfig(true, CharacterRestriction.EXTENDED, -1, ASCII_CHARSET,
      getRequiredString(delimiterCharacters, map), null)
    val writer = X12SchemaWriter(os, new DefaultX12NumberProvider, config)
    writer.write(map).get
    os.toString
  }

  /** Write 997/999 acknowledgement information from parse as output document. */
  def printAck(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    if (acks.size > 0) {
      val wconfig = X12WriterConfig(true, CharacterRestriction.EXTENDED, -1, ASCII_CHARSET,
        getRequiredString(delimiterCharacters, map), null)
      val writer = X12SchemaWriter(os, new DefaultX12NumberProvider, wconfig)
      val outmap = new ValueMapImpl(map)
      val versions = new ValueMapImpl
      val ackcode = if (config generate999) "999" else "997"
      val structures = new ValueMapImpl
      val group = getAs(groupKey, getRequiredValueMap(groupKey, acks.iterator.next), map)
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
    val config = X12WriterConfig(true, CharacterRestriction.EXTENDED, -1, ASCII_CHARSET, delims, null)
    val writer = X12SchemaWriter(os, new DefaultX12NumberProvider, config)
    foreachMapInList(list, map => writer.writeSegment(map, segTA1))
    writer.close
    os.close
    os.toString
  }
}

case class DocumentTestEdifact(es: EdiSchema, config: EdifactParserConfig) extends DocumentTest(es) {
  
  import com.mulesoft.flatfile.lexical.EdifactConstants._
  import com.mulesoft.flatfile.schema.EdifactSchemaDefs._

  def this(sch: EdiSchema) = this(sch, EdifactParserConfig(true, false, true, true, true, true, true, false, -1))

  import com.mulesoft.flatfile.schema.EdifactAcknowledgment._
  import com.mulesoft.flatfile.schema.SchemaJavaValues._

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found. */
  override def parse(is: InputStream): ValueMap = {
    val parser = new EdifactInterchangeParser(is, null, new DefaultEdifactEnvelopeHandler(config, schema))
    parser.parse match {
      case Success(x) => x
      case Failure(e) => throw e
    }
  }
  
  def hasMessage(root: ValueMap) = {
    if (root.containsKey(messagesMap)) {
        val messages = getRequiredValueMap(messagesMap, root)
        if (messages.size > 0) {
            val version = messages.values().iterator.next.asInstanceOf[ValueMap]
            !version.isEmpty
        }
    }
    false
  }
  
  def getMessage(root: ValueMap) = {
    val messages = getRequiredValueMap(messagesMap, root)
    val version = messages.values().iterator.next.asInstanceOf[ValueMap]
    version.values().iterator.next.asInstanceOf[MapList].iterator.next
  }

  /** Prepare input message for output, reversing the sender and receiver values. */
  def prepareOutput(map: ValueMap) = if (hasMessage(map)) {
    val message = getMessage(map)
    val inter = getRequiredValueMap(interchangeKey, message)
    swap(interHeadSenderQualKey, interHeadRecipientQualKey, inter)
    swap(interHeadSenderIdentKey, interHeadRecipientIdentKey, inter)
    if (message.containsKey(groupKey)) {
      val group = getRequiredValueMap(groupKey, message)
      swap(groupHeadSenderQualKey, groupHeadRecipientQualKey, inter)
      swap(groupHeadSenderIdentKey, groupHeadRecipientIdentKey, group)
    }
  }

  /** Regenerate parsed document by writing map to output. This form allows specifying whether character restrictions
   * are enforced when writing.
    */
  def printDoc(map: ValueMap, enforce: Boolean) = {
    val os = new ByteArrayOutputStream
    val inter = getRequiredValueMap(interchangeKey, getMessage(map))
    val syntax = EDIFACT_CHARSETS.get(getAs(unbSyntax.components(0).key, "UNOA", inter))
    val version = EDIFACT_VERSIONS.get(getAs(unbSyntax.components(1).key, "4", inter))
    val config = EdifactWriterConfig(syntax, version, enforce, true, -1, '.', ASCII_CHARSET,
      getAs(delimiterCharacters, null, map), "", false)
    val writer = EdifactSchemaWriter(os, new DefaultEdifactNumberProvider, config)
    val transacts = getRequiredValueMap(messagesMap, map)
    writer.write(map).get
    os.toString
  }

  /** Regenerate parsed document by writing map to output. This should give a document identical to the input, except
    * for date/times and line endings following segment terminators.
    */
  override def printDoc(map: ValueMap) = {
    printDoc(map, false)
  }

  /** Write CONTRL acknowledgement information from parse as output document. */
  override def printAck(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val inter = getRequiredValueMap(interchangeKey, getRequiredMapList(functionalAcksGenerated, map).iterator.next)
    val version = EDIFACT_VERSIONS.get(getAs(unbSyntax.components(1).key, "4", inter))
    val config = EdifactWriterConfig(LEVELA, SyntaxVersion.VERSION3, false, true, -1, '.', ASCII_CHARSET,
      getAs(delimiterCharacters, "", map), "", false)
    val writer = EdifactSchemaWriter(os, new DefaultEdifactNumberProvider, config)
    val outmap = new ValueMapImpl
    outmap put (interchangeKey, inter)
    outmap put (functionalAcksToSend, map.get(functionalAcksGenerated))
    outmap put (messagesMap, new ValueMapImpl)
    writer.write(outmap).get
    os.toString
  }
}

class DocumentTestHL7(es: EdiSchema, structId: String, config: HL7ParserConfig) extends DocumentTest(es) {
  
  def this(sch: EdiSchema, sid: String) = this(sch, sid, HL7ParserConfig(true, true, true, true, true, true, true, -1,
    Array[HL7Identity.HL7IdentityInformation](), Array[HL7Identity.HL7IdentityInformation]()))
  
  def this(sch: EdiSchema) = this(sch, sch.structures.head._1, HL7ParserConfig(true, true, true, true, true, true, true,
    -1, Array[HL7Identity.HL7IdentityInformation](), Array[HL7Identity.HL7IdentityInformation]()))
  
  lazy val structSchema = schema.structures(structId)

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found. */
  override def parse(is: InputStream): ValueMap = {
    val parser = HL7SchemaParser(is, new DefaultHL7EnvelopeHandler(structSchema), config)
    parser.parse match {
      case Success(x) => x
      case Failure(e) => throw e
    }
  }

  /** Prepare input message for output, reversing the sender and receiver values. */
  override def prepareOutput(map: ValueMap) = {}

  /** Regenerate parsed document by writing map to output. This should give a document identical to the input, except
    * for date/times and line endings following segment terminators.
    */
  override def printDoc(map: ValueMap) = {
//    val swriter = new StringWriter
//    YamlSupport.writeMap(map, swriter)
//    println(swriter.toString())
    val out = new ByteArrayOutputStream
    val writer = HL7SchemaWriter(out, structSchema, new DefaultHL7NumberProvider, HL7WriterConfig(false, -1, ASCII_CHARSET, "|^~\\&"))
    writer.write(map).get //isSuccess should be (true)
    out.toString
  }

  /** Write acknowledgement information from parse as output document. */
  override def printAck(map: ValueMap) = {
    throw new UnsupportedOperationException("HL7 acknowledgments not used")
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
      case EdiSchema.HL7 => new DocumentTestHL7(schema, schema.structures.keys.head)
      case _ => throw new IllegalArgumentException("Schema type not supported by test")
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