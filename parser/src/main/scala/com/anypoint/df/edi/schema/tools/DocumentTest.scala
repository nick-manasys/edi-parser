package com.anypoint.df.edi.schema.tools

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.io.InputStreamReader
import scala.collection.mutable.Set
import scala.util.Failure
import scala.util.Success
import com.anypoint.df.edi.schema._
import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.X12Constants._
import com.anypoint.df.edi.lexical.EdifactConstants._

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

class DefaultX12NumberValidator extends X12NumberValidator {
  var groupNums = Set[Int]()
  var setNums = Set[String]()
  def contextToken(senderQual: String, senderId: String, receiverQual: String, receiverId: String) = ""
  def validateInterchange(num: Int, interchange: String) = {
    groupNums = Set[Int]()
    true
  }
  def validateGroup(num: Int, interchange: String, senderCode: String, receiverCode: String) = {
    setNums = Set[String]()
    groupNums.add(num)
  }
  def validateSet(number: String, interchange: String, senderCode: String, receiverCode: String) = setNums.add(number)
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

sealed abstract class DocumentTest(val schema: EdiSchema) extends SchemaJavaDefs {
  def parse(is: InputStream): ValueMap
  def prepareOutput(inmap: ValueMap): Unit
  def printDoc(map: ValueMap): String
  def printAck(map: ValueMap): String
}

case class DocumentTestX12(es: EdiSchema, config: X12ParserConfig) extends DocumentTest(es) {

  def this(sch: EdiSchema) = this(sch, X12ParserConfig(true, true, true, true, true, true, true, true, -1,
    CharacterRestriction.EXTENDED, ASCII_CHARSET, Array[IdentityInformation](), Array[IdentityInformation](), Array[String]()))

  import com.anypoint.df.edi.schema.SchemaJavaValues._
  import com.anypoint.df.edi.schema.X12Acknowledgment._
  import com.anypoint.df.edi.schema.X12SchemaDefs._

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found.
    */
  def parse(is: InputStream): ValueMap = {
    val parser = X12SchemaParser(is, schema, new DefaultX12NumberValidator, config)
    parser.parse match {
      case Success(x) => x
      case Failure(e) => throw e
    }
  }
  
  def prepareOutput(map: ValueMap) = {
    val group = getRequiredValueMap(groupKey, map)
    val inter = getRequiredValueMap(interchangeKey, map)
    swap(SENDER_ID_QUALIFIER, RECEIVER_ID_QUALIFIER, inter)
    swap(SENDER_ID, RECEIVER_ID, inter)
    swap(groupApplicationSenderKey, groupApplicationReceiverKey, group)
  }

  /** Regenerate parsed document by writing map to output. This should give a document identical to the input, except
    * for date/times and line endings following segment terminators.
    * @param map
    */
  def printDoc(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val config = X12WriterConfig(CharacterRestriction.EXTENDED, -1, ASCII_CHARSET, getRequiredString(delimiterCharacters, map), null)
    val writer = X12SchemaWriter(os, schema, new DefaultX12NumberProvider, config)
    val transacts = getRequiredValueMap(transactionsMap, map)
//    foreachListInMap(transacts, (translist: MapList) =>
//      foreachMapInList(translist, (tran: ValueMap) => prepareOutput(tran)))
    writer.write(map).get
    os.toString
  }

  /** Write 997 acknowledgement information from parse as output document.
    * @param map
    */
  def printAck(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val config = X12WriterConfig(CharacterRestriction.EXTENDED, -1, ASCII_CHARSET, getRequiredString(delimiterCharacters, map), null)
    val writer = X12SchemaWriter(os, schema, new DefaultX12NumberProvider, config)
    val outmap = new ValueMapImpl(map)
    val transactions = new ValueMapImpl
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    transactions put ("997", acks)
    outmap put (transactionsMap, transactions)
    writer.write(outmap).get
    os.toString
  }
}

case class DocumentTestEdifact(es: EdiSchema, config: EdifactParserConfig) extends DocumentTest(es) {

  def this(sch: EdiSchema) = this(sch, EdifactParserConfig(true, true, true, true, true, true, true, -1,
    ASCII_CHARSET, Array[EdifactIdentityInformation](), Array[EdifactIdentityInformation]()))

  import com.anypoint.df.edi.schema.EdifactAcknowledgment._
  import com.anypoint.df.edi.schema.SchemaJavaValues._
  import com.anypoint.df.edi.schema.EdifactSchemaDefs._

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found.
    */
  override def parse(is: InputStream): ValueMap = {
    val parser = EdifactSchemaParser(is, schema, new DefaultEdifactNumberValidator, config)
    parser.parse match {
      case Success(x) => x
      case Failure(e) => throw e
    }
  }
  
  def prepareOutput(map: ValueMap) = {
//    val group: ValueMap = getRequiredValueMap(groupKey, map)
//    val inter = getRequiredValueMap(interchangeKey, map)
//    swap(SENDER_ID_QUALIFIER, RECEIVER_ID_QUALIFIER, inter)
//    swap(SENDER_ID, RECEIVER_ID, inter)
//    swap(groupApplicationSenderKey, groupApplicationReceiverKey, group)
  }

  /** Regenerate parsed document by writing map to output. This should give a document identical to the input, except
    * for date/times and line endings following segment terminators.
    * @param map
    */
  override def printDoc(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val config = EdifactWriterConfig(SyntaxIdentifier.LEVELC, SyntaxVersion.VERSION3, -1, '.', ASCII_CHARSET, getRequiredString(delimiterCharacters, map), "")
    val writer = EdifactSchemaWriter(os, schema, new DefaultEdifactNumberProvider, config)
    val transacts = getRequiredValueMap(transactionsMap, map)
    foreachListInMap(transacts, (translist: MapList) =>
      foreachMapInList(translist, (tran: ValueMap) => {
//        val group = getRequiredValueMap(transactionGroup, tran)
//        val inter = getRequiredValueMap(groupInterchange, group)
//        tran put (transactionInterSelfQualId, getRequiredString(SENDER_ID_QUALIFIER, inter))
//        tran put (transactionInterSelfId, getRequiredString(SENDER_ID, inter))
//        tran put (transactionGroupSelfId, getRequiredString(applicationSendersKey, group))
//        tran put (transactionInterPartnerQualId, getRequiredString(RECEIVER_ID_QUALIFIER, inter))
//        tran put (transactionInterPartnerId, getRequiredString(RECEIVER_ID, inter))
//        tran put (transactionGroupPartnerId, getRequiredString(applicationReceiversKey, group))
//        tran put (transactionGroupAgencyCode, getRequiredString(responsibleAgencyKey, group))
//        tran put (transactionGroupVersionCode, getRequiredString(versionIdentifierKey, group))
//        tran put (transactionInterchangeUsage, getRequiredString(TEST_INDICATOR, inter))
      }))
    writer.write(map).get
    os.toString
  }

  /** Write CONTRL acknowledgement information from parse as output document.
    * @param map
    */
  override def printAck(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val config = EdifactWriterConfig(SyntaxIdentifier.LEVELC, SyntaxVersion.VERSION3, -1, '.', ASCII_CHARSET, getRequiredString(delimiterCharacters, map), "")
    val writer = EdifactSchemaWriter(os, schema, new DefaultEdifactNumberProvider, config)
    val outmap = new ValueMapImpl(map)
    val transactions = new ValueMapImpl
    val acks = map.get(functionalAcksGenerated).asInstanceOf[MapList]
    val ackiter = acks.iterator
    while (ackiter.hasNext) {
      val ackmap = ackiter.next
//      val set = ackmap.get(transactionSet).asInstanceOf[ValueMap]
//      val group = ackmap.get(transactionGroup).asInstanceOf[ValueMap]
//      val interchange = group.get(groupInterchange).asInstanceOf[ValueMap]
//      ackmap put (transactionInterSelfQualId, interchange.get(SENDER_ID_QUALIFIER))
//      ackmap put (transactionInterSelfId, interchange.get(SENDER_ID))
//      ackmap put (transactionGroupSelfId, group.get(applicationReceiversKey))
//      ackmap put (transactionInterPartnerQualId, interchange.get(RECEIVER_ID_QUALIFIER))
//      ackmap put (transactionInterPartnerId, interchange.get(SENDER_ID_QUALIFIER))
//      ackmap put (transactionGroupPartnerId, group.get(applicationSendersKey))
//      ackmap put (transactionGroupAgencyCode, group.get(responsibleAgencyKey))
//      ackmap put (transactionGroupVersionCode, group.get(versionIdentifierKey))
//      ackmap put (transactionInterchangeUsage, "P")
//      if (set != null) {
//        val implConv = set get (implementationConventionKey)
//        if (implConv != null) ackmap put (transactionImplConventionRef, implConv)
//      }
    }
    transactions put ("997", acks)
    outmap put (transactionsMap, transactions)
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
    val parse = new DocumentTestX12(schema)
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