package com.anypoint.df.edi.schema.tools

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import scala.util.Failure
import scala.util.Success
import com.anypoint.df.edi.schema._
import com.anypoint.df.edi.lexical.EdiConstants._
import com.anypoint.df.edi.lexical.X12Constants._
import java.io.InputStream
import com.anypoint.df.edi.schema.X12SchemaValues

case class DocumentTest(schema: EdiSchema, config: X12ParserConfig) extends X12SchemaDefs with SchemaJavaDefs {

  def this(sch: EdiSchema) = this(sch, X12ParserConfig(true, true, true, true, true, true, true, true, true, true,
    true, ASCII_CHARSET, Array[IdentityInformation](), Array[IdentityInformation]()))

  import com.anypoint.df.edi.schema.X12Acknowledgment._
  import com.anypoint.df.edi.schema.SchemaJavaValues._
  import com.anypoint.df.edi.schema.X12SchemaValues._

  /** Reads a schema and parses one or more documents using that schema, reporting if any errors are found.
    */
  def parse(is: InputStream): ValueMap = {
    val parser = X12SchemaParser(is, schema, config)
    parser.parse match {
      case Success(x) => x
      case Failure(e) => throw e
    }
  }
  
  class DefaultNumberProvider extends NumberProvider {
    var number = 0
    def nextInterchange = {
      number += 1
      number
    }
  }

  /** Regenerate parsed document by writing map to output. This should give a document identical to the input, except
    * for data/times and line endings following segment terminators.
    * @param map
    */
  def printDoc(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val config = X12WriterConfig(CharacterSet.BASIC, -1, ASCII_CHARSET, getRequiredString(delimiterCharacters, map))
    val writer = X12SchemaWriter(os, schema, new DefaultNumberProvider, config)
    val transacts = getRequiredValueMap(transactionsMap, map)
    foreachListInMap(transacts, (translist: MapList) => 
      foreachMapInList(translist, (tran: ValueMap) => {
        val group = getRequiredValueMap(transactionGroup, tran)
        val inter = getRequiredValueMap(groupInterchange, group)
        tran put (transactionInterSelfQualId, getRequiredString(SENDER_ID_QUALIFIER, inter))
        tran put (transactionInterSelfId, getRequiredString(SENDER_ID, inter))
        tran put (transactionGroupSelfId, getRequiredString(applicationSendersKey, group))
        tran put (transactionInterPartnerQualId, getRequiredString(RECEIVER_ID_QUALIFIER, inter))
        tran put (transactionInterPartnerId, getRequiredString(RECEIVER_ID, inter))
        tran put (transactionGroupPartnerId, getRequiredString(applicationReceiversKey, group))
      }))
    writer.write(map).get
    os.toString
  }

  /** Write 997 acknowledgement information from parse as output document.
    * @param map
    */
  def printAck(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val config = X12WriterConfig(CharacterSet.BASIC, -1, ASCII_CHARSET, getRequiredString(delimiterCharacters, map))
    val writer = X12SchemaWriter(os, schema, new DefaultNumberProvider, config)
    val outmap = new ValueMapImpl(map)
    val transactions = new ValueMapImpl
    val acks = map.get(functionalAcknowledgments).asInstanceOf[MapList]
    val ackiter = acks.iterator
    while (ackiter.hasNext) {
      val ackmap = ackiter.next
      val set = ackmap.get(transactionSet).asInstanceOf[ValueMap]
      val group = ackmap.get(transactionGroup).asInstanceOf[ValueMap]
      val interchange = group.get(groupInterchange).asInstanceOf[ValueMap]
      ackmap put (transactionInterSelfQualId, interchange.get(SENDER_ID_QUALIFIER))
      ackmap put (transactionInterSelfId, interchange.get(SENDER_ID))
      ackmap put (transactionGroupSelfId, group.get(applicationReceiversKey))
      ackmap put (transactionInterPartnerQualId, interchange.get(RECEIVER_ID_QUALIFIER))
      ackmap put (transactionInterPartnerId, interchange.get(SENDER_ID_QUALIFIER))
      ackmap put (transactionGroupPartnerId, group.get(applicationSendersKey))
      if (set != null) {
        val implConv = set get (implementationConventionKey)
        if (implConv != null) ackmap put (transactionImplConventionRef, implConv)
      }
    }
    transactions put ("997", acks)
    outmap put (transactionsMap, transactions)
    outmap put (delimiterCharacters, "*>U~")
    outmap put (characterEncoding, "UTF-8")
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
    val schema = YamlReader.loadYaml(new InputStreamReader(new FileInputStream(schemaFile)), Array(args(1)))
    val parse = new DocumentTest(schema)
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