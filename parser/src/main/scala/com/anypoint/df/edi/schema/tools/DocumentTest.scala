package com.anypoint.df.edi.schema.tools

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import scala.util.Failure
import scala.util.Success
import com.anypoint.df.edi.schema.EdiSchema
import com.anypoint.df.edi.schema.IdentityInformation
import com.anypoint.df.edi.schema.SchemaJavaDefs
import com.anypoint.df.edi.schema.X12ParserConfig
import com.anypoint.df.edi.schema.X12SchemaDefs
import com.anypoint.df.edi.schema.X12SchemaParser
import com.anypoint.df.edi.schema.X12SchemaWriter
import com.anypoint.df.edi.schema.YamlReader
import com.anypoint.df.edi.lexical.X12Constants._
import java.io.InputStream

case class DocumentTest(schema: EdiSchema, config: X12ParserConfig) extends X12SchemaDefs with SchemaJavaDefs {

  def this(sch: EdiSchema) = this(sch, X12ParserConfig(true, true, true, true, true, true, true, true, true, true,
    true, Array[IdentityInformation](), Array[IdentityInformation]()))

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

  def printAck(map: ValueMap) = {
    val os = new ByteArrayOutputStream
    val writer = X12SchemaWriter(os, schema)
    val outmap = new ValueMapImpl(map)
    val transactions = new ValueMapImpl
    val acks = map.get(acknowledgments).asInstanceOf[MapList]
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
    outmap put (delimiterCharacters, "*>U~");
    outmap put (characterEncoding, "UTF-8");
    writer.write(outmap, 1).get
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