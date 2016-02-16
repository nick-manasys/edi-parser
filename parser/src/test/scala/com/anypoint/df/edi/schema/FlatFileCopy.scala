package com.anypoint.df.edi.schema

import java.io.FileInputStream
import java.io.File
import java.io.InputStreamReader
import java.io.FileOutputStream
import java.nio.charset.Charset

object FlatFileCopy {
  
  val testSchema = new YamlReader().loadYaml(new InputStreamReader(getClass.
    getClassLoader.getResourceAsStream("esl/QBRequest.esl"), "UTF-8"), Array())
    
  /** Reads an input flat file and writes to output file.
    */
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    val ins = new FileInputStream(new File(args(0)))
    val parser = FlatFileSchemaParser(ins, testSchema.structures.values.head)
    val data = parser.parse.get
    println(s"Input took ${(System.currentTimeMillis - start) / 1000} seconds")
    val outFile = new File(args(1))
    outFile.createNewFile()
    val out = new FileOutputStream(outFile)
    val writer = FlatFileSchemaWriter(out, testSchema.structures.values.head,
      FlatFileWriterConfig(true, Charset.forName("US-ASCII")))
    writer.write(data).get //isSuccess should be (true)
    println(s"Copy completed in total time of ${(System.currentTimeMillis - start) / 1000} seconds")
  }
}