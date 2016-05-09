package com.mulesoft.flatfile.schema

import java.io.File
import java.io.FileWriter

object FlatFileGenerator {
  val lead = """1MISSION   201308020800MISSIONAUSTRALIA              2009110401                                                                                                                                                                                           
3MISSIONAUSTRALIA              MISSION   CCCredit Card Batch 2009110401_01   AUD                                                                                                                                                                          
"""
  val tail = """70000010000010000MISSION   2009110401_01                                                                                                                                                                                                                  
90100000001000000010000MISSION   MISSIONAUSTRALIA              2009110401                                                                                                                                                                                 
"""

  def twoDigits(value: Int): String = if (value < 10) "0" + value else value.toString

  def nDigits(value: Int, digits: Int): String = {
    val text = value.toString
    val length = text.size
    if (length < digits) {
      val builder = new StringBuilder
      (length + 1 to digits) foreach { _ => builder.append('0') }
      builder.append(text)
      builder.toString
    } else text
  }

  /** Creates a test file with the specified number of detail lines.
    */
  def main(args: Array[String]): Unit = {
    val lineCount = args(0).toInt
    val outFile = new File(args(1))
    outFile.createNewFile()
    val writer = new FileWriter(outFile)
    writer.write(lead)
    (0 until lineCount).foreach { i =>
      writer.write("5  ")
      writer.write((2000 + i % 20).toString)
      writer.write(twoDigits(i % 12 + 1))
      writer.write(twoDigits(i % 28 + 1))
      writer.write("AN")
      writer.write(nDigits(i, 15))
      writer.write(nDigits(i * 17, 10))
      writer.write("CN")
      writer.write(nDigits(i, 13))
      writer.write("ACCT")
      writer.write(nDigits(i, 18))
      writer.write("TN")
      writer.write(nDigits(i, 13))
      writer.write("               ")
      writer.write(nDigits(i % 100000, 6))
      writer.write("ECI")
      writer.write("                                                                                                                                        \n")
    }
    writer.write(tail)
    writer.close
  }
}