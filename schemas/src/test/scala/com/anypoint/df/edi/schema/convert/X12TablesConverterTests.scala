package com.anypoint.df.edi.schema.convert

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import java.io.ByteArrayInputStream

/** Tests for X12 table data converter.
 */
class X12TablesConverterTests extends FlatSpec with Matchers {

  import X12TablesConverter._
  
  def stringStream(text: String) = new ByteArrayInputStream(text.getBytes("UTF-8"))

  behavior of "splitValues"

  it should "split comma-separated quoted values from string" in {
    splitValues(""""a"""") should be (List("a"))
    splitValues(""""a","b","c","d"""") should be (List("a", "b", "c", "d"))
  }
  
  it should "throw an exception when values are not quoted" in {
      intercept[IllegalArgumentException] { splitValues("""a""") }
    intercept[IllegalArgumentException] { splitValues(""""a""") }
    intercept[IllegalArgumentException] { splitValues(""""a",b","c","d"""") }
  }
  
  "foldInput" should "accumulate results of applying operation to lists of values" in {
    // tests build reversed list of first value from each line, reversed
    foldInput(stringStream(""""abc""""), List[String]())((acc, list) => list.head.reverse :: acc) should be (List("cba"))
    foldInput(stringStream(""""abc","def""""), List[String]())((acc, list) => list.head.reverse :: acc) should be (List("cba"))
    val multi1 = """"abc","def"
      |"hij","klm"
      |"nop","qrs"""".stripMargin('|')
    foldInput(stringStream(multi1), List[String]())((acc, list) => list.head.reverse :: acc) should be (List("pon", "jih", "cba"))
  }
  
  behavior of "nameMap"
  
  it should "build map of ids to names from two-column input" in {
    val multi1 = """"abc","def"
      |"hij","klm"
      |"nop","qrs"""".stripMargin('|')
    nameMap(stringStream(multi1)).toList.sorted should be (List(("abc", "def"), ("hij", "klm"), ("nop", "qrs")))
  }
  
  it should "throw an exception when the wrong number of values are present" in {
    val multi1 = """"abc","def"
      |"abc","hij","xxx"
      |"abc","klm"
      |"nop","qrs"
      |"nop","tuv"""".stripMargin('|')
    intercept[IllegalArgumentException] { nameMap(stringStream(multi1)) }
  }
  
  behavior of "gatherGroups"
  
  it should "group lists into sublists by first column value" in {
    val multi1 = """"abc","def"
      |"abc","hij"
      |"abc","klm"
      |"nop","qrs"
      |"nop","tuv"""".stripMargin('|')
    gatherGroups("", stringStream(multi1), 2, None) should be
      (List(("abc", List(List("def"), List("hij"), List("klm")), ("nop", List(List("qrs"), List("tuv"))))))
  }
  
  it should "throw an exception when the wrong number of values are present" in {
    val multi1 = """"abc","def"
      |"abc","hij","xxx"
      |"abc","klm"
      |"nop","qrs"
      |"nop","tuv"""".stripMargin('|')
    intercept[IllegalArgumentException] { gatherGroups("", stringStream(multi1), 2, None) }
  }
  
  it should "allow a fill value to be used for the last column" in {
    val multi1 = """"abc","def"
      |"abc","hij","xxx"
      |"abc","klm"
      |"nop","qrs"
      |"nop","tuv"""".stripMargin('|')
    gatherGroups("", stringStream(multi1), 3, Some("zyx")) should be
      (List(("abc", List(List("def", "zyx"), List("hij", "xxx"), List("klm", "zyx")), ("nop", List(List("qrs", "zyx"), List("tuv", "zyx"))))))
  }
}