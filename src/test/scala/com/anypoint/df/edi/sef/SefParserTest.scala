package com.anypoint.df.edi.sef

import org.scalatest.FlatSpec
import scala.collection.mutable.Stack
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Reader

import MessageParser._
import com.anypoint.df.edi.schema.EdiSchema._

class SefParserTest extends FlatSpec {
  
  behavior of "posInt"
  
  it should "parse a zero value" in {
    val result = posInt(new CharArrayReader("0".toArray))
    assert(result.successful)
    assert(0 === result.get)
  }
  
  it should "parse positive values" in {
    val result1 = posInt(new CharArrayReader("1".toArray))
    assert(result1.successful)
    assert(1 === result1.get)
    val result2 = posInt(new CharArrayReader("814371".toArray))
    assert(result2.successful)
    assert(814371 === result2.get)
  }
  
  it should "fail on other values" in {
    assert(!posInt(new CharArrayReader("".toArray)).successful)
    assert(!posInt(new CharArrayReader("-4".toArray)).successful)
    assert(!posInt(new CharArrayReader("abc123".toArray)).successful)
  }
  
  behavior of "position"
  
  it should "parse number values" in {
    val result1 = position(new CharArrayReader("1".toArray))
    assert(result1.successful)
    assert("1" === result1.get)
    val result2 = position(new CharArrayReader("814371".toArray))
    assert(result2.successful)
    assert("814371" === result2.get)
    val result3 = position(new CharArrayReader("0410".toArray))
    assert(result3.successful)
    assert("0410" === result3.get)
  }
  
  it should "fail on other values" in {
    assert(!position(new CharArrayReader("".toArray)).successful)
    assert(!position(new CharArrayReader("-4".toArray)).successful)
    assert(!position(new CharArrayReader("abc123".toArray)).successful)
  }
  
  behavior of "id"
  
  it should "parse alphanumeric values" in {
    val result1 = id(new CharArrayReader("1abcd".toArray))
    assert(result1.successful)
    assert("1abcd" === result1.get)
    val result2 = id(new CharArrayReader("x814371".toArray))
    assert(result2.successful)
    assert("x814371" === result2.get)
    val result3 = id(new CharArrayReader("0ab410".toArray))
    assert(result3.successful)
    assert("0ab410" === result3.get)
  }
  
  it should "fail on other values" in {
    assert(!id(new CharArrayReader("".toArray)).successful)
    assert(!id(new CharArrayReader("-4".toArray)).successful)
    assert(!id(new CharArrayReader("*abc123".toArray)).successful)
  }
  
  "maskNum" should "parse a mask number" in {
    val result = maskNum(new CharArrayReader("*5".toArray))
    assert(result.successful)
    assert(5 === result.get)
  }
  
  "ordNum" should "parse an ordinal number" in {
    val result = ordNum(new CharArrayReader("@5".toArray))
    assert(result.successful)
    assert(5 === result.get)
  }
  
  behavior of "useMark"
  
  it should "parse a mark character" in {
    val result1 = useMark(new CharArrayReader(".adadf".toArray))
    assert(result1.successful)
    assert(NotUsedMark === result1.get)
    val result2 = useMark(new CharArrayReader("!814371".toArray))
    assert(result2.successful)
    assert(MustUseMark === result2.get)
    val result3 = useMark(new CharArrayReader("$0ab410".toArray))
    assert(result3.successful)
    assert(RecommendedMark === result3.get)
  }
  
  it should "fail on other values" in {
    assert(!useMark(new CharArrayReader("".toArray)).successful)
    assert(!useMark(new CharArrayReader("(4".toArray)).successful)
    assert(!useMark(new CharArrayReader("*abc123".toArray)).successful)
  }
  
  behavior of "segReq"
  
  it should "parse a requirement character" in {
    val result1 = stdReq(new CharArrayReader("Madadf".toArray))
    assert(result1.successful)
    assert(MandatoryRequirement === result1.get)
    val result2 = stdReq(new CharArrayReader("F814371".toArray))
    assert(result2.successful)
    assert(FunctionalRequirement === result2.get)
    val result3 = stdReq(new CharArrayReader("C0ab410".toArray))
    assert(result3.successful)
    assert(ConditionalRequirement === result3.get)
  }
  
  it should "fail on other values" in {
    assert(!stdReq(new CharArrayReader("".toArray)).successful)
    assert(!stdReq(new CharArrayReader("X4".toArray)).successful)
    assert(!stdReq(new CharArrayReader("Aabc123".toArray)).successful)
  }
  
  behavior of "maxUse"
  
  it should "parse an integer count" in {
    val result1 = maxUse(new CharArrayReader(",10".toArray))
    assert(result1.successful)
    assert(FiniteUsage(10) === result1.get)
    val result2 = maxUse(new CharArrayReader(",1".toArray))
    assert(result2.successful)
    assert(FiniteUsage(1) === result2.get)
  }
  
  it should "parse an unlimited indication" in {
    val result = maxUse(new CharArrayReader(",>1".toArray))
    assert(result.successful)
    assert(UnlimitedUsage === result.get)
  }
  
  it should "fail on other values" in {
    assert(!maxUse(new CharArrayReader(".123".toArray)).successful)
    assert(!maxUse(new CharArrayReader(",".toArray)).successful)
    assert(!maxUse(new CharArrayReader(",abc".toArray)).successful)
  }
  
  behavior of "groupLead"
  
  it should "parse with only a repeat count" in {
    val result1 = groupLead(new CharArrayReader(":10".toArray))
    assert(result1.successful)
    assert((None, 10) === result1.get)
    val result2 = groupLead(new CharArrayReader(":2".toArray))
    assert(result2.successful)
    assert((None, 2) === result2.get)
  }
  
  it should "parse with only an identifier" in {
    val result1 = groupLead(new CharArrayReader("abc:".toArray))
    assert(result1.successful)
    assert((Some("abc"), 1) === result1.get)
    val result2 = groupLead(new CharArrayReader("0100:".toArray))
    assert(result2.successful)
    assert((Some("0100"), 1) === result2.get)
  }
  
  it should "parse with identifier and repeat count" in {
    val result1 = groupLead(new CharArrayReader("abc:10".toArray))
    assert(result1.successful)
    assert((Some("abc"), 10) === result1.get)
    val result2 = groupLead(new CharArrayReader("0100:2".toArray))
    assert(result2.successful)
    assert((Some("0100"), 2) === result2.get)
  }
  
  it should "fail on other values" in {
    assert(!groupLead(new CharArrayReader("123".toArray)).successful)
  }
  
  behavior of "segRef"
  
  it should "parse all forms of segment reference" in {
    val result1 = segRef(new CharArrayReader("[ABC]".toArray))
    assert(result1.successful)
    assert(SegmentReference("ABC", None, None, 0, RequirementDefault, UsageDefault) === result1.get)
    val result2 = segRef(new CharArrayReader("[ABC,M]".toArray))
    assert(result2.successful)
    assert(SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault) === result2.get)
    val result3 = segRef(new CharArrayReader("[ABC*1,M,100]".toArray))
    assert(result3.successful)
    assert(SegmentReference("ABC", None, None, 1, MandatoryRequirement, FiniteUsage(100)) === result3.get)
    val result4 = segRef(new CharArrayReader("[ABC,M,>1]".toArray))
    assert(result4.successful)
    assert(SegmentReference("ABC", None, None, 0, MandatoryRequirement, UnlimitedUsage) === result4.get)
    val result5 = segRef(new CharArrayReader("[ABC,,10]".toArray))
    assert(result5.successful)
    assert(SegmentReference("ABC", None, None, 0, RequirementDefault, FiniteUsage(10)) === result5.get)
    val result6 = segRef(new CharArrayReader("[.ABC,M]".toArray))
    assert(result6.successful)
    assert(SegmentReference("ABC", Some(NotUsedMark), None, 0, MandatoryRequirement, UsageDefault) === result6.get)
    val result7 = segRef(new CharArrayReader("[ABC*2@123,M]".toArray))
    assert(result7.successful)
    assert(SegmentReference("ABC", None, Some(123), 2, MandatoryRequirement, UsageDefault) === result7.get)
  }
  
  it should "fail on other values" in {
    assert(!segRef(new CharArrayReader("123[AA]".toArray)).successful)
    assert(!segRef(new CharArrayReader("[,]".toArray)).successful)
    assert(!segRef(new CharArrayReader("[ABC/]".toArray)).successful)
  }
  
  behavior of "group"
  
  it should "parse all forms of group definitions" in {
    val result1 = group(new CharArrayReader("{:10[ABC,M][DTM]}".toArray))
    assert(result1.successful)
    val segref1 = SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault)
    val segref2 = SegmentReference("DTM", None, None, 0, RequirementDefault, UsageDefault)
    assert(Group("ABC", None, None, MandatoryRequirement, 10, List(segref1, segref2)) === result1.get)
    val result2 = group(new CharArrayReader("{LOOP:+8[ABC,M]+10[DTM]}".toArray))
    assert(result2.successful)
    assert(Group("LOOP", None, None, MandatoryRequirement, 1, List(PositionIncrement(8), segref1, PositionIncrement(10), segref2)) === result2.get)
    val result3 = group(new CharArrayReader("@123{LOOP:+8[ABC,M]+10[DTM]}".toArray))
    assert(result3.successful)
    assert(Group("LOOP", None, None, MandatoryRequirement, 1, List(PositionIncrement(8), segref1, PositionIncrement(10), segref2)) === result3.get)
  }
  
  it should "fail on other values" in {
    assert(!group(new CharArrayReader("{:10[ABC,M}]".toArray)).successful)
    assert(!group(new CharArrayReader(":abc{}".toArray)).successful)
    assert(!group(new CharArrayReader("abc:[]".toArray)).successful)
  }
  
  behavior of "table"
  
  it should "parse different lists of items" in {
    val result1 = table(new CharArrayReader("^[ABC,M][DTM]".toArray))
    assert(result1.successful)
    val segref1 = SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault)
    val segref2 = SegmentReference("DTM", None, None, 0, RequirementDefault, UsageDefault)
    assert(TransactionTable(List(segref1, segref2)) === result1.get)
    val result2 = table(new CharArrayReader("^+5[ABC,M][DTM]+1{LOOP:[ABC,M][DTM]}".toArray))
    assert(result2.successful)
    assert(TransactionTable(List(PositionIncrement(5), segref1, segref2, PositionIncrement(1), Group("LOOP", None, None, MandatoryRequirement, 1, List(segref1, segref2)))) === result2.get)
  }
  
  it should "fail on other values" in {
    assert(!table(new CharArrayReader("^{:10[ABC,M}]".toArray)).successful)
    assert(!table(new CharArrayReader(":abc{}".toArray)).successful)
    assert(!table(new CharArrayReader("^abc:[]".toArray)).successful)
  }
  
  val lineEnd = sys.props("line.separator")
  
  behavior of "transSet"
  
  it should "parse one or more tables" in {
    val result1 = transSet(new CharArrayReader("ORDER=^[ABC,M][DTM]".toArray))
    assert(result1.successful)
    val segref1 = SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault)
    val segref2 = SegmentReference("DTM", None, None, 0, RequirementDefault, UsageDefault)
    val transtab1 = TransactionTable(List(segref1, segref2));
    assert(TransactionSet("ORDER", List(transtab1)) === result1.get)
    val result2 = transSet(new CharArrayReader(("ORDER=^[ABC,M][DTM]^+5[ABC,M][DTM]+1{LOOP:[ABC,M][DTM]}" + lineEnd).toArray))
    assert(result2.successful)
    val transtab2 = TransactionTable(List(PositionIncrement(5), segref1, segref2, PositionIncrement(1), Group("LOOP", None, None, MandatoryRequirement, 1, List(segref1, segref2)))) 
    assert(TransactionSet("ORDER", List(transtab1, transtab2)) === result2.get)
  }
  
  it should "fail on other values" in {
    assert(!transSet(new CharArrayReader("^{:10[ABC,M}]".toArray)).successful)
    assert(!transSet(new CharArrayReader(":abc{}".toArray)).successful)
    assert(!transSet(new CharArrayReader("^abc:[]".toArray)).successful)
  }
  
  behavior of "sets"
  
  it should "parse a .SETS section with zero or more transactions" in {
    val result1 = sets(new CharArrayReader(s".SETS${lineEnd}ORDER=^[ABC,M][DTM]".toArray))
    assert(result1.successful)
    val segref1 = SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault)
    val segref2 = SegmentReference("DTM", None, None, 0, RequirementDefault, UsageDefault)
    val transtab1 = TransactionTable(List(segref1, segref2));
    assert(List(TransactionSet("ORDER", List(transtab1))) === result1.get)
    val result2 = sets(new CharArrayReader(s".SETS${lineEnd}ORDER=^[ABC,M][DTM]${lineEnd}OTHER=^[ABC,M][DTM]^+5[ABC,M][DTM]+1{LOOP:[ABC,M][DTM]}${lineEnd}".toArray))
    assert(result2.successful)
    val transtab2 = TransactionTable(List(PositionIncrement(5), segref1, segref2, PositionIncrement(1), Group("LOOP", None, None, MandatoryRequirement, 1, List(segref1, segref2)))) 
    assert(List(TransactionSet("ORDER", List(transtab1)), TransactionSet("OTHER", List(transtab1, transtab2))) === result2.get)
  }
  
  it should "fail on other values" in {
    assert(!sets(new CharArrayReader("^{:10[ABC,M}]".toArray)).successful)
    assert(!sets(new CharArrayReader(":abc{}".toArray)).successful)
    assert(!sets(new CharArrayReader("^abc:[]".toArray)).successful)
  }
}