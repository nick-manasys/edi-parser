package com.anypoint.df.edi.sef

import org.scalatest.FlatSpec
import scala.collection.mutable.Stack
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Reader

import MessageParser._
import com.anypoint.df.edi.schema.EdiSchema._

class SetsParserTest extends FlatSpec {
  
  val lineEnd = sys.props("line.separator")
  
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
    assert(1 === result1.get)
    val result2 = position(new CharArrayReader("814371".toArray))
    assert(result2.successful)
    assert(814371 === result2.get)
    val result3 = position(new CharArrayReader("0410".toArray))
    assert(result3.successful)
    assert(410 === result3.get)
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
    val result1 = setsSection(new CharArrayReader(s".SETS${lineEnd}ORDER=^[ABC,M][DTM]".toArray))
    assert(result1.successful)
    val segref1 = SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault)
    val segref2 = SegmentReference("DTM", None, None, 0, RequirementDefault, UsageDefault)
    val transtab1 = TransactionTable(List(segref1, segref2));
    assert(List(TransactionSet("ORDER", List(transtab1))) === result1.get)
    val result2 = setsSection(new CharArrayReader(s".SETS${lineEnd}ORDER=^[ABC,M][DTM]${lineEnd}OTHER=^[ABC,M][DTM]^+5[ABC,M][DTM]+1{LOOP:[ABC,M][DTM]}${lineEnd}".toArray))
    assert(result2.successful)
    val transtab2 = TransactionTable(List(PositionIncrement(5), segref1, segref2, PositionIncrement(1), Group("LOOP", None, None, MandatoryRequirement, 1, List(segref1, segref2)))) 
    assert(List(TransactionSet("ORDER", List(transtab1)), TransactionSet("OTHER", List(transtab1, transtab2))) === result2.get)
  }
  
  it should "fail on other values" in {
    assert(!setsSection(new CharArrayReader("^{:10[ABC,M}]".toArray)).successful)
    assert(!setsSection(new CharArrayReader(":abc{}".toArray)).successful)
    assert(!setsSection(new CharArrayReader(".SEGS${lineEnd}ORDER=^[ABC,M][DTM]".toArray)).successful)
  }
}

class SegsParserTest extends FlatSpec {
  
  val lineEnd = sys.props("line.separator")
  
  behavior of "useMask"
  
  it should "parse a mask character" in {
    val result1 = useMask(new CharArrayReader(".adadf".toArray))
    assert(result1.successful)
    assert(InheritMask === result1.get)
    val result2 = useMask(new CharArrayReader("#814371".toArray))
    assert(result2.successful)
    assert(NotUsedMask === result2.get)
    val result3 = useMask(new CharArrayReader("@0ab410".toArray))
    assert(result3.successful)
    assert(MustUseMask === result3.get)
  }
  
  it should "fail on other values" in {
    assert(!useMask(new CharArrayReader("!".toArray)).successful)
    assert(!useMask(new CharArrayReader("(4".toArray)).successful)
    assert(!useMask(new CharArrayReader("*abc123".toArray)).successful)
  }
  
  behavior of "minMax"
  
  it should "parse a pair or partial pair" in {
    val result1 = minMax(new CharArrayReader("12:14".toArray))
    assert(result1.successful)
    assert(MinMaxPair(Some(12), Some(14)) === result1.get)
    val result2 = minMax(new CharArrayReader(":42".toArray))
    assert(result2.successful)
    assert(MinMaxPair(None, Some(42)) === result2.get)
    val result3 = minMax(new CharArrayReader("5:".toArray))
    assert(result3.successful)
    assert(MinMaxPair(Some(5), None) === result3.get)
    val result4 = minMax(new CharArrayReader("1111".toArray))
    assert(result4.successful)
    assert(MinMaxPair(Some(1111), None) === result4.get)
    val result5 = minMax(new CharArrayReader(":".toArray))
    assert(result5.successful)
    assert(DefaultMinMaxPair === result5.get)
  }
  
  it should "return the default when pair or partial pair not present" in {
    val result1 = minMax(new CharArrayReader("!".toArray))
    assert(result1.successful)
    assert(DefaultMinMaxPair === result1.get)
    val result2 = minMax(new CharArrayReader("(4".toArray))
    assert(result2.successful)
    assert(DefaultMinMaxPair === result2.get)
    val result3 = minMax(new CharArrayReader("*abc123".toArray))
    assert(result3.successful)
    assert(DefaultMinMaxPair === result3.get)
  }
  
  behavior of "maskMod"
  
  it should "parse a mask number or modification" in {
    val result1 = maskMod(new CharArrayReader("*5".toArray))
    assert(result1.successful)
    assert(UseMaskModification(5) === result1.get)
    val result2 = maskMod(new CharArrayReader("M".toArray))
    assert(result2.successful)
    assert(PropertiesMaskModification(Some(MandatoryRequirement), DefaultMinMaxPair) === result2.get)
    val result3 = maskMod(new CharArrayReader("F".toArray))
    assert(result3.successful)
    assert(PropertiesMaskModification(Some(FunctionalRequirement), DefaultMinMaxPair) === result3.get)
    val result4 = maskMod(new CharArrayReader("M[]".toArray))
    assert(result4.successful)
    assert(PropertiesMaskModification(Some(MandatoryRequirement), DefaultMinMaxPair) === result4.get)
    val result5 = maskMod(new CharArrayReader("M[:5]".toArray))
    assert(result5.successful)
    assert(PropertiesMaskModification(Some(MandatoryRequirement), MinMaxPair(None, Some(5))) === result5.get)
  }
  
  it should "return an empty modification if no mask number or modification found" in {
    val result1 = maskMod(new CharArrayReader("!".toArray))
    assert(result1.successful)
    assert(NoModification === result1.get)
    val result2 = maskMod(new CharArrayReader("(4".toArray))
    assert(result2.successful)
    assert(NoModification === result2.get)
    val result3 = maskMod(new CharArrayReader("*abc123".toArray))
    assert(result3.successful)
    assert(NoModification === result3.get)
  }
  
  behavior of "segMaskItem"
  
  it should "parse a mask item with or without modification" in {
    val result1 = segMaskItem(new CharArrayReader(".*5".toArray))
    assert(result1.successful)
    assert(MaskItem(InheritMask, UseMaskModification(5)) === result1.get)
    val result2 = segMaskItem(new CharArrayReader("+M".toArray))
    assert(result2.successful)
    assert(MaskItem(UsedMask, PropertiesMaskModification(Some(MandatoryRequirement), DefaultMinMaxPair)) === result2.get)
    val result3 = segMaskItem(new CharArrayReader("$F".toArray))
    assert(result3.successful)
    assert(MaskItem(RecommendedMask, PropertiesMaskModification(Some(FunctionalRequirement), DefaultMinMaxPair)) === result3.get)
    val result4 = segMaskItem(new CharArrayReader("-M[]".toArray))
    assert(result4.successful)
    assert(MaskItem(NotRecommendMask, PropertiesMaskModification(Some(MandatoryRequirement), DefaultMinMaxPair)) === result4.get)
    val result5 = segMaskItem(new CharArrayReader("&M[:5]".toArray))
    assert(result5.successful)
    assert(MaskItem(DependentMask, PropertiesMaskModification(Some(MandatoryRequirement), MinMaxPair(None, Some(5)))) === result5.get)
    val result6 = segMaskItem(new CharArrayReader(".".toArray))
    assert(result6.successful)
    assert(MaskItem(InheritMask, NoModification) === result6.get)
  }
  
  it should "fail on other values" in {
    assert(!segMaskItem(new CharArrayReader("!".toArray)).successful)
    assert(!segMaskItem(new CharArrayReader("(4".toArray)).successful)
    assert(!segMaskItem(new CharArrayReader("*abc123".toArray)).successful)
  }
  
  behavior of "segMask"
  
  it should "parse a list of mask items" in {
    val item1 = MaskItem(InheritMask, NoModification)
    val item2 = MaskItem(InheritMask, UseMaskModification(5))
    val item3 = MaskItem(DependentMask, PropertiesMaskModification(Some(MandatoryRequirement), MinMaxPair(None, Some(5))))
    val result1 = segMask(new CharArrayReader("..*5".toArray))
    assert(result1.successful)
    assert(Mask(Seq(item1, item2)) === result1.get)
    val result2 = segMask(new CharArrayReader("..*5&M[:5]".toArray))
    assert(result2.successful)
    assert(Mask(Seq(item1, item2, item3)) === result2.get)
    val result3 = segMask(new CharArrayReader("..*5.".toArray))
    assert(result3.successful)
    assert(Mask(Seq(item1, item2, item1)) === result3.get)
    val result4 = segMask(new CharArrayReader("....".toArray))
    assert(result4.successful)
    assert(Mask(Seq(item1, item1, item1, item1)) === result4.get)
  }
  
  it should "return empty mask on other values" in {
    val empty = Mask(Nil)
    val result1 = segMask(new CharArrayReader("!".toArray))
    assert(result1.successful)
    assert(empty === result1.get)
    val result2 = segMask(new CharArrayReader("(4".toArray))
    assert(result2.successful)
    assert(empty === result2.get)
    val result3 = segMask(new CharArrayReader("*abc123".toArray))
    assert(result3.successful)
    assert(empty === result3.get)
  }
  
  behavior of "depType"
  
  it should "parse a dependendency code type" in {
    val result1 = depType(new CharArrayReader("1adadf".toArray))
    assert(result1.successful)
    assert(ExactlyOneDependency === result1.get)
    val result2 = depType(new CharArrayReader("4814371".toArray))
    assert(result2.successful)
    assert(OneOrNoneDependency === result2.get)
    val result3 = depType(new CharArrayReader("70ab410".toArray))
    assert(result3.successful)
    assert(IfFirstNoneDependency === result3.get)
  }
  
  it should "fail on other values" in {
    assert(!depType(new CharArrayReader("!".toArray)).successful)
    assert(!depType(new CharArrayReader("84".toArray)).successful)
    assert(!depType(new CharArrayReader("0abc123".toArray)).successful)
  }
  
  behavior of "depList"
  
  it should "parse a list of dependency items" in {
    val result1 = depList(new CharArrayReader("(25)".toArray))
    assert(result1.successful)
    assert(Seq(25) === result1.get)
    val result2 = depList(new CharArrayReader("(48,14,371)".toArray))
    assert(result2.successful)
    assert(Seq(48, 14, 371) === result2.get)
  }
  
  it should "fail on other values" in {
    assert(!depList(new CharArrayReader("()".toArray)).successful)
    assert(!depList(new CharArrayReader("(1,)".toArray)).successful)
    assert(!depList(new CharArrayReader("84".toArray)).successful)
    assert(!depList(new CharArrayReader("(1,2".toArray)).successful)
  }
  
  behavior of "depNote"
  
  it should "parse a dependency note" in {
    val result1 = depNote(new CharArrayReader("D3(25)".toArray))
    assert(result1.successful)
    assert(DependencyNote(OneOrMoreDependency, Seq(25)) === result1.get)
    val result2 = depNote(new CharArrayReader("D1(48,14,371)".toArray))
    assert(result2.successful)
    assert(DependencyNote(ExactlyOneDependency, Seq(48, 14, 371)) === result2.get)
  }
  
  it should "fail on other values" in {
    assert(!depNote(new CharArrayReader("D0(1,2)".toArray)).successful)
    assert(!depNote(new CharArrayReader("D8(1,5)".toArray)).successful)
    assert(!depNote(new CharArrayReader("C3(84,85)".toArray)).successful)
    assert(!depNote(new CharArrayReader("(1,2".toArray)).successful)
  }
  
  behavior of "simpleValue"
  
  it should "parse all forms of simple value items" in {
    val result1 = simpleValue(new CharArrayReader("[123]".toArray))
    assert(result1.successful)
    assert(BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1) === result1.get)
    val result2 = simpleValue(new CharArrayReader("[123,M]".toArray))
    assert(result2.successful)
    assert(BaseValue("123", None, None, DefaultMinMaxPair, MandatoryRequirement, 1) === result2.get)
    val result3 = simpleValue(new CharArrayReader("[.123,M,100]".toArray))
    assert(result3.successful)
    assert(BaseValue("123", Some(NotUsedMark), None, DefaultMinMaxPair, MandatoryRequirement, 100) === result3.get)
    val result4 = simpleValue(new CharArrayReader("[$123@1,M,100]".toArray))
    assert(result4.successful)
    assert(BaseValue("123", Some(RecommendedMark), Some(1), DefaultMinMaxPair, MandatoryRequirement, 100) === result4.get)
    val result5 = simpleValue(new CharArrayReader("[-C123,,10]".toArray))
    assert(result5.successful)
    assert(BaseValue("C123", Some(NotRecommendMark), None, DefaultMinMaxPair, RequirementDefault, 10) === result5.get)
    val result6 = simpleValue(new CharArrayReader("[123;:5,M]".toArray))
    assert(result6.successful)
    assert(BaseValue("123", None, None, MinMaxPair(None, Some(5)), MandatoryRequirement, 1) === result6.get)
    val result7 = simpleValue(new CharArrayReader("[123@5;3:10,M]".toArray))
    assert(result7.successful)
    assert(BaseValue("123", None, Some(5), MinMaxPair(Some(3), Some(10)), MandatoryRequirement, 1) === result7.get)
  }
  
  it should "fail on other values" in {
    assert(!simpleValue(new CharArrayReader("123[AA]".toArray)).successful)
    assert(!simpleValue(new CharArrayReader("[,]".toArray)).successful)
    assert(!simpleValue(new CharArrayReader("[ABC/]".toArray)).successful)
  }
  
  behavior of "repGroup"
  
  it should "parse all forms of repeated item groups" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("123", None, Some(5), MinMaxPair(Some(3), Some(10)), MandatoryRequirement, 1)
    val result1 = repGroup(new CharArrayReader("{5[123]}".toArray))
    assert(result1.successful)
    assert(GroupValue(5, Seq(base1)) === result1.get)
    val result2 = repGroup(new CharArrayReader("{5[123][124,M]}".toArray))
    assert(result2.successful)
    assert(GroupValue(5, Seq(base1, base2)) === result2.get)
    val result3 = repGroup(new CharArrayReader("{5[123][124,M][123@5;3:10,M]}".toArray))
    assert(result3.successful)
    assert(GroupValue(5, Seq(base1, base2, base3)) === result3.get)
    val result4 = repGroup(new CharArrayReader("{5[123][124,M]{3[124,M]}}".toArray))
    assert(result4.successful)
    assert(GroupValue(5, Seq(base1, base2, GroupValue(3, Seq(base2)))) === result4.get)
  }
  
  it should "fail on other values" in {
    assert(!repGroup(new CharArrayReader("[123]".toArray)).successful)
    assert(!repGroup(new CharArrayReader("{[123]}".toArray)).successful)
    assert(!repGroup(new CharArrayReader("{5[123]".toArray)).successful)
  }
  
  behavior of "segDef"
  
  it should "parse lists of items" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("125", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val result1 = segDef(new CharArrayReader("SEG=[123]".toArray))
    assert(result1.successful)
    assert(SegmentDef("SEG", Seq(base1), Nil, Nil) === result1.get)
    val result2 = segDef(new CharArrayReader("SEG={5[123][124,M]}".toArray))
    assert(result2.successful)
    assert(SegmentDef("SEG", Seq(GroupValue(5, Seq(base1, base2))), Nil, Nil) === result2.get)
    val result3 = segDef(new CharArrayReader("SEG=[123]{5[123][124,M]}[125]".toArray))
    assert(result3.successful)
    assert(SegmentDef("SEG", Seq(base1, GroupValue(5, Seq(base1, base2)), base3), Nil, Nil) === result3.get)
  }
  
  it should "parse lists of items with masks and/or dependency notes" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("125", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val deps = Seq(DependencyNote(OneOrMoreDependency, Seq(1, 2)), DependencyNote(ExactlyOneDependency, Seq(2, 1)))
    val masks = Seq(Mask(Seq(MaskItem(InheritMask, NoModification), MaskItem(InheritMask, UseMaskModification(5)))), Mask(Seq(MaskItem(NotUsedMask, NoModification), MaskItem(InheritMask, NoModification))))
    val result1 = segDef(new CharArrayReader("SEG=[123][124,M]+D3(001,002)D1(002,001)".toArray))
    assert(result1.successful)
    assert(SegmentDef("SEG", Seq(base1, base2), deps, Nil) === result1.get)
    val result2 = segDef(new CharArrayReader("SEG=[123][124,M],..*5,#.".toArray))
    assert(result2.successful)
    assert(SegmentDef("SEG", Seq(base1, base2), Nil, masks) === result2.get)
    val result3 = segDef(new CharArrayReader("SEG=[123][124,M]+D3(001,002)D1(002,001),..*5,#.".toArray))
    assert(result3.successful)
    assert(SegmentDef("SEG", Seq(base1, base2), deps, masks) === result3.get)
  }
  
  it should "fail on other values" in {
    assert(!segDef(new CharArrayReader("SEG=[123".toArray)).successful)
    assert(!segDef(new CharArrayReader("=[123]".toArray)).successful)
    assert(!segDef(new CharArrayReader("{5[123]".toArray)).successful)
  }
  
  behavior of "segsSection"
  
  it should "parse a .SEGS section with zero or more definitions" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("125", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val masks = Seq(Mask(Seq(MaskItem(InheritMask, NoModification), MaskItem(InheritMask, UseMaskModification(5)))), Mask(Seq(MaskItem(NotUsedMask, NoModification), MaskItem(InheritMask, NoModification))))
    val result1 = segsSection(new CharArrayReader(s".SEGS${lineEnd}SEG1=[123]".toArray))
    assert(result1.successful)
    assert(Seq(SegmentDef("SEG1", Seq(base1), Nil, Nil)) === result1.get)
    val result2 = segsSection(new CharArrayReader(s".SEGS${lineEnd}SEG1=[123]${lineEnd}SEG={5[123][124,M]}".toArray))
    assert(result2.successful)
    assert(Seq(SegmentDef("SEG1", Seq(base1), Nil, Nil), SegmentDef("SEG", Seq(GroupValue(5, Seq(base1, base2))), Nil, Nil)) === result2.get)
    val result3 = segsSection(new CharArrayReader(s".SEGS${lineEnd}SEG1=[123]{5[123][124,M]}[125]${lineEnd}SEG2=[123][124,M],..*5,#.".toArray))
    assert(result3.successful)
    assert(Seq(SegmentDef("SEG1", Seq(base1, GroupValue(5, Seq(base1, base2)), base3), Nil, Nil), SegmentDef("SEG2", Seq(base1, base2), Nil, masks)) === result3.get)
  }
  
  it should "fail on other values" in {
    assert(!segsSection(new CharArrayReader("SEG=[123".toArray)).successful)
    assert(!segsSection(new CharArrayReader("=[123]".toArray)).successful)
    assert(!segsSection(new CharArrayReader("{5[123]".toArray)).successful)
  }
}