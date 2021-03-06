package com.mulesoft.flatfile.sef

import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.CharSequenceReader

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import com.mulesoft.flatfile.schema.EdiSchema.DependencyNote
import com.mulesoft.flatfile.schema.EdiSchema.ExactlyOneDependency
import com.mulesoft.flatfile.schema.EdiSchema.IfFirstNoneDependency
import com.mulesoft.flatfile.schema.EdiSchema.OneOrMoreDependency
import com.mulesoft.flatfile.schema.EdiSchema.OneOrNoneDependency

import MessageParser._

trait TestBase {

  def parsing[T](s: String)(implicit p: Parser[T]) = p(new CharSequenceReader(s))

  def parsingAll[T](s: String)(implicit p: Parser[T]): T = {
    val phraseParser = phrase(p)
    val input = new CharSequenceReader(s)
    val result = phraseParser(input)
    result match {
      case Success(t, _) => t
      case NoSuccess(msg, x) => {
        println(result)
        throw new IllegalArgumentException("Failed parsing '" + s + "': " + msg)
        }
    }
  }

  def parsingLead[T](s: String)(implicit p: Parser[T]): T = {
    val input = new CharSequenceReader(s)
    p(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, _) => throw new IllegalArgumentException("Failed parsing '" + s + "': " + msg)
    }
  }
}

class SetsParserTest extends FlatSpec with TestBase with ShouldMatchers {

  behavior of "posInt"

  it should "parse a zero value" in {
    implicit val parserToTest = posInt
    parsingAll("0") should equal(0)
  }

  it should "parse positive value" in {
    implicit val parserToTest = posInt
    parsingAll("1") should equal(1)
    parsingAll("814371") should equal(814371)
  }

  it should "fail on other value" in {
    implicit val parserToTest = posInt
    parsing("") should not be ('successful)
    parsing("-4") should not be ('successful)
    parsing("abc123") should not be ('successful)
  }

  behavior of "position"

  it should "parse number value" in {
    implicit val parserToTest = position
    parsingAll("1") should equal(1)
    parsingAll("814371") should equal(814371)
    parsingAll("0410") should equal(410)
  }

  it should "fail on other value" in {
    implicit val parserToTest = position
    parsing("") should not be ('successful)
    parsing("-4") should not be ('successful)
    parsing("abc123") should not be ('successful)
  }

  behavior of "id"

  it should "parse alphanumeric value" in {
    implicit val parserToTest = id
    parsingAll("1abcd") should equal("1abcd")
    parsingAll("x814371") should equal("x814371")
    parsingAll("0ab410") should equal("0ab410")
  }

  it should "fail on other value" in {
    implicit val parserToTest = id
    parsing("") should not be ('successful)
    parsing("-4") should not be ('successful)
    parsing("*abc123") should not be ('successful)
  }

  "maskNum" should "parse a mask number" in {
    implicit val parserToTest = maskNum
    parsingAll("*5") should equal(5)
  }

  "ordNum" should "parse an ordinal number" in {
    implicit val parserToTest = ordNum
    parsingAll("@5") should equal(5)
  }

  behavior of "useMark"

  it should "parse a leading mark character" in {
    implicit val parserToTest = useMark
    parsingLead(".adadf") should equal(NotUsedMark)
    parsingLead("!814371") should equal(MustUseMark)
    parsingLead("$0ab410") should equal(RecommendedMark)
  }

  it should "fail on other value" in {
    implicit val parserToTest = useMark
    parsing("") should not be ('successful)
    parsing("(4") should not be ('successful)
    parsing("*abc123") should not be ('successful)
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

  it should "fail on other value" in {
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

  it should "fail on other value" in {
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

  it should "fail on other value" in {
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

  it should "fail on other value" in {
    assert(!segRef(new CharArrayReader("123[AA]".toArray)).successful)
    assert(!segRef(new CharArrayReader("[,]".toArray)).successful)
    assert(!segRef(new CharArrayReader("[ABC/]".toArray)).successful)
  }

  behavior of "transGroup"

  it should "parse all forms of group definition" in {
    val result1 = transGroup(new CharArrayReader("{:10[ABC,M][DTM]}".toArray))
    assert(result1.successful)
    val segref1 = SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault)
    val segref2 = SegmentReference("DTM", None, None, 0, RequirementDefault, UsageDefault)
    assert(TransGroup("ABC", None, None, MandatoryRequirement, 10, List(segref1, segref2)) === result1.get)
    val result2 = transGroup(new CharArrayReader("{LOOP:+8[ABC,M]+10[DTM]}".toArray))
    assert(result2.successful)
    assert(TransGroup("LOOP", None, None, MandatoryRequirement, 1, List(PositionIncrement(8), segref1, PositionIncrement(10), segref2)) === result2.get)
    val result3 = transGroup(new CharArrayReader("@123{LOOP:+8[ABC,M]+10[DTM]}".toArray))
    assert(result3.successful)
    assert(TransGroup("LOOP", None, None, MandatoryRequirement, 1, List(PositionIncrement(8), segref1, PositionIncrement(10), segref2)) === result3.get)
  }

  it should "fail on other value" in {
    assert(!transGroup(new CharArrayReader("{:10[ABC,M}]".toArray)).successful)
    assert(!transGroup(new CharArrayReader(":abc{}".toArray)).successful)
    assert(!transGroup(new CharArrayReader("abc:[]".toArray)).successful)
  }

  behavior of "table"

  it should "parse different lists of item" in {
    val result1 = table(new CharArrayReader("^[ABC,M][DTM]".toArray))
    assert(result1.successful)
    val segref1 = SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault)
    val segref2 = SegmentReference("DTM", None, None, 0, RequirementDefault, UsageDefault)
    assert(StructureTable(List(segref1, segref2)) === result1.get)
    val result2 = table(new CharArrayReader("^+5[ABC,M][DTM]+1{LOOP:[ABC,M][DTM]}".toArray))
    assert(result2.successful)
    assert(StructureTable(List(PositionIncrement(5), segref1, segref2, PositionIncrement(1), TransGroup("LOOP", None, None, MandatoryRequirement, 1, List(segref1, segref2)))) === result2.get)
  }

  it should "fail on other value" in {
    assert(!table(new CharArrayReader("^{:10[ABC,M}]".toArray)).successful)
    assert(!table(new CharArrayReader(":abc{}".toArray)).successful)
    assert(!table(new CharArrayReader("^abc:[]".toArray)).successful)
  }

  behavior of "transSet"

  it should "parse one or more table" in {
    val result1 = transSet(new CharArrayReader("ORDER=^[ABC,M][DTM]".toArray))
    assert(result1.successful)
    val segref1 = SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault)
    val segref2 = SegmentReference("DTM", None, None, 0, RequirementDefault, UsageDefault)
    val transtab1 = StructureTable(List(segref1, segref2));
    assert(StructureSet("ORDER", List(transtab1)) === result1.get)
    val result2 = transSet(new CharArrayReader(("""ORDER=^[ABC,M][DTM]^+5[ABC,M][DTM]+1{LOOP:[ABC,M][DTM]}""").toArray))
    assert(result2.successful)
    val transtab2 = StructureTable(List(PositionIncrement(5), segref1, segref2, PositionIncrement(1), TransGroup("LOOP", None, None, MandatoryRequirement, 1, List(segref1, segref2))))
    assert(StructureSet("ORDER", List(transtab1, transtab2)) === result2.get)
  }

  it should "fail on other value" in {
    assert(!transSet(new CharArrayReader("^{:10[ABC,M}]".toArray)).successful)
    assert(!transSet(new CharArrayReader(":abc{}".toArray)).successful)
    assert(!transSet(new CharArrayReader("^abc:[]".toArray)).successful)
  }

  behavior of "set"

  it should "parse a .SETS section with zero or more structure" in {
    val result1 = setsSection(new CharArrayReader(".SETS\nORDER=^[ABC,M][DTM]".toArray))
    assert(result1.successful)
    val segref1 = SegmentReference("ABC", None, None, 0, MandatoryRequirement, UsageDefault)
    val segref2 = SegmentReference("DTM", None, None, 0, RequirementDefault, UsageDefault)
    val transtab1 = StructureTable(List(segref1, segref2));
    assert(StructuresSection(Seq(StructureSet("ORDER", List(transtab1)))) === result1.get)
    val result2 = setsSection(new CharArrayReader(".SETS\nORDER=^[ABC,M][DTM]\nOTHER=^[ABC,M][DTM]^+5[ABC,M][DTM]+1{LOOP:[ABC,M][DTM]}\n".toArray))
    assert(result2.successful)
    val transtab2 = StructureTable(List(PositionIncrement(5), segref1, segref2, PositionIncrement(1), TransGroup("LOOP", None, None, MandatoryRequirement, 1, List(segref1, segref2))))
    assert(StructuresSection(Seq(StructureSet("ORDER", List(transtab1)), StructureSet("OTHER", List(transtab1, transtab2)))) === result2.get)
  }

  it should "fail on other value" in {
    assert(!setsSection(new CharArrayReader("^{:10[ABC,M}]".toArray)).successful)
    assert(!setsSection(new CharArrayReader(":abc{}".toArray)).successful)
    assert(!setsSection(new CharArrayReader(".SEGS\nORDER=^[ABC,M][DTM]".toArray)).successful)
  }
}

class SegsParserTest extends FlatSpec with TestBase with ShouldMatchers {

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

  it should "fail on other value" in {
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

  behavior of "maskItem"

  it should "parse a mask item with or without modification" in {
    val result1 = maskItem(new CharArrayReader(".*5".toArray))
    assert(result1.successful)
    assert(MaskItem(InheritMask, UseMaskModification(5)) === result1.get)
    val result2 = maskItem(new CharArrayReader("+M".toArray))
    assert(result2.successful)
    assert(MaskItem(UsedMask, PropertiesMaskModification(Some(MandatoryRequirement), DefaultMinMaxPair)) === result2.get)
    val result3 = maskItem(new CharArrayReader("$F".toArray))
    assert(result3.successful)
    assert(MaskItem(RecommendedMask, PropertiesMaskModification(Some(FunctionalRequirement), DefaultMinMaxPair)) === result3.get)
    val result4 = maskItem(new CharArrayReader("-M[]".toArray))
    assert(result4.successful)
    assert(MaskItem(NotRecommendMask, PropertiesMaskModification(Some(MandatoryRequirement), DefaultMinMaxPair)) === result4.get)
    val result5 = maskItem(new CharArrayReader("&M[:5]".toArray))
    assert(result5.successful)
    assert(MaskItem(DependentMask, PropertiesMaskModification(Some(MandatoryRequirement), MinMaxPair(None, Some(5)))) === result5.get)
    val result6 = maskItem(new CharArrayReader(".".toArray))
    assert(result6.successful)
    assert(MaskItem(InheritMask, NoModification) === result6.get)
  }

  it should "fail on other value" in {
    assert(!maskItem(new CharArrayReader("!".toArray)).successful)
    assert(!maskItem(new CharArrayReader("(4".toArray)).successful)
    assert(!maskItem(new CharArrayReader("*abc123".toArray)).successful)
  }

  behavior of "maskDef"

  it should "parse a list of mask item" in {
    val item1 = MaskItem(InheritMask, NoModification)
    val item2 = MaskItem(InheritMask, UseMaskModification(5))
    val item3 = MaskItem(DependentMask, PropertiesMaskModification(Some(MandatoryRequirement), MinMaxPair(None, Some(5))))
    val result1 = maskDef(new CharArrayReader("..*5".toArray))
    assert(result1.successful)
    assert(Mask(Seq(item1, item2)) === result1.get)
    val result2 = maskDef(new CharArrayReader("..*5&M[:5]".toArray))
    assert(result2.successful)
    assert(Mask(Seq(item1, item2, item3)) === result2.get)
    val result3 = maskDef(new CharArrayReader("..*5.".toArray))
    assert(result3.successful)
    assert(Mask(Seq(item1, item2, item1)) === result3.get)
    val result4 = maskDef(new CharArrayReader("....".toArray))
    assert(result4.successful)
    assert(Mask(Seq(item1, item1, item1, item1)) === result4.get)
  }

  it should "return empty mask on other value" in {
    val empty = Mask(Nil)
    val result1 = maskDef(new CharArrayReader("!".toArray))
    assert(result1.successful)
    assert(empty === result1.get)
    val result2 = maskDef(new CharArrayReader("(4".toArray))
    assert(result2.successful)
    assert(empty === result2.get)
    val result3 = maskDef(new CharArrayReader("*abc123".toArray))
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

  it should "fail on other value" in {
    assert(!depType(new CharArrayReader("!".toArray)).successful)
    assert(!depType(new CharArrayReader("84".toArray)).successful)
    assert(!depType(new CharArrayReader("0abc123".toArray)).successful)
  }

  behavior of "depList"

  it should "parse a list of dependency item" in {
    val result1 = depList(new CharArrayReader("(25)".toArray))
    assert(result1.successful)
    assert(Seq(25) === result1.get)
    val result2 = depList(new CharArrayReader("(48,14,371)".toArray))
    assert(result2.successful)
    assert(Seq(48, 14, 371) === result2.get)
  }

  it should "fail on other value" in {
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

  it should "fail on other value" in {
    assert(!depNote(new CharArrayReader("D0(1,2)".toArray)).successful)
    assert(!depNote(new CharArrayReader("D8(1,5)".toArray)).successful)
    assert(!depNote(new CharArrayReader("C3(84,85)".toArray)).successful)
    assert(!depNote(new CharArrayReader("(1,2".toArray)).successful)
  }

  behavior of "segSimple"

  it should "parse all forms of simple value item" in {
    val result1 = segSimple(new CharArrayReader("[123]".toArray))
    assert(result1.successful)
    assert(BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1) === result1.get)
    val result2 = segSimple(new CharArrayReader("[123,M]".toArray))
    assert(result2.successful)
    assert(BaseValue("123", None, None, DefaultMinMaxPair, MandatoryRequirement, 1) === result2.get)
    val result3 = segSimple(new CharArrayReader("[.123,M,100]".toArray))
    assert(result3.successful)
    assert(BaseValue("123", Some(NotUsedMark), None, DefaultMinMaxPair, MandatoryRequirement, 100) === result3.get)
    val result4 = segSimple(new CharArrayReader("[$123@1,M,100]".toArray))
    assert(result4.successful)
    assert(BaseValue("123", Some(RecommendedMark), Some(1), DefaultMinMaxPair, MandatoryRequirement, 100) === result4.get)
    val result5 = segSimple(new CharArrayReader("[-C123,,10]".toArray))
    assert(result5.successful)
    assert(BaseValue("C123", Some(NotRecommendMark), None, DefaultMinMaxPair, RequirementDefault, 10) === result5.get)
    val result6 = segSimple(new CharArrayReader("[123;:5,M]".toArray))
    assert(result6.successful)
    assert(BaseValue("123", None, None, MinMaxPair(None, Some(5)), MandatoryRequirement, 1) === result6.get)
    val result7 = segSimple(new CharArrayReader("[123@5;3:10,M]".toArray))
    assert(result7.successful)
    assert(BaseValue("123", None, Some(5), MinMaxPair(Some(3), Some(10)), MandatoryRequirement, 1) === result7.get)
  }

  it should "fail on other value" in {
    assert(!segSimple(new CharArrayReader("123[AA]".toArray)).successful)
    assert(!segSimple(new CharArrayReader("[,]".toArray)).successful)
    assert(!segSimple(new CharArrayReader("[ABC/]".toArray)).successful)
  }

  behavior of "segGroup"

  it should "parse all forms of repeated item group" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("123", None, Some(5), MinMaxPair(Some(3), Some(10)), MandatoryRequirement, 1)
    val result1 = segGroup(new CharArrayReader("{5[123]}".toArray))
    assert(result1.successful)
    assert(GroupValue(5, Seq(base1)) === result1.get)
    val result2 = segGroup(new CharArrayReader("{5[123][124,M]}".toArray))
    assert(result2.successful)
    assert(GroupValue(5, Seq(base1, base2)) === result2.get)
    val result3 = segGroup(new CharArrayReader("{5[123][124,M][123@5;3:10,M]}".toArray))
    assert(result3.successful)
    assert(GroupValue(5, Seq(base1, base2, base3)) === result3.get)
    val result4 = segGroup(new CharArrayReader("{5[123][124,M]{3[124,M]}}".toArray))
    assert(result4.successful)
    assert(GroupValue(5, Seq(base1, base2, GroupValue(3, Seq(base2)))) === result4.get)
  }

  it should "fail on other value" in {
    assert(!segGroup(new CharArrayReader("[123]".toArray)).successful)
    assert(!segGroup(new CharArrayReader("{[123]}".toArray)).successful)
    assert(!segGroup(new CharArrayReader("{5[123]".toArray)).successful)
  }

  behavior of "segDef"

  it should "parse lists of item" in {
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

  it should "parse lists of items with masks and/or dependency note" in {
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

  it should "fail on other value" in {
    assert(!segDef(new CharArrayReader("SEG=[123".toArray)).successful)
    assert(!segDef(new CharArrayReader("=[123]".toArray)).successful)
    assert(!segDef(new CharArrayReader("{5[123]".toArray)).successful)
  }

  behavior of "segsSection"

  it should "parse a .SEGS section with zero or more definition" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("125", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val masks = Seq(Mask(Seq(MaskItem(InheritMask, NoModification), MaskItem(InheritMask, UseMaskModification(5)))), Mask(Seq(MaskItem(NotUsedMask, NoModification), MaskItem(InheritMask, NoModification))))
    val result1 = segsSection(new CharArrayReader(".SEGS\nSEG1=[123]".toArray))
    assert(result1.successful)
    assert(SegmentsSection(Seq(SegmentDef("SEG1", Seq(base1), Nil, Nil))) === result1.get)
    val result2 = segsSection(new CharArrayReader(".SEGS\nSEG1=[123]\nSEG={5[123][124,M]}".toArray))
    assert(result2.successful)
    assert(SegmentsSection(Seq(SegmentDef("SEG1", Seq(base1), Nil, Nil), SegmentDef("SEG", Seq(GroupValue(5, Seq(base1, base2))), Nil, Nil))) === result2.get)
    val result3 = segsSection(new CharArrayReader(".SEGS\nSEG1=[123]{5[123][124,M]}[125]\nSEG2=[123][124,M],..*5,#.".toArray))
    assert(result3.successful)
    assert(SegmentsSection(Seq(SegmentDef("SEG1", Seq(base1, GroupValue(5, Seq(base1, base2)), base3), Nil, Nil), SegmentDef("SEG2", Seq(base1, base2), Nil, masks))) === result3.get)
  }

  it should "fail on other value" in {
    assert(!segsSection(new CharArrayReader("SEG=[123".toArray)).successful)
    assert(!segsSection(new CharArrayReader("=[123]".toArray)).successful)
    assert(!segsSection(new CharArrayReader("{5[123]".toArray)).successful)
  }
}

class ComsParserTest extends FlatSpec with TestBase with ShouldMatchers {

  behavior of "comSimple"

  it should "parse all forms of simple value item" in {
    val result1 = comSimple(new CharArrayReader("[123]".toArray))
    assert(result1.successful)
    assert(BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1) === result1.get)
    val result2 = comSimple(new CharArrayReader("[123,M]".toArray))
    assert(result2.successful)
    assert(BaseValue("123", None, None, DefaultMinMaxPair, MandatoryRequirement, 1) === result2.get)
    val result3 = comSimple(new CharArrayReader("[.123,M]".toArray))
    assert(result3.successful)
    assert(BaseValue("123", Some(NotUsedMark), None, DefaultMinMaxPair, MandatoryRequirement, 1) === result3.get)
    val result4 = comSimple(new CharArrayReader("[$123@1,M]".toArray))
    assert(result4.successful)
    assert(BaseValue("123", Some(RecommendedMark), Some(1), DefaultMinMaxPair, MandatoryRequirement, 1) === result4.get)
    val result5 = comSimple(new CharArrayReader("[-C123,]".toArray))
    assert(result5.successful)
    assert(BaseValue("C123", Some(NotRecommendMark), None, DefaultMinMaxPair, RequirementDefault, 1) === result5.get)
    val result6 = comSimple(new CharArrayReader("[123;:5,M]".toArray))
    assert(result6.successful)
    assert(BaseValue("123", None, None, MinMaxPair(None, Some(5)), MandatoryRequirement, 1) === result6.get)
    val result7 = comSimple(new CharArrayReader("[123@5;3:10,M]".toArray))
    assert(result7.successful)
    assert(BaseValue("123", None, Some(5), MinMaxPair(Some(3), Some(10)), MandatoryRequirement, 1) === result7.get)
  }

  it should "fail on other value" in {
    assert(!comSimple(new CharArrayReader("123[AA]".toArray)).successful)
    assert(!comSimple(new CharArrayReader("[,]".toArray)).successful)
    assert(!comSimple(new CharArrayReader("[.123,M,100]".toArray)).successful)
  }

  behavior of "comGroup"

  it should "parse all forms of repeated item group" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("123", None, Some(5), MinMaxPair(Some(3), Some(10)), MandatoryRequirement, 1)
    val result1 = comGroup(new CharArrayReader("{5[123]}".toArray))
    assert(result1.successful)
    assert(GroupValue(5, Seq(base1)) === result1.get)
    val result2 = comGroup(new CharArrayReader("{5[123][124,M]}".toArray))
    assert(result2.successful)
    assert(GroupValue(5, Seq(base1, base2)) === result2.get)
    val result3 = comGroup(new CharArrayReader("{5[123][124,M][123@5;3:10,M]}".toArray))
    assert(result3.successful)
    assert(GroupValue(5, Seq(base1, base2, base3)) === result3.get)
    val result4 = comGroup(new CharArrayReader("{5[123][124,M]{3[124,M]}}".toArray))
    assert(result4.successful)
    assert(GroupValue(5, Seq(base1, base2, GroupValue(3, Seq(base2)))) === result4.get)
  }

  it should "fail on other value" in {
    assert(!comGroup(new CharArrayReader("[123]".toArray)).successful)
    assert(!comGroup(new CharArrayReader("{[123]}".toArray)).successful)
    assert(!comGroup(new CharArrayReader("{5[123]".toArray)).successful)
  }

  behavior of "comDef"

  it should "parse lists of item" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("125", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val result1 = comDef(new CharArrayReader("COM=[123]".toArray))
    assert(result1.successful)
    assert(CompositeDef("COM", Seq(base1), Nil, Nil) === result1.get)
    val result2 = comDef(new CharArrayReader("COM={5[123][124,M]}".toArray))
    assert(result2.successful)
    assert(CompositeDef("COM", Seq(GroupValue(5, Seq(base1, base2))), Nil, Nil) === result2.get)
    val result3 = comDef(new CharArrayReader("COM=[123]{5[123][124,M]}[125]".toArray))
    assert(result3.successful)
    assert(CompositeDef("COM", Seq(base1, GroupValue(5, Seq(base1, base2)), base3), Nil, Nil) === result3.get)
  }

  it should "parse lists of items with masks and/or dependency note" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("125", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val deps = Seq(DependencyNote(OneOrMoreDependency, Seq(1, 2)), DependencyNote(ExactlyOneDependency, Seq(2, 1)))
    val masks = Seq(Mask(Seq(MaskItem(InheritMask, NoModification), MaskItem(InheritMask, UseMaskModification(5)))), Mask(Seq(MaskItem(NotUsedMask, NoModification), MaskItem(InheritMask, NoModification))))
    val result1 = comDef(new CharArrayReader("COM=[123][124,M]+D3(001,002)D1(002,001)".toArray))
    assert(result1.successful)
    assert(CompositeDef("COM", Seq(base1, base2), deps, Nil) === result1.get)
    val result2 = comDef(new CharArrayReader("COM=[123][124,M],..*5,#.".toArray))
    assert(result2.successful)
    assert(CompositeDef("COM", Seq(base1, base2), Nil, masks) === result2.get)
    val result3 = comDef(new CharArrayReader("COM=[123][124,M]+D3(001,002)D1(002,001),..*5,#.".toArray))
    assert(result3.successful)
    assert(CompositeDef("COM", Seq(base1, base2), deps, masks) === result3.get)
  }

  it should "fail on other value" in {
    assert(!comDef(new CharArrayReader("COM=[123".toArray)).successful)
    assert(!comDef(new CharArrayReader("=[123]".toArray)).successful)
    assert(!comDef(new CharArrayReader("{5[123]".toArray)).successful)
  }

  behavior of "comsSection"

  it should "parse a .COMS section with zero or more definition" in {
    val base1 = BaseValue("123", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val base2 = BaseValue("124", None, None, DefaultMinMaxPair, MandatoryRequirement, 1)
    val base3 = BaseValue("125", None, None, DefaultMinMaxPair, RequirementDefault, 1)
    val masks = Seq(Mask(Seq(MaskItem(InheritMask, NoModification), MaskItem(InheritMask, UseMaskModification(5)))), Mask(Seq(MaskItem(NotUsedMask, NoModification), MaskItem(InheritMask, NoModification))))
    val result1 = comsSection(new CharArrayReader(".COMS\nCOM1=[123]".toArray))
    assert(result1.successful)
    assert(CompositesSection(Seq(CompositeDef("COM1", Seq(base1), Nil, Nil))) === result1.get)
    val result2 = comsSection(new CharArrayReader(".COMS\nCOM1=[123]\nCOM={5[123][124,M]}".toArray))
    assert(result2.successful)
    assert(CompositesSection(Seq(CompositeDef("COM1", Seq(base1), Nil, Nil), CompositeDef("COM", Seq(GroupValue(5, Seq(base1, base2))), Nil, Nil))) === result2.get)
    val result3 = comsSection(new CharArrayReader(".COMS\nCOM1=[123]{5[123][124,M]}[125]\nCOM2=[123][124,M],..*5,#.".toArray))
    assert(result3.successful)
    assert(CompositesSection(Seq(CompositeDef("COM1", Seq(base1, GroupValue(5, Seq(base1, base2)), base3), Nil, Nil), CompositeDef("COM2", Seq(base1, base2), Nil, masks))) === result3.get)
  }

  it should "fail on other value" in {
    assert(!comsSection(new CharArrayReader("COM=[123".toArray)).successful)
    assert(!comsSection(new CharArrayReader("=[123]".toArray)).successful)
    assert(!comsSection(new CharArrayReader("{5[123]".toArray)).successful)
  }
}

class ElmsParserTest extends FlatSpec with TestBase with ShouldMatchers {

  behavior of "elmType"

  it should "parse all forms of element type" in {
    implicit val parserToTest = elmType
    parsingAll("R") should equal(RealType)
    parsingAll("ID") should equal(IdType)
    parsingAll("AN") should equal(AlphaNumericType)
    parsingAll("A") should equal(AlphaType)
    parsingAll("DT") should equal(DateType)
    parsingAll("TM") should equal(TimeType)
    parsingAll("B") should equal(BinaryType)
    parsingAll("N") should equal(NumberType)
    parsingAll("N0") should equal(IntegerType)
    parsingAll("N1") should equal(DecimalType(1))
    parsingAll("N5") should equal(DecimalType(5))
    parsingAll("N9") should equal(DecimalType(9))
    parsingLead("IDA") should equal(IdType)
    parsingLead("ANB") should equal(AlphaNumericType)
    parsingLead("AB") should equal(AlphaType)
    parsingLead("DTA") should equal(DateType)
    parsingLead("TMA") should equal(TimeType)
    parsingLead("BA") should equal(BinaryType)
    parsingLead("NA") should equal(NumberType)
    parsingLead("N012") should equal(IntegerType)
    parsingLead("N101") should equal(DecimalType(1))
    parsingLead("N51") should equal(DecimalType(5))
    parsingLead("N932") should equal(DecimalType(9))
  }

  it should "fail on other value" in {
    implicit val parserToTest = elmType
    parsing("TX") should not be ('successful)
    parsing("XAB") should not be ('successful)
  }

  behavior of "elmDef"

  it should "parse element definition" in {
    implicit val parserToTest = elmDef
    parsingAll("1=AN,1,13") should equal(ElementDef("1", AlphaNumericType, 1, 13))
    parsingAll("2=N0,1,6") should equal(ElementDef("2", IntegerType, 1, 6))
    parsingAll("4=ID,3,3") should equal(ElementDef("4", IdType, 3, 3))
    parsingAll("6=DT,6,6") should equal(ElementDef("6", DateType, 6, 6))
    parsingAll("0001=A,4,4") should equal(ElementDef("0001", AlphaType, 4, 4))
    parsingAll("0002=N,1,1") should equal(ElementDef("0002", NumberType, 1, 1))
    parsingAll("0073=A,1,1") should equal(ElementDef("0073", AlphaType, 1, 1))
    parsingAll("18=TM,4,4") should equal(ElementDef("18", TimeType, 4, 4))
    parsingAll("58=N2,1,9") should equal(ElementDef("58", DecimalType(2), 1, 9))
    parsingAll("60=R,1,9") should equal(ElementDef("60", RealType, 1, 9))
  }

  it should "fail on other value" in {
    implicit val parserToTest = elmDef
    parsing("1=AX,1,13") should not be ('successful)
    parsing("=AN,1,13") should not be ('successful)
  }

  behavior of "elmsSection"

  it should "parse a .ELMS section with zero or more definition" in {
    implicit val parserToTest = elmsSection
    val elm1 = ElementDef("1", AlphaNumericType, 1, 13)
    val elm2 = ElementDef("2", IntegerType, 1, 6)
    val elm3 = ElementDef("0001", AlphaType, 4, 4)
    val elm4 = ElementDef("58", DecimalType(2), 1, 9)
    parsingAll(".ELMS\n1=AN,1,13") should equal(ElementsSection(Seq(elm1)))
    parsingAll(".ELMS\n1=AN,1,13\n") should equal(ElementsSection(Seq(elm1)))
    parsingAll(".ELMS\n1=AN,1,13\n2=N0,1,6") should equal(ElementsSection(Seq(elm1, elm2)))
    parsingAll(".ELMS\n1=AN,1,13\n2=N0,1,6\n0001=A,4,4") should equal(ElementsSection(Seq(elm1, elm2, elm3)))
    parsingAll(".ELMS\n1=AN,1,13\n2=N0,1,6\n0001=A,4,4\n58=N2,1,9") should equal(ElementsSection(Seq(elm1, elm2, elm3, elm4)))
  }

  it should "fail on other value" in {
    implicit val parserToTest = elmDef
    parsing("ELMS=[123") should not be ('successful)
    parsing(".ELMS\n1=AN,13") should not be ('successful)
    parsing(".ELMS\n1=AN,1,13,") should not be ('successful)
  }
}

class SefParserTest extends FlatSpec with TestBase with ShouldMatchers {

  val elm1 = ElementDef("0001", AlphaType, 4, 6)
  val elm2 = ElementDef("0002", NumberType, 1, 1)
  val cval1 = BaseValue("0001", None, None, DefaultMinMaxPair, RequirementDefault, 1)
  val cval2 = BaseValue("0002", None, None, DefaultMinMaxPair, RequirementDefault, 1)
  val com1 = CompositeDef("C001", Seq(cval1, cval2), Nil, Nil)
  val sval1 = BaseValue("0001", Some(MustUseMark), None, DefaultMinMaxPair, MandatoryRequirement, 1)
  val sval2 = BaseValue("C001", None, None, DefaultMinMaxPair, RequirementDefault, 1)
  val seg1 = SegmentDef("AAA", Seq(sval1, sval2), Nil, Nil)
  val ref1 = SegmentReference("AAA", Some(MustUseMark), None, 0, MandatoryRequirement, UsageDefault)
  val table1 = StructureTable(Seq(ref1))
  val set1 = StructureSet("ORDERS", Seq(table1))
  val doc1 = Seq(StructuresSection(Seq(set1)), SegmentsSection(Seq(seg1)), CompositesSection(Seq(com1)), ElementsSection(Seq(elm1, elm2)))

  behavior of "sefParser"

  it should "parse a SEF document with all supported sections included" in {
    implicit val parserToTest = sefParser
    val result1 = parsingAll(".SETS\nORDERS=^[!AAA,M]\n.SEGS\nAAA=[!0001,M][C001]\n.COMS\nC001=[0001][0002]\n.ELMS\n0001=A,4,6\n0002=N,1,1\n")
    result1 should equal(doc1)
  }

  // TODO: fix this
//  it should "parse a SEF document with unsupported sections included" in {
//    implicit val parserToTest = sefParser
//    val result1 = parsingAll(".VER 1.3\n.SETS\nORDERS=^[!AAA,M]\n.SEGS\nAAA=[!0001,M][C001]\n.COMS\nC001=[0001][0002]\n.ELMS\n0001=A,4,6\n0002=N,1,1")
////    val result1 = parsingAll(""".VER 1.3\n.INI\nORDERS_D94B,,D 94B,UN,D94B,Purchase Order Message\n.PRIVATE EDIdEv\nTDVER\n.DATE 8/5/2002 10:25:17 PM\n.PUBLIC\n.STD ,LS\n.SETS\nORDERS=^[!AAA,M]\n.SEGS\nAAA=[!0001,M][C001]\n.COMS\nC001=[0001][0002]\n.ELMS\n0001=A,4,6\n0002=N,1,1\n.CODES\n0001=UNOA,UNOB,UNOC,UNOD,UNOE,UNOF""")
//    result1 should equal(doc1)
//  }
}