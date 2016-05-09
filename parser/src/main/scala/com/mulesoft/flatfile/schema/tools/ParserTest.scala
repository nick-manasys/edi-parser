package com.mulesoft.flatfile.schema.tools
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{ Position, Reader }

object ParserTest extends Parsers {

  type Elem = String

  val fillerParser: Parser[Boolean] = "FILLER" ^^^ false
  val wrappedFillerParser: Parser[Boolean] = new Parser[Boolean] {
    def apply(in: Input) = {
      val result = fillerParser(in)
      result match {
        case Success(result, next) => Error("Duplicate FILLER clause", in)
        case _ => result
      }
    }
  }
  val globalParser: Parser[Boolean] = "GLOBAL" ^^^ false
  val justifyParser: Parser[Boolean] = ("JUSTIFIED" | "JUST") ~> "RIGHT".? ^^^ false
  val blankParser: Parser[Boolean] = "BLANK" ~> "WHEN".? ~> ("ZERO" | "ZEROS" | "ZEROES") ^^^ true
  val clauseParser: Parser[Boolean] = blankParser | globalParser | justifyParser
  val descriptionParser = wrappedFillerParser.? ~> clauseParser.* ^^ {
    case l: List[Boolean] => l.foldLeft(false)((acc, x) => acc | x)
  }

  class WordReader(lineNumber: Int, wordNumber: Int, words: Array[String]) extends Reader[String] with Position {
    def atEnd: Boolean = wordNumber > words.length
    def first: String = words(wordNumber - 1)
    def pos: Position = this
    def column = wordNumber
    def line = lineNumber
    def lineContents = {
      words.foldLeft(new StringBuilder)((acc, w) => acc.append(w).append(' ')).toString
    }
    override def longString = {
      val position = words.take(wordNumber - 1).foldLeft(-1)((acc, w) => acc + w.length + 1)
      val marker = new java.lang.StringBuilder
      (0 to position).foreach { _ => marker.append(' ') }
      lineContents + '\n' + marker.append('^').toString
    }
    def rest: Reader[String] = new WordReader(lineNumber, wordNumber + 1, words)
  }

  private def describe(input: Input) = {
    if (input.atEnd) "empty"
    else input.first
  }

  def parse(input: Array[String]) = {
    println(s"\nParsing ${input.toList}")
    val outcome = descriptionParser(new WordReader(1, 1, input))
    println(s"Outcome is $outcome")
    outcome match {
      case Success(result, next) => println(s"Got result $result with next ${describe(next)}")
      case Failure(message, next) => println(s"Got failure '$message' with next ${describe(next)}")
      case Error(message, next) => println(s"Got error '$message' with next ${describe(next)}")
    }
  }

  // simple base parsers
  def main(args: Array[String]): Unit = {
    parse(Array("JUST", "RIGHT", "GLOBAL", "BLANK", "ZERO", "END"))
    parse(Array("FILLER", "JUST", "RIGHT", "GLOBAL", "BLANK", "ZERO"))
    parse(Array("xyz", "JUST", "RIGHT", "GLOBAL", "BLANK", "ZERO"))
    parse(Array("xyz", "JUST", "abc", "RIGHT", "GLOBAL", "BLANK", "ZERO"))
    parse(Array("xyz", "JUST", "RIGHT", "abc", "GLOBAL", "BLANK", "ZERO"))
    parse(Array("abc", "REDEFINES", "xyz", "JUST", "RIGHT", "GLOBAL", "BLANK", "ZERO"))
    val reader = new WordReader(1, 1, Array("abc", "REDEFINES", "xyz", "JUST", "RIGHT", "GLOBAL", "BLANK", "ZERO"))
    println(reader.pos.longString)
    println(reader.pos.toString)
    println(reader.rest.pos.longString)
    println(reader.rest.pos.toString)
  }

}