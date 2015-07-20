package com.anypoint.df.edi.schema.tools

import java.io.Reader
import java.io.StringReader

import scala.annotation.tailrec

/** Read map from JSON.
  */
class JsonReader extends Maps {

  case class WrappedReader(reader: Reader) {

    var peek = reader.read.asInstanceOf[Char]
    var position = 0

    def next = {
      val hold = peek
      peek = reader.read.asInstanceOf[Char]
      position += 1
      hold
    }

    def isEnd = peek == -1.asInstanceOf[Char]

    def isWhite(ch: Int) = ch == ' ' || ch == '\n' || ch == '\r'

    def skipWhite = while (isWhite(peek)) next

    def collect(test: (Char) => Boolean) = {
      val builder = new StringBuilder
      while (test(peek)) { builder.append(next.asInstanceOf[Char]) }
      builder.toString
    }

    def token = collect((c: Char) => c.isLetterOrDigit || c == '_')

    def value = {
      @tailrec
      def quoted(quote: Char): String = {
        next
        val builder = new StringBuilder
        while (peek != quote && !isEnd) {
          val chr = next
          if (chr == '\\') {
            val escaped = next
            escaped match {
              case 'b' => builder.append("\b")
              case 'f' => builder.append("\f")
              case 'n' => builder.append("\n")
              case 'r' => builder.append("\r")
              case 't' => builder.append("\t")
              case _ => builder.append(escaped)
            }
            builder.append(next)
          } else builder.append(chr)
        }
        if (isEnd) error("missing closing quote")
        next
        if (peek == quote) quoted(quote)
        else builder.toString
      }

      if (peek == '"') quoted('"')
      else {
        val digits = collect((c: Char) => c >= '0' && c <= '9')
        if (digits.isEmpty) token
        else {
          Integer.valueOf(digits)
        }
      }
    }

    def error(msg: String) = {
      throw new IllegalStateException(s"$msg at position $position")
    }
  }

  def read(rdr: Reader): ValueMap = {

    val wrapped = WrappedReader(rdr)

    def readValue: Object = {
      wrapped.peek match {
        case '[' => {
          wrapped.next
          wrapped.skipWhite
          val list = new MapListImpl
          while (wrapped.peek != ']') {
            if (wrapped.next != '{') wrapped.error("missing expected opening curly brace for map value")
            list add readMap
            if (wrapped.next != '}') wrapped.error("missing expected closing curly brace for map value")
            wrapped.skipWhite
          }
          wrapped.next
          list
        }
        case _ => wrapped.value
      }
    }

    def readMap: ValueMap = {
      val map = new ValueMapImpl
      wrapped.skipWhite
      while (wrapped.peek != '}') {
        val key = wrapped.token.replace('_', ' ')
        if (key.isEmpty) wrapped.error("missing expected key")
        wrapped.skipWhite
        if (wrapped.next != ':') wrapped.error("missing expected colon after key")
        wrapped.skipWhite
        val value =
          if (wrapped.peek == '{') {
            wrapped.next
            val map = readMap
            if (wrapped.next != '}') wrapped.error("no closing curly brace for nested map")
            map
          } else readValue
        wrapped.skipWhite
        map put (key, value)
        if (wrapped.peek == ',') {
          wrapped.next
          wrapped.skipWhite
        }
      }
      map
    }

    wrapped.skipWhite
    if (wrapped.next != '{') wrapped.error("no opening curly brace for map")
    val map = readMap
    if (wrapped.next != '}') wrapped.error("no closing curly brace for map")
    wrapped.skipWhite
    if (!wrapped.isEnd) wrapped.error("data past end of map")
    map
  }
}

object JsonReader extends App with Maps {
  val text = """{ 
  a: 123,
  b: "abc",
  e: [
    { c: 456, d: "def"}
  ]
  f: { g: 789, h: "ghi" }
}"""
  val reader = new StringReader(text)
  new JsonReader().read(reader)
}