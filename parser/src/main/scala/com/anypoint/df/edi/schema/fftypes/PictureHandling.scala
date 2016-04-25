package com.anypoint.df.edi.schema.fftypes

import scala.annotation.tailrec

object PictureHandling {

  implicit class BuilderAssist(b: StringBuilder) {
    def appendReps(count: Int, chr: Char): Unit = {
      @tailrec
      def appendr(remain: Int): Unit = {
        if (remain > 0) {
          b.append(chr)
          appendr(remain - 1)
        }
      }
      appendr(count)
    }
  }

  abstract class PictureExpander {
    
    val allowedChars: Set[Char]
    val dataChars: Set[Char]
    
    def expandPicture(picture: String): (String, Int) = {
      val builder = new StringBuilder

      @tailrec
      def repeatr(rem: Seq[Char], count: Int): Unit =
        if (rem.isEmpty) throw new IllegalArgumentException(s"No closing ')' for repeat count")
        else {
          val next = rem.head
          if (next.isDigit) repeatr(rem.tail, count * 10 + next - '0')
          else if (next == ')') {
            builder.appendReps(count - 1, builder.last)
            picturer(rem.tail)
          } else throw new IllegalArgumentException(s"Invalid character '$next' in repeat count")
        }

      @tailrec
      def picturer(rem: Seq[Char]): Unit =
        if (rem.nonEmpty) {
          val next = rem.head
          if (allowedChars.contains(next)) {
              builder.append(next)
              picturer(rem.tail)
          } else if (next == '(') {
              if (builder.isEmpty) throw new IllegalArgumentException(s"Picture character must precede repeat count")
              else repeatr(rem.tail, 0)
          } else throw new IllegalArgumentException(s"Invalid character '$next' in alpha/alphanumeric picture")
        }

      picturer(picture.toSeq)
      val expanded = builder.toString
      (expanded, expanded.count { dataChars.contains(_) })
    }
  }
  
  class PictureFiller(picture: List[Char]) {
    def build(lead: Int, text: List[Char]): String = {
      val builder = new StringBuilder
      def fillr(count: Int, pic: List[Char]): List[Char] = {
        if (count <= 0) pic
        else pic match {
          case h :: t =>
            h match {
              case 'A' | 'X' | 'B' | 'Z' => builder.append(' ')
              case '/' | ',' | '.' | '0' | '*'  => builder.append(h)
              case _ => throw new IllegalArgumentException(s"Picture character '$h' with no matching data")
            }
            fillr(count - 1, t)
          case _ => pic
        }
      }
      def matchr(text: List[Char], pic: List[Char]): List[Char] = {
        text match {
          case r :: remt =>
            pic match {
                case h :: remp =>
                  h match {
                    case 'A' =>
                      if (r.isLetter || r == ' ') {
                        builder.append(r)
                        matchr(remt, remp)
                      } else throw new IllegalArgumentException(s"Character '$r' doesn't match picture character 'A'")
                    case 'X' =>
                        builder.append(r)
                        matchr(remt, remp)
                    case 'Z' | '*' | '9' =>
                      if (r.isDigit) builder.append(r)
                      else throw new IllegalArgumentException(s"Character '$r' doesn't match picture character 'Z'")
                      matchr(remt, remp)
                    case 'B' =>
                      builder.append(' ')
                      matchr(text, remp)
                    case '/' | ',' | '.' | '0' =>
                      builder.append(h)
                      matchr(text, remp)
                    case _ => throw new IllegalArgumentException(s"Pattern character '$h' with no matching data")
                  }
                case _ => Nil
            }
          case _ => pic
        }
      }
      
      val body = fillr(lead, picture)
      val rest = matchr(text, body)
      fillr(Int.MaxValue, rest)
      builder.toString
    }
  }
}