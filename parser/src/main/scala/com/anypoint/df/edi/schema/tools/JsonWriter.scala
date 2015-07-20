package com.anypoint.df.edi.schema.tools

import java.io.Writer
import java.io.StringWriter
import scala.annotation.tailrec
import collection.JavaConverters._

trait Maps {
  type ValueMap = java.util.Map[String, Object]
  type ValueMapImpl = java.util.HashMap[String, Object]
  type MapList = java.util.List[ValueMap]
  type MapListImpl = java.util.ArrayList[ValueMap]
  type SimpleList = java.util.List[Object]
  type SimpleListImpl = java.util.ArrayList[Object]
}

/** Write map as JSON.
  */
class JsonWriter extends Maps {
  
  def write(map: ValueMap) = {

    val indentText = "  "
    val builder = new StringBuilder
    var depth = 0

    /** Append simple key-value pair. */
    def keyValuePair[T](key: String, map: ValueMap) = {
      builder ++= key.replace(' ', '_') ++= ": "
      map.get(key) match {
        case s: String => {
          builder ++= "\""
          s.toList.foreach(chr =>
            if (chr == '"') builder ++= "\\\""
            else builder + chr)
          builder ++= "\""
        }
        case v => builder ++= v.toString
      }
    }

    /** Append newline and indent to level. */
    def newline = builder ++= "\n" ++= (indentText * depth)

    def writeMap(map: ValueMap): Unit = {
      val keys = map.keySet.asScala.toList.sorted
      val simple = keys.forall { key =>
        {
          val value = map.get(key)
          !value.isInstanceOf[ValueMap] && !value.isInstanceOf[MapList]
        }
      }
      builder ++= "{ "
      if (simple) {
        keyValuePair(keys.head, map)
        keys.tail.foreach { key =>
          {
            builder ++= ", "
            keyValuePair(key, map)
          }
        }
        builder ++= "}"
      } else {
        depth += 1
        newline
        keyValuePair(keys.head, map)
        keys.tail.foreach { key =>
          {
            builder ++= ","
            newline
            map.get(key) match {
              case l: MapList => {
                builder ++= key ++= ": "
                writeList(l)
              }
              case _ => keyValuePair(key, map)
            }
          }
        }
        depth -= 1
        newline
        builder ++= "}"
      }
    }

    def writeList(list: MapList) = {
      builder ++= "["
      depth += 1
      list.asScala.foreach { map =>
        {
          newline
          writeMap(map)
        }
      }
      depth -= 1
      newline
      builder ++= "]"
    }
    writeMap(map)
    builder.toString
  }
}

object JsonWriter extends App with Maps {
  val testmap = new ValueMapImpl
  testmap put ("a", new Integer(123))
  testmap put ("b", "abc")
  val maplist = new MapListImpl
  val childmap = new ValueMapImpl
  childmap put ("c", new Integer(456))
  childmap put ("d", "def")
  maplist add childmap
  testmap put ("e", maplist)
  println(new JsonWriter().write(testmap))
}