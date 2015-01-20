package com.anypoint.df.edi.schema.tools

import collection.Map
import collection.JavaConverters._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalactic.Equality
import com.anypoint.df.edi.schema.SchemaJavaDefs

class OverlayByExampleTests extends FlatSpec with Matchers with SchemaJavaDefs {

  import OverlayByExample._

  def toScala(jm: ValueMap): Map[String, Object] = {
    val sm = jm.asScala
    sm.mapValues(v => v match {
      case m: ValueMap => toScala(m)
      case l: SimpleList => throw new IllegalStateException("should be no embedded lists in result")
      case v => v
    })
  }

  behavior of "mergeMaps"

  it should "merge one simple map into another simple map" in {
    val map1 = new ValueMapImpl
    map1 put ("a", "1")
    map1 put ("b", "2")
    val map2 = new ValueMapImpl
    map1 put ("a", "1")
    map1 put ("c", "3")
    mergeMaps(map1, map2)
    toScala(map2) should equal (Map("a" -> "1", "b" -> "2", "c" -> "3"))
    val map3 = new ValueMapImpl
    map3 put ("x", Integer.valueOf(1))
    map3 put ("y", Integer.valueOf(2))
    mergeMaps(map1, map3)
    toScala(map3) should equal (Map("a" -> "1", "b" -> "2", "c" -> "3", "x" -> Integer.valueOf(1), "y" -> Integer.valueOf(2)))
  }

  it should "merge lists of maps down to maps" in {
    val map1 = new ValueMapImpl
    map1 put ("a", "1")
    map1 put ("b", "2")
    val map2 = new ValueMapImpl
    map1 put ("a", "1")
    map1 put ("c", "3")
    val map3 = new ValueMapImpl
    map3 put ("x", Integer.valueOf(1))
    map3 put ("y", Integer.valueOf(2))
    val list = new MapListImpl
    list add map1
    list add map2
    val rootmap = new ValueMapImpl
    rootmap put ("m", map3)
    rootmap put ("l", list)
    val root = new ValueMapImpl
    mergeMaps(rootmap, root)
    toScala(root) should equal (Map("l" -> Map("a" -> "1", "b" -> "2", "c" -> "3"), "m" -> Map("x" -> Integer.valueOf(1), "y" -> Integer.valueOf(2))))
  }
}