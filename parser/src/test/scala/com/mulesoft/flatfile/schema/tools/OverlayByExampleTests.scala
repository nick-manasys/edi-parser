package com.mulesoft.flatfile.schema.tools

import collection.Map
import collection.JavaConverters._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalactic.Equality
import com.mulesoft.flatfile.schema.SchemaJavaDefs

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
    map2 put ("a", "1")
    map2 put ("c", "3")
    mergeMaps(map1, map2)
    toScala(map2) should equal (Map("a" -> "1", "b" -> "2", "c" -> "3"))
    val map3 = new ValueMapImpl
    map3 put ("x", Integer.valueOf(1))
    map3 put ("y", Integer.valueOf(2))
    mergeMaps(map1, map3)
    toScala(map3) should equal (Map("a" -> "1", "b" -> "2", "x" -> Integer.valueOf(1), "y" -> Integer.valueOf(2)))
  }

  it should "merge lists of maps down to maps" in {
    val map1 = new ValueMapImpl
    map1 put ("a", "1")
    map1 put ("b", "2")
    val map2 = new ValueMapImpl
    map2 put ("a", "1")
    map2 put ("c", "3")
    val map3 = new ValueMapImpl
    map3 put ("x", Integer.valueOf(1))
    map3 put ("y", Integer.valueOf(2))
    val list1 = new MapListImpl
    list1 add map1
    val root1 = new ValueMapImpl
    root1 put ("m", map3)
    root1 put ("l", list1)
    val to1 = new ValueMapImpl
    mergeMaps(root1, to1)
    toScala(to1) should equal (Map("l" -> Map("a" -> "1", "b" -> "2"), "m" -> Map("x" -> Integer.valueOf(1), "y" -> Integer.valueOf(2))))
    val list2 = new MapListImpl
    list2 add map1
    list2 add map2
    val root2 = new ValueMapImpl
    root2 put ("m", map3)
    root2 put ("l", list2)
    val to2 = new ValueMapImpl
    mergeMaps(root2, to2)
    toScala(to2) should equal (Map("l" -> Map("a" -> "1", "b" -> "2", "c" -> "3"), "m" -> Map("x" -> Integer.valueOf(1), "y" -> Integer.valueOf(2))))
  }
}