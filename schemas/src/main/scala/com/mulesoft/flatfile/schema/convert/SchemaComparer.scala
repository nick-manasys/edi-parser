//package com.mulesoft.flatfile.schema.convert
//
//import java.io.InputStream
//import scala.collection.mutable.MutableList
//
//object SchemaComparer {
//
//  /** Convert input to list of arrays of strings (list reverse ordered). */
//  //  def lineList(in: InputStream) = foldInput(in, Nil.asInstanceOf[LineFields])((list, line) => line.toArray :: list)
//
//  //  val groupedMsgStructs = lineList(fileInput(version, messageStructures)).reverse.groupBy { _(0) }
//  //  val structures = buildStructures(groupedMsgStructs, segments, schemaVersion).sortBy { _.ident }
//
//  def main(args: Array[String]): Unit = {
//    println("hi")
//
//  }
//
//  val test = List(
//    new Line("RPL_I02", "1", "MSH", "", "0", "0"),
//    new Line("RPL_I02", "2", "MSA", "", "0", "0"),
//    new Line("RPL_I02", "3", "{", "PROVIDER", "0", "0"),
//    new Line("RPL_I02", "4", "PRD", "", "0", "0"),
//    new Line("RPL_I02", "7", "CTD", "", "1", "1"),
//    new Line("RPL_I02", "10", "}", "", "0", "0"),
//    new Line("RPL_I02", "13", "NTE", "", "1", "1"),
//    new Line("RPL_I02", "18", "DSP", "", "1", "1"),
//    new Line("RPL_I02", "22", "DSC", "", "0", "1"))
//  var result = constructStructure(test)
//  println(result)
//
//  //  /**
//  //   * Construct a structure from a list of lines
//  //   */
//    def constructStructure(lines: List[Line]): Message = {
//      var message = new Message(lines(0).code, MutableList())
//      var line: Line = null
//      var linesRest = lines
//      // println(lines)
//      while (!linesRest.isEmpty) {
//        line = linesRest.head
//        linesRest = linesRest.tail
//        // println(line)
//        if (groupStart(line.segmentName)) {
//          val (l, sG) = constructStructureGroup(line.group, line.segmentName.contains("{"), line.segmentName.contains("["), linesRest)
//          linesRest = l
//          message.add(sG)
//        } else
//          message.add(new HL7Segment(line.segmentName, "1".equals(line.cardinality), "1".equals(line.optional)))
//      }
//      println(message)
//      message
//    }
//  
//    def constructStructureGroup(name: String, cardinality: Boolean, optional: Boolean, lines: List[Line]): (List[Line], HL7SegmentGroup) = {
//      var result = new HL7SegmentGroup(name, cardinality, optional)
//      var line: Line = null
//      var linesRest = lines
//      while (!linesRest.isEmpty) {
//        line = linesRest.head
//        linesRest = linesRest.tail
//        if (groupStart(line.segmentName)) {
//          val (l, sG) = constructStructureGroup(line.group, line.segmentName.contains("{"), line.segmentName.contains("["), linesRest)
//          linesRest = l
//          result.add(sG)
//        } else if (groupEnd(line.segmentName)) {
//          return (linesRest, result)
//        } else {
//          result.add(new HL7Segment(line.segmentName, "1".equals(line.cardinality), "1".equals(line.optional)))
//        }
//      }
//      (linesRest, result)
//    }
//  
//    def groupStart(segmentName: String) = {
//      "[".equals(segmentName) || "{".equals(segmentName) || "[{".equals(segmentName)
//    }
//  
//    def groupEnd(segmentName: String) = {
//      "]".equals(segmentName) || "}".equals(segmentName) || "}]".equals(segmentName)
//    }
//  }
//  
//  object Line {
//    def apply(fields: Array[String]): Line = {
//      val code = fields(0)
//      val position = fields(1)
//      val messageType = fields(2)
//      val group = fields(3)
//      val cardinality = fields(4)
//      val optional = fields(5)
//      new Line(code, position, messageType, group, cardinality, optional)
//    }
//  }
//  
//  class Line(val code: String, position: String, val segmentName: String, val group: String, val cardinality: String, val optional: String) {
//    override def toString(): String = "(" + code + ", " + position + ", " + segmentName + ", " + group + ", " + cardinality + ", " + optional + ")";
//  }
//  
//  /**
//   * Base class for HL7 Segment
//   */
//  class HL7SegmentBase {
//  
//  }
//  
//  class HL7Segment(val name: String, cardinality: Boolean, optional: Boolean) extends HL7SegmentBase {
//    override def toString(): String = {
//      var result = name
//      if (cardinality) result = "{ " + result + " }"
//      if (optional) result = "[ " + result + " ]"
//      result
//    }
//  }
//  
//  class HL7SegmentGroup(val name: String, cardinality: Boolean, optional: Boolean) extends HL7SegmentBase {
//    val segments = new MutableList[HL7SegmentBase]()
//  
//    def add(segment: HL7SegmentBase): Unit = {
//      segments += segment
//    }
//  
//    override def toString(): String = {
//      var result = name + "\n"
//      for (segment <- segments) {
//        result += "    " + segment.toString() + "\n"
//      }
//      if (cardinality) result = "{ " + result + "    }"
//      if (optional) result = "[ " + result + "    ]"
//      result = result + " " + name + "\n"
//      result
//    }
//  }
//  
//  class Message(code: String, segments: MutableList[HL7SegmentBase]) {
//    def add(segment: HL7SegmentBase): Unit = {
//      segments += segment
//    }
//  
//    override def toString(): String = {
//      var result = ""
//      result += "Message Structure: " + code + "\n=====>\n"
//      for (segment <- segments) {
//        result += "    " + segment + "\n"
//      }
//      result += "\n<===="
//      result
//    }
//}
