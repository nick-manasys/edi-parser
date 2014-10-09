package com.anypoint.df.edi.schema

import java.io.Writer
import java.io.StringWriter

/**
 * TODO
 *
 */
object YamlWriter {

  import EdiSchema._

  val indentText = "  "

  def keyValuePair(key: String, value: String) = key + ": " + value
  def keyValueQuote(key: String, value: String) = key + ": '" + value + '\''

  def countText(count: Int) =
    if (count == 0) ">1"
    else count toString


  def write(schema: Schema, writer: Writer) = {

  def writeIndent(indent: Int) = writer append (indentText * indent)

  def writeIndented(text: String, indent: Int) = {
    writeIndent(indent)
    writer append text
    writer append "\n"
  }

  def writeTransactionComps(label: String, segments: List[TransactionComponent], indent: Int): Unit = {
    writeIndented(label + ":", indent)
    segments foreach (segbase => segbase match {
      case refer: ReferenceComponent =>
        writeIndented("- { " + keyValueQuote("idRef", refer.ident) + ", " +
          keyValuePair("usage", refer.usage.code toString) +
          (if (refer.count != 1) ", " + keyValuePair("count", countText(refer.count)) else "") +
          " }", indent)
      case group: GroupComponent => {
        writeIndented("- " + keyValueQuote("loopId", group.ident), indent)
        writeIndented(keyValuePair("usage", group.usage.code toString), indent + 1)
        if (group.count != 1) writeIndented(keyValuePair("count", countText(group.count)), indent + 1)
        writeTransactionComps("items", group.items, indent + 1)
      }
    })
  }
  
  def writeSegmentComponents(label: String, comps: List[SegmentComponent], indent: Int): Unit = {
    writeIndented(label + ":", indent)
    comps foreach (comp => 
        writeIndented("- { " + keyValueQuote("idRef", comp.ident) + ", " +
          keyValueQuote("name", comp.name) + ", " +
          keyValuePair("usage", comp.usage.code toString) +
          (if (comp.count != 1) ", " + keyValuePair("count", countText(comp.count)) else "") +
          " }", indent)
      )
  }
  writeIndented("transactions:", 0)
    schema.transactions foreach (transact => {
      writeIndented("- " + keyValueQuote("id", transact.ident), 0)
      if (transact.heading.size > 0) writeTransactionComps("heading", transact.heading, 1)
      if (transact.detail.size > 0) writeTransactionComps("detail", transact.detail, 1)
      if (transact.summary.size > 0) writeTransactionComps("summary", transact.summary, 1)
    })
    writeIndented("segments:", 0)
    schema.segments foreach (segment => {
        writeIndented("- " + keyValueQuote("id", segment.ident), 0)
        writeIndented(keyValuePair("name", segment name), 1)
        writeSegmentComponents("values", segment.components, 1)
    })
    writeIndented("elements:", 0)
    schema.elements foreach (element =>
        writeIndented("- { " + keyValueQuote("id", element.ident) + ", " +
          keyValuePair("type", element.dataType code) + ", " +
          keyValuePair("minLength", element.minLength toString) + ", " +
          keyValuePair("maxLength", element.maxLength toString) + " }", 0)
    )
  }

}
