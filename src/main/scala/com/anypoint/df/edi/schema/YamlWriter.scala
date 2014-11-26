package com.anypoint.df.edi.schema

import java.io.Writer
import java.io.StringWriter
import scala.annotation.tailrec

/** Write YAML representation of EDI schema.
  *
  * @author MuleSoft, Inc.
  */
object YamlWriter {

  import EdiSchema._

  val indentText = "  "

  /** Write simple key-value pair. */
  def keyValuePair(key: String, value: String) = key + ": " + value

  /** Write key-quoted value pair. */
  def keyValueQuote(key: String, value: String) = key + ": '" + value + '\''

  /** Get repetition count text value. */
  def countText(count: Int) =
    if (count == 0) "'>1'"
    else count toString

  /** Write schema in YAML form.
    *
    * @param schema
    * @param writer
    */
  def write(schema: EdiSchema, writer: Writer) = {

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
          writeIndented("- { " + keyValueQuote("idRef", refer.segment.ident) + ", " +
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
      def componentId(component: SegmentComponent) = component match {
        case ElementComponent(element, _, _, _, _) => element.ident
        case CompositeComponent(composite, _, _, _, _) => composite.ident
      }
      @tailrec
      def writer(remain: List[SegmentComponent], dfltpos: Int): Unit = remain match {
        case comp :: t =>
          {
            writeIndented("- { " + keyValueQuote("idRef", componentId(comp)) + ", " +
              keyValueQuote("name", comp.name) + ", " +
              (if (comp.position == dfltpos) "" else (keyValuePair("position", comp.position.toString) + ", ")) +
              keyValuePair("usage", comp.usage.code toString) +
              (if (comp.count != 1) ", " + keyValuePair("count", countText(comp.count)) else "") +
              " }", indent)
            writer(t, dfltpos + 1)
          }
        case _ =>
      }
      writeIndented(label + ":", indent)
      writer(comps, 1)
    }

    // start with schema type
    writeIndented(keyValuePair("form", schema.ediForm.text), 0)

    // write transaction details
    writeIndented("transactions:", 0)
    schema.transactions.values foreach (transact => {
      writeIndented("- " + keyValueQuote("id", transact.ident), 0)
      writeIndented(keyValuePair("name", transact.name), 1)
      writeIndented(keyValuePair("group", transact.group), 1)
      if (transact.heading.size > 0) writeTransactionComps("heading", transact.heading, 1)
      if (transact.detail.size > 0) writeTransactionComps("detail", transact.detail, 1)
      if (transact.summary.size > 0) writeTransactionComps("summary", transact.summary, 1)
    })

    // next write segment details
    writeIndented("segments:", 0)
    schema.segments.values foreach (segment => {
      writeIndented("- " + keyValueQuote("id", segment.ident), 0)
      writeIndented(keyValuePair("name", segment name), 1)
      writeSegmentComponents("values", segment.components, 1)
    })

    // next write composites details
    if (!schema.composites.isEmpty) {
      writeIndented("composites:", 0)
      schema.composites.values foreach (composite => {
        writeIndented("- " + keyValueQuote("id", composite.ident), 0)
        writeIndented(keyValuePair("name", composite name), 1)
        writeSegmentComponents("values", composite.components, 1)
      })
    }

    // finish with element details
    writeIndented("elements:", 0)
    schema.elements.values foreach (element =>
      writeIndented("- { " + keyValueQuote("id", element.ident) + ", " +
        keyValuePair("type", element.dataType code) + ", " +
        keyValuePair("minLength", element.minLength toString) + ", " +
        keyValuePair("maxLength", element.maxLength toString) + " }", 0))
  }
}