package com.anypoint.df.edi.schema

import java.io.Writer
import java.io.StringWriter
import scala.annotation.tailrec

trait WritesYaml {

  val indentText = "  "

  /** Write simple key-value pair. */
  def keyValuePair(key: String, value: String) = key + ": " + value

  /** Write simple key-value pair. */
  def keyValuePair(key: String, value: Int) = key + ": " + value.toString

  /** Write key-quoted value pair. */
  def keyValueQuote(key: String, value: String) = {
    val builder = new StringBuilder
    builder ++= key ++= ": '"
    value.toList.foreach(chr =>
      if (chr == '\'') builder ++= "''"
      else builder + chr)
    builder + '\''
    builder.toString
  }

  /** Write text indended to level. */
  def writeIndented(text: String, indent: Int, writer: Writer) = {
    def writeIndent(indent: Int) = writer append (indentText * indent)
    writeIndent(indent)
    writer append text
    writer append "\n"
  }

  /** Get repetition count text value. */
  def countText(count: Int) =
    if (count == 0) "'>1'"
    else count toString
}

/** Write YAML representation of EDI schema.
  *
  * @author MuleSoft, Inc.
  */
object YamlWriter extends WritesYaml with YamlDefs {

  import EdiSchema._

  /** Write schema in YAML form.
    *
    * @param schema
    * @param imports
    * @param writer
    */
  def write(schema: EdiSchema, imports: Array[String], writer: Writer) = {

    def writeReferenceComponent(refer: ReferenceComponent, indent: Int): Unit = {
      writeIndented("- { " + keyValueQuote(idRefKey, refer.segment.ident) + ", " +
        keyValueQuote(positionKey, refer.position.position) + ", " + keyValuePair(usageKey, refer.usage.code toString) +
        (if (refer.count != 1) ", " + keyValuePair(countKey, countText(refer.count)) else "") +
        " }", indent, writer)
    }

    def writeWrapperComponent(wrap: LoopWrapperComponent, indent: Int): Unit = {
      writeIndented("- " + keyValueQuote(wrapIdKey, wrap.ident), indent, writer)
      writeIndented(keyValueQuote(positionKey, wrap.position.position), indent + 1, writer)
      writeIndented(keyValueQuote(endPositionKey, wrap.endPosition.position), indent + 1, writer)
      writeIndented(keyValuePair(usageKey, wrap.usage.code toString), indent + 1, writer)
      writeIndented(s"$loopKey:", indent + 1, writer)
      writeGroupComponent(wrap.loopGroup, indent + 1)
    }

    def writeGroupComponent(group: GroupComponent, indent: Int): Unit = {
      writeIndented("- " + keyValueQuote(loopIdKey, group.ident), indent, writer)
      writeIndented(keyValuePair(usageKey, group.usage.code toString), indent + 1, writer)
      if (group.count != 1) writeIndented(keyValuePair(countKey, countText(group.count)), indent + 1, writer)
      writeTransactionComps(itemsKey, group.items, indent + 1)
    }

    def writeTransactionComps(label: String, segments: List[TransactionComponent], indent: Int): Unit = {
      writeIndented(label + ":", indent, writer)
      segments foreach (segbase => segbase match {
        case refer: ReferenceComponent => writeReferenceComponent(refer, indent)
        case wrap: LoopWrapperComponent => writeWrapperComponent(wrap, indent)
        case group: GroupComponent => writeGroupComponent(group, indent)
      })
    }

    def writeSegmentComponents(label: String, comps: List[SegmentComponent], indent: Int): Unit = {
      def componentId(component: SegmentComponent) = component match {
        case ElementComponent(element, _, _, _, _, _) => element.ident
        case CompositeComponent(composite, _, _, _, _, _) => composite.ident
      }
      def componentDefaultNamed(component: SegmentComponent) = component match {
        case ElementComponent(element, name, _, _, _, _) => component.name == element.name
        case CompositeComponent(composite, _, _, _, _, _) => component.name == composite.name
      }
      @tailrec
      def writerr(remain: List[SegmentComponent], dfltpos: Int): Unit = remain match {
        case comp :: t =>
          {
            writeIndented("- { " + keyValueQuote(idRefKey, componentId(comp)) + ", " +
              (if (componentDefaultNamed(comp)) "" else (keyValueQuote(nameKey, comp.name) + ", ")) +
              (if (comp.position == dfltpos) "" else (keyValuePair(positionKey, comp.position.toString) + ", ")) +
              keyValuePair(usageKey, comp.usage.code toString) +
              (if (comp.count != 1) ", " + keyValuePair(countKey, countText(comp.count)) else "") +
              " }", indent, writer)
            writerr(t, dfltpos + 1)
          }
        case _ =>
      }
      writeIndented(label + ":", indent, writer)
      writerr(comps, 1)
    }

    // start with schema type and version
    writeIndented(keyValuePair(formKey, schema.ediForm.text), 0, writer)
    writeIndented(keyValueQuote(versionKey, schema.version), 0, writer)
    if (imports.nonEmpty) {

      // write list of imports
      writer.append(s"$importsKey: [ '${imports.head}'")
      imports.tail.foreach { path => writer.append(s", '$path'") }
      writer.append(" ]\n")

    }
    if (!schema.transactions.isEmpty) {

      // write transaction details
      writeIndented(s"$transactionsKey:", 0, writer)
      schema.transactions.values.toList.sortBy { transact => transact.ident } foreach (transact => {
        writeIndented("- " + keyValueQuote(idKey, transact.ident), 0, writer)
        writeIndented(keyValuePair(nameKey, transact.name), 1, writer)
        writeIndented(keyValuePair(groupKey, transact.group), 1, writer)
        if (transact.heading.size > 0) writeTransactionComps(headingKey, transact.heading, 1)
        if (transact.detail.size > 0) writeTransactionComps(detailKey, transact.detail, 1)
        if (transact.summary.size > 0) writeTransactionComps(summaryKey, transact.summary, 1)
      })
    }
    if (!schema.segments.isEmpty) {

      // write segment details
      writeIndented("segments:", 0, writer)
      schema.segments.values.toList.sortBy { segment => segment.ident } foreach (segment => {
        writeIndented("- " + keyValueQuote(idKey, segment.ident), 0, writer)
        writeIndented(keyValuePair(nameKey, segment name), 1, writer)
        writeSegmentComponents(valuesKey, segment.components, 1)
        if (!segment.rules.isEmpty) {
          writeIndented(s"$rulesKey:", 1, writer)
          segment.rules foreach (rule => {
            val builder = new StringBuilder
            builder ++= "- {" ++= keyValueQuote(typeKey, rule.code) ++= s" $valuesKey: ["
            builder ++= rule.components.head.position.toString
            rule.components.tail foreach (comp => builder ++= comp.position.toString ++= ", ")
            builder ++= "]"
            writeIndented(builder.toString, 1, writer)
          })
        }
      })
    }
    if (!schema.composites.isEmpty) {

      // write composites details
      writeIndented(compositesKey + ":", 0, writer)
      schema.composites.values.toList.sortBy { composite => composite.ident } foreach (composite => {
        writeIndented("- " + keyValueQuote(idKey, composite.ident), 0, writer)
        writeIndented(keyValueQuote(nameKey, composite name), 1, writer)
        writeSegmentComponents(valuesKey, composite.components, 1)
      })
    }
    if (!schema.elements.isEmpty) {

      // write element details
      writeIndented("elements:", 0, writer)
      schema.elements.values.toList.sortBy { element => element.ident } foreach (element =>
        writeIndented("- { " + keyValueQuote(idKey, element.ident) + ", " +
          keyValueQuote(nameKey, element name) + ", " +
          keyValuePair(typeKey, element.dataType.code) + ", " +
          keyValuePair(minLengthKey, element.minLength toString) + ", " +
          keyValuePair(maxLengthKey, element.maxLength toString) + " }", 1, writer))
    }
  }
}