package com.anypoint.df.edi.schema

import java.io.Writer
import java.io.StringWriter
import scala.annotation.tailrec

trait WritesYaml {

  val indentText = "  "

  /** Write simple key-value pair. */
  def keyValuePair(key: String, value: String) = key + ": " + value

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
}

/** Write YAML representation of EDI schema.
  *
  * @author MuleSoft, Inc.
  */
object YamlWriter extends WritesYaml with YamlDefs {

  import EdiSchema._

  /** Get repetition count text value. */
  def countText(count: Int) =
    if (count == 0) "'>1'"
    else count toString

  /** Write schema in YAML form.
    *
    * @param schema
    * @param writer
    */
  def write(schema: EdiSchema, imports: List[String], writer: Writer) = {

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
          writeIndented("- { " + keyValueQuote(idRefKey, refer.segment.ident) + ", " +
            keyValueQuote(positionKey, refer.position) + ", " + keyValuePair(usageKey, refer.usage.code toString) +
            (if (refer.count != 1) ", " + keyValuePair(countKey, countText(refer.count)) else "") +
            " }", indent)
        case group: GroupComponent => {
          writeIndented("- " + keyValueQuote(loopIdKey, group.ident), indent)
          writeIndented(keyValuePair(usageKey, group.usage.code toString), indent + 1)
          if (group.count != 1) writeIndented(keyValuePair(countKey, countText(group.count)), indent + 1)
          writeTransactionComps(itemsKey, group.items, indent + 1)
        }
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
      def writer(remain: List[SegmentComponent], dfltpos: Int): Unit = remain match {
        case comp :: t =>
          {
            writeIndented("- { " + keyValueQuote(idRefKey, componentId(comp)) + ", " +
              (if (componentDefaultNamed(comp)) "" else (keyValueQuote(nameKey, comp.name) + ", ")) +
              (if (comp.position == dfltpos) "" else (keyValuePair(positionKey, comp.position.toString) + ", ")) +
              keyValuePair(usageKey, comp.usage.code toString) +
              (if (comp.count != 1) ", " + keyValuePair(countKey, countText(comp.count)) else "") +
              " }", indent)
            writer(t, dfltpos + 1)
          }
        case _ =>
      }
      writeIndented(label + ":", indent)
      writer(comps, 1)
    }

    // start with schema type and version
    writeIndented(keyValuePair(formKey, schema.ediForm.text), 0)
    writeIndented(keyValueQuote(versionKey, schema.version), 0)
    if (imports.nonEmpty) {
      
      // write list of imports
      writer.append(s"$importsKey: ['${imports.head}'")
      imports.tail.foreach { path => writer.append(s", '$path'") }
      writer.append(" ]\n")
      
    }
    if (!schema.transactions.isEmpty) {

      // write transaction details
      writeIndented(s"$transactionsKey:", 0)
      schema.transactions.values foreach (transact => {
        writeIndented("- " + keyValueQuote(idKey, transact.ident), 0)
        writeIndented(keyValuePair(nameKey, transact.name), 1)
        writeIndented(keyValuePair(groupKey, transact.group), 1)
        if (transact.heading.size > 0) writeTransactionComps(headingKey, transact.heading, 1)
        if (transact.detail.size > 0) writeTransactionComps(detailKey, transact.detail, 1)
        if (transact.summary.size > 0) writeTransactionComps(summaryKey, transact.summary, 1)
      })
    }
    if (!schema.segments.isEmpty) {

      // write segment details
      writeIndented("segments:", 0)
      schema.segments.values foreach (segment => {
        writeIndented("- " + keyValueQuote(idKey, segment.ident), 0)
        writeIndented(keyValuePair(nameKey, segment name), 1)
        writeSegmentComponents(valuesKey, segment.components, 1)
        if (!segment.rules.isEmpty) {
          writeIndented(s"$rulesKey:", 1)
          segment.rules foreach (rule => {
            val builder = new StringBuilder
            builder ++= "- {" ++= keyValueQuote(typeKey, rule.code) ++= s" $valuesKey: ["
            builder ++= rule.components.head.position.toString
            rule.components.tail foreach (comp => builder ++= comp.position.toString ++= ", ")
            builder ++= "]"
            writeIndented(builder.toString, 1)
          })
        }
      })
    }
    if (!schema.composites.isEmpty) {

      // write composites details
      writeIndented(compositesKey + ":", 0)
      schema.composites.values foreach (composite => {
        writeIndented("- " + keyValueQuote(idKey, composite.ident), 0)
        writeIndented(keyValueQuote(nameKey, composite name), 1)
        writeSegmentComponents(valuesKey, composite.components, 1)
      })
    }
    if (!schema.elements.isEmpty) {

      // write element details
      writeIndented("elements:", 0)
      schema.elements.values foreach (element =>
        writeIndented("- { " + keyValueQuote(idKey, element.ident) + ", " +
          keyValueQuote(nameKey, element name) + ", " +
          keyValuePair(typeKey, element.dataType.code) + ", " +
          keyValuePair(minLengthKey, element.minLength toString) + ", " +
          keyValuePair(maxLengthKey, element.maxLength toString) + " }", 0))
    }
  }
}