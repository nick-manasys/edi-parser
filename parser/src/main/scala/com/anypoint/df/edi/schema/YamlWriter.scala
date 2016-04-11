package com.anypoint.df.edi.schema

import scala.annotation.tailrec

import java.io.{ StringWriter, Writer }
import java.{ util => ju }

class YamlFormatter(writer: Writer) {

  import EdiSchema.SegmentPosition

  val indentText = "  "
  var indentCount = 0
  var formStack = List(false)
  var firstInner = true

  private def indent = writer append (indentText * indentCount)

  /** Write key with value to follow. */
  def writeKey(key: String): YamlFormatter = {
    if (formStack.head) {
      if (firstInner) firstInner = false
      else writer append ", "
    } else {
      if (firstInner) firstInner = false
      else indent
    }
    writer append key + ": "
    this
  }

  /** Write key and end line. */
  def keyLine(key: String): YamlFormatter = {
    writeKey(key)
    writer append "\n"
    this
  }

  /** Write simple key-value pair. */
  def keyValuePair(key: String, value: String): YamlFormatter = {
    writeKey(key)
    writer append value
    if (!formStack.head) writer append "\n"
    this
  }

  /** Write optional (only if value is non-empty) key-value pair. */
  def keyValueOptionalPair(key: String, value: String): YamlFormatter = {
    if (value != null && value.nonEmpty) keyValuePair(key, value)
    else this
  }

  /** Write simple key-value pair. */
  def keyValuePair(key: String, value: Int): YamlFormatter = keyValuePair(key, value.toString)

  /** Write key-repetition count pair. */
  def keyCountPair(key: String, count: Int): YamlFormatter = {
    if (count == 0) keyValuePair(key, "'>1'")
    else keyValuePair(key, count)
  }

  /** Write optional (only if != 1) key-repetition count pair. */
  def keyCountOptionalPair(key: String, count: Int): YamlFormatter = {
    if (count == 1) this
    else keyCountPair(key, count)
  }

  /** Write optional (only if value != 0) key-value pair. */
  def keyNonzeroOptionalPair(key: String, count: Int): YamlFormatter = {
    if (count == 0) this
    else keyValuePair(key, count)
  }

  /** Write key-quoted value pair. */
  def keyValueQuote(key: String, value: String): YamlFormatter = {
    val builder = new StringBuilder
    builder += '\''
    value.toList.foreach(chr =>
      if (chr == '\'') builder ++= "''"
      else builder + chr)
    builder + '\''
    keyValuePair(key, builder.toString)
  }

  /** Write optional (only if value is non-empty) key-quoted value pair. */
  def keyValueOptionalQuote(key: String, value: String): YamlFormatter = {
    if (value != null && value.nonEmpty) keyValueQuote(key, value)
    else this
  }

  def keyPositionOptionalPair(key: String, pos: SegmentPosition): YamlFormatter = {
    if (pos.defined) keyValueQuote(key, pos.position) else this
  }

  /** Write arbitrary text indended to level. */
  def writeIndented(text: String) = {
    indent
    writer append text
    writer append "\n"
  }

  def openGrouping(compact: Boolean): YamlFormatter = {
    indent
    formStack = compact :: formStack
    if (compact) writer append "- { "
    else writer append "- "
    indentCount += 1
    firstInner = true
    this
  }

  def openGrouping: YamlFormatter = openGrouping(formStack.head)
  
  def closeGrouping = {
    val compact = formStack.head
    formStack = formStack.tail
    if (compact) writer append " }\n"
    indentCount -= 1
  }
  
  def openEmbeddedGrouping: YamlFormatter = {
    writer append "{ "
    firstInner = true
    this
  }
  
  def closeEmbeddedGrouping = {
    writer append " }"
  }
}

/** Write YAML representation of EDI schema.
  */
object YamlWriter extends YamlDefs {

  import EdiSchema._
  
  /** Write schema in YAML form.
    *
    * @param schema
    * @param imports
    * @param writer
    */
  def write(schema: EdiSchema, imports: Array[String], writer: Writer) = {

    val formatter = new YamlFormatter(writer)

    def writeReferenceComponent(refer: ReferenceComponent): Unit = {
      val segment = refer.segment
      formatter.openGrouping(segment.ident.nonEmpty).keyValueOptionalQuote(idRefKey, segment.ident).
        keyPositionOptionalPair(positionKey, refer.position)
      if (!schema.ediVersion.ediForm.fixed) formatter.keyValuePair(usageKey, refer.usage.code)
      formatter.keyCountOptionalPair(countKey, refer.count)
      if (segment.ident.isEmpty) writeSegmentDetails(segment)
      formatter.closeGrouping
    }

    def writeWrapperComponent(wrap: LoopWrapperComponent): Unit = {
      formatter.openGrouping(false).keyValueQuote(wrapIdKey, wrap.groupId).
        keyPositionOptionalPair(positionKey, wrap.position).keyPositionOptionalPair(endPositionKey, wrap.endPosition)
      if (!schema.ediVersion.ediForm.fixed) formatter.keyValuePair(usageKey, wrap.usage.code)
      formatter.keyLine(groupKey)
      writeGroupComponent(wrap.wrapped)
      formatter.closeGrouping
    }

    def writeGroupComponent(group: GroupComponent): Unit = {
      formatter.openGrouping(false).keyValueQuote(groupIdKey, group.ident).keyCountOptionalPair(countKey, group.count)
      if (!schema.ediVersion.ediForm.fixed) formatter.keyValuePair(usageKey, group.usage.code)
      group.tagStart.foreach { x => formatter.keyValuePair(tagStartKey, x) }
      group.tagLength.foreach { x => formatter.keyValuePair(tagLengthKey, x) }
      val childPos = group.seq.items.head.position
      if (group.position != childPos) formatter.keyValueQuote(positionKey, group.position.position)
      writeStructureComps(itemsKey, group.seq.items)
      formatter.closeGrouping
    }

    def writeStructureComps(key: String, segments: List[StructureComponent]): Unit = {
      formatter.keyLine(key)
      segments foreach (segbase => segbase match {
        case refer: ReferenceComponent => writeReferenceComponent(refer)
        case wrap: LoopWrapperComponent => writeWrapperComponent(wrap)
        case group: GroupComponent => writeGroupComponent(group)
      })
    }
    
    def writePair(key: String, value: Object): Unit = {
      value match {
        case i: Integer => formatter.keyValuePair(key, i.intValue)
        case s: String =>
          if (s.length > 0) {
            if (s.head.isLetter && s.forall { x => x.isLetterOrDigit }) formatter.keyValuePair(key, s)
            else formatter.keyValueQuote(key, s)
          }
        case m: ju.Map[String, Object] =>
          formatter.writeKey(key)
          formatter.openEmbeddedGrouping
          val iter = m.keySet.iterator
          while (iter.hasNext) {
            val key = iter.next.asInstanceOf[String]
            writePair(key, m.get(key))
          }
          formatter.closeEmbeddedGrouping
        case null =>
        case _ => throw new IllegalArgumentException(s"No support for type ${value.getClass}")
      }
    }

    def writeElementDetails(elem: Element) = {
      val typ = elem.typeFormat
      formatter.keyValuePair(typeKey, typ.typeCode)
      schema.ediVersion.ediForm.writeFormat(typ, writePair)
    }

    def writeCompositeDetails(comp: Composite) = {
      formatter.keyNonzeroOptionalPair(maxLengthKey, comp.maxLength).keyLine(valuesKey)
      writeSegmentComponents(comp.components)
    }

    def writeSegmentComponents(comps: List[SegmentComponent]): Unit = {
      def writeElement(ecomp: ElementComponent, dfltpos: Int) = {
        val elem = ecomp.element
        formatter.openGrouping(true).keyValueOptionalQuote(idRefKey, elem.ident)
        val named = ecomp.name != elem.name || elem.ident.isEmpty
        if (named) formatter.keyValueOptionalQuote(nameKey, ecomp.name)
        if (ecomp.position != dfltpos) formatter.keyValuePair(positionKey, ecomp.position)
        if (!schema.ediVersion.ediForm.fixed || ecomp.usage != MandatoryUsage) formatter.keyValuePair(usageKey, ecomp.usage.code toString)
        formatter.keyCountOptionalPair(countKey, ecomp.count)
        if (elem.ident.isEmpty) writeElementDetails(elem)
        formatter.closeGrouping
      }
      def writeComposite(ccomp: CompositeComponent, dfltpos: Int) = {
        val comp = ccomp.composite
        formatter.openGrouping(comp.ident.nonEmpty).keyValueOptionalQuote(idRefKey, comp.ident)
        val named = ccomp.name != comp.name || comp.ident.isEmpty
        if (named) formatter.keyValueOptionalQuote(nameKey, ccomp.name)
        if (ccomp.position != dfltpos) formatter.keyValuePair(positionKey, ccomp.position)
        if (!schema.ediVersion.ediForm.fixed || ccomp.usage != MandatoryUsage) formatter.keyValuePair(usageKey, ccomp.usage.code toString)
        formatter.keyCountOptionalPair(countKey, ccomp.count)
        if (comp.ident.isEmpty) writeCompositeDetails(comp)
        formatter.closeGrouping
      }
      @tailrec
      def writerr(remain: List[SegmentComponent], dfltpos: Int): Unit = {
        remain match {
          case comp :: t =>
            comp match {
              case ec: ElementComponent => writeElement(ec, dfltpos)
              case cc: CompositeComponent => writeComposite(cc, dfltpos)
            }
            writerr(t, dfltpos + 1)
          case _ =>
        }
      }

      writerr(comps, 1)
    }

    def writeSegmentDetails(segment: Segment) = {
      formatter.keyValueOptionalQuote(nameKey, segment.name)
      if (segment.ident != segment.tag) formatter.keyValueOptionalQuote(tagKey, segment tag)
      formatter.keyLine(valuesKey)
      writeSegmentComponents(segment.components)
      if (!segment.rules.isEmpty) {
        formatter.keyLine(rulesKey)
        segment.rules foreach (rule => {
          val builder = new StringBuilder
          builder ++= "- { " ++= typeKey ++= ": " ++= rule.code ++= ", " ++= valuesKey ++= ": ["
          builder ++= rule.components.head.position.toString
          rule.components.tail foreach (comp => builder ++= ", " ++= comp.position.toString)
          builder ++= " ]"
          formatter.writeIndented(builder.toString)
        })
      }
    }

    def countNonEmpty(map: Map[String, Any]) = if (map.isEmpty) 0 else 1

    // start with schema type and version
    formatter.keyValuePair(formKey, schema.ediVersion.ediForm.text)
    formatter.keyValueOptionalQuote(versionKey, schema.ediVersion.version)
    if (imports.nonEmpty) {

      // write list of imports
      val builder = new StringBuilder
      builder ++= importsKey ++= ": [ '" ++= imports.head ++= "'"
      imports.tail.foreach { builder ++= ", '" ++= _ ++= "'" }
      builder ++= " ]"
      formatter.writeIndented(builder.toString)
    }
    val groupCount = countNonEmpty(schema.structures) + countNonEmpty(schema.segments) +
      countNonEmpty(schema.composites) + countNonEmpty(schema.elements)
    if (!schema.structures.isEmpty) {

      def writeStructure(struct: Structure) = {
        formatter.keyValueOptionalQuote(idKey, struct.ident).keyValueOptionalQuote(nameKey, struct.name)
        struct.group match {
          case Some(g) => formatter.keyValuePair(classKey, g)
          case None =>
        }
        struct.tagStart.foreach { x => formatter.keyValuePair(tagStartKey, x) }
        struct.tagLength.foreach { x => formatter.keyValuePair(tagLengthKey, x) }
        if (schema.ediVersion.ediForm.layout.sectioned) {
          struct.heading.foreach { seq => writeStructureComps(headingKey, seq.items) }
          struct.detail.foreach { seq => writeStructureComps(detailKey, seq.items) }
          struct.summary.foreach { seq => writeStructureComps(summaryKey, seq.items) }
        } else struct.heading.foreach { seq => writeStructureComps(dataKey, seq.items) }
      }

      // write structure details
      if (groupCount == 1 && schema.structures.size == 1) {
        schema.structures.values.foreach { writeStructure(_) }
      } else {
        formatter.keyLine(structuresKey)
        schema.structures.values.toList.sortBy { _.ident } foreach (struct => {
          formatter.openGrouping
          writeStructure(struct)
          formatter.closeGrouping
        })
      }
    }
    if (!schema.segments.isEmpty) {

      // write segment details
      if (groupCount == 1 && schema.segments.size == 1) {
        schema.segments.values.foreach { segment =>
          formatter.keyValueOptionalQuote(idKey, segment ident)
          writeSegmentDetails(segment)
        }
      } else {
      formatter.keyLine(segmentsKey)
      schema.segments.values.toList.sortBy { _.ident } foreach (segment => {
        formatter.openGrouping.keyValueOptionalQuote(idKey, segment ident)
        writeSegmentDetails(segment)
        formatter.closeGrouping
      })
      }
    }
    if (!schema.composites.isEmpty) {

      // write composites details
      formatter.keyLine(compositesKey)
      schema.composites.values.toList.sortBy { _.ident }.foreach (comp => {
        formatter.openGrouping.keyValueOptionalQuote(idKey, comp ident).keyValueOptionalQuote(nameKey, comp.name)
        writeCompositeDetails(comp)
        formatter.closeGrouping
      })
    }
    if (!schema.elements.isEmpty) {

      // write element details
      formatter.keyLine(elementsKey)
      formatter.indentCount += 1
      schema.elements.values.toList.sortBy { _.ident }.foreach (elem => {
        formatter.openGrouping(true).keyValueOptionalQuote(idKey, elem ident).keyValueOptionalQuote(nameKey, elem.name)
        writeElementDetails(elem)
        formatter.closeGrouping
      })
      formatter.indentCount -= 1
    }
  }
}