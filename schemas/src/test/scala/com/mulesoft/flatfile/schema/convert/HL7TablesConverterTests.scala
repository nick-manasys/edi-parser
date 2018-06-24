package com.mulesoft.flatfile.schema.convert

import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
 * Tests for HL7 table data converter.
 */
class HL7TablesConverterTests extends FlatSpec with Matchers {
  import HL7TablesConverter._

  behavior of "constructStructure"

  val test = List(
    new Line("RPL_I02", "1", "MSH", "", "0", "0"),
    new Line("RPL_I02", "2", "MSA", "", "0", "0"),
    new Line("RPL_I02", "3", "{", "PROVIDER", "0", "0"),
    new Line("RPL_I02", "4", "PRD", "", "0", "0"),
    new Line("RPL_I02", "7", "CTD", "", "1", "1"),
    new Line("RPL_I02", "10", "}", "", "0", "0"),
    new Line("RPL_I02", "13", "NTE", "", "1", "1"),
    new Line("RPL_I02", "18", "DSP", "", "1", "1"),
    new Line("RPL_I02", "22", "DSC", "", "0", "1"))

  val test2 = List(
    new Line("CRM_C01", "1", "MSH", "", "0", "0"),
    new Line("CRM_C01", "2", "SFT", "", "1", "1"),
    new Line("CRM_C01", "4", "{", "PATIENT", "0", "0"),
    new Line("CRM_C01", "5", "PID", "", "0", "0"),
    new Line("CRM_C01", "6", "PV1", "", "0", "1"),
    new Line("CRM_C01", "7", "CSR", "", "0", "0"),
    new Line("CRM_C01", "8", "CSP", "", "1", "1"),
    new Line("CRM_C01", "9", "}", "PATIENT", "0", "0"))

  it should "parse lines in a list with matching first field RPL_I02" in {
    var result = constructStructure(test)
    result.code should be("RPL_I02")
    result.segments.size should be(6)
  }

  it should "parse lines in a list with matching first field CRM_C01" in {
    var result = constructStructure(test2)
    result.code should be("CRM_C01")
    result.segments.size should be(3)
  }
}
