package com.anypoint.df.edi.schema.fftypes

trait FlatFileFormat {
  def code: String
}

object FlatFileFormat {
  def formats = List(BooleanFormat, IntegerFormat, LocalDateFormat, LocalDateTimeFormat, LocalTimeFormat, RealFormat, StringFormat)
}