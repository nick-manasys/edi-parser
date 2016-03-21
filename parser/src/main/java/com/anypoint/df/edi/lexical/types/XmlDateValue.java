package com.anypoint.df.edi.lexical.types;

import java.io.IOException;
import java.math.BigDecimal;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;
import com.anypoint.df.edi.lexical.LexerBase;
import com.anypoint.df.edi.lexical.LexicalException;
import com.anypoint.df.edi.lexical.WriteException;
import com.anypoint.df.edi.lexical.WriterBase;

/**
 * Date and/or time value in HL7 style with variable number of components. This handles the three HL7 date/time formats
 * Date ("DT") as YYYY[MM[DD]], Date/time ("DTM") as YYYY[MM[DD[HH[MM[SS[.S[S[S[S]]]]]]]]][+/-ZZZZ], and Time ("TM") as
 * HH[MM[SS[.S[S[S[S]]]]]][+/-ZZZZ]. Values used by this class are generally not valid XML date/time since XML requires
 * all components to be populated for each format variation while HL7 allows any number of components to be included
 * following the four-digit year (or two-digit hour, in the case of a time value).
 */
public class XmlDateValue extends ValueTypeBase {
    
    public enum Variation { DATE, TIME, DATETIME }
    
    /** Factory for creating XMLGregorianCalendars. */
    private final DatatypeFactory typeFactory;
    
    private final Variation formatVariation;
    
    public XmlDateValue(String code, int min, int max, Variation var) {
        super(code, min, max);
        formatVariation = var;
        try {
            typeFactory = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new RuntimeException(e);
        }
    }

    private int parseComponent(int start, int end, LexerBase lexer) throws LexicalException {
        StringBuilder builder = lexer.tokenBuilder();
        int value = 0;
        for (int i = start; i < end; i++) {
            char chr = builder.charAt(i);
            if (chr < '0' || chr > '9') {
                lexer.error(this, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
                chr = '0';
            }
            value = value * 10 + chr - '0';
        }
        return value;
    }
    
    /**
     * Read variable-length date/time value from XML representation.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @param zone allow time zone
     * @return
     * @throws IOException
     */
    private XMLGregorianCalendar parseDateTime(LexerBase lexer) throws LexicalException {
        XMLGregorianCalendar dt = typeFactory.newXMLGregorianCalendar();
        StringBuilder builder = lexer.tokenBuilder();
        int length = builder.length();
        if (length > 5) {
            char sign = builder.charAt(length - 5);
            if (sign == '-' || sign == '+') {
                int hours = parseComponent(length - 4, length - 2, lexer);
                int mins = parseComponent(length - 2, length, lexer);
                if (hours > 23) {
                    lexer.error(this, ErrorCondition.INVALID_DATE, "time zone offset hours value out of range: " + hours);
                    hours = 23;
                }
                if (mins > 59) {
                    lexer.error(this, ErrorCondition.INVALID_DATE, "time zone offset minutes value out of range: " + mins);
                    mins = 59;
                }
                int offset = hours * 60 + mins;
                dt.setTimezone(sign == '-' ? -offset : offset);
                length -= 5;
            }
        }
        if (length < 4 || (length < 14 && length % 2 == 1) || length > 19) {
            lexer.error(this, ErrorCondition.INVALID_DATE, "length does not match a date/time format: " + builder.substring(0, length));
        }
        if (length >= 4) {
            dt.setYear(parseComponent(0, 4, lexer));
            if (length >= 6) {
                dt.setMonth(parseComponent(4, 6, lexer));
                if (length >= 8) {
                    dt.setDay(parseComponent(6, 8, lexer));
                    if (length >= 10) {
                        dt.setHour(parseComponent(8, 10, lexer));
                        if (length >= 12) {
                            dt.setMinute(parseComponent(10, 12, lexer));
                            if (length >= 14) {
                                dt.setSecond(parseComponent(12, 14, lexer));
                                if (length >= 16) {
                                    if (builder.charAt(14) == '.') {
                                        try {
                                            dt.setFractionalSecond(new BigDecimal(builder.substring(14, length)));
                                        } catch (NumberFormatException e) {
                                            lexer.error(this, ErrorCondition.INVALID_DATE,
                                                "invalid second fraction: " + builder.substring(0, length));
                                        }
                                    }
                                }
                            }
                            
                        }
                    }
                }
            }
        }
        return dt;
    }
    
    @Override
    public Object parse(LexerBase lexer) throws LexicalException {
        checkInputLength(lexer);
        return parseDateTime(lexer);
    }

    /**
     * Write variable-length date/time value from XML representation.
     *
     * @param dt
     * @param writer
     * @throws IOException
     */
    private void writeDateTime(XMLGregorianCalendar dt, WriterBase writer) throws IOException {
        StringBuilder builder = new StringBuilder();
        boolean fulldate = false;
        if (formatVariation == Variation.DATE || formatVariation == Variation.DATETIME) {
            int value = dt.getYear();
            if (value == DatatypeConstants.FIELD_UNDEFINED) {
                throw new WriteException("missing required year value for date/time");
            }
            appendFourDigit(value, builder);
            value = dt.getMonth();
            if (value != DatatypeConstants.FIELD_UNDEFINED) {
                appendTwoDigit(value, builder);
                value = dt.getDay();
                if (value != DatatypeConstants.FIELD_UNDEFINED) {
                    appendTwoDigit(value, builder);
                    fulldate = true;
                }
            }
        }
        if (formatVariation == Variation.TIME || (fulldate && formatVariation == Variation.DATETIME)) {
            int value = dt.getHour();
            if (value != DatatypeConstants.FIELD_UNDEFINED) {
                appendTwoDigit(value, builder);
                value = dt.getMinute();
                if (value != DatatypeConstants.FIELD_UNDEFINED) {
                    appendTwoDigit(value, builder);
                    value = dt.getSecond();
                    if (value != DatatypeConstants.FIELD_UNDEFINED) {
                        appendTwoDigit(value, builder);
                        BigDecimal fract = dt.getFractionalSecond();
                        if (fract != null) {
                            builder.append(fract.toPlainString());
                        }
                    }
                }
            }
        }
        int offset = dt.getTimezone();
        if (offset != DatatypeConstants.FIELD_UNDEFINED) {
            builder.append(offset < 0 ? '-' : '+');
            offset = Math.abs(offset);
            appendTwoDigit(offset / 60, builder);
            appendTwoDigit(offset % 60, builder);
        }
        if (builder.length() < minLength) {
            writer.error(this, ErrorCondition.INVALID_DATE, "date/time value missing required components");
        }
        if (builder.length() > maxLength) {
            tooLong(builder.length(), writer);
            builder.setLength(maxLength);
        }
        writer.writeToken(builder.toString());
    }

    @Override
    public void write(Object value, WriterBase writer) throws IOException {
        if (value instanceof XMLGregorianCalendar) {
            writeDateTime((XMLGregorianCalendar)value, writer);
        } else {
            wrongType(value, writer);
        }
    }
}