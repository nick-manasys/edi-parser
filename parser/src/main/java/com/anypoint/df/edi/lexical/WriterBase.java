
package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.EdiConstants.maximumYear;

import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Map;

import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * Base token writing code for all forms of output. The actual token arranging code is implemented by subclasses, this
 * just provides methods for dealing with common datatypes.
 */
public abstract class WriterBase
{
    /** Spaces used for padding. */
    protected static final String SPACES = "                                        ";
    
    /** Zeroes used for padding. */
    protected static final String ZEROES = "00000000000000000000";
    
    /** Milliseconds per second. */
    private static final int MILLIS_PER_SECOND = 1000;
    
    /** Milliseconds per minute. */
    private static final int MILLIS_PER_MINUTE = MILLIS_PER_SECOND * 60;
    
    /** Milliseconds per hour. */
    private static final int MILLIS_PER_HOUR = MILLIS_PER_MINUTE * 60;
    
    /** Character used for decimal mark. */
    public final char decimalMark;
    
    /** Writer wrapping document data stream. */
    protected final Writer writer;
    
    /** Number of segments written. */
    protected int segmentCount;
    
    /**
     * Constructor.
     *
     * @param os output
     * @param encoding character set encoding
     * @param mark decimal mark character
     */
    protected WriterBase(Writer wrtr, char mark) {
        writer = wrtr;
        decimalMark = mark;
    }
    
    /**
     * Get number of segments written.
     *
     * @return count
     */
    public int getSegmentCount() {
        return segmentCount; 
    }
    
    /**
     * Close writer when output completed.
     *
     * @throws IOException
     */
    public void close() throws IOException {
        writer.close();
    }
    
    /**
     * Initialize document output, writing any interchange header segment(s) required by the protocol variation.
     *
     * @param props
     * @throws IOException 
     */
    public abstract void init(Map<String, Object> props) throws IOException;
    
    /**
     * Complete document output, writing any interchange trailer segment(s) required by the protocol variation.
     *
     * @param props
     * @throws IOException
     */
    public abstract void term(Map<String,Object> props) throws IOException;
    
    /**
     * Write a segment start tag. This must always be called before any of the write value methods are called for the
     * segment data.
     *
     * @param tag
     * @throws IOException
     */
    public abstract void writeSegmentTag(String tag) throws IOException;
    
    /**
     * Write a segment terminator.
     *
     * @throws IOException
     */
    public abstract void writeSegmentTerminator() throws IOException;
    
    /**
     * Write token text with no checks on content or length.
     *
     * @param text
     * @throws IOException
     */
    public abstract void writeToken(String text) throws IOException;
    
    /**
     * Write text with trailing space character padding. If the text is longer than the maximum length this throws an
     * exception.
     *
     * @param text
     * @param minl minimum length
     * @param maxl maximum length
     * @return value, <code>null</code> if empty
     * @throws IOException 
     */
    public abstract void writeSpacePadded(String text, int minl, int maxl) throws IOException;
    
    /**
     * Write numeric value with leading zero padding.
     *
     * @param value
     * @param minl minimum length
     * @param maxl maximum length
     * @param adj extra character count include (decimal point, exponent, etc.)
     * @throws IOException 
     */
    public abstract void writeZeroPadded(String value, int minl, int maxl, int adj) throws IOException;
    
    /**
     * Get required property value, throwing an exception if the value is not defined.
     *
     * @param key
     * @param props
     * @return value
     * @throws WriteException
     */
    protected static Object getRequired(String key, Map<String, Object> props) throws WriteException {
        Object value = props.get(key);
        if (value == null) {
            throw new WriteException("missing required property value '" + key + "'");
        }
        return value;
    }
    
    /**
     * Write text as an alpha value.
     *
     * @param text
     * @param minl minimum length
     * @param maxl maximum length
     * @throws IOException
     */
    public void writeAlpha(String text, int minl, int maxl) throws IOException {
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr >= '0' && chr <= '9') {
                throw new WriteException("alpha value type cannot contain digit");
            }
        }
        writeSpacePadded(text, minl, maxl);
    }
    
    /**
     * Write integer value.
     *
     * @param value
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeInt(int value, int minl, int maxl) throws IOException {
        writeZeroPadded(Integer.toString(value), minl, maxl, 0);
    }
    
    /**
     * Write long value.
     *
     * @param value
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeLong(long value, int minl, int maxl) throws IOException {
        writeZeroPadded(Long.toString(value), minl, maxl, 0);
    }
    
    /**
     * Write big integer value.
     *
     * @param value
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeBigInteger(BigInteger value, int minl, int maxl) throws IOException {
        writeZeroPadded(value.toString(), minl, maxl, 0);
    }
    
    /**
     * Write a big decimal number as a number with implied decimal point. This rounds the supplied value to the implied
     * accuracy.
     *
     * @param value
     * @param scale
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeImplicitDecimal(BigDecimal value, int scale, int minl, int maxl) throws IOException {
        writeBigInteger(value.movePointRight(scale).setScale(scale, RoundingMode.HALF_UP).toBigIntegerExact(), minl,
            maxl);
    }
    
    /**
     * Write big decimal value.
     *
     * @param value
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeDecimal(BigDecimal value, int minl, int maxl) throws IOException {
        int precision = value.precision();
        int scale = value.scale();
        if (scale <= 0 && (precision - scale) <= maxl) {
            
            // write as simple integer
            writeBigInteger(value.toBigIntegerExact(), minl, maxl);
            return;
            
        } else if (scale >= 0 && Math.max(precision, scale) <= maxl) {
            
            // write as simple decimal
            String text = value.toPlainString();
            if (text.startsWith("0")) {
                text = text.substring(1);
            }
            writeZeroPadded(text, minl, maxl, 1);
            return;
            
        } else {
            
            // convert using implied decimal (at end) and exponent
            BigDecimal adjusted = value.movePointRight(scale);
            String text = adjusted.toBigIntegerExact().toString() + "E" + Integer.toString(-scale);
            int adj = value.signum() < 0 ? 2 : 1;
            if (scale > 0) {
                adj++;
            }
            writeZeroPadded(text, minl, maxl, adj);
        }
    }
    
    /**
     * Write HL7 numeric big decimal value.
     *
     * @param value
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeNumeric(Number number, int minl, int maxl) throws IOException {
        if (number instanceof BigInteger) {
            writeBigInteger((BigInteger)number, minl, maxl);
        } else {
            BigDecimal value = (BigDecimal)number;
            if (value.scale() <= 0) {
                
                // write as simple integer
                writeBigInteger(value.toBigIntegerExact(), minl, maxl);
                return;
                
            } else {
                
                // write as simple decimal
                writeZeroPadded(value.toPlainString(), minl, maxl, 1);
                return;
                
            }
        }
    }
    
    /**
     * Append positive number as two-digit value, using a leading zero if necessary.
     *
     * @param num
     * @param builder
     */
    protected void appendTwoDigit(int num, StringBuilder builder) {
        if (num < 10) {
            builder.append('0');
        }
        builder.append(num);
    }
    
    /**
     * Append positive number as four-digit value, using leading zeroes if necessary.
     *
     * @param num
     * @param builder
     */
    protected void appendFourDigit(int num, StringBuilder builder) {
        if (num < 1000) {
            builder.append('0');
            if (num < 100) {
                builder.append('0');
                if (num < 10) {
                    builder.append('0');
                }
            }
        }
        builder.append(num);
    }

    /**
     * Write X12 date value. Note that this avoids the use of the Java DateFormat class, which has high time and memory
     * overhead.
     *
     * @param calendar
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeDate(Calendar calendar, int minl, int maxl) throws IOException {
        
        // split into components
        int year = calendar.get(Calendar.YEAR);
        int month = calendar.get(Calendar.MONTH) + 1;
        int day = calendar.get(Calendar.DAY_OF_MONTH);
        
        // use year format determined by length
        StringBuilder builder = new StringBuilder();
        builder.append(year);
        if (maxl == 6) {
            if (year > maximumYear || year <= maximumYear - 100) {
                throw new WriteException("year out of range for short form date");
            }
            builder.delete(0, 2);
        } else if (builder.length() > 4) {
            throw new WriteException("year outside of allowed range");
        } else {
            while (builder.length() < 4) {
                builder.insert(0, '0');
            }
        }
        
        // force month and day to use two digits each
        appendTwoDigit(month, builder);
        appendTwoDigit(day, builder);
        writeToken(builder.toString());
    }
    
    /**
     * Write X12 date value from date.
     * 
     * @param date
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeDate(Date date, int minl, int maxl) throws IOException {
        GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(date);
        writeDate(calendar, minl, maxl);
    }

    /**
     * Write X12 time value. Note that this avoids the use of the Java DateFormat class, which has high time and memory
     * overhead.
     *
     * @param time millisecond of day
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeTime(int time, int minl, int maxl) throws IOException {
        
        // start with required component values
        int remain = time;
        StringBuilder builder = new StringBuilder();
        int hour = remain / MILLIS_PER_HOUR;
        remain = time % MILLIS_PER_HOUR;
        appendTwoDigit(hour, builder);
        int minute = remain / MILLIS_PER_MINUTE;
        remain = time % MILLIS_PER_MINUTE;
        appendTwoDigit(minute, builder);
        if (maxl > 4 && remain > 0) {
            
            // append optional components
            int second = remain / MILLIS_PER_SECOND;
            remain = time % MILLIS_PER_SECOND;
            appendTwoDigit(second, builder);
            
            // avoid trailing zeroes in decimal seconds
            if (maxl > 7 && remain > 10) {
                appendTwoDigit(remain / 10, builder);
            } else if (maxl == 7 && remain > 100) {
                builder.append(remain / 100);
            } else if (minl > 7) {
                builder.append("00");
            } else if (minl > 6) {
                builder.append('0');
            }
            
        }
        while (builder.length() < minl) {
            builder.append('0');
        }
        writeToken(builder.toString());
    }

    /**
     * Write X12 date value. Note that this avoids the use of the Java DateFormat class, which has high time and memory
     * overhead.
     *
     * @param dt
     * @param minl minimum length
     * @param maxl maximum length
     * @param zone allow time zone
     * @throws IOException
     */
    public void writeDateTime(XMLGregorianCalendar dt, int minl, int maxl, boolean zone) throws IOException {
        int value = dt.getYear();
        if (value == DatatypeConstants.FIELD_UNDEFINED) {
            throw new WriteException("missing required year value for date/time");
        }
        StringBuilder builder = new StringBuilder();
        appendFourDigit(value, builder);
        value = dt.getMonth();
        if (value != DatatypeConstants.FIELD_UNDEFINED) {
            appendTwoDigit(value, builder);
            value = dt.getDay();
            if (value != DatatypeConstants.FIELD_UNDEFINED) {
                appendTwoDigit(value, builder);
                value = dt.getHour();
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
        }
        if (builder.length() < minl) {
            throw new WriteException("date/time value missing required components: " + builder.toString());
        }
        if (builder.length() > maxl) {
            builder.setLength(maxl);
        }
        if (zone) {
            int offset = dt.getTimezone();
            if (offset != DatatypeConstants.FIELD_UNDEFINED) {
                builder.append(offset < 0 ? '-' : '+');
                offset = Math.abs(offset);
                appendTwoDigit(offset / 60, builder);
                appendTwoDigit(offset % 60, builder);
            }
        }
        writeToken(builder.toString());
    }
}