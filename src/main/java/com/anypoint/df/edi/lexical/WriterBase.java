
package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.EdiConstants.maximumYear;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Map;

/**
 * Base EDI token writer. The writer outputs tokens of various types, with specified delimiter types.
 */
public abstract class WriterBase
{
    /** Spaces used for padding. */
    private static final String SPACES = "                    ";
    
    /** Zeroes used for padding. */
    private static final String ZEROES = "00000000000000000000";
    
    /** Milliseconds per second. */
    private static final int MILLIS_PER_SECOND = 1000;
    
    /** Milliseconds per minute. */
    private static final int MILLIS_PER_MINUTE = MILLIS_PER_SECOND * 60;
    
    /** Milliseconds per hour. */
    private static final int MILLIS_PER_HOUR = MILLIS_PER_MINUTE * 60;
    
    /** Destination stream for document data. */
    protected OutputStream stream;
    
    /** Writer wrapping document data stream. */
    protected Writer writer;
    
    /** Sub-element delimiter. */
    protected char subElement;
    
    /** Data element delimiter. */
    protected char dataSeparator;
    
    /** Repeated component delimiter (-1 if unused). */
    protected int repetitionSeparator;
    
    /** Release character (-1 if unused). */
    protected int releaseIndicator;

    /** Segment terminator. */
    protected char segmentTerminator;
    
    /** Number of segments written. */
    protected int segmentCount;
    
    /** Number of groups in interchange. */
    protected int groupCount;
    
    /** Number of skipped data elements. */
    protected int skippedElementCount;
    
    /** Number of skipped sub-elements. */
    protected int skippedSubCount;
    
    /**
     * Configure writer for use.
     *
     * @param os output
     * @param encoding character set encoding
     * @param datasep data separator character
     * @param subsep sub-element separator character
     * @param repsep repetition separator character
     * @param segterm segment terminator character
     * @param release release character
     */
    protected void configure(OutputStream os, Charset encoding, char datasep, char subsep, int repsep, char segterm,
        int release) {
        stream = os;
        dataSeparator = datasep;
        subElement = subsep;
        repetitionSeparator = repsep;
        segmentTerminator = segterm;
        releaseIndicator = release;
        writer = new OutputStreamWriter(new BufferedOutputStream(os), encoding);
    }
    
    /**
     * Count a group present in interchange.
     */
    public void countGroup() {
        groupCount++;
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
     * Count a skipped data element in segment. This supports lazy writing of separators for skipped data elements to
     * avoid trailing separators.
     */
    public void skipElement() {
        skippedElementCount++;
        skippedSubCount = 0;
    }
    
    /**
     * Count a skipped sub-element in segment. This supports lazy writing of separators for skipped sub-elements to
     * avoid trailing separators.
     */
    public void skipSubElement() {
        skippedSubCount++;
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
     * Write data separator(s). If any data values have been skipped this also writes out the separators for those
     * values.
     *
     * @throws IOException
     */
    public void writeDataSeparator() throws IOException {
        writer.write(dataSeparator);
        for (int i = 0; i < skippedElementCount; i++) {
            writer.write(dataSeparator);
        }
        skippedElementCount = 0;
        skippedSubCount = 0;
    }
    
    /**
     * Write sub-element delimiter(s). If any sub-element values have been skipped this also writes out the separators
     * for those values.
     *
     * @param count
     * @throws IOException
     */
    public void writeSubDelimiter() throws IOException {
        writer.write(subElement);
        for (int i = 0; i < skippedSubCount; i++) {
            writer.write(subElement);
        }
        skippedSubCount = 0;
    }
    
    /**
     * Write a segment terminator.
     *
     * @throws IOException
     */
    public void writeSegmentTerminator() throws IOException {
        writer.write(segmentTerminator);
        skippedElementCount = 0;
        skippedSubCount = 0;
        segmentCount++;
    }
    
    /**
     * Write a repetition separator.
     *
     * @throws IOException
     */
    public void writeRepetitionSeparator() throws IOException {
        writer.write(repetitionSeparator);
    }
    
    /**
     * Write token text with no checks on content or length.
     *
     * @param text
     * @throws IOException
     */
    public void writeToken(String text) throws IOException {
        writer.write(text);
    }
    
    /**
     * Write text with space characters appended to minimum length. If the text is longer than the maximum length this
     * throws an exception.
     *
     * @param text
     * @param minl minimum length
     * @param maxl maximum length
     * @return value, <code>null</code> if empty
     * @throws IOException 
     */
    protected void writePadded(String text, int minl, int maxl) throws IOException {
        String token = text;
        int length = token.length();
        while (length < minl) {
            int pad = Math.min(minl - length, SPACES.length());
            token = token + SPACES.substring(0, pad);
            length += pad;
        }
        if (token.length() > maxl) {
            throw new WriteException("length outside of allowed range");
        }
        writeToken(token);
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
        writePadded(text, minl, maxl);
    }
    
    /**
     * Write text as an alphanumeric value.
     *
     * @param text
     * @param minl minimum length
     * @param maxl maximum length
     * @throws IOException
     */
    public void writeAlphaNumeric(String text, int minl, int maxl) throws IOException {
        writePadded(text, minl, maxl);
    }
    
    /**
     * Write text as an id value.
     *
     * @param text
     * @param minl minimum length
     * @param maxl maximum length
     * @throws IOException
     */
    public void writeId(String text, int minl, int maxl) throws IOException {
        int length = text.length();
        if (length < minl || length > maxl) {
            throw new WriteException("length outside of allowed range");
        }
        for (int i = 0; i < length; i++) {
            char chr = text.charAt(i);
            if (!Character.isAlphabetic(chr) && (chr < '0' || chr > '9')) {
                throw new LexicalException("id value type characters must be alphas or digits");
            }
        }
        writeToken(text);
    }
    
    /**
     * Pad a numeric value to a minimum length with leading zeroes. This recognizes a leading minus sign and doesn't
     * include it in the length, but if any other non-counted characters are present (such as a decimal point) the
     * passed-in minimum length needs to be adjusted to compensate.
     *
     * @param value
     * @param minl
     */
    protected String padZeroes(String value, int minl) {
        String text = value;
        boolean negate = text.startsWith("-");
        int length = text.length() - (negate ? 1 : 0);
        while (length < minl) {
            int pad = Math.min(minl - length, ZEROES.length());
            if (negate) {
                text = "-" + ZEROES.substring(0, pad) + text.substring(1);
            } else {
                text = ZEROES.substring(0, pad) + text;
            }
            length += pad;
        }
        return text;
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
        String text = padZeroes(Integer.toString(value), minl);
        if (text.length() > maxl) {
            if (!text.startsWith("-") || (text.length() -1 > maxl)) {
                throw new WriteException("value too long");
            }
        }
        writeToken(text);
    }
    
    /**
     * Write big integer value.
     *
     * @param value
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeInteger(BigInteger value, int minl, int maxl) throws IOException {
        String text = padZeroes(value.toString(), minl);
        if (text.length() > maxl) {
            if (!text.startsWith("-") || (text.length() -1 > maxl)) {
                throw new WriteException("value too long");
            }
        }
        writeToken(text);
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
        writeInteger(value.movePointRight(scale).setScale(scale, RoundingMode.HALF_UP).toBigIntegerExact(), minl, maxl);
    }
    
    /**
     * Write big decimal value.
     * TODO: handle writing exponential indicators E+/-n
     * TODO: X12 explicitly forbids triad separators, check EDIFACT
     *
     * @param value
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeDecimal(BigDecimal value, int minl, int maxl) throws IOException {
        
        // if no decimal point needed just write as an integer
        if (value.scale() <= 0) {
            writeInteger(value.toBigIntegerExact(), minl, maxl);
            return;
        }
        
        // check against maximum length, not counting decimal point or minus sign
        String text = value.toString();
        int length = text.length() - 1;
        if (value.signum() < 0) {
            length--;
        }
        if (length > maxl) {
            throw new WriteException("length outside of allowed range");
        }
        
        // write with leading zeroes if needed to reach minimum length
        writeToken(padZeroes(text, minl + 1));
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
     * Write X12 date value. Note that this avoids the use of the Java DateFormat class, which has high time and memory
     * overhead.
     *
     * @param value
     * @param minl
     * @param maxl
     * @throws IOException
     */
    public void writeDate(Date date, int minl, int maxl) throws IOException {
        
        // split into components
        GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(date);
        int year = calendar.get(Calendar.YEAR);
        int month = calendar.get(Calendar.MONTH);
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
        if (maxl > 4) {
            
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
        writeToken(builder.toString());
    }
}