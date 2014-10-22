
package com.anypoint.df.edi.parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Map;

/**
 * Base EDI parser.
 */
public abstract class ParserBase
{
    /** Maximum year number accepted (otherwise wrapped to previous century). */
    public static final int maximumYear = 2070;
    
    public enum ItemType {  SEGMENT, DATA_ELEMENT, QUALIFIER, REPETITION, END }
    
    protected static final Charset ASCII_CHARSET = Charset.forName("US-ASCII");
    
    /** Stream supplying document data. */
    protected final InputStream stream;
    
    /** Reader wrapping document data stream (created by {@link #init()}). */
    protected Reader reader;
    
    /** Sub-element delimiter. */
    protected char subElement;
    
    /** Data element delimiter. */
    protected char dataSeparator;
    
    /** Repeated component delimiter. */
    protected char repetitionSeparator;
    
    /** Release character. */
    protected int releaseIndicator;
    
    /**
     * Read bytes from stream into array. Throws an IOException if not enough bytes are present to fill the array.
     *
     * @param byts array
     * @param from starting offset in array
     * @throws IOException
     */
    protected void readArray(byte[] byts, int from) throws IOException {
        int offset = from;
        while (byts.length > offset) {
            int count = stream.read(byts, offset, byts.length - offset);
            if (count <= 0) {
                throw new IOException("Required data missing from message");
            }
            offset += count;
        }
    }

    /** Segment terminator. */
    protected char segmentTerminator;
    
    /** Next item type. */
    protected ItemType nextType;
    
    /**
     * Constructor.
     *
     * @param is input
     * @param datasep default data separator character
     * @param subsep default sub-element separator character
     * @param repsep default repetition separator character
     * @param segterm default segment terminator character
     * @param release default release character
     */
    public ParserBase(InputStream is, char datasep, char subsep, char repsep, char segterm, int release) {
        stream = is;
        dataSeparator = datasep;
        subElement = subsep;
        repetitionSeparator = repsep;
        segmentTerminator = segterm;
        releaseIndicator = release;
    }
    
    /**
     * Read bytes from stream. Throws an IOException if the required number of bytes is not present.
     *
     * @param num required number of bytes
     * @return bytes
     * @throws IOException
     */
    protected byte[] readBytes(int num) throws IOException {
        byte[] byts = new byte[num];
        readArray(byts, 0);
        return byts;
    }
    
    /**
     * Initialize document parse. This checks the start of the document to interpret any configuration information
     * included. Returns with parser positioned past the header segment(s).
     *
     * @return interchange properties
     * @throws IOException 
     */
    public abstract Map<String,Object> init() throws IOException;
   
    /**
     * Get the next item type.
     *
     * @return type
     */
    public ItemType nextType() {
        return nextType;
    }
    
    /**
     * Get the next item value, removing it from the input. Once this returns, {@link #nextType()} returns the type of
     * the following item in the input.
     *
     * @return value, <code>null</code> if empty
     * @throws IOException 
     */
    public String nextItem() throws IOException {
        
        // start by skipping whitespace, if necessary
        int value = reader.read();
        if (nextType == ItemType.SEGMENT) {
            while (value == '\n' || value == '\r' || value == ' ') {
                value = reader.read();
            }
        }
        if (value < 0) {
            nextType = ItemType.END;
            return null;
        }
        char chr = (char)value;
        
        // accumulate value text to next delimiter
        StringBuilder builder = new StringBuilder();
        boolean escape = false;
        while (true) {
            if (escape) {
                builder.append(chr);
                escape = false;
            } else if (chr == subElement) {
                nextType = ItemType.QUALIFIER;
                break;
            } else if (chr == dataSeparator) {
                nextType = ItemType.DATA_ELEMENT;
                break;
            } else if (chr == segmentTerminator) {
                nextType = ItemType.SEGMENT;
                break;
            } else if (chr == repetitionSeparator) {
                nextType = ItemType.REPETITION;
                break;
            } else if (chr == releaseIndicator) {
                escape = true;
            } else if (chr == -1) {
                nextType = ItemType.END;
                return null;
            } else {
                builder.append(chr);
            }
            chr = (char)reader.read();
        }
        return builder.length() > 0 ? builder.toString() : null;
    }
    
    /**
     * Get the next item value, which must be of the specified type. If the type matches, this returns the next item
     * value, removing it from the input.
     *
     * @param type
     * @return value, <code>null</code> if empty
     * @throws IOException
     */
    public String requireNextItem(ItemType type) throws IOException {
        if (nextType != type) {
            throw new IOException("Missing required item");
        }
        return nextItem();
    }
    
    /**
     * Get next item as an alpha value.
     *
     * @return
     * @throws IOException
     */
    public String parseAlpha() throws IOException {
        String text = nextItem();
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr >= '0' && chr <= '9') {
                throw new ParseException("alpha value type cannot contain digit");
            }
        }
        return text;
    }
    
    /**
     * Get next item as an alphanumeric value.
     *
     * @return
     * @throws IOException
     */
    public String parseAlphanumeric() throws IOException {
        return nextItem();
    }
    
    /**
     * Get next item as an id value.
     *
     * @return
     * @throws IOException
     */
    public String parseId() throws IOException {
        String text = nextItem();
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (!Character.isAlphabetic(chr) && (chr < '0' || chr > '9')) {
                throw new ParseException("id value type characters must be alphas or digits");
            }
        }
        return text;
    }
    
    /**
     * Get next item as a EDIFACT number value.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public BigInteger parseInteger(int minl, int maxl) throws IOException {
        String text = nextItem();
        int length = 0;
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr >= '0' && chr <= '9') {
                length++;
            } else if (i != 0 || chr != '-') {
                throw new ParseException("number value contains invalid character");
            }
        }
        if (length < minl || length > maxl) {
            throw new ParseException("number value incorrect length");
        }
        return new BigInteger(text);
    }
    
    /**
     * Get next item as an X12 real number value.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public BigDecimal parseNumber(int minl, int maxl) throws IOException {
        String text = nextItem();
        int length = 0;
        boolean decimal = false;
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr >= '0' && chr <= '9') {
                length++;
            } else if (!decimal && (chr == ',' || chr == '.')) {
                decimal = true;
            } else if (i != 0 || chr != '-') {
                throw new ParseException("number value contains invalid character");
            }
        }
        if (length < minl || length > maxl) {
            throw new ParseException("number value incorrect length");
        }
        return new BigDecimal(text);
    }
    
    /**
     * Get next item as an X12 date value.
     *
     * @return
     * @throws IOException
     */
    public Date parseDate() throws IOException {
        String text = nextItem();
        if (text.length() != 6 && text.length() != 8) {
            throw new ParseException("date value must be either 6 or 8 characters");
        }
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr < '0' || chr > '9') {
                throw new ParseException("date value must consist only of digits");
            }
        }
        
        // quick (and loose) check for date sanity
        int length = text.length();
        int day = (text.charAt(length-1) - '0') + (text.charAt(length-2) - '0') * 10;
        int month = (text.charAt(length-3) - '0') + (text.charAt(length-4) - '0') * 10;
        if (month == 0 || month > 12 || day == 0 || day > 31) {
            throw new ParseException("date value out of allowed ranges");
        }
        int year;
        if (length == 8) {
            year = Integer.parseInt(text.substring(0, 4));
        } else {
            year = 2000 + (text.charAt(length-2) - '0') + (text.charAt(length-1)) * 10;
            if (year > maximumYear) {
                year -= 100;
            }
        }
        return new GregorianCalendar(year, month, day).getTime();
    }
    
    /**
     * Get next item as an X12 number value with implied decimal.
     *
     * @param scale inverse power of ten multiplier
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public BigDecimal parseImpliedDecimalNumber(int scale, int minl, int maxl) throws IOException {
        return new BigDecimal(parseInteger(minl, maxl), scale);
    }
    
    /**
     * Get next item as an X12 time value.
     *
     * @return
     * @throws IOException
     */
    public int parseTime() throws IOException {
        String text = nextItem();
        if (text.length() != 4 && (text.length() < 6 || text.length() > 8)) {
            throw new ParseException("time value must be either 4 or 6-8 characters");
        }
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr < '0' || chr > '9') {
                throw new ParseException("time value must consist only of digits");
            }
        }
        
        // quick (and loose) check for time sanity
        int hour = (text.charAt(0) - '0') * 10 + (text.charAt(1) - '0');
        int minute = (text.charAt(2) - '0') * 10 + (text.charAt(3) - '0');
        int second = text.length() < 6 ? 0 : (text.charAt(4) - '0') * 10 + (text.charAt(5) - '0');
        if (hour > 23 || minute > 59 || second > 59) {
            throw new ParseException("time value out of allowed ranges");
        }
        int milli = 0;
        if (text.length() > 6) {
            milli = (text.charAt(6) - '0') * 100;
            if (text.length() > 7) {
                milli = (text.charAt(7) - '0') * 10;
            }
        }
        return ((hour * 60 + minute) * 60 + second) * 1000 + milli;
    }
}