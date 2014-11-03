
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
 * Base EDI token scanner. The scanner supplies input tokens to consumers along with token delimiter types, with three
 * properties exposed: the delimiter at the start of the current input token ({@link #currentType}), the delimiter at 
 * the end of the current input token ({@link #nextType}), and the actual current token ({@link #token}). Various typed
 * parseXXX methods work with the current token as typed data.
 */
public abstract class ParserBase
{
    // standard character sets
    public static final Charset ASCII_CHARSET = Charset.forName("US-ASCII");
    public static final Charset UTF8_CHARSET = Charset.forName("UTF-8");
    
    /** Maximum year number accepted (otherwise wrapped to previous century). */
    public static final int maximumYear = 2070;
    
    /** Token delimiter types. */
    public enum ItemType {  SEGMENT, DATA_ELEMENT, QUALIFIER, REPETITION, END }
    
    /** Stream supplying document data. */
    protected final InputStream stream;
    
    /** Reader wrapping document data stream (created by {@link #init()}). */
    protected Reader reader;
    
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
    
    /** Type of current token (starting delimiter). */
    protected ItemType currentType;
    
    /** Current token. */
    protected String token;
    
    /** Type of next token (ending delimiter of current token). */
    protected ItemType nextType;
    
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
     * included. Returns with parser positioned past the interchange header segment(s).
     *
     * @param default interchange properties (from partner configuration)
     * @return interchange properties
     * @throws IOException 
     */
    public abstract Map<String,Object> init(Map<String,Object> dflts) throws IOException;
    
    /**
     * Complete document parse. This must be called with the parser positioned at the start of the interchange trailer
     * segment.
     *
     * @param props
     * @throws IOException
     */
    public abstract void term(Map<String,Object> props) throws IOException;
    
    /**
     * Get the current token.
     *
     * @return token
     */
    public String token() {
        return token;
    }
    
    /**
     * Get the current token type (as determined by the preceding delimiter).
     *
     * @return type
     */
    public ItemType currentType() {
        return currentType;
    }
   
    /**
     * Get the next token type (as determined by the trailing delimiter of the current token).
     *
     * @return type
     */
    public ItemType nextType() {
        return nextType;
    }
    
    /**
     * Require particular item type.
     *
     * @param type
     * @throws IOException
     */
    public void require(ItemType type) throws IOException {
        if (currentType != type) {
            throw new IOException("expected type " + type);
        }
    }
    
    /**
     * Parse next item from input and advance. This sets the current state to the pending state, and sets the new
     * pending state to the item parsed.
     *
     * @return token
     * @throws IOException 
     */
    public String advance() throws IOException {
        
        // set current state to pending state
        currentType = nextType;
        
        // start by skipping whitespace, if necessary
        int value = reader.read();
        if (currentType == ItemType.SEGMENT) {
            while (value == '\n' || value == '\r' || value == ' ') {
                value = reader.read();
            }
        }
        if (value < 0) {
            token = "";
            nextType = ItemType.END;
            return token;
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
                break;
            } else {
                builder.append(chr);
            }
            chr = (char)reader.read();
        }
        token = builder.length() > 0 ? builder.toString() : "";
        return token;
    }
    
    /**
     * Verifying the current token text length.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return value, <code>null</code> if empty
     * @throws IOException 
     */
    public void checkLength(int minl, int maxl) throws IOException {
        if (token.length() < minl || token.length() > maxl) {
            throw new ParseException("length outside of allowed range");
        }
    }
    
    /**
     * Get current token as an alpha value and advance to next token.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public String parseAlpha(int minl, int maxl) throws IOException {
        checkLength(minl, maxl);
        String text = token;
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr >= '0' && chr <= '9') {
                throw new ParseException("alpha value type cannot contain digit");
            }
        }
        advance();
        return text;
    }
    
    /**
     * Get current token as an alphanumeric value and advance to next token.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public String parseAlphaNumeric(int minl, int maxl) throws IOException {
        checkLength(minl, maxl);
        String text = token;
        advance();
        return text;
    }
    
    /**
     * Get current token as an id value and advance to next token.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public String parseId(int minl, int maxl) throws IOException {
        checkLength(minl, maxl);
        String text = token;
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (!Character.isAlphabetic(chr) && (chr < '0' || chr > '9')) {
                throw new ParseException("id value type characters must be alphas or digits");
            }
        }
        advance();
        return text;
    }
    
    /**
     * Get current token as an integer value and advance to next token.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public BigInteger parseInteger(int minl, int maxl) throws IOException {
        String text = token;
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
        advance();
        return new BigInteger(text);
    }
    
    /**
     * Get current token as an X12 real number value and advance to next token.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public BigDecimal parseNumber(int minl, int maxl) throws IOException {
        String text = token;
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
        advance();
        return new BigDecimal(text);
    }
    
    /**
     * Get current token as an X12 date value and advance to next token.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public Date parseDate(int minl, int maxl) throws IOException {
        checkLength(minl, maxl);
        String text = token;
        int length = text.length();
        if (length != 6 && length != 8) {
            throw new ParseException("date value must be either 6 or 8 characters");
        }
        for (int i = 0; i < length; i++) {
            char chr = text.charAt(i);
            if (chr < '0' || chr > '9') {
                throw new ParseException("date value must consist only of digits");
            }
        }
        
        // quick (and loose) check for date sanity
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
        advance();
        return new GregorianCalendar(year, month, day).getTime();
    }
    
    /**
     * Get current token as an X12 number value and advance to next token.
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
     * Get current token as an X12 time value and advance to next token.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public int parseTime(int minl, int maxl) throws IOException {
        checkLength(minl, maxl);
        String text = token;
        int length = text.length();
        if (length != 4 && (length < 6 || length > 8)) {
            throw new ParseException("time value must be either 4 or 6-8 characters");
        }
        for (int i = 0; i < length; i++) {
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
        if (length > 6) {
            milli = (text.charAt(6) - '0') * 100;
            if (length > 7) {
                milli = (text.charAt(7) - '0') * 10;
            }
        }
        advance();
        return ((hour * 60 + minute) * 60 + second) * 1000 + milli;
    }
}