package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.EdiConstants.maximumYear;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.GregorianCalendar;

import org.apache.log4j.Logger;

import com.anypoint.df.edi.lexical.EdiConstants.DataType;
import com.anypoint.df.edi.lexical.EdiConstants.ItemType;
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;

/**
 * Base token handling code for all forms of input. The actual token splitting code is implemented by subclasses, this
 * just provides methods for dealing with common datatypes.
 */
public abstract class LexerBase
{
    protected final Logger logger = Logger.getLogger(getClass());
    
    /** Stream supplying document data. */
    final InputStream stream;
    
    /** Reader wrapping document data stream. */
    Reader reader;
    
    /** Handler for lexical errors. */
    ErrorHandler errorHandler;
    
    /** Alternative decimal mark character (-1 if unused). */
    int altDecimalMark;
    
    /** Total number of groups in interchange. */
    int groupCount;
    
    /** Current segment number. */
    int segmentNumber;
    
    /** Data element number (from start of segment). */
    int elementNumber;
    
    /** Type of current token. */
    ItemType currentType;
    
    /** Holder for current token. */
    StringBuilder tokenBuilder;
    
    /** Current segment tag. */
    String segmentTag;
    
    /**
     * Constructor.
     *
     * @param is input
     */
    public LexerBase(InputStream is) {
        stream = is;
        altDecimalMark = -1;
        tokenBuilder = new StringBuilder();
    }
    
    /**
     * Set error handler in use.
     *
     * @param handler
     */
    public void setHandler(ErrorHandler handler) {
        errorHandler = handler;
    }
    
    /**
     * Handle lexical error. This passes off to the configured handler, but logs the error appropriately based on the
     * result.
     *
     * @param typ data type
     * @param err error condition
     * @param explain optional supplemental explanation text (<code>null</code> if none)
     * @throws LexicalException
     */
    abstract void handleError(DataType typ, ErrorCondition err, String explain) throws LexicalException;
    
    /**
     * Discard to specified item type.
     * 
     * @param typ
     * @throws IOException
     */
    abstract public void discardTo(ItemType typ) throws IOException;
    
    /**
     * Discard current segment.
     * 
     * @throws IOException
     */
    public void discardSegment() throws IOException {
        discardTo(ItemType.SEGMENT);
    }
    
    /**
     * Close input.
     */
    public void close() {
        if (reader != null) {
            try {
                reader.close();
            } catch (Throwable t) { /* nothing to do */ }
        } else {
            try {
                stream.close();
            } catch (Throwable t) { /* nothing to do */ }
        }
    }
    
    /**
     * Read bytes from stream into array. Throws an IOException if not enough bytes are present to fill the array.
     *
     * @param byts array
     * @param from starting offset in array
     * @throws IOException
     */
    void readArray(byte[] byts, int from) throws IOException {
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
     * Read bytes from stream. Throws an IOException if the required number of bytes is not present.
     *
     * @param num required number of bytes
     * @return bytes
     * @throws IOException
     */
    byte[] readBytes(int num) throws IOException {
        byte[] byts = new byte[num];
        readArray(byts, 0);
        return byts;
    }
    
    /**
     * Count a group present in interchange.
     */
    public void countGroup() {
        groupCount++;
    }
    
    /**
     * Get current segment number (since last reset).
     *
     * @return number
     */
    public int getSegmentNumber() {
        return segmentNumber;
    }
    
    /**
     * Get data element number within segment.
     *
     * @return number
     */
    public int getElementNumber() {
        return elementNumber;
    }
    
    /**
     * Get the current segment tag.
     *
     * @return tag
     */
    public String segmentTag() {
        return segmentTag;
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
     * Get token text as a string. Lexer methods should instead work directly with the builder, but this method is
     * provided so clients can have access to the raw token when needed.
     * 
     * @return text
     */
    public String token() {
    	return tokenBuilder.toString();
    }
    
    /**
     * Verify token effective length.
     *
     * @param type data type
     * @param length effective length
     * @param minl minimum length
     * @param maxl maximum length
     * @throws LexicalException
     */
    public void checkLength(DataType type, int length, int minl, int maxl) throws LexicalException {
        if (length < minl) {
            handleError(type, ErrorCondition.TOO_SHORT, "effective length " + length + " is less than " + minl);
        } else if (length > maxl) {
            handleError(type, ErrorCondition.TOO_LONG, "effective length " + length + " is greater than " + maxl);
        }
    }
    
    /**
     * Verify current token length.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @throws LexicalException
     */
    public void checkLength(DataType type, int minl, int maxl) throws LexicalException {
        checkLength(type, tokenBuilder.length(), minl, maxl);
    }
    
    /**
     * Get current token as a pure alpha value (no digits allowed) with space padding.
     * TODO: is this actually needed?
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return text
     * @throws LexicalException
     */
    public String parseAlpha(int minl, int maxl) throws IOException {
    	int length = tokenBuilder.length();
        checkLength(DataType.ALPHA, length, minl, maxl);
        int lastns = -1;
        for (int i = 0; i < length; i++) {
            char chr = tokenBuilder.charAt(i);
            if (chr >= '0' && chr <= '9') {
                handleError(DataType.ALPHA, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            } else if (chr != ' ') {
                lastns = i;
            }
        }
        return tokenBuilder.substring(0, lastns + 1);
    }
    
    /**
     * Check that the current token is a valid integer number (contains only decimal digits and optional leading minus
     * sign).
     *
     * @param minl
     * @param maxl
     * @return number of digits
     * @throws IOException
     */
    int checkInteger() throws LexicalException {
    	int length = tokenBuilder.length();
    	int digits = 0;
        for (int i = 0; i < length; i++) {
            char chr = tokenBuilder.charAt(i);
            if (chr >= '0' && chr <= '9') {
            	if (digits > 0 || chr != '0') {
            		digits++;
            	}
            } else if (i != 0 || chr != '-') {
                handleError(DataType.INTEGER, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            }
        }
        return digits;
    }
    
    /**
     * Get current token as a normal integer value.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public Integer parseInteger(int minl, int maxl) throws IOException {
        checkInteger();
        int length = tokenBuilder.length();
        if (length > 0 && tokenBuilder.charAt(0) == '-') {
        	length--;
        }
        checkLength(DataType.INTEGER, length, minl, maxl);
        return Integer.valueOf(tokenBuilder.toString());
    }
    
    /**
     * Convert token to integer value appropriate for number of digits.
     * 
     * @param digits
     * @return
     */
    Object sizedInt(int digits) {
        String text = tokenBuilder.toString();
        if (digits < 10) {
            return Integer.valueOf(text);
        } else if (digits < 20) {
            return Long.valueOf(text);
        }
        return new BigInteger(text);
    }
    
    /**
     * Get current token as an integer value of varying size.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public Object parseBigInteger(int minl, int maxl) throws IOException {
    	int digits = checkInteger();
        int length = tokenBuilder.length();
        if (length > 0 && tokenBuilder.charAt(0) == '-') {
        	length--;
        }
        checkLength(DataType.INTEGER, length, minl, maxl);
        return sizedInt(digits);
    }
    
    /**
     * Get current token as a real number value of varying size.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public Object parseBigDecimal(int minl, int maxl) throws IOException {
        int length = tokenBuilder.length();
        int actual = 0;
        int digits = 0;
        boolean decimal = false;
        int exponent = -1;
        for (int i = 0; i < length; i++) {
            char chr = tokenBuilder.charAt(i);
            if (chr >= '0' && chr <= '9') {
            	actual++;
            	if (digits > 0 || chr != '0') {
            		digits++;
            	}
            } else if (!decimal && (chr == '.' || chr == altDecimalMark)) {
                decimal = true;
                if (chr == altDecimalMark) {
                	tokenBuilder.setCharAt(i, '.');
                }
            } else if (exponent < 0 && (chr == 'E' || chr == 'e')) {
                exponent = i;
            } else if ((i != 0 && i != exponent + 1) || chr != '-') {
                handleError(DataType.REAL, ErrorCondition.INVALID_CHARACTER, "character '" + chr +
                    "' not allowed or wrong placement");
            }
        }
        checkLength(DataType.REAL, actual, minl, maxl);
        if (exponent >= 0) {
            BigDecimal base = new BigDecimal(tokenBuilder.substring(0, exponent));
            int power = Integer.valueOf(tokenBuilder.substring(exponent + 1));
            return base.scaleByPowerOfTen(power);
        }
        if (decimal) {
            return new BigDecimal(tokenBuilder.toString());
        }
        return sizedInt(digits);
    }
    
    /**
     * Get current token as a general numeric value without exponents.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public Object parseUnscaledNumber(int minl, int maxl) throws IOException {
        int length = tokenBuilder.length();
        int digits = 0;
        boolean decimal = false;
        for (int i = 0; i < length; i++) {
            char chr = tokenBuilder.charAt(i);
            if (chr >= '0' && chr <= '9') {
                digits++;
            } else if (!decimal && chr == '.') {
                decimal = true;
            } else if (i != 0 || (chr != '+' && chr != '-')) {
                handleError(DataType.REAL, ErrorCondition.INVALID_CHARACTER, "character '" + chr
                    + "' not allowed or wrong placement");
            }
        }
        checkLength(DataType.REAL, digits, minl, maxl);
        if (decimal) {
            return new BigDecimal(tokenBuilder.toString());
        }
        return sizedInt(digits);
    }
    
    /**
     * Get current token as a number value with implied decimal point.
     *
     * @param scale inverse power of ten multiplier
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public BigDecimal parseImpliedDecimalNumber(int scale, int minl, int maxl) throws IOException {
        checkInteger();
        int length = tokenBuilder.length();
        if (length > 0 && tokenBuilder.charAt(0) == '-') {
            length--;
        }
        checkLength(DataType.INTEGER, length, minl, maxl);
        return new BigDecimal(new BigInteger(tokenBuilder.toString()), scale);
    }
    
    /**
     * Get current token as a date value. Note that this avoids the use of the Java DateFormat class, which has high
     * time and memory overhead.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public Calendar parseDate(int minl, int maxl) throws IOException {
        int length = tokenBuilder.length();
        if (length != 6 && length != 8) {
            handleError(DataType.DATE, ErrorCondition.INVALID_DATE, "date value must be either 6 or 8 characters");
        }
        checkLength(DataType.DATE, minl, maxl);
        for (int i = 0; i < length; i++) {
            char chr = tokenBuilder.charAt(i);
            if (chr < '0' || chr > '9') {
                handleError(DataType.DATE, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            }
        }
        
        // quick (and loose) check for date sanity
        int day = (tokenBuilder.charAt(length - 1) - '0') + (tokenBuilder.charAt(length - 2) - '0') * 10;
        int month = (tokenBuilder.charAt(length - 3) - '0') + (tokenBuilder.charAt(length - 4) - '0') * 10;
        if (month == 0 || month > 12 || day == 0 || day > 31) {
            handleError(DataType.DATE, ErrorCondition.INVALID_DATE, "month or day out of allowed range");
        }
        int year;
        if (length == 8) {
            year = Integer.parseInt(tokenBuilder.substring(0, 4));
        } else {
            year = 2000 + (tokenBuilder.charAt(1) - '0') + (tokenBuilder.charAt(0) - '0') * 10;
            if (year > maximumYear) {
                year -= 100;
            }
        }
        return new GregorianCalendar(year, month - 1, day);
    }
    
    /**
     * Get current token as ax time value.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public int parseTime(int minl, int maxl) throws IOException {
        int length = tokenBuilder.length();
        if (length != 4 && (length < 6 || length > 8)) {
            handleError(DataType.TIME, ErrorCondition.INVALID_DATE, "time value must be either 4 or 6-8 characters");
        }
        checkLength(DataType.TIME, minl, maxl);
        for (int i = 0; i < length; i++) {
            char chr = tokenBuilder.charAt(i);
            if (chr < '0' || chr > '9') {
                handleError(DataType.TIME, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            }
        }
        
        // quick (and loose) check for time sanity
        int hour = (tokenBuilder.charAt(0) - '0') * 10 + (tokenBuilder.charAt(1) - '0');
        int minute = (tokenBuilder.charAt(2) - '0') * 10 + (tokenBuilder.charAt(3) - '0');
        int second = tokenBuilder.length() < 6 ? 0 : (tokenBuilder.charAt(4) - '0') * 10 +
            (tokenBuilder.charAt(5) - '0');
        if (hour > 23 || minute > 59 || second > 59) {
            handleError(DataType.TIME, ErrorCondition.INVALID_TIME, "time value out of allowed ranges");
        }
        int milli = 0;
        if (length > 6) {
            milli = (tokenBuilder.charAt(6) - '0') * 100;
            if (length > 7) {
                milli = (tokenBuilder.charAt(7) - '0') * 10;
            }
        }
        return ((hour * 60 + minute) * 60 + second) * 1000 + milli;
    }
}