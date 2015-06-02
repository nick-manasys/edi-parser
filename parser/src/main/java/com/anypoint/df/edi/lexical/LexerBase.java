package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.EdiConstants.maximumYear;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Map;

import org.apache.log4j.Logger;

import com.anypoint.df.edi.lexical.EdiConstants.DataType;
import com.anypoint.df.edi.lexical.EdiConstants.ItemType;
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;

/**
 * Base EDI token scanner. The scanner supplies input tokens to consumers along with token delimiter types, with three
 * properties exposed: the delimiter at the start of the current input token ({@link #currentType}), the delimiter at
 * the end of the current input token ({@link #nextType}), and the actual current token ({@link #token}). Various typed
 * parseXXX methods work with the current token as typed data.
 */
public abstract class LexerBase
{
    protected final Logger logger = Logger.getLogger(getClass());
    
    /** Stream supplying document data. */
    final InputStream stream;
    
    /** Substitution character for invalid character in string (-1 if unused). */
    final int substitutionChar;
    
    /** Allowed character set for string data (<code>null</code> if unrestricted). */
    final boolean[] allowedChars;
    
    /** Reader wrapping document data stream (created by {@link #init()}). */
    Reader reader;
    
    /** Data element delimiter. */
    char dataSeparator;
    
    /** Repeated element delimiter (-1 if unused). */
    int repetitionSeparator;
    
    /** Component delimiter. */
    char componentSeparator;
    
    /** Sub-component delimiter (-1 if unused). */
    int subCompSeparator;
    
    /** Release character (-1 if unused). */
    int releaseIndicator;
    
    /** Segment terminator. */
    char segmentTerminator;
    
    /** Alternative decimal mark character (-1 if unused). */
    int altDecimalMark;
    
    /** Total number of groups in interchange. */
    int groupCount;
    
    /** Current segment number. */
    int segmentNumber;
    
    /** Handler for lexical errors. */
    private ErrorHandler errorHandler;
    
    /** Data element number (from start of segment). */
    private int elementNumber;
    
    /** Repetition number (from start of data element). */
    private int repetitionNumber;
    
    /** Component number (from start of composite). */
    private int componentNumber;
    
    /** Component number (from start of nested composite). */
    private int subCompNumber;
    
    /** Type of current token (starting delimiter). */
    private ItemType currentType;
    
    /** Current token. */
    private String token;
    
    /** Type of next token (ending delimiter of current token). */
    private ItemType nextType;
    
    /** Next token (<code>null</code> if not yet scanned). */
    private String peekToken;
    
    /** Type of token following peeked token (ending delimiter of peek token, <code>null</code> if not yet scanned). */
    private ItemType peekType;
    
    /**
     * Constructor.
     *
     * @param is input
     * @param subst substitution character for invalid character in string (-1 if unused)
     * @param altdec alternative decimal mark character (to '.', -1 if unused)
     * @param chars allowed character set flags for string data (<code>null</code> if unrestricted)
     */
    public LexerBase(InputStream is, int subst, int altdec, boolean[] chars) {
        stream = is;
        substitutionChar = subst;
        altDecimalMark = altdec;
        allowedChars = chars;
    }
    
    /**
     * Initialize lexer for start of input.
     * 
     * @param props
     * @return status
     * @throws LexicalException
     */
    public abstract Object init(Map<String,Object> props) throws LexicalException;
    
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
     * Set error handler in use.
     *
     * @param handler
     */
    public void setHandler(ErrorHandler handler) {
        errorHandler = handler;
    }
    
    /**
     * Get data separator character.
     *
     * @return separator
     */
    public char getDataSeparator() {
        return dataSeparator;
    }
    
    /**
     * Get repetition character.
     *
     * @return repetition character, or -1 if none
     */
    public int getRepetitionSeparator() {
        return repetitionSeparator;
    }
    
    /**
     * Get component separator character.
     *
     * @return separator
     */
    public char getComponentSeparator() {
        return componentSeparator;
    }
    
    /**
     * Get release (escape) character.
     *
     * @return release character, or -1 if none
     */
    public int getReleaseIndicator() {
        return releaseIndicator;
    }
    
    /**
     * Get segment terminator character.
     *
     * @return terminator
     */
    public char getSegmentTerminator() {
        return segmentTerminator;
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
     * Get data element repetition number.
     *
     * @return number
     */
    public int getRepetitionNumber() {
        return repetitionNumber;
    }
    
    /**
     * Get component number within composite.
     *
     * @return number
     */
    public int getComponentNumber() {
        return componentNumber;
    }
    
    /**
     * Get subcomponent number within nested composite.
     *
     * @return number
     */
    public int getSubComponentNumber() {
        return subCompNumber;
    }
    
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
     * Handle lexical error. This passes off to the configured handler, but logs the error appropriately based on the
     * result.
     *
     * @param typ data type
     * @param err error condition
     * @param explain optional supplemental explanation text (<code>null</code> if none)
     * @throws LexicalException
     */
    void handleError(DataType typ, ErrorCondition err, String explain) throws LexicalException {
        boolean abort = false;
        String position = "element " + Integer.toString(elementNumber + 1);
        if (repetitionNumber > 0) {
            position = "repetition " + Integer.toString(repetitionNumber + 1) + " of " + position;
        }
        switch (currentType) {
            case SUB_COMPONENT:
            case COMPONENT:
                position = "component " + Integer.toString(componentNumber + 1) + " of " + position;
                if (currentType == ItemType.SUB_COMPONENT) {
                    position = "subcomponent " + Integer.toString(subCompNumber + 1) + " of " + position;
                }
            default:
                break;
        }
        String text = err.text() + " for data type " + typ.code() + " at " + position  + ": '" + token + "'";
        if (explain != null) {
            text += " (" + explain + ")";
        }
        try {
            if (errorHandler == null) {
                throw new LexicalDataException(typ, err, text);
            } else {
                errorHandler.error(this, typ, err, explain);
            }
        } catch (LexicalException e) {
            abort = true;
            throw e;
        } finally {
            if (abort) {
                logger.error("Unrecoverable lexer error " + text);
            } else {
                logger.info("Recoverable lexer error " + text);
            }
        }
    }
    
    /**
     * Parse token beyond the current token.
     *
     * @return token
     * @throws IOException
     */
    public String peek() throws IOException {
        if (peekToken == null) {
            
            // start by skipping whitespace, if necessary
            int value = reader.read();
            if (nextType == ItemType.SEGMENT) {
                while (value == '\n' || value == '\r' || value == ' ') {
                    value = reader.read();
                }
            }
            
            // exit if now at end
            if (value < 0) {
                peekToken = "";
                peekType = ItemType.END;
                return peekToken;
            }
            
            // accumulate value text to next delimiter
            char chr = (char)value;
            StringBuilder builder = new StringBuilder();
            boolean escape = false;
            while (true) {
                if (escape) {
                    builder.append(chr);
                    escape = false;
                } else if (chr == subCompSeparator) {
                    peekType = ItemType.SUB_COMPONENT;
                    break;
                } else if (chr == componentSeparator) {
                    peekType = ItemType.COMPONENT;
                    break;
                } else if (chr == dataSeparator) {
                    peekType = ItemType.DATA_ELEMENT;
                    break;
                } else if (chr == segmentTerminator) {
                    peekType = ItemType.SEGMENT;
                    break;
                } else if (chr == repetitionSeparator) {
                    peekType = ItemType.REPETITION;
                    break;
                } else if (chr == releaseIndicator) {
                    escape = true;
                } else if (chr == -1) {
                    peekType = ItemType.END;
                    break;
                } else {
                    builder.append(chr);
                }
                value = reader.read();
                if (value < 0) {
                    throw new IOException("Unexpected end of file in data");
                }
                chr = (char)value;
            }
            peekToken = builder.length() > 0 ? builder.toString() : "";
            
        }
        return peekToken;
    }
    
    /**
     * Parse next item from input and advance. This sets the current state to the pending state, and sets the new
     * pending state to the item parsed.
     *
     * @return token
     * @throws IOException
     */
    public String advance() throws IOException {
        
        // scan next token and set state
        peek();
        currentType = nextType;
        
        // update counters
        switch (currentType) {
        
            case DATA_ELEMENT:
                elementNumber++;
                componentNumber = 0;
                repetitionNumber = 0;
                break;
            
            case SEGMENT:
                segmentNumber++;
            case END:
                elementNumber = 0;
                componentNumber = 0;
                repetitionNumber = 0;
                break;
            
            case SUB_COMPONENT:
                subCompNumber++;
                break;
            
            case COMPONENT:
                componentNumber++;
                break;
            
            case REPETITION:
                repetitionNumber++;
                componentNumber = 0;
                break;
        
        }
        
        // advance to next token
        token = peekToken;
        nextType = peekType;
        peekToken = null;
        peekType = null;
        return token;
    }
    
    /**
     * Advance with next token type specified. This is used by lexer implementations during initialization.
     *
     * @param type
     * @return token
     * @throws IOException
     */
    String advance(ItemType type) throws IOException {
        nextType = type;
        return advance();
    }
    
    /**
     * Verify the current token effective length.
     *
     * @param type data type
     * @param length effective length
     * @param minl minimum length
     * @param maxl maximum length
     * @return value, <code>null</code> if empty
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
     * Verify the current token text length.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return value, <code>null</code> if empty
     * @throws LexicalException
     */
    public void checkLength(DataType type, int minl, int maxl) throws LexicalException {
        checkLength(type, token.length(), minl, maxl);
    }
    
    /**
     * Get current token as an alpha value and advance to next token.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws LexicalException
     */
    public String parseAlpha(int minl, int maxl) throws IOException {
        checkLength(DataType.ALPHA, minl, maxl);
        String text = token;
        int lastns = -1;
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr >= '0' && chr <= '9') {
                handleError(DataType.ALPHA, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            } else if (chr != ' ') {
                lastns = i;
            }
        }
        advance();
        return text.substring(0, lastns + 1);
    }
    
    /**
     * Get current token as a string value and advance to next token.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public String parseAlphaNumeric(int minl, int maxl) throws IOException {
        checkLength(DataType.ALPHANUMERIC, minl, maxl);
        String text = token;
        int lastns = -1;
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (allowedChars != null && (chr > allowedChars.length || !allowedChars[chr])) {
                handleError(DataType.ALPHANUMERIC, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
                if (substitutionChar >= 0) {
                    text = text.replace(chr, (char)substitutionChar);
                }
            }
            if (chr != ' ') {
                lastns = i;
            }
        }
        advance();
        return text.substring(0, lastns + 1);
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
        checkLength(DataType.ID, minl, maxl);
        String text = token;
        int lastns = -1;
        boolean space = false;
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (Character.isAlphabetic(chr) || (chr >= '0' && chr <= '9')) {
                if (space) {
                    handleError(DataType.ID, ErrorCondition.INVALID_CHARACTER, "embedded space not allowed");
                }
                lastns = i;
            } else if (chr == ' ') {
                space = true;
            } else {
                handleError(DataType.ID, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            }
        }
        advance();
        return text.substring(0, lastns + 1);
    }
    
    /**
     * Check that the current token is an integer number is valid (contains only decimal digits) and of an allowed
     * length.
     *
     * @param minl
     * @param maxl
     * @throws IOException
     */
    protected void checkInteger(int minl, int maxl) throws LexicalException {
        String text = token;
        int length = 0;
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr >= '0' && chr <= '9') {
                length++;
            } else if (i != 0 || chr != '-') {
                handleError(DataType.INTEGER, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            }
        }
        checkLength(DataType.INTEGER, length, minl, maxl);
    }
    
    /**
     * Get current token as an integer value and advance to next token.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public Integer parseInteger(int minl, int maxl) throws IOException {
        checkInteger(minl, maxl);
        String text = token;
        advance();
        return Integer.valueOf(text);
    }
    
    /**
     * Get current token as an integer value and advance to next token.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public BigInteger parseBigInteger(int minl, int maxl) throws IOException {
        checkInteger(minl, maxl);
        String text = token;
        advance();
        return new BigInteger(text);
    }
    
    /**
     * Get current token as an X12 real number value and advance to next token. TODO: handle exponential indicators,
     * allow comma decimal point for EDIFACT
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
        boolean replace = false;
        for (int i = 0; i < text.length(); i++) {
            char chr = text.charAt(i);
            if (chr >= '0' && chr <= '9') {
                length++;
            } else if (!decimal && (chr == '.' || (replace = chr == altDecimalMark))) {
                decimal = true;
            } else if (i != 0 || chr != '-') {
                handleError(DataType.REAL, ErrorCondition.INVALID_CHARACTER, "character '" + chr
                    + "' not allowed or wrong placement");
            }
        }
        checkLength(DataType.REAL, length, minl, maxl);
        advance();
        return new BigDecimal(replace ? text.replace((char)altDecimalMark, '.') : text);
    }
    
    /**
     * Get current token as an X12 date value and advance to next token. Note that this avoids the use of the Java
     * DateFormat class, which has high time and memory overhead.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public Calendar parseDate(int minl, int maxl) throws IOException {
        String text = token;
        int length = text.length();
        if (length != 6 && length != 8) {
            handleError(DataType.DATE, ErrorCondition.INVALID_DATE, "date value must be either 6 or 8 characters");
        }
        checkLength(DataType.DATE, minl, maxl);
        for (int i = 0; i < length; i++) {
            char chr = text.charAt(i);
            if (chr < '0' || chr > '9') {
                handleError(DataType.DATE, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            }
        }
        
        // quick (and loose) check for date sanity
        int day = (text.charAt(length - 1) - '0') + (text.charAt(length - 2) - '0') * 10;
        int month = (text.charAt(length - 3) - '0') + (text.charAt(length - 4) - '0') * 10;
        if (month == 0 || month > 12 || day == 0 || day > 31) {
            handleError(DataType.DATE, ErrorCondition.INVALID_DATE, "month or day out of allowed range");
        }
        int year;
        if (length == 8) {
            year = Integer.parseInt(text.substring(0, 4));
        } else {
            year = 2000 + (text.charAt(1) - '0') + (text.charAt(0) - '0') * 10;
            if (year > maximumYear) {
                year -= 100;
            }
        }
        advance();
        return new GregorianCalendar(year, month - 1, day);
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
        return new BigDecimal(parseBigInteger(minl, maxl), scale);
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
        String text = token;
        int length = text.length();
        if (length != 4 && (length < 6 || length > 8)) {
            handleError(DataType.TIME, ErrorCondition.INVALID_DATE, "time value must be either 4 or 6-8 characters");
        }
        checkLength(DataType.TIME, minl, maxl);
        for (int i = 0; i < length; i++) {
            char chr = text.charAt(i);
            if (chr < '0' || chr > '9') {
                handleError(DataType.TIME, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            }
        }
        
        // quick (and loose) check for time sanity
        int hour = (text.charAt(0) - '0') * 10 + (text.charAt(1) - '0');
        int minute = (text.charAt(2) - '0') * 10 + (text.charAt(3) - '0');
        int second = text.length() < 6 ? 0 : (text.charAt(4) - '0') * 10 + (text.charAt(5) - '0');
        if (hour > 23 || minute > 59 || second > 59) {
            handleError(DataType.TIME, ErrorCondition.INVALID_TIME, "time value out of allowed ranges");
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