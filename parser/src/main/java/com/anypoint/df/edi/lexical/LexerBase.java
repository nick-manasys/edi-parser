package com.anypoint.df.edi.lexical;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import org.apache.log4j.Logger;

import com.anypoint.df.edi.lexical.EdiConstants.ItemType;

/**
 * Base token handling code for all forms of input. The actual token splitting code is implemented by subclasses, this
 * just provides methods for dealing with common datatypes.
 */
public abstract class LexerBase implements ErrorHandler
{
    protected final Logger logger = Logger.getLogger(getClass());
    
    /** Stream supplying document data. */
    final InputStream stream;
    
    /** Alternative decimal mark character (-1 if unused). */
    final int altDecimalMark;
    
    /** Reader wrapping document data stream. */
    Reader reader;
    
    /** Delegate handler for lexical errors. */
    ErrorHandler errorHandler;
    
    /** Substitution character for invalid character in string (0 if delete, -1 if unused). */
    int substitutionChar;
    
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
     * @param altmark alternative decimal mark (-1 if unused)
     */
    public LexerBase(InputStream is, int altmark) {
        stream = is;
        altDecimalMark = altmark;
        tokenBuilder = new StringBuilder();
    }
    
    /**
     * Get alternative decimal mark.
     * 
     * @return mark character (-1 if not used)
     */
    public int getAltDecimalMark() {
        return altDecimalMark;
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
     * Get substitution character for invalid characters in strings.
     * 
     * @return substitution (0 if delete, -1 if none)
     */
    public int getSubstitutionChar() {
        return substitutionChar;
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
     * Get token text as a string.
     * 
     * @return text
     */
    public String token() {
    	return tokenBuilder.toString();
    }
    
    /**
     * Get token text as builder. Lexer methods should instead work directly with the builder, but this method is
     * provided so clients can have access to the raw token when needed.
     * 
     * @return text
     */
    public StringBuilder tokenBuilder() {
        return tokenBuilder;
    }
}