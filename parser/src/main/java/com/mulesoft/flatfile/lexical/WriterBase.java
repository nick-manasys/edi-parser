
package com.mulesoft.flatfile.lexical;

import java.io.IOException;
import java.io.Writer;
import java.util.Map;

/**
 * Base token writing code for all forms of output. The actual token arranging code is implemented by subclasses, this
 * just provides methods for dealing with common datatypes.
 */
public abstract class WriterBase implements ErrorHandler
{
    /** Character used for decimal mark. */
    public final char decimalMark;
    
    /** Writer wrapping document data stream. */
    protected final Writer writer;
    
    /** Delegate handler for lexical errors. */
    ErrorHandler errorHandler;
    
    /** Substitution character for invalid character in string (0 if delete, -1 if unused). */
    protected int substitutionChar;
    
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
     * Set error handler in use.
     *
     * @param handler
     */
    public void setHandler(ErrorHandler handler) {
        errorHandler = handler;
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
     * Prepare for writing token text. This is automatically called by {@link #writeToken(String)}.
     * 
     * @throws IOException
     */
    public abstract void startToken() throws IOException;
    
    /**
     * Write characters without checking content. This should only be used for constant data known in advance to be safe.
     * 
     * @param chars
     * @param offset
     * @param length
     * @throws IOException
     */
    public abstract void writeUnchecked(char[] chars, int offset, int length) throws IOException;
    
    /**
     * Write text without checking content. This should only be used for constant data known in advance to be safe.
     * 
     * @param text
     * @throws IOException
     */
    public abstract void writeUnchecked(String text) throws IOException;
    
    /**
     * Write text with character checks and appropriate escaping.
     * 
     * @param text
     * @throws IOException
     */
    public abstract void writeEscaped(String text) throws IOException;
    
    /**
     * Write token text with character checks and appropriate escaping.
     *
     * @param text
     * @throws IOException
     */
    public abstract void writeToken(String text) throws IOException;
    
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
}