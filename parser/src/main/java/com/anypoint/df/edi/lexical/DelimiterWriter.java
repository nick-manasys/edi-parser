
package com.anypoint.df.edi.lexical;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Map;

/**
 * Base EDI token writer. The writer outputs tokens of various types, with specified delimiter types.
 */
public abstract class DelimiterWriter extends WriterBase
{
    /** Allowed character set for string data (<code>null</code> if unrestricted). */
    private final boolean[] allowedChars;
    
    /** Substitution character for invalid character in string (-1 if unused). */
    final int substitutionChar;
    
    /** Sub-component delimiter (-1 if unused). */
    final int subCompSeparator;
    
    /** Sub-component delimiter. */
    final char componentSeparator;
    
    /** Data element delimiter. */
    final char dataSeparator;
    
    /** Repeated component delimiter (-1 if unused). */
    final int repetitionSeparator;
    
    /** Release character (-1 if unused). */
    final int releaseIndicator;
    
    /** Separator between segments (following segment terminator; <code>null</code> if none). */
    protected final String segmentSeparator;

    /** Segment terminator. */
    private final char segmentTerminator;
    
    /** Flag for positioned at segment start. */
    private boolean segmentStart;
    
    /** Number of groups in interchange. */
    protected int groupCount;
    
    /** Number of skipped data elements. */
    private int skippedElementCount;
    
    /** Number of skipped components. */
    private int skippedCompCount;
    
    /** Number of skipped sub-components. */
    private int skippedSubCompCount;
    
    /**
     * Constructor.
     *
     * @param os output
     * @param encoding character set encoding
     * @param datasep data separator character
     * @param compsep component separator character
     * @param subsep sub-component separator character (-1 if unused)
     * @param repsep repetition separator character (-1 if unused)
     * @param segterm segment terminator character
     * @param segsep inter-segment separator (following segment terminator; <code>null</code> if none)
     * @param release release character
     * @param subst substitution character for invalid character in string (-1 if unused)
     * @param mark decimal mark character
     * @param chars allowed character set flags for string data (<code>null</code> if unrestricted)
     */
    protected DelimiterWriter(OutputStream os, Charset encoding, char datasep, char compsep, int subsep, int repsep, char segterm,
        String segsep, int release, int subst, char mark, boolean[] chars) {
        super(os, encoding, mark);
        dataSeparator = datasep;
        componentSeparator = compsep;
        subCompSeparator = subsep;
        repetitionSeparator = repsep;
        segmentTerminator = segterm;
        segmentSeparator = segsep;
        releaseIndicator = release;
        substitutionChar = subst;
        int limit = Math.max(datasep, Math.max(compsep, Math.max(subsep, Math.max(repsep, Math.max(segterm,
            release)))));
        boolean[] allowed = null;
        if (chars == null) {
            int size = Math.max(limit, 128);
            allowed = initFlags(size);
        } else if (limit > chars.length) {
            allowed = initFlags(limit);
            System.arraycopy(chars, 0, allowed, 0, chars.length);
        } else {
            allowed = new boolean[chars.length];
            System.arraycopy(chars, 0, allowed, 0, chars.length);
        }
        clearFlag(datasep, allowed);
        clearFlag(compsep, allowed);
        clearFlag(subsep, allowed);
        clearFlag(repsep, allowed);
        clearFlag(segterm, allowed);
        allowedChars = allowed;
    }
    
    private static boolean[] initFlags(int length) {
        boolean[] flags = new boolean[length];
        for (int i = 0; i < length; i++) {
            flags[i] = true;
        }
        return flags;
    }
    
    /**
     * Clear flag for character in array, if in range.
     *
     * @param chr
     * @param flags
     */
    private static void clearFlag(int chr, boolean[] flags) {
        if (chr >= 0 && chr < flags.length) {
            flags[chr] = false;
        }
    }
    
    /**
     * Count a group present in interchange.
     */
    public void countGroup() {
        groupCount++;
    }
    
    /**
     * Count a skipped data element in segment. This supports lazy writing of separators for skipped data elements to
     * avoid trailing separators.
     */
    public void skipElement() {
        skippedElementCount++;
        skippedCompCount = 0;
        skippedSubCompCount = 0;
    }
    
    /**
     * Count a skipped component in segment. This supports lazy writing of separators for skipped components to
     * avoid trailing separators.
     */
    public void skipComponent() {
        skippedCompCount++;
        skippedSubCompCount = 0;
    }
    
    /**
     * Count a skipped sub-component in segment. This supports lazy writing of separators for skipped sub-components to
     * avoid trailing separators.
     */
    public void skipSubcomponent() {
        skippedSubCompCount++;
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
     * Convert escaped character.
     * 
     * @param chr
     * @return escape
     * @throws WriteException 
     */
    abstract String convertEscape(char chr) throws WriteException;
    
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
     * Check for start of segment, and write segment separator if so.
     *
     * @throws IOException
     */
    private void checkSegmentStart() throws IOException {
        if (segmentStart && segmentSeparator != null) {
            writer.write(segmentSeparator);
        }
        segmentStart = false;
    }

    /**
     * Write data separator(s). If any data values have been skipped this also writes out the separators for those
     * values.
     *
     * @throws IOException
     */
    public void writeDataSeparator() throws IOException {
        checkSegmentStart();
        writer.write(dataSeparator);
        for (int i = 0; i < skippedElementCount; i++) {
            writer.write(dataSeparator);
        }
        skippedElementCount = 0;
        skippedCompCount = 0;
        skippedSubCompCount = 0;
    }
    
    /**
     * Write component separator(s). If any component values have been skipped this also writes out the separators
     * for those values.
     *
     * @param count
     * @throws IOException
     */
    public void writeComponentSeparator() throws IOException {
        checkSegmentStart();
        writer.write(componentSeparator);
        for (int i = 0; i < skippedCompCount; i++) {
            writer.write(componentSeparator);
        }
        skippedCompCount = 0;
        skippedSubCompCount = 0;
    }
    
    /**
     * Write sub-component separator(s). If any sub-component values have been skipped this also writes out the
     * separators for those values.
     *
     * @param count
     * @throws IOException
     */
    public void writeSubcomponentSeparator() throws IOException {
        checkSegmentStart();
        writer.write((char)subCompSeparator);
        for (int i = 0; i < skippedSubCompCount; i++) {
            writer.write(subCompSeparator);
        }
        skippedSubCompCount = 0;
    }
    
    /**
     * Write a segment terminator.
     *
     * @throws IOException
     */
    public void writeSegmentTerminator() throws IOException {
        writer.write(segmentTerminator);
        skippedElementCount = 0;
        skippedCompCount = 0;
        skippedSubCompCount = 0;
        segmentCount++;
        segmentStart = true;
    }
    
    /**
     * Write a repetition separator.
     *
     * @throws IOException
     */
    public void writeRepetitionSeparator() throws IOException {
        checkSegmentStart();
        writer.write(repetitionSeparator);
        skippedElementCount = 0;
        skippedCompCount = 0;
        skippedSubCompCount = 0;
    }

    /**
     * Write text with character checks.
     * 
     * @param text
     * @throws IOException
     * @throws WriteException
     */
    private void writeText(String text) throws IOException, WriteException {
        if (releaseIndicator >= 0) {
            StringBuilder builder = new StringBuilder(text);
            for (int i = 0; i < builder.length(); i++) {
                char chr = builder.charAt(i);
                if (chr < allowedChars.length && !allowedChars[chr]) {
                    String escape = convertEscape(chr);
                    builder.replace(i, i+1, escape);
                    i += escape.length() - 1;
                }
            }
            writer.write(builder.toString());
        } else if (text.length() > 0) {
            for (int i = 0; i < text.length(); i++) {
                char chr = text.charAt(i);
                if (chr < allowedChars.length && !allowedChars[chr]) {
                    throw new WriteException("invalid character " + chr + " in data with no release character defined");
                }
            }
            writer.write(text);
        }
    }
    
    /**
     * Write value text (or start of value text).
     *
     * @param text
     * @throws IOException
     */
    public void writeToken(String text) throws IOException {
        checkSegmentStart();
        writeText(text);
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
    public void writeSpacePadded(String text, int minl, int maxl) throws IOException {
        String token = text;
        int length = token.length();
        if (length > maxl) {
            throw new WriteException("length outside of allowed range");
        }
        writeText(token);
        while (length < minl) {
            int pad = Math.min(minl - length, SPACES.length());
            writer.write(SPACES, 0, pad);
            length += pad;
        }
    }
    
    /**
     * Write a numeric value padded to a minimum length with leading zeroes. This recognizes a leading minus sign and
     * doesn't include it in the length, but if any other non-counted characters are present (such as a decimal point)
     * the passed-in lengths need to be adjusted to compensate.
     *
     * @param value
     * @param minl minimum length
     * @param maxl maximum length
     * @param adj extra character count include (decimal point, exponent, etc.)
     * @throws IOException 
     */
    public void writeZeroPadded(String value, int minl, int maxl, int adj) throws IOException {
        String text = value;
        int length = text.length() - adj;
        if (text.startsWith("-")) {
            writeToken("-");
            text = text.substring(1);
        } else  {
            writeToken("");
        }
        while (length < minl) {
            int pad = Math.min(minl - length, ZEROES.length());
            writer.write(ZEROES, 0, pad);
            length += pad;
        }
        if (length > maxl) {
            throw new WriteException("value too long");
        }
        writer.write(text);
    }
    
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
     * Write property value as alphanumeric token.
     *
     * @param key
     * @param props
     * @param dflt default value (<code>null</code> if none)
     * @param minl minimum length
     * @param maxl maximum length
     * @throws IOException 
     */
    protected void writeProperty(String key, Map<String, Object> props, String dflt, int minl, int maxl)
        throws IOException {
        String text;
        if (dflt == null) {
            writeAlphaNumeric(getRequired(key, props).toString(), minl, maxl);
        } else {
            text = (String)props.get(key);
            if (text == null) {
                text = dflt;
            }
            writeAlphaNumeric(text, minl, maxl);
        }
        writeDataSeparator();
    }
    
    /**
     * Write HL7 sequence ID value.
     *
     * @param value
     * @throws IOException
     */
    public void writeSeqId(int value) throws IOException {
        if (value < 0) {
            throw new WriteException("value cannot be negative");
        }
        String text = Integer.toString(value);
        if (text.length() > 4) {
            throw new WriteException("value too long");
        }
        writeToken(text);
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
        writeSpacePadded(text, minl, maxl);
    }
    
    /**
     * Write text as an id value. This is the same as alphanumeric, except that at least one character must be present
     * and the first character cannot be a space.
     *
     * @param text
     * @param minl minimum length
     * @param maxl maximum length
     * @throws IOException
     */
    public void writeId(String text, int minl, int maxl) throws IOException {
        if (minl > 0 && text.length() == 0) {
            throw new WriteException("at least one character must be present in id");
        }
        if (text.charAt(0) == ' ') {
            throw new WriteException("first character of an id cannot be a space");
        }
        writeAlphaNumeric(text, minl, maxl);
    }
}