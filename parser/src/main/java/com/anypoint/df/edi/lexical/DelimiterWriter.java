
package com.anypoint.df.edi.lexical;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.Map;

import org.apache.log4j.Logger;

/**
 * Base EDI token writer. The writer outputs tokens of various types, with specified delimiter types.
 */
public abstract class DelimiterWriter extends WriterBase
{
    protected final Logger logger = Logger.getLogger(getClass());
    
    /** Allowed character set for string data (<code>null</code> if unrestricted). */
    private final boolean[] allowedChars;
    
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
        super(new BufferedWriter(new OutputStreamWriter(os, encoding)), mark);
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
    
    /* (non-Javadoc)
     * @see com.anypoint.df.edi.lexical.WriterBase#close()
     */
    @Override
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
    
    /* (non-Javadoc)
     * @see com.anypoint.df.edi.lexical.WriterBase#writeSegmentTag(java.lang.String)
     */
    @Override
    public void writeSegmentTag(String tag) throws IOException {
        writeToken(tag);
    }

    /* (non-Javadoc)
     * @see com.anypoint.df.edi.lexical.WriterBase#writeSegmentTerminator()
     */
    @Override
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
    
    @Override
    public void writeUnchecked(char[] chars, int offset, int length) throws IOException {
        writer.write(chars, offset, length);
    }

    @Override
    public void writeUnchecked(String text) throws IOException {
        writer.write(text);
    }

    @Override
    public void writeEscaped(String text) throws IOException {
        StringBuilder builder = new StringBuilder(text);
        for (int i = 0; i < builder.length(); i++) {
            char chr = builder.charAt(i);
            if (chr < allowedChars.length && !allowedChars[chr]) {
                if (releaseIndicator >= 0 && (chr == segmentTerminator || chr == dataSeparator ||
                    chr == componentSeparator || chr == subCompSeparator || chr == repetitionSeparator ||
                    chr == releaseIndicator)) {
                    String escape = convertEscape(chr);
                    builder.replace(i, i+1, escape);
                    i += escape.length() - 1;
                } else if (substitutionChar >= 0) {
                    builder.setCharAt(i, (char)substitutionChar);
                } else {
                    throw new WriteException("invalid character " + chr + " in data with no release character and/or substitution character defined");
                }
            }
        }
        writer.write(builder.toString());
    }

    @Override
    public void startToken() throws IOException {
        checkSegmentStart();
    }

    /* (non-Javadoc)
     * @see com.anypoint.df.edi.lexical.WriterBase#writeToken(java.lang.String)
     */
    @Override
    public void writeToken(String text) throws IOException {
        startToken();
        writeEscaped(text);
    }
    
    @Override
    public void error(ValueType typ, ErrorCondition err, String text) throws LexicalException {
        boolean abort = false;
        // TODO: add more tracking of position, as with the lexer equivalent
//        String position = "element " + Integer.toString(elementNumber + 1);
//        if (repetitionNumber > 0) {
//            position = "repetition " + Integer.toString(repetitionNumber + 1) + " of " + position;
//        }
//        switch (currentType) {
//            case SUB_COMPONENT:
//            case COMPONENT:
//                position = "component " + Integer.toString(componentNumber + 1) + " of " + position;
//                if (currentType == ItemType.SUB_COMPONENT) {
//                    position = "subcomponent " + Integer.toString(subCompNumber + 1) + " of " + position;
//                }
//            default:
//                break;
//        }
//        String text = err.text() + " for data type " + typ.typeCode() + " at " + position  + ": '" + tokenBuilder + "'";
//        if (explain != null) {
//            text += " (" + explain + ")";
//        }
        try {
            if (errorHandler == null) {
                throw new LexicalDataException(typ, err, text);
            } else {
                errorHandler.error(typ, err, text);
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
    protected void writeProperty(String key, Map<String, Object> props, Object dflt, ValueType vtype)
        throws IOException {
        if (dflt == null) {
            vtype.write(getRequired(key, props), this);
        } else {
            Object value = props.get(key);
            if (value == null) {
                value = dflt;
            }
            vtype.write(value, this);
        }
        writeDataSeparator();
    }
}