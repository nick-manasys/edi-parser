
package com.anypoint.df.edi.lexical;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Map;

/**
 * Writer variation for HL7.
 */
public class HL7Writer extends DelimiterWriter
{
    /**
     * Constructor.
     *
     * @param os output
     * @param encoding character set encoding
     * @param delims delimiter characters (data separator, component separator, repetition separator, escape character,
     *  subcomponent separator)
     * @param subst substitution character for invalid character in string (-1 if unused)
     */
    public HL7Writer(OutputStream os, Charset encoding, String delims, int subst) {
        super(os, encoding, delims.charAt(0), delims.charAt(1), delims.charAt(4), delims.charAt(2), '\r',
            null, delims.charAt(3), subst, '.', null);
    }
    
    /**
     * Convert escaped character.
     * 
     * @param chr
     * @return escape
     * @throws WriteException 
     */
    String convertEscape(char chr) throws WriteException {
        StringBuilder builder = new StringBuilder();
        char rls = (char)releaseIndicator;
        builder.append(rls);
        if (chr == releaseIndicator) {
            builder.append('E');
        } else if (chr == dataSeparator) {
            builder.append('F');
        } else if (chr == repetitionSeparator) {
            builder.append('R');
        } else if (chr == componentSeparator) {
            builder.append('S');
        } else if (chr == subCompSeparator) {
            builder.append('T');
        } else {
            throw new WriteException("unsupported character in data " + chr);
        }
        builder.append(rls);
        return builder.toString();
    }

    /**
     * Initialize document write. This writes the MSH segment tag and separators, returning with the writer positioned
     * for writing the MSH-3 value.
     *
     * @param props
     * @throws IOException 
     * @see com.anypoint.df.edi.lexical.WriterBase#init(java.util.Map)
     */
    public void init(Map<String, Object> props) throws IOException {
        writer.write("MSH");
        writeDataSeparator();
        writer.write(((String)props.get("MSH-02")));
    }

    /**
     * @param props
     * @throws IOException
     * @see com.anypoint.df.edi.lexical.WriterBase#term(java.util.Map)
     */
    public void term(Map<String, Object> props) throws IOException {
        // unused, to be eliminated
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
}