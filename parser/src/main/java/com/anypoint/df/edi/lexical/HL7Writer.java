
package com.anypoint.df.edi.lexical;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Map;

/**
 * Writer variation for HL7.
 */
public class HL7Writer extends WriterBase
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
     * Initialize document write. This writes the MSH segment tag and separators, returing with the writer positioned
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
}