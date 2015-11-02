
package com.anypoint.df.edi.lexical;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Map;

/**
 * Writer variation for flat files.
 */
public class FlatFileWriter extends WriterBase
{
    /**
     * Constructor.
     *
     * @param os output
     * @param encoding character set encoding
     */
    public FlatFileWriter(OutputStream os, Charset encoding) {
        super(os, encoding, '.');
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
     * Write token text with no checks on content or length.
     *
     * @param text
     * @throws IOException
     */
    public void writeToken(String text) throws IOException {
        writer.write(text);
    }
    
    /**
     * Write blank value.
     * 
     * @param size
     * @throws IOException
     */
    public void writeBlank(int size) throws IOException {
        int rem = size;
        while (rem >= SPACES.length()) {
            writer.write(SPACES);
            rem -= SPACES.length();
        }
        if (rem > 0) {
            writer.write(SPACES, 0, rem);
        }
    }
    
    /**
     * Write text with space characters appended to maximum length. If the text is longer than the maximum length this
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
        writeToken(token);
        if (length < maxl) {
            writeBlank(maxl - length);
        }
    }
    
    /**
     * Write a numeric value padded to a maximum length with leading zeroes. This recognizes a leading minus sign and
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
        int length = text.length();
        if (length > maxl) {
            throw new WriteException("value too long");
        }
        if (text.startsWith("-")) {
            writeToken("-");
            text = text.substring(1);
        } else  {
            writeToken("");
        }
        while (length < maxl) {
            int pad = Math.min(maxl - length, ZEROES.length());
            writer.write(ZEROES, 0, pad);
            length += pad;
        }
        writer.write(text);
    }
    
    /**
     * Write a segment terminator.
     *
     * @throws IOException
     */
    public void writeSegmentTerminator() throws IOException {
        writer.write('\n');
        segmentCount++;
    }
}