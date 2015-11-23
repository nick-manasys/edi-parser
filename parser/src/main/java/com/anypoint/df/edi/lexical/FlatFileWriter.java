
package com.anypoint.df.edi.lexical;

import java.io.BufferedWriter;
import java.io.FilterWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
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
        super(new LineBasedWriter(os, encoding), '.');
    }

    /**
     * @param props
     * @see com.anypoint.df.edi.lexical.WriterBase#init(java.util.Map)
     */
    @Override
    public void init(Map<String, Object> props) {
        // unused, to be eliminated
    }

    /**
     * @param props
     * @see com.anypoint.df.edi.lexical.WriterBase#term(java.util.Map)
     */
    @Override
    public void term(Map<String, Object> props) {
        // unused, to be eliminated
    }
    
    /* (non-Javadoc)
     * @see com.anypoint.df.edi.lexical.WriterBase#writeSegmentTag(java.lang.String)
     */
    @Override
    public void writeSegmentTag(String tag) throws IOException {
        ((LineBasedWriter)writer).segmentStart(tag);
    }

    /* (non-Javadoc)
     * @see com.anypoint.df.edi.lexical.WriterBase#writeToken(java.lang.String)
     */
    @Override
    public void writeToken(String text) throws IOException {
        writer.write(text);
    }
    
    /**
     * Define segment tag position in line. This must be called before the {@link #writeSegmentTag(String)} method is
     * called for the first segment using the specified position. If any values being written span the segment tag
     * position an exception will be thrown by the write.
     * 
     * @param start
     */
    public void setTagField(int start) {
        ((LineBasedWriter)writer).setTagField(start);
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
    @Override
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
     * Write a numeric value padded to a maximum length with leading zeroes.
     *
     * @param value
     * @param minl minimum length
     * @param maxl maximum length
     * @param adj extra character count include (decimal point, exponent, etc.)
     * @throws IOException 
     */
    @Override
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
    
    /* (non-Javadoc)
     * @see com.anypoint.df.edi.lexical.WriterBase#writeSegmentTerminator()
     */
    @Override
    public void writeSegmentTerminator() throws IOException {
        writer.write('\n');
        segmentCount++;
    }
    
    /**
     * Writer which inserts segment tag at the appropriate location in each line written.
     */
    private static class LineBasedWriter extends FilterWriter {
        
        /** Segment tag start position in line. */
        private int tagStart;
        
        /** Tag for current segment. */
        private String segmentTag;
        
        /** Remaining characters before writing segment tag (ignored if not >0). */
        private int remainLead;

        protected LineBasedWriter(OutputStream os, Charset encoding) {
            super(new BufferedWriter(new OutputStreamWriter(os, encoding)));
        }
        
        /**
         * Define segment tag position in line. This must be called before the {@link #segmentStart(String)} method is
         * called for the first segment using the specified position.
         * 
         * @param start
         */
        protected void setTagField(int start) {
            tagStart = start;
        }
        
        /**
         * Set start of new segment. This resets the remaining lead count before the segment tag is written, and either
         * writes the segment tag if it's the first item in the line or saves it for writing later.
         * 
         * @param tag
         * @throws IOException 
         */
        public void segmentStart(String tag) throws IOException {
            if (tagStart == 0) {
                write(tag);
                remainLead = 0;
            } else {
                segmentTag = tag;
                remainLead = tagStart;
            }
        }

        @Override
        public void write(int c) throws IOException {
            super.write(c);
            if (remainLead > 0) {
                remainLead--;
                if (remainLead == 0) {
                    write(segmentTag);
                }
            }
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            super.write(cbuf, off, len);
            if (remainLead > 0) {
                remainLead -= len;
                if (remainLead == 0) {
                    write(segmentTag);
                } else if (remainLead < 0) {
                    throw new IllegalArgumentException("value spans start tag position");
                }
            }
        }

        @Override
        public void write(String str, int off, int len) throws IOException {
            super.write(str, off, len);
            if (remainLead > 0) {
                remainLead -= len;
                if (remainLead == 0) {
                    write(segmentTag);
                } else if (remainLead < 0) {
                    throw new IllegalArgumentException("value spans start tag position");
                }
            }
        }
    }
}