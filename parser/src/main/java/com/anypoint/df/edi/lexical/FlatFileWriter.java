
package com.anypoint.df.edi.lexical;

import java.io.BufferedWriter;
import java.io.FilterWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.util.Map;

import org.apache.log4j.Logger;

/**
 * Writer variation for flat files.
 */
public class FlatFileWriter extends WriterBase
{
    protected final Logger logger = Logger.getLogger(getClass());
    
    private static final char[] SPACES =
        "                                                                                               ".toCharArray();
    
    /** Character encoding used for writing. */
    private final Charset encoding;
    
    /** Flag for raw data writing supported (meaning symmetrical conversions used). */
    private Boolean supportsRaw;
    
    /** Transform from raw data to character code. */
    private char[] rawTransform;
    
    /**
     * Constructor.
     *
     * @param os output
     * @param enc character set encoding
     */
    public FlatFileWriter(OutputStream os, Charset enc) {
        super(new LineBasedWriter(os, enc), '.');
        encoding = enc;
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
    
    @Override
    public void error(TypeFormat typ, ErrorCondition err, String text) throws LexicalException {
        boolean abort = false;
        // TODO: add better tracking of output position, as with input
//        String position = "element " + Integer.toString(elementNumber + 1);
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

    @Override
    public void startToken() throws IOException {}

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
        writer.write(text);
    }
    
    /**
     * Write raw data to output. This does an inverse transform of the byte data to be written, converting it into
     * characters, and writes the characters to the writer. Not all encodings support inverse transforms, so this first
     * checks that the encoding being used by the writer does support it.
     * 
     * @param bytes
     * @throws IOException
     */
    public void writeRaw(byte[] bytes) throws IOException {
        if (supportsRaw == null) {
            ByteBuffer bbuf = ByteBuffer.allocate(256);
            for (int i = 0; i < 256; i++) {
                bbuf.put((byte)i);
            }
            bbuf.position(0);
            CharBuffer cbuf = encoding.decode(bbuf);
            byte[] rbytes = encoding.encode(cbuf).array();
            supportsRaw = Boolean.FALSE;
            if (rbytes.length == 256) {
                supportsRaw = Boolean.TRUE;
                for (int i = 0; i < 256; i++) {
                    if ((rbytes[i] & 0xFF) != i) {
                        supportsRaw = Boolean.FALSE;
                        break;
                    }
                }
                rawTransform = cbuf.array();
            }
        }
        if (supportsRaw.booleanValue()) {
            char[] chars = new char[bytes.length];
            for (int i = 0; i < chars.length; i++) {
                chars[i] = rawTransform[bytes[i] & 0xFF];
            }
            writer.write(chars);
        } else {
            throw new IllegalStateException("Raw data is not supported for character encoding " + encoding.name());
        }
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
        while (rem >= SPACES.length) {
            writer.write(SPACES);
            rem -= SPACES.length;
        }
        if (rem > 0) {
            writer.write(SPACES, 0, rem);
        }
    }
    
    /* (non-Javadoc)
     * @see com.anypoint.df.edi.lexical.WriterBase#writeSegmentTerminator()
     */
    @Override
    public void writeSegmentTerminator() throws IOException {
        ((LineBasedWriter)writer).setSegmentEnd();
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
        
        /** Positioned at end of segment flag. */
        private boolean atEnd;
        
        /** Remaining characters before writing segment tag (ignored if not >0). */
        private int remainLead;

        protected LineBasedWriter(OutputStream os, Charset encoding) {
            super(new BufferedWriter(new OutputStreamWriter(os, encoding)));
            tagStart = -1;
        }
        
        /**
         * Set end of segment state. A line break will be written before any more data is output following this call.
         */
        protected void setSegmentEnd() {
            atEnd = true;
        }
        
        /**
         * Check for end of segment state, writing a line break if set.
         * 
         * @throws IOException
         */
        private void checkEnd() throws IOException {
            if (atEnd) {
                atEnd = false;
                super.write('\n');
            }
        }
        
        /**
         * Define segment tag position in line. This must be called before the {@link #segmentStart(String)} method is
         * called for the first segment using the specified position.
         * 
         * @param start (no tag if < 0)
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
            } else if (tagStart > 0) {
                segmentTag = tag;
                remainLead = tagStart;
            }
        }

        @Override
        public void write(int c) throws IOException {
            checkEnd();
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
            checkEnd();
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
            checkEnd();
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
//    
//    public static void main(String[] args) throws IOException {
//        ByteArrayOutputStream bos = new ByteArrayOutputStream();
//        FlatFileWriter writer = new FlatFileWriter(bos, Charset.forName("Cp1047"));
//        byte[] ibytes = new byte[256];
//        for (int i = 0; i < 256; i++) {
//            ibytes[i] = (byte)i;
//        }
//        writer.writeRaw(ibytes);
//        writer.close();
//        byte[] obytes = bos.toByteArray();
//        for (int i = 0; i < 256; i++) {
//            if ((obytes[i] & 0xFF) != i) {
//                System.out.println("Error at " + i);
//            }
//        }
//    }
}