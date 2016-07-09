package com.mulesoft.flatfile.lexical;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.FilterReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import com.mulesoft.flatfile.lexical.EdiConstants.ItemType;

/**
 * Lexer variation for flat files.
 */
public class FlatFileLexer extends LexerBase
{	
    /** Reader for accessing data. */
    private final LineBasedReader typedReader;
    
    /** Character encoding used for writing. */
    private final Charset encoding;
    
    /** Allow raw access to data (disables end-of-line checks). */
    private final boolean allowRaw;
    
    /** Discard line past end of data used. */
    private final boolean discardExtra;
    
    /** Fill character for short line (-1 if no fill). */
    private final int fillShort;
    
    /** Character used for missing values in line (-1 if no missing). */
    private final int missingChar;
    
    /** Flag for raw data writing supported (meaning symmetrical conversions used). */
    private Boolean supportsRaw;
    
    /** Transform from character code to raw data. */
    private byte[] rawTransform;
    
    /**
     * Constructor.
     *
     * @param is input
     * @param enc character set for reading stream
     * @param raw support reading raw values
     * @param discard trailing data on line ignored (to end of line)
     * @param fill character used as fill for short line (-1 if no fill)
     * @param miss character used for fields with no value (-1 if all fields present)
     */
    public FlatFileLexer(InputStream is, Charset enc, boolean raw, boolean discard, int fill, int miss) {
        super(is, -1);
        encoding = enc;
        allowRaw = raw;
        discardExtra = discard;
        fillShort = fill;
        missingChar = miss;
        reader = typedReader = new LineBasedReader(stream, enc);
    }
    
    /**
     * Discard to specified item type.
     * 
     * @param typ
     * @throws IOException
     */
    @Override
    public void discardTo(ItemType typ) throws IOException {
        if (typ != ItemType.SEGMENT) {
            throw new IllegalArgumentException("Flat files do not support " + typ + " data type positioning");
        }
        typedReader.nextLine(true);
    }
    
    /**
     * Advance to next line of input. This checks for an expected line break as the next input (unless discarding extra
     * characters in line) and advances to the next line.
     * 
     * @return <code>true</code> if line present, <code>false</code> if end of input
     * @throws IOException 
     */
    public boolean nextLine() throws IOException {
        return typedReader.nextLine(discardExtra);
    }
    
    /**
     * Load tag field from line.
     * 
     * @param start
     * @param length
     * @throws IOException 
     */
    public String loadTagField(int start, int length) throws IOException {
        return typedReader.loadTagField(start, length);
    }
    
    /**
     * Initialize for parsing input.
     * 
     * @throws IOException
     */
    public void init() throws IOException {
        int chr = reader.read();
        if (chr >= 0) {
            typedReader.startLine(chr);
        } else {
            currentType = ItemType.END;
        }
    }
    
    /**
     * Load next input field with specified number of characters.
     * 
     * @param width
     * @return <code>true</code> if token present, <code>false</code> if not
     * @throws IOException 
     */
    public boolean load(int width) throws IOException {
        tokenBuilder.setLength(0);
        boolean result = typedReader.readToken(width);
        currentType = ItemType.DATA_ELEMENT;
        return result;
    }
    
    /**
     * Get raw bytes for token. This does an inverse transform of the character data which was read, converting it back
     * into bytes and returning the bytes. Not all encodings support inverse transforms, so this first checks that the
     * encoding being used by the reader does support it.
     * 
     * @param bytes
     * @throws IOException
     */
    public byte[] rawToken() throws IOException {
        if (!allowRaw) {
            throw new IllegalStateException("Lexer is not configured for raw data access");
        }
        if (supportsRaw == null) {
            CharBuffer cbuf = CharBuffer.allocate(256);
            for (int i = 0; i < 256; i++) {
                cbuf.put((char)i);
            }
            cbuf.position(0);
            ByteBuffer bbuf = encoding.encode(cbuf);
            char[] rchars = encoding.decode(bbuf).array();
            supportsRaw = Boolean.FALSE;
            if (rchars.length == 256) {
                supportsRaw = Boolean.TRUE;
                for (int i = 0; i < 256; i++) {
                    if (rchars[i] != i) {
                        supportsRaw = Boolean.FALSE;
                        break;
                    }
                }
                rawTransform = bbuf.array();
            }
        }
        if (supportsRaw.booleanValue()) {
            String token = token();
            byte[] bytes = new byte[token.length()];
            for (int i = 0; i < bytes.length; i++) {
                bytes[i] = rawTransform[token.charAt(i)];
            }
            return bytes;
        } else {
            throw new IllegalStateException("Raw data is not supported for character encoding " + encoding.name());
        }
    }
    
    /**
     * Handle lexical error. This passes off to the configured handler, but logs the error appropriately based on the
     * result.
     *
     * @param typ value type
     * @param err error condition
     * @param explain optional supplemental explanation text (<code>null</code> if none)
     * @throws LexicalException
     */
    public void error(TypeFormat typ, ErrorCondition err, String explain) throws LexicalException {
        boolean abort = false;
        String position = "element " + Integer.toString(elementNumber + 1);
        String text = err.text() + " for data type " + typ.typeCode() + " at " + position  + ": '" + tokenBuilder + "'";
        if (explain != null) {
            text += " (" + explain + ")";
        }
        try {
            if (errorHandler == null) {
                throw new LexicalDataException(typ, err, text);
            } else {
                errorHandler.error(typ, err, explain);
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
     * Reader which extracts segment tag from each line read, buffering and holding any text prior to the segment tag.
     */
    private class LineBasedReader extends FilterReader
    {
        /** At end of line flag. */
        private boolean lineEnd;
        
        /** Number of buffered characters. */
        private int leadCount;
        
        /** Offset for reading data from buffered. */
        private int leadOffset;
        
        /** Buffer for characters preceding the tag in line (filled for every line). */
        private char[] leadBuffer;

        protected LineBasedReader(InputStream in, Charset enc) {
            super(new BufferedReader(new InputStreamReader(stream, enc)));
            leadBuffer = new char[128];
            leadCount = 0;
        }
        
        /**
         * Read tag field from input line. There line must be at least as long as the end of the field.
         * 
         * @param start
         * @param length
         * @throws IOException 
         */
        protected String loadTagField(int start, int length) throws IOException {
            int limit = start + length;
            if (leadBuffer.length < limit) {
                char[] newbuff = new char[limit];
                System.arraycopy(leadBuffer, 0, newbuff, 0, leadCount);
                leadBuffer = newbuff;
            }
            int remain = limit - leadCount;
            int actual = 0;
            while (remain > 0 && (actual = super.read(leadBuffer, leadCount, remain)) > 0) {
                leadCount += actual;
                remain -= actual;
            }
            if (actual < 0) {
                throw new EOFException("read only " + leadCount + " with at least " + limit + " expected");
            }
            return new String(leadBuffer, start, length);
        }
        
        /**
         * Initialize state for start of line, saving the first character of the line to the input buffer.
         * 
         * @param chr
         */
        protected void startLine(int chr) {
            lineEnd = false;
            leadBuffer[0] = (char)chr;
            leadCount = 1;
            leadOffset = 0;
            segmentNumber++;
            elementNumber = 0;
            currentType = ItemType.SEGMENT;
        }
        
        /**
         * Advance to next line of input. This checks for an expected line break and advances to the next line,
         * buffering any leading text and extracting the segment tag.
         * 
         * @param force discard remainder of current line
         * @return <code>true</code> if line present, <code>false</code> if end of input
         * @throws IOException 
         */
        protected boolean nextLine(boolean force) throws IOException {
            int chr = '\n';
            if (!lineEnd) {
                while ((chr = read()) != -1) {
                    if (!force || chr == '\n' || chr == '\r') {
                        break;
                    }
                }
            }
            if (chr == -1) {
                currentType = ItemType.END;
                return false;
            }
            if (chr != '\n' && chr != '\r') {
                throw new LexicalException("Missing expected line break after line " + segmentNumber);
            }
            while ((chr = reader.read()) == '\n' || chr == '\r');
            if (chr == -1) {
                currentType = ItemType.END;
                return false;
            }
            startLine(chr);
            return true;
        }

        /**
         * Read token characters.
         * 
         * @param length
         * @return <code>true</code> if token present, <code>false</code> if not
         * @throws IOException
         */
        protected boolean readToken(int length) throws IOException {
            boolean hasData = false;
            for (int i = 0; i < length; i++) {
                int chr = fillShort;
                if (!lineEnd) {
                    chr = read();
                    if (chr == -1) {
                        if (fillShort >= 0) {
                            lineEnd = true;
                            chr = fillShort;
                        } else {
                            throw new LexicalException("Unexpected end of file in line " + segmentNumber + " (read " + i + " of expected " + length + " characters in field)");
                        }
                    } else if (!allowRaw && (chr == '\r' || chr == '\n')) {
                        if (fillShort >= 0) {
                            lineEnd = true;
                            chr = fillShort;
                        } else {
                            throw new LexicalException("Unexpected end of line" + segmentNumber + " (read " + i + " of expected " + length + " characters in field)");
                        }
                    }
                }
                if (chr != missingChar) {
                    hasData = true;
                }
                tokenBuilder.append((char)chr);
            }
            elementNumber++;
            return hasData;
        }

        @Override
        public int read() throws IOException {
            if (leadOffset < leadCount) {
                return leadBuffer[leadOffset++];
            }
            return super.read();
        }

        @Override
        public int read(char[] cbuf, int off, int len) throws IOException {
            int actual = 0;
            int remain = len;
            if (leadOffset < leadCount) {
                int use = Math.min(len, leadCount - leadOffset);
                System.arraycopy(leadBuffer, leadOffset, cbuf, off, use);
                leadOffset += use;
                actual = use;
                remain -= use;
            }
            if (remain > 0) {
                return actual + super.read(cbuf, off + actual, remain);
            }
            return actual;
        }

        @Override
        public boolean markSupported() {
            return false;
        }

        @Override
        public void mark(int readAheadLimit) throws IOException {
            throw new UnsupportedOperationException("mark() is not supported");
        }
    }
}