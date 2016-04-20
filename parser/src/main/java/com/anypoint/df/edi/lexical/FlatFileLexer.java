package com.anypoint.df.edi.lexical;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.FilterReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import com.anypoint.df.edi.lexical.EdiConstants.ItemType;

/**
 * Lexer variation for flat files.
 */
public class FlatFileLexer extends LexerBase
{	
    /** Reader for accessing data. */
    private final LineBasedReader typedReader;
    
    /** Character encoding used for writing. */
    private final Charset encoding;
    
    /** Flag for raw data writing supported (meaning symmetrical conversions used). */
    private Boolean supportsRaw;
    
    /** Transform from character code to raw data. */
    private byte[] rawTransform;
    
    /**
     * Constructor.
     *
     * @param is input
     * @param enc character set for reading stream
     */
    public FlatFileLexer(InputStream is, Charset enc) {
        super(is, -1);
        encoding = enc;
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
        nextLine();
    }
    
    /**
     * Advance to next line of input. This checks for an expected line break as the next input and advances to the next
     * line, taking the lead characters of the line as the segment tag.
     * 
     * @return <code>true</code> if line present, <code>false</code> if end of input
     * @throws IOException 
     */
    public boolean nextLine() throws IOException {
        return typedReader.nextLine();
    }
    
    /**
     * Define segment tag position in line.
     * 
     * @param start
     * @param length
     */
    public void setTagField(int start, int length) {
        typedReader.setTagField(start, length);
    }
    
    /**
     * Initialize for parsing input.
     * 
     * @throws IOException
     */
    public void init() throws IOException {
        int chr = reader.read();
        if (chr >= 0) {
            ((LineBasedReader)reader).loadTag((char)chr);
        } else {
            currentType = ItemType.END;
        }
    }
    
    /**
     * Load next input field with specified number of characters.
     * 
     * @param width
     * @throws IOException 
     */
    public void load(int width) throws IOException {
        tokenBuilder.setLength(0);
        typedReader.readToken(width);
        currentType = ItemType.DATA_ELEMENT;
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
        /** Segment tag start position in line. If no tag used, this must be 1 to buffer first character of line. */
        private int tagStart;
        
        /** Length of segment tag. */
        private int tagLength;
        
        /** Number of buffered character prior to start tag (from end of buffer). */
        private int leadOffset;
        
        /** Buffer for characters preceding the tag in line (filled for every line). */
        private char[] leadBuffer;

        protected LineBasedReader(InputStream in, Charset enc) {
            super(new BufferedReader(new InputStreamReader(stream, enc)));
            leadBuffer = new char[1];
            leadOffset = 1;
            tagStart = 1;
        }
        
        /**
         * Define segment tag position in line.
         * 
         * @param start
         * @param length
         */
        protected void setTagField(int start, int length) {
            tagStart = length > 0 ? start : 1;
            tagLength = length;
            leadBuffer = new char[tagStart];
            leadOffset = tagStart;
        }
        
        /**
         * Load segment tag from line. The current input position must be at the start of line when this is called.
         * 
         * @param chr initial character of line
         */
        protected void loadTag(char chr) throws IOException {
            if (tagLength > 0) {
                tokenBuilder.setLength(0);
                if (tagStart > 0) {
                    leadBuffer[0] = chr;
                    int offset = 1;
                    int remain = tagStart - 1;
                    int actual = 0;
                    while (remain > 0 && (actual = read(leadBuffer, offset, remain)) > 0) {
                        offset += actual;
                        remain -= actual;
                    }
                    if (actual < 0) {
                        throw new EOFException("read only " + offset + " with " + tagStart + " expected");
                    }
                    leadOffset = 0;
                } else {
                    tokenBuilder.append((char)chr);
                }
                readToken(tagLength - tokenBuilder.length());
                segmentTag = tokenBuilder.toString();
            } else {
                leadBuffer[0] = (char)chr;
                leadOffset = 0;
                segmentTag = "";
            }
            segmentNumber++;
            elementNumber = 0;
            currentType = ItemType.SEGMENT;
        }
        
        /**
         * Advance to next line of input. This checks for an expected line break as the next input and advances to the
         * next line, buffering any leading text and extracting the segment tag.
         * 
         * @return <code>true</code> if line present, <code>false</code> if end of input
         * @throws IOException 
         */
        protected boolean nextLine() throws IOException {
            int chr = read();
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
            loadTag((char)chr);
            return true;
        }

        /**
         * Read token characters.
         * 
         * @param length
         * @throws IOException
         */
        protected void readToken(int length) throws IOException {
            for (int i = 0; i < length; i++) {
                int chr = read();
                if (chr == -1) {
                    throw new LexicalException("Unexpected end of file in line " + segmentNumber);
                } else if (chr == '\r' || chr == '\n') {
                    throw new LexicalException("Unexpected end of line (expected " + (length - i) + " more characters) for line " + segmentNumber);
                }
                tokenBuilder.append((char)chr);
            }
            elementNumber++;
        }

        @Override
        public int read() throws IOException {
            if (leadOffset < tagStart) {
                return leadBuffer[leadOffset++];
            }
            return super.read();
        }

        @Override
        public int read(char[] cbuf, int off, int len) throws IOException {
            int actual = 0;
            int remain = len;
            if (leadOffset < tagStart) {
                int use = Math.min(len, tagStart - leadOffset);
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