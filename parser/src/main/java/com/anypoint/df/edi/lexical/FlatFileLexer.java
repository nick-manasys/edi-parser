package com.anypoint.df.edi.lexical;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.FilterReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Calendar;

import com.anypoint.df.edi.lexical.EdiConstants.DataType;
import com.anypoint.df.edi.lexical.EdiConstants.ItemType;
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;

/**
 * Lexer variation for flat files.
 */
public class FlatFileLexer extends LexerBase
{	
    /**
     * Constructor.
     *
     * @param is input
     */
    public FlatFileLexer(InputStream is) {
        super(is);
        reader = new LineBasedReader(stream);
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
        return ((LineBasedReader)reader).nextLine();
    }
    
    /**
     * Define segment tag position in line.
     * 
     * @param start
     * @param length
     */
    public void setTagField(int start, int length) {
        ((LineBasedReader)reader).setTagField(start, length);
    }
    
    /**
     * Initialize for parsing input.
     * 
     * @throws IOException
     */
    public void init() throws IOException {
        int chr = reader.read();
        if (chr >= 0) {
            ((LineBasedReader)reader).loadTag(chr);
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
        ((LineBasedReader)reader).readToken(width);
        currentType = ItemType.DATA_ELEMENT;
    }
    
    /**
     * Handle lexical error. This passes off to the configured handler, but logs the error appropriately based on the
     * result.
     *
     * @param typ data type
     * @param err error condition
     * @param explain optional supplemental explanation text (<code>null</code> if none)
     * @throws LexicalException
     */
    void handleError(DataType typ, ErrorCondition err, String explain) throws LexicalException {
        boolean abort = false;
        String position = "element " + Integer.toString(elementNumber + 1);
        String text = err.text() + " for data type " + typ.code() + " at " + position  + ": '" + tokenBuilder + "'";
        if (explain != null) {
            text += " (" + explain + ")";
        }
        try {
            if (errorHandler == null) {
                throw new LexicalDataException(typ, err, text);
            } else {
                errorHandler.error(this, typ, err, explain);
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
     * Get token as a plain alphanumeric value with space padding.
     *
     * @param minl minimum length (excluding trailing spaces)
     * @param maxl maximum length (actual width of field)
     * @return
     * @throws IOException
     */
    public String parseAlphaNumeric(int minl, int maxl) throws IOException {
        load(maxl);
        int lastns = -1;
        for (int i = 0; i < maxl; i++) {
            if (tokenBuilder.charAt(i) != ' ') {
                lastns = i;
            }
        }
        checkLength(DataType.ALPHANUMERIC, lastns + 1, minl, maxl);
        return tokenBuilder.substring(0, lastns + 1);
    }
    
    /**
     * Get token as a normal integer value.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (actual width of field)
     * @return
     * @throws IOException
     */
    public Integer parseInteger(int minl, int maxl) throws IOException {
        load(maxl);
        return super.parseInteger(minl, maxl);
    }

    /**
     * Get token as an integer value of varying size.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public Object parseBigInteger(int minl, int maxl) throws IOException {
        load(maxl);
        return super.parseBigInteger(minl, maxl);
    }

    /**
     * Get token as a real number value of varying size.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public Object parseBigDecimal(int minl, int maxl) throws IOException {
        load(maxl);
        return super.parseBigDecimal(minl, maxl);
    }
    
    /**
     * Get token as a general numeric value without exponents.
     *
     * @param minl minimum length (excluding sign and/or decimal)
     * @param maxl maximum length (excluding sign and/or decimal)
     * @return
     * @throws IOException
     */
    public Object parseUnscaledNumber(int minl, int maxl) throws IOException {
        load(maxl);
        return super.parseUnscaledNumber(minl, maxl);
    }

    /**
     * Get token as a date value. Note that this avoids the use of the Java DateFormat class, which has high
     * time and memory overhead.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public Calendar parseDate(int minl, int maxl) throws IOException {
        load(maxl);
        return super.parseDate(minl, maxl);
    }

    /**
     * Get token as ax time value.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @return
     * @throws IOException
     */
    public int parseTime(int minl, int maxl) throws IOException {
        load(maxl);
        return super.parseTime(minl, maxl);
    }
    
    /**
     * Reader which extracts segment tag from each line read, buffering and holding any text prior to the segment tag.
     */
    private class LineBasedReader extends FilterReader
    {
        /** Segment tag start position in line. */
        private int tagStart;
        
        /** Length of segment tag. */
        private int tagLength;
        
        /** Number of buffered character prior to start tag (from end of buffer). */
        private int leadOffset;
        
        /** Buffer for characters preceding the tag in line (filled for every line). */
        private char[] leadBuffer;

        protected LineBasedReader(InputStream in) {
            super(new BufferedReader(new InputStreamReader(stream, EdiConstants.ASCII_CHARSET)));
        }
        
        /**
         * Define segment tag position in line.
         * 
         * @param start
         * @param length
         */
        protected void setTagField(int start, int length) {
            tagStart = start;
            tagLength = length;
            leadBuffer = new char[start];
        }
        
        /**
         * Load segment tag from line. The current input position must be at the start of line when this is called.
         * 
         * @param chr initial character of line
         */
        protected void loadTag(int chr) throws IOException {
            tokenBuilder.setLength(0);
            if (tagStart > 0) {
                leadBuffer[0] = (char)chr;
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
            segmentNumber++;
            elementNumber = 0;
            readToken(tagLength - tokenBuilder.length());
            segmentTag = tokenBuilder.toString();
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
            loadTag(chr);
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