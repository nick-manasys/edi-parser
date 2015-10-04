package com.anypoint.df.edi.lexical;

import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;

import com.anypoint.df.edi.lexical.EdiConstants.DataType;
import com.anypoint.df.edi.lexical.EdiConstants.ItemType;
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;

/**
 * Lexer variation for flat files.
 */
public class FlatFileLexer extends LexerBase
{
	/** Number of characters at start of line representing the segment tag. */
	private final int tagWidth;
	
    /**
     * Constructor.
     *
     * @param is input
     * @param tag number of characters in tag at start of each line
     */
    public FlatFileLexer(InputStream is, int tag) {
        super(is);
        tagWidth = tag;
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
     * Read token characters.
     * 
     * @param length
     * @throws IOException
     */
    private void readToken(int length) throws IOException {
        for (int i = 0; i < length; i++) {
            int chr = reader.read();
            if (chr == -1) {
                throw new LexicalException("Unexpected end of file in line " + segmentNumber);
            }
            tokenBuilder.append((char)chr);
        }
    }
    
    /**
     * Advance to next line of input. This checks for an expected line break as the next input and advances to the next
     * line, taking the lead characters of the line as the segment tag.
     * 
     * @return <code>true</code> if line present, <code>false</code> if end of input
     * @throws IOException 
     */
    public boolean nextLine() throws IOException {
        int chr = reader.read();
        if (chr == -1) {
            return false;
        }
        if (chr != '\n' && chr != '\r') {
            throw new LexicalException("Missing expected line break after line " + segmentNumber);
        }
        while ((chr = reader.read()) == '\n' || chr == '\r');
        if (chr == -1) {
            return false;
        }
        tokenBuilder.setLength(0);
        tokenBuilder.append((char)chr);
        segmentNumber++;
        readToken(tagWidth - 1);
        segmentTag = tokenBuilder.toString();
        currentType = ItemType.SEGMENT;
        return true;
    }
    
    /**
     * Load next input field with specified number of characters.
     * 
     * @param width
     * @throws IOException 
     */
    public void load(int width) throws IOException {
        tokenBuilder.setLength(0);
        readToken(width);
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
        checkLength(DataType.ALPHANUMERIC, lastns, minl, maxl);
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
}