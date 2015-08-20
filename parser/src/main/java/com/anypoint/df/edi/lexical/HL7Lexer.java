package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.EdiConstants.ASCII_CHARSET;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Map;

import com.anypoint.df.edi.lexical.EdiConstants.ItemType;

/**
 * Lexer variation for HL7.
 */
public class HL7Lexer extends LexerBase
{
    /** Status returned by {@link EDIFACT#term(Map)} method. */
    public enum InterchangeEndStatus {
        VALID, GROUP_COUNT_ERROR, CONTROL_NUMBER_ERROR
    }
    
    /**
     * Constructor.
     *
     * @param is input
     * @param subst substitution character for invalid character in string (-1 if unused)
     */
    public HL7Lexer(InputStream is, int subst) {
        super(is);
        substitutionChar = subst;
        segmentTerminator = (char)0x0D;
    }
    
    /**
     * Initialize document parse. This checks the start of the document to find the separator characters used in
     * parsing. Returns with the lexer positioned at the MSH-3 value.
     *
     * @param props store for property values from interchange
     * @return delimiter characters
     * @throws LexicalException
     */
    public String init(Map<String,Object> props) throws LexicalException {
        try {
            
            // check the segment tag for optional UNA
            byte[] byts = readBytes(3);
            String tag = new String(byts, ASCII_CHARSET);
            if (!"MSH".equals(tag)) {
                throw new RuntimeException("Message does not start with 'MSH'");
            }
            
            // get separator and encoding characters
            byts = readBytes(5);
            dataSeparator = (char)byts[0];
            componentSeparator = (char)byts[1];
            repetitionSeparator =(char)byts[2];
            releaseIndicator = (char)byts[3];
            subCompSeparator = (char)byts[4];
            if (stream.read() != dataSeparator) {
                throw new RuntimeException("Field separator not present following MSH-2");
            }
            
            // initialize reader and set lexer to MSH-3 field
            reader = new ByteReader();
            advance(ItemType.DATA_ELEMENT);
            elementNumber = 2;
            return new String(byts, ASCII_CHARSET);
            
        } catch (IOException e) {
            throw new LexicalException("Message aborted due to error reading header", e);
        }
    }

    /**
     * Finish document parse.
     * 
     * @param props
     * @throws IOException
     */
    public void term(Map<String, Object> props) throws IOException {
    }
    
    /**
     * Reader that just uses each input byte as a character. This is used for reading the MSH segment, since we don't
     * know the actual character encoding for the message body until it's been read.
     */
    private class ByteReader extends Reader
    {
        @Override
        public int read(char[] cbuf, int off, int len) throws IOException {
            int actual = -1;
            while (actual < len - 1) {
                int chr = stream.read();
                if (chr < 0) {
                    break;
                }
                cbuf[off + ++actual] = (char)chr;
            }
            return actual;
        }

        @Override
        public void close() throws IOException {
            stream.close();
        }
    }
}