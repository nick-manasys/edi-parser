
package com.anypoint.df.edi.lexical;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import static com.anypoint.df.edi.lexical.EdiConstants.*;
import static com.anypoint.df.edi.lexical.X12Constants.*;

/**
 * Parser variation for X12.
 */
public class X12Lexer extends LexerBase
{
    /**
     * Constructor.
     *
     * @param is input
     */
    public X12Lexer(InputStream is) {
        super(is, '*', '\\', '^', '~', -1);
    }
    
    /**
     * Initialize document parse. This checks the start of the document to find the separator characters used in
     * parsing, along with the character encoding. Returns with the parser positioned past the end of the ISA
     * Interchange Control Header segment.
     * 
     * @param default interchange properties (from partner configuration)
     * @return interchange properties
     * @throws IOException 
     */
    public Map<String,Object> init(Map<String, Object> dflts) throws IOException {
        
        // check the segment tag
        byte[] byts = readBytes(3);
        String tag = new String(byts, ASCII_CHARSET);
        if (!"ISA".equals(tag)) {
            throw new IOException(String.format("Message is missing ISA segment (starts with bytes %02X, %02X, %02X)", byts[0], byts[1], byts[2]));
        }
        
        // get record separator as next character
        dataSeparator = (char)readBytes(1)[0];
        
        // turn stream into reader with appropriate character set
        Charset charset = (Charset)dflts.get(CHAR_SET);
        if (charset == null) {
            charset = UTF8_CHARSET;
        }
        reader = new BufferedReader(new InputStreamReader(stream, charset));
        
        // build interchange properties from segment data
        Map<String,Object> props = new HashMap<>();
        props.put(AUTHORIZATION_QUALIFIER, advance());
        props.put(AUTHORIZATION_INFO, advance());
        props.put(SECURITY_QUALIFIER, advance());
        props.put(SECURITY_INFO, advance());
        props.put(SENDER_ID_QUALIFIER, advance());
        props.put(SENDER_ID, advance());
        props.put(RECEIVER_ID_QUALIFIER, advance());
        props.put(RECEIVER_ID, advance());
        advance();
        props.put(INTERCHANGE_DATE, parseDate(6, 6));
        props.put(INTERCHANGE_TIME, Integer.valueOf(parseTime(4, 4)));
        String sep = token();
        repetitionSeparator = "U".equals(sep) ? -1 : sep.charAt(0);
        props.put(VERSION_ID, advance());
        advance();
        props.put(INTER_CONTROL, parseInteger(9, 9));
        props.put(ACK_REQUESTED, token());
        props.put(TEST_INDICATOR, advance());
        subElement = (char)reader.read();
        segmentTerminator = (char)reader.read();
        
        // advance to start of next segment
        nextType = ItemType.SEGMENT;
        advance();
        return props;
    }

    /**
     * @param props
     * @throws IOException
     * @see com.anypoint.df.edi.lexical.LexerBase#term(java.util.Map)
     */
    public void term(Map<String, Object> props) throws IOException {
        if (!"IEA".equals(token())) {
            throw new LexicalException("not at trailer");
        }
        advance();
        int count = parseInteger(1, 5).intValue();
        if (count != groupCount) {
            throw new LexicalException("wrong group count in trailer");
        }
        int number = parseInteger(9, 9).intValue();
        Object expected = props.get(INTER_CONTROL);
        if (!(expected instanceof Integer)) {
            throw new LexicalException(INTER_CONTROL + " value must be an Integer");
        }
        if (((Integer)expected).intValue() != number) {
            throw new LexicalException(INTER_CONTROL + " value does not match");
        }
    }
}