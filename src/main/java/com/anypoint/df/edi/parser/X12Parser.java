
package com.anypoint.df.edi.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import static com.anypoint.df.edi.parser.X12Constants.*;

/**
 * Parser variation for X12.
 */
public class X12Parser extends ParserBase
{
    /**
     * Constructor.
     *
     * @param is input
     */
    public X12Parser(InputStream is) {
        super(is, '*', '\\', '^', '~', -1);
    }
    
    /**
     * Initialize document parse. This checks the start of the document to find the separator characters used in
     * parsing, along with the character encoding. Returns with the parser positioned past the end of the ISA
     * Interchange Control Header segment.
     * @param default interchange properties (from partner configuration)
     * @return interchange properties
     * @throws IOException 
     */
    public Map<String,Object> init(Map<String, Object> dflts) throws IOException {
        
        // check the segment tag
        byte[] byts = readBytes(3);
        String tag = new String(byts, ASCII_CHARSET);
        if (!"ISA".equals(tag)) {
            throw new IOException("Message is missing ISA segment");
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
        props.put(INTERCHANGE_DATE, advance());
        props.put(INTERCHANGE_TIME, advance());
        String sep = advance();
        repetitionSeparator = "U".equals(sep) ? -1 : sep.charAt(0);
        props.put(VERSION_ID, advance());
        props.put(INTER_CONTROL, advance());
        props.put(ACK_REQUESTED, advance());
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
     * @see com.anypoint.df.edi.parser.ParserBase#term(java.util.Map)
     */
    public void term(Map<String, Object> props) throws IOException {
        // TODO Auto-generated method stub
        
    }
}