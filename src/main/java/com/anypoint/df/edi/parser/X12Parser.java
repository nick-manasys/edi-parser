
package com.anypoint.df.edi.parser;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

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
     * parsing, along with the character encoding. Returns with the parser positioned past the end of the UNB
     * Interchange Header segment.
     *
     * @return interchange properties
     * @throws IOException 
     */
    public Map<String,Object> init() throws IOException {
        Map<String,Object> props = new HashMap<>();
        return props;
    }
}