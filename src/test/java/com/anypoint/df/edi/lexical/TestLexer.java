
package com.anypoint.df.edi.lexical;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import com.anypoint.df.edi.lexical.EdiConstants.ItemType;

/**
 * Parser for use in tests. This doesn't do any hanling of envelope segments, but allows data segments to be parsed and
 * used directly.
 */
public class TestLexer extends LexerBase
{
    /**
     * Constructor.
     *
     * @param is input
     */
    public TestLexer(InputStream is) {
        super(is, '*', '\\', '^', '~', -1);
    }
    
    /**
     * Initialize document parse.
     * 
     * @param default interchange properties (from partner configuration)
     * @return interchange properties
     * @throws IOException 
     */
    public Map<String,Object> init(Map<String, Object> dflts) throws IOException {
        reader = new BufferedReader(new InputStreamReader(stream, EdiConstants.UTF8_CHARSET));
        advance(ItemType.SEGMENT);
        return new HashMap<>();
    }

    /**
     * @param props
     * @throws IOException
     * @see com.anypoint.df.edi.lexical.LexerBase#term(java.util.Map)
     */
    public Object term(Map<String, Object> props) throws IOException {
        return null;
    }
}