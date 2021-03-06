
package com.mulesoft.flatfile.lexical;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;

import com.mulesoft.flatfile.lexical.EdiConstants.ItemType;

/**
 * Parser for use in tests. This doesn't do any handling of envelope segments, but allows data segments to be parsed and
 * used directly.
 */
public class TestLexer extends DelimiterLexer
{
    /**
     * Constructor.
     *
     * @param is input
     */
    public TestLexer(InputStream is) {
        super(is, -1);
        dataSeparator = '*';
        componentSeparator = '\\';
        repetitionSeparator = '^';
        segmentTerminator = '~';
    }
    
    /**
     * Initialize document parse.
     * 
     * @param data unused
     * @return <code>null</code>
     * @throws IOException 
     */
    public Object init(Map<String,Object> data) {
        try {
            reader = new BufferedReader(new InputStreamReader(stream, EdiConstants.ASCII_CHARSET));
            advance(ItemType.SEGMENT);
            return null;
        } catch (IOException e) {
            throw new RuntimeException("Error", e);
        }
    }

    /**
     * @param props
     * @throws IOException
     * @see com.mulesoft.flatfile.lexical.LexerBase#term(java.util.Map)
     */
    public Object term(Map<String, Object> props) throws IOException {
        return null;
    }

    @Override
    void handleEscape() throws IOException {
    }
}