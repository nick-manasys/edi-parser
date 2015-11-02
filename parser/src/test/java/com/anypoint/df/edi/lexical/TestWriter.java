
package com.anypoint.df.edi.lexical;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Map;

/**
 * Writer for use in tests. This doesn't do any handling of envelope segments, but allows data segments to be written
 * directly.
 */
public class TestWriter extends DelimiterWriter
{
    /**
     * Constructor.
     *
     * @param os output
     */
    public TestWriter(OutputStream os) {
        super(os, EdiConstants.ASCII_CHARSET, '*', '-', -1, -1, '~', null, -1, -1, '.', null);
    }

    @Override
    public void init(Map<String, Object> props) throws IOException {
    }

    @Override
    public void term(Map<String, Object> props) throws IOException {
    }
}