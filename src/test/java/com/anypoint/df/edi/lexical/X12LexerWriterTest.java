
package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.EdiConstants.UTF8_CHARSET;
import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.Collections;
import java.util.Map;

import org.junit.Test;

/**
 * Test for X12 document handling.
 */
public class X12LexerWriterTest
{
    private static final String ENVELOPE =
        "ISA*00*ABC       *00*DEF       *01*013227180      *ZZ*IJDIECAFOX     *090604*1205*U*00401*000001244*0*P*>~" +
        "IEA*0*000001244~";
    
    @Test
    public void roundTripEnvelope() throws Exception {
        InputStream in = new ByteArrayInputStream(ENVELOPE.getBytes("UTF-8"));
        X12Lexer lexer = new X12Lexer(in);
        Map<String, Object> dflts = Collections.EMPTY_MAP;
        Map<String, Object> props = lexer.init(dflts);
        lexer.term(props);
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        X12Writer writer = new X12Writer(os, UTF8_CHARSET, '*', '>', -1, '~');
        writer.init(props);
        writer.term(props);
        String result = new String(os.toByteArray(), UTF8_CHARSET);
        assertEquals(ENVELOPE, result);
    }
}