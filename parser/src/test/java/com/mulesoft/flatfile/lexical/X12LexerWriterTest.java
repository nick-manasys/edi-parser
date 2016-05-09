
package com.mulesoft.flatfile.lexical;

import static com.mulesoft.flatfile.lexical.EdiConstants.ASCII_CHARSET;
import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.mulesoft.flatfile.lexical.X12Constants.CharacterRestriction;

/**
 * Test for X12 document handling.
 */
public class X12LexerWriterTest
{
    private static final String DATETIME = "090604*1205";
    private static final String ENVELOPE =
        "ISA*00*ABC       *00*DEF       *01*013227180      *ZZ*IJDIECAFOX     *" + DATETIME +
        "*U*00401*000001244*0*P*>~IEA*0*000001244~";
    
    @Test
    public void roundTripEnvelope() throws Exception {
        InputStream in = new ByteArrayInputStream(ENVELOPE.getBytes("UTF-8"));
        X12Lexer lexer = new X12Lexer(in, ASCII_CHARSET);
        lexer.configure(-1, CharacterRestriction.EXTENDED);
        Map<String, Object> props = new HashMap<>();
        lexer.init(props);
        lexer.term(props);
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        X12Writer writer = new X12Writer(os, ASCII_CHARSET, '*', '>', -1, '~', null, -1, CharacterRestriction.BASIC);
        writer.init(props);
        writer.term(props);
        writer.close();
        String result = new String(os.toByteArray(), ASCII_CHARSET);
        int datestart = ENVELOPE.indexOf(DATETIME);
        int dateend = datestart + DATETIME.length();
        assertEquals(ENVELOPE.substring(0, datestart), result.substring(0, datestart));
        assertEquals(ENVELOPE.substring(dateend), result.substring(dateend));
    }
}