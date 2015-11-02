
package com.anypoint.df.edi.lexical;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;

/**
 * Test for writer base handling of data types.
 */
public class WriterBaseTest
{
    @Test
    public void decimalFormatsTest() throws Exception {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        TestWriter writer = new TestWriter(bos);
        writer.writeDecimal(new BigDecimal("12.345"), 5, 5);
        writer.writeDataSeparator();
        writer.writeDecimal(new BigDecimal("12.345").movePointLeft(5), 6, 6);
        writer.writeDataSeparator();
        writer.writeDecimal(new BigDecimal(new BigInteger("12345"), 5), 5, 5);
        writer.writeDataSeparator();
        writer.writeDecimal(new BigDecimal(new BigInteger("12345"), 9), 6, 6);
        writer.writeDataSeparator();
        writer.writeDecimal(new BigDecimal(new BigInteger("12345"), -9), 6, 6);
        writer.writeDataSeparator();
        writer.writeDecimal(new BigDecimal("12.345"), 8, 8);
        writer.writeDataSeparator();
        writer.writeDecimal(new BigDecimal("12.345").movePointLeft(5), 7, 7);
        writer.writeDataSeparator();
        writer.writeDecimal(new BigDecimal(new BigInteger("12345"), 5), 8, 8);
        writer.writeDataSeparator();
        writer.close();
        String text = new String(bos.toByteArray(), EdiConstants.ASCII_CHARSET);
        assertEquals("12.345*12345E-8*.12345*12345E-9*12345E9*00012.345*012345E-8*000.12345*", text);
        
        // now parse back the values to make sure we match
        InputStream is = new ByteArrayInputStream(text.getBytes(EdiConstants.ASCII_CHARSET));
        TestLexer lexer = new TestLexer(is);
        lexer.init(null);
        assertEquals(new BigDecimal("12.345"), lexer.parseBigDecimal(5, 5));
        lexer.advance();
        assertEquals(new BigDecimal("12.345").movePointLeft(5), lexer.parseBigDecimal(6, 6));
        lexer.advance();
        assertEquals(new BigDecimal(new BigInteger("12345"), 5), lexer.parseBigDecimal(5, 5));
        lexer.advance();
        assertEquals(new BigDecimal(new BigInteger("12345"), 9), lexer.parseBigDecimal(6, 6));
        lexer.advance();
        assertEquals(new BigDecimal(new BigInteger("12345"), -9), lexer.parseBigDecimal(6, 6));
        lexer.advance();
        assertEquals(new BigDecimal("12.345"), lexer.parseBigDecimal(8, 8));
        lexer.advance();
        assertEquals(new BigDecimal("12.345").movePointLeft(5), lexer.parseBigDecimal(7, 7));
        lexer.advance();
        assertEquals(new BigDecimal(new BigInteger("12345"), 5), lexer.parseBigDecimal(8, 8));
        lexer.advance();
    }
}