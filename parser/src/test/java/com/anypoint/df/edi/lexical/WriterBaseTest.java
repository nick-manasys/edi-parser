
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
public class WriterBaseTest extends TypeFormatsBase
{
    @Test
    public void decimalFormatsTest() throws Exception {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        TestWriter writer = new TestWriter(bos);
        
        VALR5.write(new BigDecimal("12.345"), writer);
        writer.writeDataSeparator();
        VALR6.write(new BigDecimal("12.345").movePointLeft(5), writer);
        writer.writeDataSeparator();
        VALR5.write(new BigDecimal(new BigInteger("12345"), 5), writer);
        writer.writeDataSeparator();
        VALR6.write(new BigDecimal(new BigInteger("12345"), 9), writer);
        writer.writeDataSeparator();
        VALR6.write(new BigDecimal(new BigInteger("12345"), -9), writer);
        writer.writeDataSeparator();
        VALR8.write(new BigDecimal("12.345"), writer);
        writer.writeDataSeparator();
        VALR8.write(new BigDecimal("12.3456").movePointLeft(5), writer);
        writer.writeDataSeparator();
        VALR8.write(new BigDecimal(new BigInteger("12345"), 5), writer);
        writer.writeDataSeparator();
        writer.close();
        String text = new String(bos.toByteArray(), EdiConstants.ASCII_CHARSET);
        assertEquals("12.345*12345E-8*.12345*12345E-9*12345E9*00012.345*0123456E-9*000.12345*", text);
        
        // now parse back the values to make sure we match
        InputStream is = new ByteArrayInputStream(text.getBytes(EdiConstants.ASCII_CHARSET));
        TestLexer lexer = new TestLexer(is);
        lexer.init(null);
        assertEquals(new BigDecimal("12.345"), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal("12.345").movePointLeft(5), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal(new BigInteger("12345"), 5), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal(new BigInteger("12345"), 9), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal(new BigInteger("12345"), -9), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal("12.345"), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal("12.3456").movePointLeft(5), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal(new BigInteger("12345"), 5), VALR1_10.parse(lexer));
        lexer.advance();
    }
}