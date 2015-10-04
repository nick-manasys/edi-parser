
package com.anypoint.df.edi.lexical;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.Calendar;

import org.junit.Test;

import com.anypoint.df.edi.lexical.EdiConstants.ItemType;

/**
 * Test for basic parser functionality.
 */
public class LexerBaseTest
{
    public TestLexer initializeLexer(String text) throws IOException {
        InputStream is = new ByteArrayInputStream(text.getBytes(EdiConstants.ASCII_CHARSET));
        TestLexer lexer = new TestLexer(is);
        lexer.init(null);
        return lexer;
    }
    
    @Test
    public void testSegmentBoundary() throws Exception {
        TestLexer lexer = initializeLexer("A~B~");
        assertEquals("A", lexer.token());
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("A", lexer.parseId(1, 1));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("B", lexer.parseId(1, 1));
        lexer.advance();
        lexer = initializeLexer("A~   B~");
        assertEquals("A", lexer.token());
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("A", lexer.parseId(1, 1));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("B", lexer.parseId(1, 1));
        lexer.advance();
        lexer = initializeLexer("A~\n   \nB~");
        assertEquals("A", lexer.token());
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("A", lexer.parseId(1, 1));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("B", lexer.parseId(1, 1));
        lexer.advance();
    }
    
    @Test
    public void testTokenTypes() throws Exception {
        TestLexer lexer = initializeLexer("ID*ALPHA*1ALPHANUM*12345*123.456*123.456*1234567890*20090604*1205~");
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("ID", lexer.parseId(2, 2));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("ALPHA", lexer.parseAlpha(5, 5));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("1ALPHANUM", lexer.parseAlphaNumeric(9, 9));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(Integer.valueOf(12345), lexer.parseInteger(5, 5));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseBigDecimal(6, 6));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseUnscaledNumber(6, 6));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new Long("1234567890"), lexer.parseBigDecimal(6, 10));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        Calendar date = lexer.parseDate(8, 8);
        lexer.advance();
        assertEquals(2009, date.get(Calendar.YEAR));
        assertEquals(5, date.get(Calendar.MONTH));
        assertEquals(4, date.get(Calendar.DAY_OF_MONTH));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals((12 * 60 + 5) * 60000, lexer.parseTime(4, 4));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
    }
    
    @Test
    public void testTrimmingPadded() throws Exception {
        TestLexer lexer = initializeLexer("ID *ALPHA   *1ALPHANUM  *00012345*00123.456*000123.456~");
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("ID", lexer.parseId(3, 3));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("ALPHA", lexer.parseAlpha(8, 8));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("1ALPHANUM", lexer.parseAlphaNumeric(11, 11));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(Integer.valueOf(12345), lexer.parseInteger(8, 8));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseBigDecimal(8, 8));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseUnscaledNumber(9, 9));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
    }
    
    @Test
    public void testTrimmingGeneral() throws Exception {
        TestLexer lexer = initializeLexer("ID *ALPHA   *1ALPHANUM  *00012345*00123.456*000123.456~");
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("ID", lexer.parseId(1, 3));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("ALPHA", lexer.parseAlpha(1, 8));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("1ALPHANUM", lexer.parseAlphaNumeric(1, 11));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(Integer.valueOf(12345), lexer.parseInteger(1, 8));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseBigDecimal(1, 9));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseUnscaledNumber(6, 9));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
    }
    
    @Test
    public void testTooShortErrors() throws Exception {
        TestLexer lexer = initializeLexer("ID*ALPH*1ALPHANU*1234*123.45*12345*090604*205~");
        try {
        	lexer.parseId(3, 3);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseAlpha(5, 5);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseAlphaNumeric(9, 9);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseInteger(5, 5);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseBigDecimal(6, 6);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseUnscaledNumber(6, 6);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseDate(8, 8);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseTime(4, 4);
        	fail();
		} catch (LexicalException e) { }
    }
    
    @Test
    public void testTooLongErrors() throws Exception {
        TestLexer lexer = initializeLexer("IDAB*ALPHAA*1ALPHANUNN*123456*123.4567*1234567*2009060412*120506~");
        try {
        	lexer.parseId(3, 3);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseAlpha(5, 5);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseAlphaNumeric(9, 9);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseInteger(5, 5);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseBigDecimal(6, 6);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseUnscaledNumber(6, 6);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseDate(8, 8);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseTime(4, 4);
        	fail();
		} catch (LexicalException e) { }
    }
    
    @Test
    public void testCharacterErrors() throws Exception {
        TestLexer lexer = initializeLexer("I D*AL0PH*12345A*+123.45*12-345*09 604* 205~");
        try {
        	lexer.parseId(3, 3);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseAlpha(5, 5);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseInteger(5, 5);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseBigDecimal(6, 6);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseUnscaledNumber(6, 6);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseDate(8, 8);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseTime(4, 4);
        	fail();
		} catch (LexicalException e) { }
    }
    
    @Test
    public void testExponentialNotation() throws Exception {
        TestLexer lexer = initializeLexer("12.34*.1234E2*1234E-2*12.345*.1234E20*1234E-20~");
        assertEquals(new BigDecimal("12.34"), lexer.parseBigDecimal(4, 4));
        lexer.advance();
        assertEquals(new BigDecimal("12.34"), lexer.parseBigDecimal(5, 5));
        lexer.advance();
        assertEquals(new BigDecimal("12.34"), lexer.parseBigDecimal(5, 5));
        lexer.advance();
        try {
            lexer.parseBigDecimal(4, 4);
            fail();
        } catch (LexicalException e) { }
        lexer.advance();
        try {
            lexer.parseBigDecimal(5, 5);
            fail();
        } catch (LexicalException e) { }
        lexer.advance();
        try {
            lexer.parseBigDecimal(5, 5);
            fail();
        } catch (LexicalException e) { }
    }
}