
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
        InputStream is = new ByteArrayInputStream(text.getBytes(EdiConstants.UTF8_CHARSET));
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
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("B", lexer.parseId(1, 1));
        lexer = initializeLexer("A~   B~");
        assertEquals("A", lexer.token());
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("A", lexer.parseId(1, 1));
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("B", lexer.parseId(1, 1));
        lexer = initializeLexer("A~\n   \nB~");
        assertEquals("A", lexer.token());
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("A", lexer.parseId(1, 1));
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("B", lexer.parseId(1, 1));
    }
    
    @Test
    public void testTokenTypes() throws Exception {
        TestLexer lexer = initializeLexer("ID*ALPHA*1ALPHANUM*12345*123.456*123456*20090604*1205~");
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("ID", lexer.parseId(2, 2));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("ALPHA", lexer.parseAlpha(5, 5));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("1ALPHANUM", lexer.parseAlphaNumeric(9, 9));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(Integer.valueOf(12345), lexer.parseInteger(5, 5));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseNumber(6, 6));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseImpliedDecimalNumber(3, 6, 6));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        Calendar date = lexer.parseDate(8, 8);
        assertEquals(2009, date.get(Calendar.YEAR));
        assertEquals(6, date.get(Calendar.MONTH));
        assertEquals(4, date.get(Calendar.DAY_OF_MONTH));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals((12 * 60 + 5) * 60000, lexer.parseTime(4, 4));
        assertEquals(ItemType.SEGMENT, lexer.currentType());
    }
    
    @Test
    public void testTrimmingPadded() throws Exception {
        TestLexer lexer = initializeLexer("ID *ALPHA   *1ALPHANUM  *00012345*00123.456*000123456~");
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("ID", lexer.parseId(3, 3));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("ALPHA", lexer.parseAlpha(8, 8));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("1ALPHANUM", lexer.parseAlphaNumeric(11, 11));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(Integer.valueOf(12345), lexer.parseInteger(8, 8));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseNumber(8, 8));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseImpliedDecimalNumber(3, 9, 9));
        assertEquals(ItemType.SEGMENT, lexer.currentType());
    }
    
    @Test
    public void testTrimmingGeneral() throws Exception {
        TestLexer lexer = initializeLexer("ID *ALPHA   *1ALPHANUM  *00012345*00123.456*000123456~");
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("ID", lexer.parseId(1, 3));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("ALPHA", lexer.parseAlpha(1, 8));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("1ALPHANUM", lexer.parseAlphaNumeric(1, 11));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(Integer.valueOf(12345), lexer.parseInteger(1, 8));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseNumber(1, 9));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), lexer.parseImpliedDecimalNumber(3, 6, 9));
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
        	lexer.parseNumber(6, 6);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseImpliedDecimalNumber(3, 6, 6);
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
        	lexer.parseNumber(6, 6);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseImpliedDecimalNumber(3, 6, 6);
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
        TestLexer lexer = initializeLexer("I D*AL0PH*1ALPHANU/*12345A*+123.45*12-345*09 604* 205~");
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
        	lexer.parseNumber(6, 6);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
        	lexer.parseImpliedDecimalNumber(3, 6, 6);
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
}