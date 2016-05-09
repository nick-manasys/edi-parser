
package com.mulesoft.flatfile.lexical;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.Calendar;

import org.junit.Test;

import com.mulesoft.flatfile.lexical.EdiConstants.ItemType;

/**
 * Test for basic parser functionality.
 */
public class LexerBaseTest extends TypeFormatsBase
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
        assertEquals("A", VALa1.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("B", VALa1.parse(lexer));
        lexer.advance();
        lexer = initializeLexer("A~   B~");
        assertEquals("A", lexer.token());
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("A", VALa1.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("B", VALa1.parse(lexer));
        lexer.advance();
        lexer = initializeLexer("A~\n   \nB~");
        assertEquals("A", lexer.token());
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("A", VALa1.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("B", VALa1.parse(lexer));
        lexer.advance();
    }
    
    @Test
    public void testTokenTypes() throws Exception {
        TestLexer lexer = initializeLexer("ID*ALPHA*1ALPHANUMX*12345*123.456*123.456*12345671090*20090604*1205~");
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("ID", VALa1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("ALPHA", VALa1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("1ALPHANUMX", VALAN10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(Integer.valueOf(12345), VALN1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), VALNM1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new Long("12345671090"), VALNM1_12.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        Calendar date = (Calendar)VALDT8.parse(lexer);
        lexer.advance();
        assertEquals(2009, date.get(Calendar.YEAR));
        assertEquals(5, date.get(Calendar.MONTH));
        assertEquals(4, date.get(Calendar.DAY_OF_MONTH));
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals((12 * 60 + 5) * 60000, VALTM4.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
    }
    
    @Test
    public void testTrimmingPadded() throws Exception {
        TestLexer lexer = initializeLexer("ID *ALPHA   *1ALPHANUM *00012345*00123.456*000123.456~");
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("ID", VALAN3.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("ALPHA", VALAN8.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("1ALPHANUM", VALAN10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(Integer.valueOf(12345), VALN1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), VALNM1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), VALNM1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
    }
    
    @Test
    public void testTrimmingGeneral() throws Exception {
        TestLexer lexer = initializeLexer("ID *ALPHA   *1ALPHANUM *00012345*00123.456*000123.456~");
        assertEquals(ItemType.SEGMENT, lexer.currentType());
        assertEquals("ID", VALa1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("ALPHA", VALa1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals("1ALPHANUM", VALAN1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(Integer.valueOf(12345), VALN1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.DATA_ELEMENT, lexer.currentType());
        assertEquals(new BigDecimal("123.456"), VALR1_10.parse(lexer));
        lexer.advance();
        assertEquals(ItemType.SEGMENT, lexer.currentType());
    }
    
    @Test
    public void testTooShortErrors() throws Exception {
        TestLexer lexer = initializeLexer("ID*ALPH*1ALPHANU*1234*123.45*12345*090604*205~");
        try {
            VALa10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALAN10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALAN10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALN10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALNM10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALNM10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALDT8.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALTM4.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
    }
    
    @Test
    public void testTooLongErrors() throws Exception {
        TestLexer lexer = initializeLexer("IDAB*ALPHAA*1ALPHANUNNN*123456*123.4567*1234567*2009060412*120506~");
        try {
            VALa1.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALa1.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALAN10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALN5.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALNM5.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALNM5.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALDT8.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALTM4.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
    }
    
    @Test
    public void testCharacterErrors() throws Exception {
        TestLexer lexer = initializeLexer("I.D*AL0PH*12345A*+123.45*12-345*09 604* 205~");
        try {
            VALa1_10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALa1_10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALN10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALNM10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALNM10.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALDT8.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
        lexer.advance();
        try {
            VALTM4.parse(lexer);
        	fail();
		} catch (LexicalException e) { }
    }
    
    @Test
    public void testExponentialNotation() throws Exception {
        TestLexer lexer = initializeLexer("12.340*.1234E2*1234E-2*12.345*.1234E20*1234E-20~");
        assertEquals(new BigDecimal("12.340"), VALR5.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal("12.34"), VALR5.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal("12.34"), VALR5.parse(lexer));
        lexer.advance();
        assertEquals(new BigDecimal("12.345"), VALR5.parse(lexer));
        lexer.advance();
        try {
            VALR5.parse(lexer);
            fail();
        } catch (LexicalException e) { }
        lexer.advance();
        try {
            VALR5.parse(lexer);
            fail();
        } catch (LexicalException e) { }
    }
}