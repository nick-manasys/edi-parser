package com.mulesoft.flatfile.lexical.formats;

import java.io.IOException;

import com.mulesoft.flatfile.lexical.LexerBase;
import com.mulesoft.flatfile.lexical.LexicalException;
import com.mulesoft.flatfile.lexical.TypeFormatConstants.*;
import com.mulesoft.flatfile.lexical.WriterBase;

/**
 * Integer value with optional leading sign and optional leading zero or leading or trailing space padding. For input,
 * only digit characters and (depending on configuration) an optional leading sign are allowed. For output, leading
 * zeroes or leading or trailing spaces are optionally used to pad the value to the minimum length. The sign character
 * is configurable to count or not count as part of the effective length of the value. The result value object is
 * approximately sized to match the value, one of <code>Integer</code>, <code>Long</code>, or <code>BigInteger</code>.
 */
public class IntegerFormat extends NumberFormatBase
{
    /**
     * @param code
     * @param min
     * @param max
     * @param sign sign option
     * @param count sign counted in length flag
     * @param fill pad option
     */
    public IntegerFormat(String code, int min, int max, NumberSign sign, boolean count, FillMode fill) {
        super(code, min, max, sign, count, fill);
    }


    @Override
    public Object parse(LexerBase lexer) throws LexicalException {
        int digits = checkIntegerFormat(lexer);
        return convertSizedInteger(lexer, digits);
    }
    
    @Override
    public void write(Object value, WriterBase writer) throws IOException {
        writer.startToken();
        writeIntegerValue(value, null, writer);
    }
}