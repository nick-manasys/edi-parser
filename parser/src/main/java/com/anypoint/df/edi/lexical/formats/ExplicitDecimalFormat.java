package com.anypoint.df.edi.lexical.formats;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;

import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;
import com.anypoint.df.edi.lexical.LexerBase;
import com.anypoint.df.edi.lexical.LexicalException;
import com.anypoint.df.edi.lexical.TypeFormatConstants.*;
import com.anypoint.df.edi.lexical.WriterBase;

/**
 * Numeric value with optional leading sign, optional decimal point, and optional leading zero or trailing space
 * padding. For input, only digit characters and (depending on configuration) an optional leading sign, optional
 * decimal point, and optional exponent are allowed. For output, leading zeroes or trailing spaces are optionally
 * used to pad the value to the minimum length.
 */
public class ExplicitDecimalFormat extends DecimalFormatBase
{
    /**
     * @param code
     * @param min
     * @param max
     * @param sign sign option
     * @param countsign sign counted in length flag
     * @param pad pad option
     * @param countdec count decimal point in length flag
     * @param zerobefore write zero before decimal point flag
     * @param allowexp allow exponent flag
     * @param countexp count exponent marker 'E' in effective length flag
     */
    public ExplicitDecimalFormat(String code, int min, int max, NumberSign sign, boolean countsign, NumberPad pad,
        boolean countdec, boolean zerobefore, boolean allowexp, boolean countexp) {
        super(code, min, max, sign, countsign, pad, countdec, zerobefore, allowexp, countexp);
    }

    @Override
    public Object parse(LexerBase lexer) throws LexicalException {
        return convertDecimalValue(lexer);
    }
    
    @Override
    public void write(Object value, WriterBase writer) throws IOException {
        writer.startToken();
        writeDecimalValue(value, writer);
    }
}
