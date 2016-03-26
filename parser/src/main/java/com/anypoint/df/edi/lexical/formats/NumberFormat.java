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
public class NumberFormat extends NumberFormatBase
{
    private final boolean countDecimal;
    private final boolean zeroBeforeDecimal;
    private final boolean allowExponent;
    private final boolean countExponent;
    
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
    public NumberFormat(String code, int min, int max, NumberSign sign, boolean countsign, NumberPad pad,
        boolean countdec, boolean zerobefore, boolean allowexp, boolean countexp) {
        super(code, min, max, sign, countsign, pad);
        countDecimal = countdec;
        zeroBeforeDecimal = zerobefore;
        allowExponent = allowexp;
        countExponent = countexp;
    }

    @Override
    public Object parse(LexerBase lexer) throws LexicalException {
        StringBuilder builder = lexer.tokenBuilder();
        int spaces = stripPadding(lexer);
        boolean signed = signToNormalForm(lexer);
        int index = signed ? 1 : 0;
        int digits = 0;
        boolean number = false;
        boolean decimal = false;
        int exponent = -1;
        int altmark = lexer.getAltDecimalMark();
        for (; index < builder.length(); index++) {
            char chr = builder.charAt(index);
            if (chr >= '0' && chr <= '9') {
                number = true;
                if (digits > 0 || chr != '0') {
                    digits++;
                }
            } else if (!decimal && (chr == '.' || chr == altmark)) {
                decimal = true;
                if (chr == altmark) {
                    builder.setCharAt(index, '.');
                }
            } else if (allowExponent && exponent < 0 && (chr == 'E' || chr == 'e')) {
                exponent = index + 1;
                if (digits == 0) {
                    lexer.error(this, ErrorCondition.INVALID_FORMAT, "value required before exponent");
                    builder.setLength(0);
                    builder.append('0');
                }
            } else if ((index != 0 && index != exponent) || chr != '-') {
                lexer.error(this, ErrorCondition.INVALID_CHARACTER, "character '" + chr +
                    "' not allowed or wrong placement");
                invalidCharacter(chr, lexer);
                builder.deleteCharAt(index--);
            }
        }
        if (!number) {
            noValuePresent(lexer);
        } else {
            int adj = spaces + (!countDecimal && decimal ? 1 : 0) + ((!countSign && signed) ? 1 : 0);
            if (exponent > 0) {
                if (!countExponent) {
                    adj++;
                }
                if (!countDecimal && builder.length() >= exponent && builder.charAt(exponent) == '-') {
                    adj++;
                }
            }
            int effective = builder.length() - adj;
            validateLength(effective, lexer);
        }
        if (decimal || exponent >= 0) {
            return new BigDecimal(lexer.token());
        }
        return convertSizedInteger(lexer, digits);
    }
    
    private void writeDecimal(String text, boolean negate, WriterBase writer) throws IOException {
        if (!zeroBeforeDecimal && text.startsWith("0")) {
            text = text.substring(1);
        }
        int length = text.length();
        if (!countDecimal) {
            length--;
        }
        writePadded(text, length, negate, writer);
    }
    
    @Override
    public void write(Object value, WriterBase writer) throws IOException {
        writer.startToken();
        if (value instanceof BigDecimal) {
            BigDecimal big = (BigDecimal)value;
            int precision = big.precision();
            int scale = big.scale();
            if (scale <= 0 && (precision - scale) <= maxLength) {
                writeBigInteger(big.toBigIntegerExact(), writer);
            } else {
                boolean negate = big.signum() < 0;
                if (negate) {
                    big = big.abs();
                }
                if (scale >= 0 && Math.max(precision, scale) <= maxLength) {
                    
                    // write as simple decimal
                    writeDecimal(big.toPlainString(), negate, writer);
                    
                } else if (allowExponent) {
                    
                    // convert using implied decimal (at end) and exponent
                    BigDecimal adjusted = big.movePointRight(scale);
                    String text = adjusted.toBigIntegerExact().toString() + "E" + Integer.toString(-scale);
                    if (!zeroBeforeDecimal && text.startsWith("0")) {
                        text = text.substring(1);
                    }
                    int length = text.length() - (countExponent ? 0 : 1) - (scale > 0 && !countSign ? 1 : 0);
                    writePadded(text, length, negate, writer);
                    
                } else {
                    
                    // exact representation not possible, round to specified precision
                    int allowed = maxLength - (countDecimal ? 1 : 0) - (negate && countSign ? 1 : 0);
                    if (allowed + scale < 0) {
                        writer.error(this, ErrorCondition.INVALID_FORMAT,
                            "value representation not possible for " + ((BigDecimal)value).toString());
                        writePadded("0", 1, false, writer);
                    } else {
                        MathContext mc = new MathContext(allowed);
                        big.round(mc);
                        writeDecimal(big.toPlainString(), negate, writer);
                    }
                }
            }
       } else {
            writeIntegerValue(value, writer);
        }
    }
}
