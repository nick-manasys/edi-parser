package com.anypoint.df.edi.lexical.formats;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;
import com.anypoint.df.edi.lexical.LexerBase;
import com.anypoint.df.edi.lexical.LexicalException;
import com.anypoint.df.edi.lexical.TypeFormatConstants.*;
import com.anypoint.df.edi.lexical.WriterBase;

/**
 * Base class for numeric values, with optional space or right padding, or zero left padding, to minimum length, and
 * with sign options.
 */
public abstract class NumberFormatBase extends TypeFormatBase
{
    protected final NumberSign signType;
    protected final boolean countSign;
    protected final FillMode fillMode;
    
    /**
     * @param code
     * @param min
     * @param max
     * @param sign sign option
     * @param count sign counted in length flag
     * @param pad pad option
     */
    public NumberFormatBase(String code, int min, int max, NumberSign sign, boolean count, FillMode pad) {
        super(code, min, max);
        signType = sign;
        countSign = count;
        fillMode = pad;
    }

    /**
     * Strip padding from current input token, and report an error if the resulting token is empty.
     *
     * @param lexer 
     * @return number of spaces removed
     * @throws IOException
     */
    protected int stripPadding(LexerBase lexer) throws LexicalException {
        StringBuilder builder = lexer.tokenBuilder();
        int start = builder.length();
        switch (fillMode) {
            case RIGHT:
                stripSpaceLeft(builder);
                break;
            case LEFT:
                stripSpaceRight(builder);
                break;
            default:
                break;
        }
        int spaces = start - builder.length();
        if (builder.length() == 0) {
            noValuePresent(lexer);
            builder.append('0');
        }
        return spaces;
    }

    protected void missingRequiredSign(LexerBase lexer) throws LexicalException {
        lexer.error(this, ErrorCondition.INVALID_FORMAT, "missing required sign");
    }

    protected void plusNotAllowed(LexerBase lexer) throws LexicalException {
        lexer.error(this, ErrorCondition.INVALID_CHARACTER, "plus sign (+) not allowed");
        lexer.tokenBuilder().delete(0, 1);
    }

    protected void minusNotAllowed(LexerBase lexer) throws LexicalException {
        lexer.error(this, ErrorCondition.INVALID_CHARACTER, "minus sign (-) not allowed");
        lexer.tokenBuilder().delete(0, 1);
    }
    
    /**
     * Check and validate sign usage, converting to conventional leading-sign form if necessary.
     * 
     * @param lexer
     * @return <code>true</code> if sign present, <code>false</code> if not (or invalid and deleted)
     * @throws LexicalException
     */
    protected boolean signToNormalForm(LexerBase lexer) throws LexicalException {
        StringBuilder builder = lexer.tokenBuilder();
        boolean signed = false;
        int index = signType.trailingSign() ? builder.length() - 1 : 0;
        char chr = builder.charAt(index);
        if (chr == '-') {
            if (signType.useMinus()) {
                signed = true;
            } else {
                minusNotAllowed(lexer);
                builder.deleteCharAt(index);
            }
        } else if (chr == '+') {
            if (signType.acceptPlus()) {
                signed = true;
            } else {
                plusNotAllowed(lexer);
                builder.deleteCharAt(index);
            }
        } else if (signType.forceSign()) {
            missingRequiredSign(lexer);
        }
        if (signed && signType.trailingSign()) {
            builder.deleteCharAt(index);
            builder.insert(0, chr);
        }
        return signed;
    }
    
    /**
     * Validate effective length of number.
     * 
     * @param length
     * @param lexer
     * @throws LexicalException 
     */
    protected void validateLength(int length, LexerBase lexer) throws LexicalException {
        if (length < minLength) {
            tooShort(length, lexer);
        }
        if (length > maxLength) {
            tooLong(length, lexer);
            lexer.tokenBuilder().setLength(maxLength);
        }
    }

    /**
     * Check that the current token is a valid integer number (contains only decimal digits and optional sign) and of
     * valid length. If using a trailing sign the number text is converted to leading sign form.
     *
     * @param lexer 
     * @return number of digits
     * @throws IOException
     */
    protected int checkIntegerFormat(LexerBase lexer) throws LexicalException {
        StringBuilder builder = lexer.tokenBuilder();
        int spaces = stripPadding(lexer);
        boolean signed = signToNormalForm(lexer);
        int index = signed ? 1 : 0;
        int digits = 0;
        boolean number = false;
        for (; index < builder.length(); index++) {
            char chr = builder.charAt(index);
            if (chr >= '0' && chr <= '9') {
                number = true;
                if (digits > 0 || chr != '0') {
                    digits++;
                }
            } else {
                invalidCharacter(chr, lexer);
                builder.deleteCharAt(index--);
            }
        }
        if (!number) {
            noValuePresent(lexer);
            lexer.tokenBuilder().append('0');
        } else {
            int length = builder.length() - ((!countSign && signed) ? 1 : 0) + spaces;
            validateLength(length, lexer);
        }
        return digits;
    }
    
    /**
     * Convert token to integer value appropriate for number of digits.
     * 
     * @param digits
     * @return
     */
    protected Object convertSizedInteger(LexerBase lexer, int digits) {
        String text = lexer.token();
        if (digits < 10) {
            return Integer.valueOf(text);
        } else if (digits < 20) {
            return Long.valueOf(text);
        }
        return new BigInteger(text);
    }

    /**
     * Convert a decimal input value with no exponent or length checks to a value.
     * 
     * @param lexer
     * @return value
     * @throws LexicalException
     */
    protected Object convertPlainDecimal(LexerBase lexer) throws LexicalException {
        StringBuilder builder = lexer.tokenBuilder();
        stripPadding(lexer);
        boolean signed = signToNormalForm(lexer);
        int index = signed ? 1 : 0;
        boolean number = false;
        int digits = 0;
        boolean decimal = false;
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
            } else if (index != 0 || !signed) {
                invalidCharacter(chr, lexer);
                builder.deleteCharAt(index--);
            }
        }
        if (!number) {
            noValuePresent(lexer);
            builder.append('0');
        }
        if (!decimal) {
            lexer.error(this, ErrorCondition.INVALID_FORMAT, "missing required decimal mark");
        }
        char last = builder.charAt(builder.length() - 1);
        if (last == '.' || last == altmark) {
            builder.deleteCharAt(builder.length() - 1);
            return convertSizedInteger(lexer, digits);
        }
        return new BigDecimal(lexer.token());
    }
    
    /**
     * Write a numeric value padded to a minimum length with leading zeroes.
     *
     * @param text
     * @param length effective length of number text (adjusted for any characters excluded by format rules)
     * @param negate negative value flag
     * @param writer
     * @throws IOException 
     */
    protected void writePadded(String text, int length, boolean negate, WriterBase writer) throws IOException {
        int effect = length;
        switch (signType) {
            case ALWAYS_LEFT:
                writer.writeEscaped(negate ? "-" : "+");
                if (countSign) {
                    effect++;
                }
                break;
            case ALWAYS_RIGHT:
                text = text + (negate ? '-' : '+');
                if (countSign) {
                    effect++;
                }
                break;
            case NEGATIVE_ONLY:
            case OPTIONAL:
                if (negate) {
                    writer.writeEscaped("-");
                    if (countSign) {
                        effect++;
                    }
                }
                break;
            case UNSIGNED:
                if (negate) {
                    writer.error(this, ErrorCondition.INVALID_VALUE, "negative value not allowed");
                    negate = false;
                }
                break;
        }
        if (effect > maxLength) {
            tooLong(effect, writer);
        }
        int pad = minLength - effect;
        switch (fillMode) {
            case RIGHT:
                writePadding(pad, SPACES, writer);
                writer.writeEscaped(text);
                break;
            case LEFT:
                writer.writeEscaped(text);
                writePadding(pad, SPACES, writer);
                break;
            case NONE:
                if (effect < minLength) {
                    tooShort(effect, writer);
                }
                writer.writeEscaped(text);
                break;
            case ZEROES:
                writePadding(pad, ZEROES, writer);
                writer.writeEscaped(text);
                break;
        }
    }
    
    /**
     * Write a numeric value padded to a minimum length with leading zeroes. This version of the method uses the actual
     * length of the number text as the effective length.
     *
     * @param text
     * @param negate negative value flag
     * @param writer
     * @throws IOException 
     */
    protected void writePadded(String text, boolean negate, WriterBase writer) throws IOException {
        writePadded(text, text.length(), negate, writer);
    }

    protected void writeBigInteger(BigInteger big, WriterBase writer) throws IOException {
        if (big.signum() < 0) {
            writePadded(big.abs().toString(), true, writer);
        } else {
            writePadded(big.toString(), false, writer);
        }
    }
    
    private static String appendSuffix(String text, String suffix) {
        if (suffix == null) {
            return text;
        }
        return text + suffix;
    }
    
    /**
     * Write an integer value of any supported type, with optional suffix to value.
     * 
     * @param value
     * @param suffix
     * @param writer
     * @throws IOException
     */
    protected void writeIntegerValue(Object value, String suffix, WriterBase writer) throws IOException {
        if (value instanceof Integer) {
            int actual = ((Integer)value).intValue();
            if (actual < 0) {
                writePadded(appendSuffix(Integer.toString(-actual), suffix), true, writer);
            } else {
                writePadded(appendSuffix(Integer.toString(actual), suffix), false, writer);
            }
        } else if (value instanceof Long) {
            long actual = ((Long)value).longValue();
            if (actual < 0) {
                writePadded(appendSuffix(Long.toString(-actual), suffix), true, writer);
            } else {
                writePadded(appendSuffix(Long.toString(actual), suffix), false, writer);
            }
        } else if (value instanceof BigInteger) {
            BigInteger actual = (BigInteger)value;
            if (actual.signum() < 0) {
                writePadded(appendSuffix(actual.abs().toString(), suffix), true, writer);
            } else {
                writePadded(appendSuffix(actual.toString(), suffix), false, writer);
            }
        } else {
            wrongType(value, writer);
        }
    }
    
    /**
     * Write an integer value of any supported type.
     * 
     * @param value
     * @param writer
     * @throws IOException
     */
    protected void writeIntegerValue(Object value, WriterBase writer) throws IOException {
        writeIntegerValue(value, null, writer);
    }
    
    /**
     * Write an decimal value of any supported type without an exponent.
     * 
     * @param value
     * @param writer
     * @throws IOException
     */
    protected void writeDecimalValue(Object value, WriterBase writer) throws IOException {
        if (value instanceof BigDecimal) {
            BigDecimal big = (BigDecimal)value;
            int precision = big.precision();
            int scale = big.scale();
            if (scale <= 0 && (precision - scale) <= maxLength) {
                writeIntegerValue(big.toBigIntegerExact(), ".", writer);
            } else {
                boolean negate = big.signum() < 0;
                if (negate) {
                    big = big.abs();
                }
                if (scale >= 0 && Math.max(precision, scale) <= maxLength) {
                    
                    // write as simple decimal
                    String text = big.toPlainString();
                    if (text.length() > 1 && text.charAt(0) == '0') {
                        text = text.substring(1);
                    }
                    writePadded(text, text.length(), negate, writer);
                    
                } else {
                    
                    // exact representation not possible, round to specified precision
                    int allowed = maxLength - 1 - (negate && countSign ? 1 : 0);
                    if (allowed + scale < 0) {
                        writer.error(this, ErrorCondition.INVALID_FORMAT,
                            "value representation not possible for " + ((BigDecimal)value).toString());
                        writePadded("0", 1, false, writer);
                    } else {
                        writer.error(this, ErrorCondition.DATA_TRUNCATION,
                            "rounding value to fit " + ((BigDecimal)value).toString());
                        MathContext mc = new MathContext(allowed);
                        big.round(mc);
                        String text = big.toPlainString();
                        if (text.length() > 1 && text.charAt(0) == '0') {
                            text = text.substring(1);
                        }
                        writePadded(text, text.length(), negate, writer);
                    }
                }
            }
       } else {
            writeIntegerValue(value, ".", writer);
        }
    }
}