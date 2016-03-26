package com.anypoint.df.edi.lexical.formats;

import java.io.IOException;

import com.anypoint.df.edi.lexical.ErrorHandler;
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;
import com.anypoint.df.edi.lexical.LexerBase;
import com.anypoint.df.edi.lexical.LexicalException;
import com.anypoint.df.edi.lexical.TypeFormat;
import com.anypoint.df.edi.lexical.WriterBase;

/**
 * Base class for standard value types. This supplies useful supporting methods for validations and conversions.
 */
public abstract class TypeFormatBase implements TypeFormat
{
    protected static final char[] SPACES = "                   ".toCharArray();
    protected static final char[] ZEROES = "0000000000000000000".toCharArray();
    
    protected final String typeCode;
    protected final int minLength;
    protected final int maxLength;
    
    public TypeFormatBase(String code, int min, int max) {
        if (max < min) {
            throw new IllegalArgumentException("Maximum length cannot be less than minimum length");
        } else if (min < 0) {
            throw new IllegalArgumentException("Negative length is not allowed");
        }
        typeCode = code;
        minLength = min;
        maxLength = max;
    }

    @Override
    public String typeCode() {
        return typeCode;
    }
    
    @Override
    public int minLength() {
        return minLength;
    }
    
    @Override
    public int maxLength() {
        return maxLength;
    }
    
    protected void wrongType(Object obj, ErrorHandler handler) throws LexicalException {
        handler.error(this, ErrorCondition.WRONG_TYPE, "incompatible type for supplied value object: " + obj.getClass().getName());
    }
    
    protected void tooShort(int length, ErrorHandler handler) throws LexicalException {
        handler.error(this, ErrorCondition.TOO_SHORT, "effective length " + length + " is less than " + minLength);
    }
    
    protected void tooLong(int length, ErrorHandler handler) throws LexicalException {
        handler.error(this, ErrorCondition.TOO_LONG, "effective length " + length + " is greater than " + maxLength);
    }
    
    protected void invalidCharacter(char chr, ErrorHandler handler) throws LexicalException {
        handler.error(this, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
    }

    protected void noValuePresent(LexerBase lexer) throws LexicalException {
        lexer.error(this, ErrorCondition.INVALID_FORMAT, "no value present");
    }

    /**
     * Verify current lexer token length. If the token is too long but the error is not fatal the token is truncated to
     * the maximum allowed length.
     *
     * @param lexer 
     * @throws LexicalException
     */
    public void checkInputLength(LexerBase lexer) throws LexicalException {
        StringBuilder builder = lexer.tokenBuilder();
        int length = builder.length();
        if (length < minLength) {
            tooShort(length, lexer);
        }
        if (length > maxLength) {
            tooLong(length, lexer);
            builder.setLength(maxLength);
        }
    }
    
    /**
     * Verify string token length for writing. If the token is too long but the error is not fatal the token is
     * truncated to the maximum allowed length.
     *
     * @param token
     * @param writer 
     * @throws LexicalException
     */
    public String checkOutputLength(String token, WriterBase writer) throws LexicalException {
        int length = token.length();
        if (length < minLength) {
            tooShort(length, writer);
        }
        if (length > maxLength) {
            tooLong(length, writer);
            return token.substring(0, maxLength);
        }
        return token;
    }
    
    /**
     * Verify maximum string token length for writing. If the token is too long but the error is not fatal the token is
     * truncated to the maximum allowed length.
     *
     * @param token
     * @param writer 
     * @throws LexicalException
     */
    public String checkOutputMaxLength(String token, WriterBase writer) throws LexicalException {
        int length = token.length();
        if (length > maxLength) {
            tooLong(length, writer);
            return token.substring(0, maxLength);
        }
        return token;
    }
    
    /**
     * Verify current lexer token consists only of digits. If the token contains not-digits but the error is not fatal
     * the non-digits are deleted from the token.
     *
     * @param lexer 
     * @throws LexicalException
     */
    public void verifyDigits(LexerBase lexer) throws LexicalException {
        StringBuilder builder = lexer.tokenBuilder();
        for (int i = 0; i < builder.length(); i++) {
            char chr = builder.charAt(i);
            if (chr < '0' || chr > '9') {
                invalidCharacter(chr, lexer);
                builder.deleteCharAt(i--);
            }
        }
    }
    
    /**
     * Strip leading spaces from text.
     * 
     * @param builder
     */
    protected static void stripSpaceLeft(StringBuilder builder) {
        int index = 0;
        while (index < builder.length() && builder.charAt(index) == ' ') {
            index++;
        }
        if (index > 0) {
            builder.delete(0, index);
        }
    }
    
    /**
     * Strip trailing spaces from text.
     * 
     * @param builder
     */
    protected static void stripSpaceRight(StringBuilder builder) {
        int index = builder.length() - 1;
        while (index >= 0 && builder.charAt(index) == ' ') {
            index--;
        }
        if (index < builder.length() - 1) {
            builder.delete(index + 1, builder.length());
        }
    }
    
    /**
     * Write padding to output.
     * 
     * @param count
     * @param padding
     * @param writer
     * @throws IOException
     */
    protected static void writePadding(int count, char[] padding, WriterBase writer) throws IOException {
        int remain = count;
        while (remain > 0) {
            int use = Math.min(remain, padding.length);
            writer.writeUnchecked(padding, 0, use);
            remain -= use;
        }
    }
    
    /**
     * Append positive number as two-digit value, using a leading zero if necessary.
     *
     * @param num
     * @param builder
     */
    protected static void appendTwoDigit(int num, StringBuilder builder) {
        if (num < 10) {
            builder.append('0');
        }
        builder.append(num);
    }
    
    /**
     * Append positive number as four-digit value, using leading zeroes if necessary.
     *
     * @param num
     * @param builder
     */
    protected static void appendFourDigit(int num, StringBuilder builder) {
        if (num < 1000) {
            builder.append('0');
            if (num < 100) {
                builder.append('0');
                if (num < 10) {
                    builder.append('0');
                }
            }
        }
        builder.append(num);
    }
}
