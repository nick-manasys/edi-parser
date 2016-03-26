package com.anypoint.df.edi.lexical.formats;

import java.io.IOException;

import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;
import com.anypoint.df.edi.lexical.LexerBase;
import com.anypoint.df.edi.lexical.LexicalException;
import com.anypoint.df.edi.lexical.WriterBase;

/**
 * Time value as milliseconds count. This format is specifically for times as digit sequences with no embedded marker
 * characters, as HHMM[SS[d[d[d]]]] (where the trailing d's represent fractional seconds), subject to length
 * restrictions. The implementation avoids the use of the Java DateFormat class, which has high time and memory
 * overhead.
 */
public class MillisecondTimeFormat extends TypeFormatBase {
    
    /** Milliseconds per second. */
    private static final int MILLIS_PER_SECOND = 1000;
    
    /** Milliseconds per minute. */
    private static final int MILLIS_PER_MINUTE = MILLIS_PER_SECOND * 60;
    
    /** Milliseconds per hour. */
    private static final int MILLIS_PER_HOUR = MILLIS_PER_MINUTE * 60;
    
    public MillisecondTimeFormat(String code, int min, int max) {
        super(code, min, max);
        if (min < 4) {
            throw new IllegalArgumentException("Time value representation requires at least 4 characters");
        } else if (max > 9) {
            throw new IllegalArgumentException("Time value representation is a maximum of 9 characters");
        }
    }

    @Override
    public Object parse(LexerBase lexer) throws LexicalException {
        verifyDigits(lexer);
        checkInputLength(lexer);
        StringBuilder builder = lexer.tokenBuilder();
        int length = builder.length();
        if (length != 4 && length < 6) {
            lexer.error(this, ErrorCondition.INVALID_DATE, "time value must be either exactly 4 or 6-9 characters");
        }
        
        // quick (and loose) check for time sanity
        int hour = (builder.charAt(0) - '0') * 10 + (builder.charAt(1) - '0');
        int minute = (builder.charAt(2) - '0') * 10 + (builder.charAt(3) - '0');
        int second = builder.length() < 6 ? 0 : (builder.charAt(4) - '0') * 10 + (builder.charAt(5) - '0');
        if (hour > 23 || minute > 59 || second > 59) {
            lexer.error(this, ErrorCondition.INVALID_TIME, "time value out of allowed ranges");
        }
        int milli = 0;
        if (length > 6) {
            milli += (builder.charAt(6) - '0') * 100;
            if (length > 7) {
                milli += (builder.charAt(7) - '0') * 10;
                if (length > 8) {
                    milli += builder.charAt(8) - '0';
                }
            }
        }
        return Integer.valueOf(((hour * 60 + minute) * 60 + second) * 1000 + milli);
    }

    @Override
    public void write(Object value, WriterBase writer) throws IOException {
        if (value instanceof Integer) {
            
            // start with required component values
            int remain = ((Integer)value).intValue();
            StringBuilder builder = new StringBuilder();
            int hour = remain / MILLIS_PER_HOUR;
            remain = remain % MILLIS_PER_HOUR;
            appendTwoDigit(hour, builder);
            int minute = remain / MILLIS_PER_MINUTE;
            remain = remain % MILLIS_PER_MINUTE;
            appendTwoDigit(minute, builder);
            if (maxLength > 4 && remain > 0) {
                
                // append optional components
                int second = remain / MILLIS_PER_SECOND;
                remain = remain % MILLIS_PER_SECOND;
                appendTwoDigit(second, builder);
                
                // avoid trailing zeroes in decimal seconds
                if (maxLength > 7 && remain > 10) {
                    appendTwoDigit(remain / 10, builder);
                } else if (maxLength == 7 && remain > 100) {
                    builder.append(remain / 100);
                } else if (minLength > 7) {
                    builder.append("00");
                } else if (minLength > 6) {
                    builder.append('0');
                }
                
            }
            writer.startToken();
            while (builder.length() < minLength) {
                builder.append('0');
            }
            writer.writeUnchecked(builder.toString());
            
        } else {
            wrongType(value, writer);
        }
    }
}