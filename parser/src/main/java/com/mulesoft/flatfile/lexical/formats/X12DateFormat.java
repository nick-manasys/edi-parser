package com.mulesoft.flatfile.lexical.formats;

import static com.mulesoft.flatfile.lexical.EdiConstants.maximumYear;

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import org.threeten.bp.Instant;
import org.threeten.bp.LocalDate;
import org.threeten.bp.ZoneId;
import org.threeten.bp.ZonedDateTime;

import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition;
import com.mulesoft.flatfile.lexical.LexerBase;
import com.mulesoft.flatfile.lexical.LexicalException;
import com.mulesoft.flatfile.lexical.TypeFormatConstants.GenericType;
import com.mulesoft.flatfile.lexical.WriteException;
import com.mulesoft.flatfile.lexical.WriterBase;

/**
 * Date value in X12 style. This format is either YYMMDD (6 character width) or CCYYMMDD (8 character width).
 */
public class X12DateFormat extends TypeFormatBase {
    
    public X12DateFormat(String code, int min, int max) {
        super(code, min, max);
    }
    
    @Override
    public GenericType genericType() {
        return GenericType.DATE;
    }

    @Override
    public Object parse(LexerBase lexer) throws LexicalException {
        StringBuilder builder = lexer.tokenBuilder();
        int length = builder.length();
        if (length != 6 && length != 8) {
            lexer.error(this, ErrorCondition.INVALID_DATE, "date value must be either 6 or 8 characters");
        }
        checkInputLength(lexer);
        verifyDigits(lexer);
        
        // quick (and loose) check for date sanity
        int day = (builder.charAt(length - 1) - '0') + (builder.charAt(length - 2) - '0') * 10;
        int month = (builder.charAt(length - 3) - '0') + (builder.charAt(length - 4) - '0') * 10;
        if (month == 0 || month > 12 || day == 0 || day > 31) {
            lexer.error(this, ErrorCondition.INVALID_DATE, "month or day out of allowed range");
        }
        int year;
        if (length == 8) {
            year = Integer.parseInt(builder.substring(0, 4));
        } else {
            year = 2000 + (builder.charAt(1) - '0') + (builder.charAt(0) - '0') * 10;
            if (year > maximumYear) {
                year -= 100;
            }
        }
        return new GregorianCalendar(year, month - 1, day);
    }

    /**
     * Write date value. Note that this avoids the use of the Java DateFormat class, which has high time and memory
     * overhead.
     *
     * @param calendar
     * @param writer
     * @throws IOException
     */
    public void writeDate(Calendar calendar, WriterBase writer) throws IOException {
        
        // split into components
        int year = calendar.get(Calendar.YEAR);
        int month = calendar.get(Calendar.MONTH) + 1;
        int day = calendar.get(Calendar.DAY_OF_MONTH);
        
        // use year format determined by length
        StringBuilder builder = new StringBuilder();
        builder.append(year);
        if (maxLength == 6) {
            if (year > maximumYear || year <= maximumYear - 100) {
                throw new WriteException("year out of range for short form date");
            }
            builder.delete(0, 2);
        } else if (builder.length() > 4) {
            throw new WriteException("year outside of allowed range");
        } else {
            while (builder.length() < 4) {
                builder.insert(0, '0');
            }
        }
        
        // force month and day to use two digits each
        appendTwoDigit(month, builder);
        appendTwoDigit(day, builder);
        writer.startToken();
        writer.writeUnchecked(builder.toString());
    }

    @Override
    public void write(Object value, WriterBase writer) throws IOException {
        if (value instanceof Calendar) {
            writeDate((Calendar)value, writer);
        } else if (value instanceof Date) {
            GregorianCalendar calendar = new GregorianCalendar();
            calendar.setTime((Date)value);
            writeDate(calendar, writer);
        } else if (value instanceof Instant) {
            GregorianCalendar calendar = new GregorianCalendar();
            calendar.setTimeInMillis(((Instant)value).toEpochMilli());
            writeDate(calendar, writer);
        } else if (value instanceof LocalDate) {
            LocalDate localDate = (LocalDate)value;
            GregorianCalendar calendar = new GregorianCalendar();
            calendar.setTimeInMillis(localDate.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
            writeDate(calendar, writer);
        } else if (value instanceof ZonedDateTime) {
            ZonedDateTime zoned = (ZonedDateTime)value;
            GregorianCalendar calendar = new GregorianCalendar();
            calendar.setTimeInMillis(zoned.toInstant().toEpochMilli());
            writeDate(calendar, writer);
        } else {
            wrongType(value, writer);
        }
    }
}