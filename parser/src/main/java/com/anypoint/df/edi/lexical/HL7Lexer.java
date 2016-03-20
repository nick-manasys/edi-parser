package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.EdiConstants.ASCII_CHARSET;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.util.Map;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import com.anypoint.df.edi.lexical.EdiConstants.DataType;
import com.anypoint.df.edi.lexical.EdiConstants.ItemType;
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;

/**
 * Lexer variation for HL7.
 */
public class HL7Lexer extends DelimiterLexer
{
    /** Status returned by {@link EDIFACT#term(Map)} method. */
    public enum InterchangeEndStatus {
        VALID, GROUP_COUNT_ERROR, CONTROL_NUMBER_ERROR
    }
    
    /** Factory for creating XMLGregorianCalendars. */
    private final DatatypeFactory typeFactory;
    
    /**
     * Constructor.
     *
     * @param is input
     * @param subst substitution character for invalid character in string (-1 if unused)
     */
    public HL7Lexer(InputStream is, int subst) {
        super(is);
        substitutionChar = subst;
        segmentTerminator = (char)0x0D;
        try {
            typeFactory = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new RuntimeException(e);
        }
    }
    
    /**
     * Process escape character in input.
     * 
     * @throws IOException
     */
    @Override
    void handleEscape() throws IOException {
        int code = reader.read();
        if (code < 0) {
            throw new LexicalException("end of input in escape sequence");
        }
        StringBuilder builder = new StringBuilder();
        int value;
        while ((value = reader.read()) >= 0 && value != releaseIndicator) {
            builder.append((char)value);
        }
        if (value < 0) {
            throw new LexicalException("malformed escape sequence in input");
        }
        char rls = (char)releaseIndicator;
        if (builder.length() == 0) {
            switch (code) {
                case 'E':
                    peekToken.append(rls);
                    break;
                case 'F':
                    peekToken.append(dataSeparator);
                    break;
                case 'R':
                    peekToken.append((char)repetitionSeparator);
                    break;
                case 'S':
                    peekToken.append(componentSeparator);
                    break;
                case 'T':
                    peekToken.append((char)subCompSeparator);
                    break;
                case 'H':
                case 'N':
                    peekToken.append(rls);
                    peekToken.append((char)code);
                    peekToken.append(rls);
                    break;
                default:
                    throw new LexicalException("unsupported escape sequence code " + (char)code);
            }
        } else {
            throw new LexicalException("unsupported escape sequence code " + (char)code);
        }
    }
    
    /**
     * Get current token as an HL7 sequence ID value and advance to next token.
     *
     * @return
     * @throws IOException
     */
    public Integer parseSeqId() throws IOException {
        for (int i = 0; i < tokenBuilder.length(); i++) {
            char chr = tokenBuilder.charAt(i);
            if (chr < '0' || chr > '9') {
                handleError(DataType.INTEGER, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            }
        }
        checkLength(DataType.INTEGER, 1, 4);
        return Integer.valueOf(tokenBuilder.toString());
    }
    
    /**
     * Initialize document parse. This checks the start of the document to find the separator characters used in
     * parsing. Returns with the lexer positioned at the MSH-3 value.
     *
     * @param props store for property values from interchange
     * @return delimiter characters
     * @throws LexicalException
     */
    public String init(Map<String, Object> props) throws LexicalException {
        try {
            
            // check the segment tag for optional UNA
            byte[] byts = readBytes(3);
            String tag = new String(byts, ASCII_CHARSET);
            if (!"MSH".equals(tag)) {
                throw new RuntimeException("Message does not start with 'MSH'");
            }
            
            // get separator and encoding characters
            byts = readBytes(5);
            dataSeparator = (char)byts[0];
            componentSeparator = (char)byts[1];
            repetitionSeparator = (char)byts[2];
            releaseIndicator = (char)byts[3];
            subCompSeparator = (char)byts[4];
            if (stream.read() != dataSeparator) {
                throw new RuntimeException("Field separator not present following MSH-01");
            }
            
            // initialize reader and set lexer to MSH-3 field
            reader = new ByteReader();
            advance(ItemType.DATA_ELEMENT);
            elementNumber = 2;
            return new String(byts, ASCII_CHARSET);
            
        } catch (IOException e) {
            throw new LexicalException("Message aborted due to error reading header", e);
        }
    }
    
    /**
     * Finish document parse.
     * 
     * @param props
     * @throws IOException
     */
    public void term(Map<String, Object> props) throws IOException {
    }
    
    private int parseComponent(int start, int end) throws IOException {
        int value = 0;
        for (int i = start; i < end; i++) {
            char chr = tokenBuilder.charAt(i);
            if (chr < '0' || chr > '9') {
                handleError(DataType.DATETIME, ErrorCondition.INVALID_CHARACTER, "character '" + chr + "' not allowed");
            }
            value = value * 10 + chr - '0';
        }
        return value;
    }
    
    /**
     * Get current token as a date/time value. The returned XMLGregorianCalendar instance includes all component values
     * from the input, but is generally not a valid XML date/time since XML requires all components to be populated for
     * each format variation while HL7 allows any number of components to be included following the four-digit year.
     *
     * @param minl minimum length
     * @param maxl maximum length
     * @param zone allow time zone
     * @return
     * @throws IOException
     */
    public XMLGregorianCalendar parseDateTime(int minl, int maxl, boolean zone) throws IOException {
        int length = tokenBuilder.length();
        XMLGregorianCalendar dt = typeFactory.newXMLGregorianCalendar();
        if (zone && length > 5) {
            char sign = tokenBuilder.charAt(length - 5);
            if (sign == '-' || sign == '+') {
                int hours = parseComponent(length - 4, length - 2);
                int mins = parseComponent(length - 2, length);
                if (hours > 23) {
                    handleError(DataType.DATETIME, ErrorCondition.INVALID_DATE,
                        "time zone offset hours value out of range: " + hours);
                } else if (mins > 59) {
                    handleError(DataType.DATETIME, ErrorCondition.INVALID_DATE,
                        "time zone offset minutes value out of range: " + mins);
                } else {
                    int offset = hours * 60 + mins;
                    dt.setTimezone(sign == '-' ? -offset : offset);
                }
                length -= 5;
            }
        }
        checkLength(DataType.DATETIME, length, minl, maxl);
        if (length < 4 || (length < 14 && length % 2 == 1) || length > 19) {
            handleError(DataType.DATETIME, ErrorCondition.INVALID_DATE, "length does not match a date/time format: "
                + tokenBuilder.substring(0, length));
        }
        if (length >= 4) {
            dt.setYear(parseComponent(0, 4));
            if (length >= 6) {
                dt.setMonth(parseComponent(4, 6));
                if (length >= 8) {
                    dt.setDay(parseComponent(6, 8));
                    if (length >= 10) {
                        dt.setHour(parseComponent(8, 10));
                        if (length >= 12) {
                            dt.setMinute(parseComponent(10, 12));
                            if (length >= 14) {
                                dt.setSecond(parseComponent(12, 14));
                                if (length >= 16) {
                                    if (tokenBuilder.charAt(14) == '.') {
                                        try {
                                            dt.setFractionalSecond(new BigDecimal(tokenBuilder.substring(14, length)));
                                        } catch (NumberFormatException e) {
                                            handleError(DataType.DATETIME, ErrorCondition.INVALID_DATE,
                                                "invalid second fraction: " + tokenBuilder.substring(0, length));
                                        }
                                    }
                                }
                            }
                            
                        }
                    }
                }
            }
        }
        return dt;
    }
    
    /**
     * Reader that just uses each input byte as a character. This is used for reading the MSH segment, since we don't
     * know the actual character encoding for the message body until it's been read.
     */
    private class ByteReader extends Reader
    {
        @Override
        public int read(char[] cbuf, int off, int len) throws IOException {
            int actual = -1;
            while (actual < len - 1) {
                int chr = stream.read();
                if (chr < 0) {
                    break;
                }
                cbuf[off + ++actual] = (char)chr;
            }
            return actual;
        }
        
        @Override
        public void close() throws IOException {
            stream.close();
        }
    }
}