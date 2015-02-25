
package com.anypoint.df.edi.lexical;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.Map;

import static com.anypoint.df.edi.lexical.EdiConstants.*;
import static com.anypoint.df.edi.lexical.X12Constants.*;

/**
 * Parser variation for X12.
 */
public class X12Lexer extends LexerBase
{
    /** Status returned by {@link X12Lexer#term(Map)} method. */
    public enum InterchangeStartStatus { VALID, AUTHORIZATION_QUALIFIER_ERROR, AUTHORIZATION_INFO_ERROR,
        SECURITY_QUALIFIER_ERROR, SECURITY_INFO_ERROR, SENDER_ID_QUALIFIER_ERROR, SENDER_ID_ERROR,
        RECEIVER_ID_QUALIFIER_ERROR, RECEIVER_ID_ERROR, INTERCHANGE_DATE_ERROR, INTERCHANGE_TIME_ERROR,
        VERSION_ID_ERROR, INTER_CONTROL_ERROR, ACK_REQUESTED_ERROR, TEST_INDICATOR_ERROR, NO_DATA }
    
    /** Status returned by {@link X12Lexer#term(Map)} method. */
    public enum InterchangeEndStatus { VALID, GROUP_COUNT_ERROR, CONTROL_NUMBER_ERROR }
    
    /**
     * Constructor.
     * 
     * @param is input
     * @param charset input character encoding
     * @param subst substitution character for invalid character in string (-1 if unused)
     * @param chset character set selection
     */
    public X12Lexer(InputStream is, Charset charset, int subst, CharacterSet chset) {
        super(is, (char)0, (char)0, -1, (char)0, -1, subst, chset.flags());
        reader = new BufferedReader(new InputStreamReader(stream, charset));
    }
    
    /**
     * Replace current status only if current status is {@link InterchangeStartStatus.VALID}. This is used so that if
     * multiple errors are found in the interchange header the first error be preserved and returned.
     *
     * @param status
     * @param replace
     * @return status
     */
    private static InterchangeStartStatus replaceValidStatus(InterchangeStartStatus status, InterchangeStartStatus replace) {
        if (status == InterchangeStartStatus.VALID) {
            return replace;
        }
        return status;
    }
    
    /**
     * Initialize document parse. This checks the start of the document to find the separator characters used in
     * parsing, along with the character encoding. Returns with the parser positioned past the end of the ISA
     * Interchange Control Header segment.
     * 
     * @param props store for property values from interchange
     * @return status
     */
    public InterchangeStartStatus init(Map<String,Object> props) {
        try {
            // make sure data is present
            int value = reader.read();
            while (value == '\n' || value == '\r' || value == ' ') {
                value = reader.read();
            }
            if (value < 0) {
                return InterchangeStartStatus.NO_DATA;
            }
            
            // check the segment tag
            char[] chrs = new char[3];
            chrs[0] = (char)value;
            chrs[1] = (char)reader.read();
            chrs[2] = (char)reader.read();
            String tag = new String(chrs);
            if (!"ISA".equals(tag)) {
                throw new IllegalStateException("Message is missing ISA segment (starts with " + tag + ")");
            }
            
            // get data element separator as next character
            dataSeparator = (char)reader.read();
            
            // set interchange properties from segment data
            advance(ItemType.DATA_ELEMENT);
            InterchangeStartStatus result = InterchangeStartStatus.VALID;
            try {
                props.put(AUTHORIZATION_QUALIFIER, parseId(2, 2));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.AUTHORIZATION_QUALIFIER_ERROR);
                advance();
            }
            try {
                props.put(AUTHORIZATION_INFO, parseAlphaNumeric(10, 10));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.AUTHORIZATION_INFO_ERROR);
                advance();
            }
            try {
                props.put(SECURITY_QUALIFIER, parseId(2, 2));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.SECURITY_QUALIFIER_ERROR);
                advance();
            }
            try {
                props.put(SECURITY_INFO, parseAlphaNumeric(10, 10));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.SECURITY_INFO_ERROR);
                advance();
            }
            try {
                props.put(SENDER_ID_QUALIFIER, parseId(2, 2));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.SENDER_ID_QUALIFIER_ERROR);
                advance();
            }
            try {
                props.put(SENDER_ID, parseAlphaNumeric(15, 15));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.SENDER_ID_ERROR);
                advance();
            }
            try {
                props.put(RECEIVER_ID_QUALIFIER, parseId(2, 2));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.RECEIVER_ID_QUALIFIER_ERROR);
                advance();
            }
            try {
                props.put(RECEIVER_ID, parseAlphaNumeric(15, 15));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.RECEIVER_ID_ERROR);
                advance();
            }
            try {
                props.put(INTERCHANGE_DATE, parseDate(6, 6));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.INTERCHANGE_DATE_ERROR);
                advance();
            }
            try {
                props.put(INTERCHANGE_TIME, Integer.valueOf(parseTime(4, 4)));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.INTERCHANGE_TIME_ERROR);
                advance();
            }
            String sep = token();
            repetitionSeparator = "U".equals(sep) ? -1 : sep.charAt(0);
            advance();
            try {
                props.put(VERSION_ID, parseId(5, 5));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.VERSION_ID_ERROR);
                advance();
            }
            try {
                props.put(INTER_CONTROL, parseInteger(9, 9));
            } catch (LexicalException e) {
                throw new LexicalException("Interchange aborted due to Interchange Control Number error", e);
            }
            try {
                props.put(ACK_REQUESTED, parseId(1, 1));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.ACK_REQUESTED_ERROR);
                advance();
            }
            String indicator = token();
            if (indicator.length() != 1 || !("I".equals(indicator) || "P".equals(indicator) || "T".equals(indicator))) {
                result = replaceValidStatus(result, InterchangeStartStatus.TEST_INDICATOR_ERROR);
            }
            props.put(TEST_INDICATOR, indicator);
            componentSeparator = (char)reader.read();
            segmentTerminator = (char)reader.read();
            
            // advance to start of next segment
            advance(ItemType.SEGMENT);
            return result;
            
        } catch (IOException e) {
            throw new IllegalStateException("Interchange aborted due to error reading header", e);
        }
    }

    /**
     * @param props
     * @throws IOException
     * @see com.anypoint.df.edi.lexical.LexerBase#term(java.util.Map)
     */
    public InterchangeEndStatus term(Map<String, Object> props) throws IOException {
        if (!"IEA".equals(token())) {
            throw new IllegalStateException("not at trailer");
        }
        advance();
        int count = parseInteger(1, 5).intValue();
        if (count != groupCount) {
            return InterchangeEndStatus.GROUP_COUNT_ERROR;
        }
        
        // this may be followed by another ISA segment, so don't advance beyond the segment terminator
        checkInteger(9, 9);
        String text = token();
        int number = Integer.valueOf(text);
        Object expected = props.get(INTER_CONTROL);
        if (!(expected instanceof Integer)) {
            throw new IllegalStateException(INTER_CONTROL + " value must be an Integer");
        }
        if (((Integer)expected).intValue() != number) {
            return InterchangeEndStatus.CONTROL_NUMBER_ERROR;
        }
        return InterchangeEndStatus.VALID;
    }
}