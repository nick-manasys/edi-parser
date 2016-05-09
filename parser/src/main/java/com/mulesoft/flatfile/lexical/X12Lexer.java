
package com.mulesoft.flatfile.lexical;

import static com.mulesoft.flatfile.lexical.X12Constants.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.Map;

import com.mulesoft.flatfile.lexical.EdiConstants.ItemType;
import com.mulesoft.flatfile.lexical.X12Constants.CharacterRestriction;

/**
 * Lexer variation for X12.
 */
public class X12Lexer extends DelimiterLexer
{
    /** Status returned by {@link X12Lexer#init(Map)} method. */
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
    public X12Lexer(InputStream is, Charset charset) {
        super(is, -1);
        reader = new BufferedReader(new InputStreamReader(stream, charset));
    }
    
    /**
     * Configure character processing differences for partners. This is intended for use once the specifics of an
     * interchange are known.
     * 
     * @param subst substitution character for invalid character in string (-1 if unused)
     * @param chset character set selection
     */
    public void configure(int subst, CharacterRestriction chset) {
        substitutionChar = subst;
        allowedChars = chset.flags();
    }
    
    /**
     * Process escape character in input. Not used by X12.
     */
    @Override
    void handleEscape()  {
        throw new RuntimeException("Not used for X12");
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
                props.put(AUTHORIZATION_QUALIFIER, X12Constants.VALID2.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.AUTHORIZATION_QUALIFIER_ERROR);
            }
            advance();
            try {
                props.put(AUTHORIZATION_INFO, X12Constants.VALAN10.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.AUTHORIZATION_INFO_ERROR);
            }
            advance();
            try {
                props.put(SECURITY_QUALIFIER, X12Constants.VALID2.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.SECURITY_QUALIFIER_ERROR);
            }
            advance();
            try {
                props.put(SECURITY_INFO, X12Constants.VALAN10.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.SECURITY_INFO_ERROR);
            }
            advance();
            try {
                props.put(SENDER_ID_QUALIFIER, X12Constants.VALID2.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.SENDER_ID_QUALIFIER_ERROR);
            }
            advance();
            try {
                props.put(SENDER_ID, X12Constants.VALAN15.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.SENDER_ID_ERROR);
            }
            advance();
            try {
                props.put(RECEIVER_ID_QUALIFIER, X12Constants.VALID2.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.RECEIVER_ID_QUALIFIER_ERROR);
            }
            advance();
            try {
                props.put(RECEIVER_ID, X12Constants.VALAN15.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.RECEIVER_ID_ERROR);
            }
            advance();
            try {
                props.put(INTERCHANGE_DATE, X12Constants.VALDT6.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.INTERCHANGE_DATE_ERROR);
            }
            advance();
            try {
                props.put(INTERCHANGE_TIME, Integer.valueOf((Integer)X12Constants.VALTM4.parse(this)));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.INTERCHANGE_TIME_ERROR);
            }
            advance();
            String sep = X12Constants.VALAN1.parse(this).toString();
            repetitionSeparator = "U".equals(sep) ? -1 : sep.charAt(0);
            advance();
            try {
                props.put(VERSION_ID, X12Constants.VALID5.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.VERSION_ID_ERROR);
            }
            advance();
            try {
                props.put(INTER_CONTROL, X12Constants.VALN9.parse(this));
            } catch (LexicalException e) {
                throw new LexicalException("Interchange aborted due to Interchange Control Number error", e);
            }
            advance();
            try {
                props.put(ACK_REQUESTED, X12Constants.VALID1.parse(this));
            } catch (LexicalException e) {
                result = replaceValidStatus(result, InterchangeStartStatus.ACK_REQUESTED_ERROR);
            }
            advance();
            String indicator = X12Constants.VALAN1.parse(this).toString();
            if (indicator.length() != 1 || !("I".equals(indicator) || "P".equals(indicator) || "T".equals(indicator))) {
                result = replaceValidStatus(result, InterchangeStartStatus.TEST_INDICATOR_ERROR);
            }
            props.put(TEST_INDICATOR, indicator);
            componentSeparator = (char)reader.read();
            segmentTerminator = (char)reader.read();
            
            // advance to start of next segment
            groupCount = 0;
            advance(ItemType.SEGMENT);
            return result;
            
        } catch (IOException e) {
            throw new IllegalStateException("Interchange aborted due to error reading header", e);
        }
    }

    /**
     * @param props
     * @throws IOException
     * @see com.mulesoft.flatfile.lexical.LexerBase#term(java.util.Map)
     */
    public InterchangeEndStatus term(Map<String, Object> props) throws IOException {
        if (!"IEA".equals(segmentTag())) {
            throw new IllegalStateException("not at trailer");
        }
        advance();
        int count = ((Integer)X12Constants.VALN1_5.parse(this)).intValue();
        if (count != groupCount) {
            return InterchangeEndStatus.GROUP_COUNT_ERROR;
        }
        advance();
        int number = ((Integer)X12Constants.VALN9.parse(this)).intValue();
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