
package com.anypoint.df.edi.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

/**
 * Parser variation for EDIFACT.
 */
public class EdiFactParser extends ParserBase
{
    public static final String SYNTAX_IDENTIFIER = "Syntax identifier";
    public static final String SYNTAX_VERSION_NUMBER = "Syntax version number";
    public static final String SENDER_IDENTIFICATION = "Sender identification";
    public static final String SENDER_IDENTIFICATION_CODE_QUALIFIER = "Sender partner identification code qualifier";
    public static final String REVERSE_ROUTING_ADDRESS = "Address for reverse routing";
    public static final String RECIPIENT_IDENTIFICATION = "Recipient identification";
    public static final String RECIPIENT_IDENTIFICATION_CODE_QUALIFIER =
        "Recipient partner identification code qualifier";
    public static final String RECIPIENT_ROUTING_ADDRESS = "Onward routing address";
    public static final String PREPARATION_DATE = "Date of preparation";
    public static final String PREPARATION_TIME = "Time of preparation";
    public static final String INTERCHANGE_CONTROL_REFERENCE = "Interchange control reference";
    public static final String RECIPIENT_REFERENCE = "Recipient's reference/password";
    public static final String RECIPIENT_REFERENCE_QUALIFIER = "Recipient's reference/password qualifier";
    public static final String APPLICATION_REFERENCE = "Application reference";
    public static final String PROCESSING_PRIORITY_CODE = "Processing priority code";
    public static final String ACKNOWLEDGEMENT_REQUEST = "Acknowledgement request";
    public static final String COMMUNICATIONS_AGREEMENT_ID = "Communications agreement id";
    public static final String TEST_INDICATOR = "Test indicator";
    
    private static final Map<String,Charset> EDIFACT_CHARSETS;
    static {
        EDIFACT_CHARSETS = new HashMap<>();
        // should really be restricted, with lowercase alphas not allowed
        EDIFACT_CHARSETS.put("UNOA", ASCII_CHARSET);
        EDIFACT_CHARSETS.put("UNOB", ASCII_CHARSET);
        EDIFACT_CHARSETS.put("UNOC", charsetOrNull("ISO8859_1"));
        EDIFACT_CHARSETS.put("UNOD", charsetOrNull("ISO8859_2"));
        EDIFACT_CHARSETS.put("UNOE", charsetOrNull("ISO8859_5"));
        EDIFACT_CHARSETS.put("UNOF", charsetOrNull("ISO8859_7"));
        EDIFACT_CHARSETS.put("UNOG", charsetOrNull("ISO8859_3"));
        EDIFACT_CHARSETS.put("UNOH", charsetOrNull("ISO8859_4"));
        EDIFACT_CHARSETS.put("UNOI", charsetOrNull("ISO8859_6"));
        EDIFACT_CHARSETS.put("UNOJ", charsetOrNull("ISO8859_8"));
        EDIFACT_CHARSETS.put("UNOK", charsetOrNull("ISO8859_9"));
        // should be "ISO 2022 utilising the escape techniques in accordance with ISO 2375"
        //EDIFACT_CHARSETS.put("UNOX", Charset.forName("XXX"));
        // should be "ISO 10646-1 octet without code extension technique"
        //EDIFACT_CHARSETS.put("UNOY", Charset.forName("XXX"));
        EDIFACT_CHARSETS.put("KECA", Charset.forName("MS949"));
        
    }
    
    private static Charset charsetOrNull(String name) {
        try {
            return Charset.forName("ISO8859_1");
        } catch (Exception e) {
            return null;
        }
    }
    
    /**
     * Constructor.
     *
     * @param is input
     */
    public EdiFactParser(InputStream is) {
        super(is, '+', ':', '\'', '?');
//        super(is, 0x1D, 0x1F, 0x1C, -1);
    }
    
    /**
     * Initialize document parse. This checks the start of the document to find the separator characters used in
     * parsing, along with the character encoding. Returns with the parser positioned past the end of the UNB
     * Interchange Header segment.
     *
     * @return interchange properties
     * @throws IOException 
     */
    public Map<String,Object> init() throws IOException {
        
        // check the segment tag
        byte[] byts = readBytes(3);
        String tag = new String(byts, ASCII_CHARSET);
        if ("UNA".equals(tag)) {
            
            // get delimiter and other characters directly from segment
            byts = readBytes(6);
            subElement = (char)byts[0];
            dataSeparator = (char)byts[1];
            releaseIndicator = (char)byts[3];
            segmentTerminator = (char)byts[5];
            
            // skip any whitespace following segment
            char chr;
            while ((chr = (char)stream.read()) == '\n' || chr == '\r' || chr == ' ');
            
            // get the next segment tag
            byts = new byte[3];
            byts[0] = (byte)chr;
            readArray(byts, 1);
            tag = new String(byts, ASCII_CHARSET);
        }
        
        // must be at a UNB Interchange Header
        if (!"UNB".equals(tag)) {
            throw new IOException("Message is missing UNB segment");
        }
        
        // get syntax identifier value
        if (stream.read() != dataSeparator) {
            throw new IOException("UNB segment separator error");
        }
        String synid = new String(readBytes(4), ASCII_CHARSET);
        Charset charset = EDIFACT_CHARSETS.get(synid);
        if (charset == null) {
            throw new IOException("Unsupported syntax identifier: " + synid);
        }
        
        // turn stream into reader with appropriate character set
        reader = new BufferedReader(new InputStreamReader(stream, charset));
        nextType = ItemType.DATA_ELEMENT;
        nextItem();
        
        // build interchange properties from segment data
        Map<String,Object> props = new HashMap<>();
        props.put(SYNTAX_IDENTIFIER, synid);
        props.put(SYNTAX_VERSION_NUMBER, requireNextItem(ItemType.QUALIFIER));
        props.put(SENDER_IDENTIFICATION, requireNextItem(ItemType.DATA_ELEMENT));
        if (ItemType.QUALIFIER == nextType()) {
            props.put(SENDER_IDENTIFICATION_CODE_QUALIFIER, nextItem());
            if (ItemType.QUALIFIER == nextType()) {
                props.put(REVERSE_ROUTING_ADDRESS, nextItem());
            }
        }
        props.put(RECIPIENT_IDENTIFICATION, requireNextItem(ItemType.DATA_ELEMENT));
        if (ItemType.QUALIFIER == nextType()) {
            props.put(RECIPIENT_IDENTIFICATION_CODE_QUALIFIER, nextItem());
            if (ItemType.QUALIFIER == nextType()) {
                props.put(RECIPIENT_ROUTING_ADDRESS, nextItem());
            }
        }
        props.put(PREPARATION_DATE, requireNextItem(ItemType.DATA_ELEMENT));
        props.put(PREPARATION_TIME, requireNextItem(ItemType.QUALIFIER));
        props.put(INTERCHANGE_CONTROL_REFERENCE, requireNextItem(ItemType.DATA_ELEMENT));
        if (ItemType.SEGMENT == nextType()) {
            return props;
        }
        props.put(RECIPIENT_REFERENCE, nextItem());
        if (ItemType.QUALIFIER == nextType()) {
            props.put(RECIPIENT_REFERENCE_QUALIFIER, nextItem());
        }
        if (ItemType.SEGMENT == nextType()) {
            return props;
        }
        props.put(APPLICATION_REFERENCE, nextItem());
        if (ItemType.SEGMENT == nextType()) {
            return props;
        }
        props.put(PROCESSING_PRIORITY_CODE, nextItem());
        if (ItemType.SEGMENT == nextType()) {
            return props;
        }
        props.put(ACKNOWLEDGEMENT_REQUEST, nextItem());
        if (ItemType.SEGMENT == nextType()) {
            return props;
        }
        props.put(COMMUNICATIONS_AGREEMENT_ID, nextItem());
        if (ItemType.SEGMENT != nextType()) {
            props.put(TEST_INDICATOR, nextItem());
        }
        return props;
    }
}