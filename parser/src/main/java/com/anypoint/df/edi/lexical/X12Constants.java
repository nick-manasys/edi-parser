package com.anypoint.df.edi.lexical;

import java.nio.charset.Charset;

/**
 * Constants for X12 documents.
 */
public final class X12Constants
{
    // standard character sets
    public static final Charset EBCDIC_CHARSET = Charset.forName("IBM1047");
    
    // configuration properties
    public static final String CHAR_SET = "Character set";
    
    // interchange properties from ISA segment
    public static final String AUTHORIZATION_QUALIFIER = "ISA01";
    public static final String AUTHORIZATION_INFO = "ISA02";
    public static final String SECURITY_QUALIFIER = "ISA03";
    public static final String SECURITY_INFO = "ISA04";
    public static final String SENDER_ID_QUALIFIER = "ISA05";
    public static final String SENDER_ID = "ISA06";
    public static final String RECEIVER_ID_QUALIFIER = "ISA07";
    public static final String RECEIVER_ID = "ISA08";
    public static final String INTERCHANGE_DATE = "ISA09";
    public static final String INTERCHANGE_TIME = "ISA10";
    public static final String VERSION_ID = "ISA12";
    public static final String INTER_CONTROL = "ISA13";
    public static final String ACK_REQUESTED = "ISA14";
    public static final String TEST_INDICATOR = "ISA15";
    
    /** X12 basic character set. */
    private static final boolean[] basicCharacterSet;
    
    /** X12 extended character set. */
    private static final boolean[] extendedCharacterSet;
    
    static {
        basicCharacterSet = new boolean[128];
        EdiConstants.fillChars('A', 'Z', basicCharacterSet);
        EdiConstants.fillChars('0', '9', basicCharacterSet);
        EdiConstants.setChars(" !|&'()*+,-./:;?=\"".toCharArray(), basicCharacterSet);
        extendedCharacterSet = new boolean[8320];
        System.arraycopy(basicCharacterSet, 0, extendedCharacterSet, 0, basicCharacterSet.length);
        EdiConstants.fillChars('a', 'z', extendedCharacterSet);
        EdiConstants.setChars("%@[]_{}\\<>~^`#$“ÀÁÂÄàáâä“ÈÉÊèéêëÌÍÎìíîï“ÒÓÔÖòóôöÙÚÛÜ“ùúûüÇçÑñ¿¡".toCharArray(), extendedCharacterSet);
    }
    
    /**
     * String character set alternatives.
     */
    public enum CharacterRestriction
    {
        BASIC(basicCharacterSet),
        EXTENDED(extendedCharacterSet),
        UNRESTRICTED(null);
        
        private final boolean[] characterFlags;
        
        CharacterRestriction(boolean[] flags) {
            characterFlags = flags;
        }
        
        public boolean[] flags() {
            return characterFlags;
        }
    }
    
    /**
     * Types of errors reported to user.
     */
    public enum ErrorType
    {
        INTERCHANGE_NOTE, GROUP_SYNTAX, TRANSACTION_SYNTAX, SEGMENT_SYNTAX, ELEMENT_SYNTAX
    }
}