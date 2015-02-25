package com.anypoint.df.edi.lexical;

import java.nio.charset.Charset;

/**
 * Constants for X12 documents.
 */
public final class X12Constants
{
    private X12Constants() {
    }
    
    // standard character sets
    public static final Charset EBCDIC_CHARSET = Charset.forName("IBM1047");
    
    // configuration properties
    public static final String CHAR_SET = "Character set";
    
    // interchange properties from ISA segment
    public static final String AUTHORIZATION_QUALIFIER = "Authorization qualifier";
    public static final String AUTHORIZATION_INFO = "Authorization info";
    public static final String SECURITY_QUALIFIER = "Security qualifier";
    public static final String SECURITY_INFO = "Security info";
    public static final String SENDER_ID_QUALIFIER = "Sender identification qualifier";
    public static final String SENDER_ID = "Sender identification";
    public static final String RECEIVER_ID_QUALIFIER = "Receiver identification qualifier";
    public static final String RECEIVER_ID = "Receiver identification";
    public static final String INTERCHANGE_DATE = "Interchange date";
    public static final String INTERCHANGE_TIME = "Interchange time";
    public static final String VERSION_ID = "Version identification";
    public static final String INTER_CONTROL = "Interchange control";
    public static final String ACK_REQUESTED = "Ack requested";
    public static final String TEST_INDICATOR = "Test indicator";
    
    /** X12 basic character set. */
    private static final boolean[] basicCharacterSet;
    
    /** X12 extended character set. */
    private static final boolean[] extendedCharacterSet;
    
    static {
        basicCharacterSet = new boolean[128];
        fillChars('A', 'Z', basicCharacterSet);
        fillChars('0', '9', basicCharacterSet);
        setChars(" !|&'()*+,-./:;?=\"".toCharArray(), basicCharacterSet);
        extendedCharacterSet = new boolean[8320];
        System.arraycopy(basicCharacterSet, 0, extendedCharacterSet, 0, basicCharacterSet.length);
        fillChars('a', 'z', extendedCharacterSet);
        setChars("%@[]_{}\\<>~^`#$“ÀÁÂÄàáâä“ÈÉÊèéêëÌÍÎìíîï“ÒÓÔÖòóôöÙÚÛÜ“ùúûüÇçÑñ¿¡".toCharArray(), extendedCharacterSet);
    }
    
    /**
     * String character set alternatives.
     */
    public enum CharacterSet
    {
        BASIC(basicCharacterSet),
        EXTENDED(extendedCharacterSet),
        UNRESTRICTED(null);
        
        private final boolean[] characterFlags;
        
        CharacterSet(boolean[] flags) {
            characterFlags = flags;
        }
        
        public boolean[] flags() {
            return characterFlags;
        }
    }
    
    /**
     * Set flags for range of characters in array.
     *
     * @param from
     * @param to
     * @param flags
     */
    private static void fillChars(char from, char to, boolean[] flags) {
        for (int i = from; i <= to; i++) {
            flags[i] = true;
        }
    }
    
    /**
     * Set flags for specific characters in array.
     *
     * @param chars
     * @param flags
     */
    private static void setChars(char[] chars, boolean[] flags) {
        for (int i = 0; i < chars.length; i++) {
            flags[chars[i]] = true;
        }
    }
}