
package com.anypoint.df.edi.lexical;

import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

/**
 * Constants for EDIFACT documents.
 */
public final class EdifactConstants extends EdiConstants
{
    private EdifactConstants() {}
    
    // UNB message values stored in map
    public static final String SYNTAX_IDENTIFIER = "UNB0101";
    public static final String SYNTAX_VERSION_NUMBER = "UNB0102";
    public static final String SERVICE_CODE_LIST = "UNB0103";
    public static final String CHARACTER_ENCODING = "UNB0104";
    
    private static Charset charsetOrNull(String name) {
        try {
            return Charset.forName(name);
        } catch (Throwable e) {
            return null;
        }
    }
    
    /** EDIFACT Level A character set. */
    public static final boolean[] levelACharacterSet;
    
    /** EDIFACT Level B character set. */
    public static final boolean[] levelBCharacterSet;
    
    static {
        levelACharacterSet = new boolean[128];
        fillChars('A', 'Z', levelACharacterSet);
        fillChars('0', '9', levelACharacterSet);
        setChars(" .,-()/=,+:?!\"%&*;<>".toCharArray(), levelACharacterSet);
        levelBCharacterSet = new boolean[128];
        System.arraycopy(levelACharacterSet, 0, levelBCharacterSet, 0, levelACharacterSet.length);
        fillChars('a', 'z', levelBCharacterSet);
        setChars(" .,-()/'+:=?!\"%&*;<>\014\015\017".toCharArray(), levelBCharacterSet);
    }
    
    // standard character sets
    public static final Charset UTF8 = Charset.forName("UTF-8");
    
    public static final Map<String,SyntaxIdentifier> EDIFACT_CHARSETS = new HashMap<>();
    
    /** Syntax identifiers. */
    public enum SyntaxIdentifier
    {
        LEVELA("UNOA", ASCII_CHARSET, levelACharacterSet),
        LEVELB("UNOB", ASCII_CHARSET, levelBCharacterSet),
        LEVELC("UNOC", charsetOrNull("ISO8859_1"), levelBCharacterSet),
        LEVELD("UNOD", charsetOrNull("ISO8859_2"), levelBCharacterSet),
        LEVELE("UNOE", charsetOrNull("ISO8859_5"), levelBCharacterSet),
        LEVELF("UNOF", charsetOrNull("ISO8859_7"), levelBCharacterSet),
        LEVELG("UNOG", charsetOrNull("ISO8859_3"), levelBCharacterSet),
        LEVELH("UNOH", charsetOrNull("ISO8859_4"), levelBCharacterSet),
        LEVELI("UNOI", charsetOrNull("ISO8859_6"), levelBCharacterSet),
        LEVELJ("UNOJ", charsetOrNull("ISO8859_8"), levelBCharacterSet),
        LEVELK("UNOK", charsetOrNull("ISO8859_9"), levelBCharacterSet),
        LEVELX("UNOX", charsetOrNull("ISO2375"), levelBCharacterSet),
        LEVELY("UNOY", UTF8, levelBCharacterSet);
        
        private final String syntaxCode;
        private final Charset defaultCharset;
        private final boolean[] characterFlags;
        
        SyntaxIdentifier(String code, Charset chset, boolean[] flags) {
            syntaxCode = code;
            defaultCharset = chset;
            characterFlags = flags;
            EDIFACT_CHARSETS.put(syntaxCode, this);
        }
        
        public String code() {
            return syntaxCode;
        }
        
        public Charset defaultCharSet() {
            return defaultCharset;
        }
        
        public boolean[] flags() {
            return characterFlags;
        }
    }
    
    // delimiters: data separator, subelement separator, repetition separator, segment terminator, release
    private static final String basicDelimiters = "+: '?";
    private static final String alternateDelimiters = "\035\037 \034 ";
    private static final String version4Delimiters = "+:*'?";
    
    public enum SyntaxVersion {
        VERSION3("3"), VERSION4("4");
        
        private final String codeValue;
        
        SyntaxVersion(String code) {
            codeValue = code;
        }
        
        public String code() {
            return codeValue;
        }
        
        /**
         * Get default delimiters string.
         * 
         * @param sid syntax identifier in use
         * @return data separator, subelement separator, repetition separator, segment terminator, release
         */
        public String defaultDelimiters(SyntaxIdentifier sid) {
            if (this == VERSION4) {
                return version4Delimiters;
            }
            if (sid == SyntaxIdentifier.LEVELA) {
                return basicDelimiters;
            }
            return alternateDelimiters;
        }
    }
    
    /**
     * Helper method to convert blank character to unused code.
     * 
     * @param chr
     * @return chr or -1
     */
    public static int charNonBlank(char chr) {
        return chr == ' ' ? -1 : chr;
    }
}