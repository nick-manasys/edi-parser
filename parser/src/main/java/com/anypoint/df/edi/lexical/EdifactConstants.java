
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
    
    public static final String FORCE_UNA = "forceUNA";
    
    // UNZ message values stored in map
    public static final String INTER_CONTROL_COUNT = "UNZ01";
    public static final String INTER_CONTROL_REF = "UNZ02";
    
    /** EDIFACT Level A character set. */
    public static final boolean[] levelACharacterSet;
    
    /** EDIFACT Level B character set. */
    public static final boolean[] levelBCharacterSet;
    
    /** General character set (allows everything above ASCII control character range). */
    public static final boolean[] generalCharacterSet;
    
    static {
        levelACharacterSet = new boolean[128];
        fillChars('A', 'Z', levelACharacterSet);
        fillChars('0', '9', levelACharacterSet);
        setChars(" .,-()/=,+:?!\"%&*;<>".toCharArray(), levelACharacterSet);
        levelBCharacterSet = new boolean[128];
        System.arraycopy(levelACharacterSet, 0, levelBCharacterSet, 0, levelACharacterSet.length);
        fillChars('a', 'z', levelBCharacterSet);
        setChars(" .,-()/'+:=?!\"%&*;<>\014\015\017".toCharArray(), levelBCharacterSet);
        generalCharacterSet = new boolean[0x10000];
        fillChars((char)0x20, (char)0xFFFF, generalCharacterSet);
    }
    
    // standard character sets
    public static final Charset UTF8 = Charset.forName("UTF-8");
    
    /** Syntax identifiers. */
    public static class SyntaxIdentifier
    {
        private final String syntaxCode;
        private final Charset defaultCharset;
        private final boolean[] characterFlags;
        
        SyntaxIdentifier(String code, Charset chset, boolean[] flags) {
            syntaxCode = code;
            defaultCharset = chset;
            characterFlags = flags;
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
    public static final SyntaxIdentifier LEVELA = new SyntaxIdentifier("UNOA", ASCII_CHARSET, levelACharacterSet);
    public static final SyntaxIdentifier LEVELB = new SyntaxIdentifier("UNOB", ASCII_CHARSET, levelBCharacterSet);
    public static final SyntaxIdentifier LEVELC = new SyntaxIdentifier("UNOC", Charset.forName("ISO8859_1"),
        generalCharacterSet);
    public static final SyntaxIdentifier LEVELY = new SyntaxIdentifier("UNOY", ASCII_CHARSET, generalCharacterSet);
    public static final Map<String,SyntaxIdentifier> EDIFACT_CHARSETS;
    static {
        EDIFACT_CHARSETS = new HashMap<>();
        EDIFACT_CHARSETS.put(LEVELA.syntaxCode, LEVELA);
        EDIFACT_CHARSETS.put(LEVELB.syntaxCode, LEVELB);
        EDIFACT_CHARSETS.put(LEVELC.syntaxCode, LEVELC);
        EDIFACT_CHARSETS.put(LEVELY.syntaxCode, LEVELY);
        addSyntax("UNOD", "ISO8859_2", generalCharacterSet);
        addSyntax("UNOE", "ISO8859_5", generalCharacterSet);
        addSyntax("UNOF", "ISO8859_7", generalCharacterSet);
        addSyntax("UNOG", "ISO8859_3", generalCharacterSet);
        addSyntax("UNOH", "ISO8859_4", generalCharacterSet);
        addSyntax("UNOI", "ISO8859_6", generalCharacterSet);
        addSyntax("UNOJ", "ISO8859_8", generalCharacterSet);
        addSyntax("UNOK", "ISO8859_9", generalCharacterSet);
        addSyntax("UNOX", "ISO2375", generalCharacterSet);
    }
    public static void addSyntax(String code, String chname, boolean[] flags) {
        try {
            Charset chset = Charset.forName(chname);
            SyntaxIdentifier synid = new SyntaxIdentifier(code, chset, flags);
            EDIFACT_CHARSETS.put(code, synid);
        } catch (Throwable e) { /* unsupported encoding, ignore */ }
    }
    
    // delimiters: data separator, subelement separator, repetition separator, segment terminator, release
    private static final String basicDelimiters = "+: '?";
    private static final String alternateDelimiters = "\035\037 \034 ";
    private static final String version4Delimiters = "+:*'?";
    
    public static final Map<String,SyntaxVersion> EDIFACT_VERSIONS = new HashMap<>();
    
    public enum SyntaxVersion {
        VERSION2("2"), VERSION3("3"), VERSION4("4");
        
        private final String codeValue;
        
        SyntaxVersion(String code) {
            codeValue = code;
            EDIFACT_VERSIONS.put(code, this);
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
            if (this == VERSION3 && sid == LEVELB) {
                return alternateDelimiters;
            }
            return basicDelimiters;
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