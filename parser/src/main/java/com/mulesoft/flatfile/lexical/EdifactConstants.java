
package com.mulesoft.flatfile.lexical;

import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import com.mulesoft.flatfile.lexical.TypeFormatConstants.*;
import com.mulesoft.flatfile.lexical.formats.ExplicitDecimalFormat;
import com.mulesoft.flatfile.lexical.formats.RestrictedCharacterStringFormat;

/**
 * Constants for EDIFACT documents.
 */
public final class EdifactConstants
{
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
    
    /** Alphanumeric characters (everything outside control character range). */
    public static final boolean[] alphaNumerics;
    
    /** Alpha characters (letters and space only). */
    public static final boolean[] plainAlphas;
    
    static {
        levelACharacterSet = new boolean[128];
        EdiConstants.fillChars('A', 'Z', levelACharacterSet);
        EdiConstants.fillChars('0', '9', levelACharacterSet);
        EdiConstants.setChars(" .,-()/=,+:?!\"%&*;<>".toCharArray(), levelACharacterSet);
        levelBCharacterSet = new boolean[128];
        System.arraycopy(levelACharacterSet, 0, levelBCharacterSet, 0, levelACharacterSet.length);
        EdiConstants.fillChars('a', 'z', levelBCharacterSet);
        EdiConstants.setChars(" .,-()/'+:=?!\"%&*;<>\014\015\017".toCharArray(), levelBCharacterSet);
        alphaNumerics = new boolean[128];
        EdiConstants.fillChars((char)0x20, (char)0x7F, alphaNumerics);
        plainAlphas = new boolean[128];
        plainAlphas[' '] = true;
        EdiConstants.fillChars('a', 'z', plainAlphas);
        EdiConstants.fillChars('A', 'Z', plainAlphas);
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
    public static final SyntaxIdentifier LEVELA = new SyntaxIdentifier("UNOA", EdiConstants.ASCII_CHARSET, levelACharacterSet);
    public static final SyntaxIdentifier LEVELB = new SyntaxIdentifier("UNOB", EdiConstants.ASCII_CHARSET, levelBCharacterSet);
    public static final SyntaxIdentifier LEVELC = new SyntaxIdentifier("UNOC", Charset.forName("ISO8859_1"), null);
    public static final SyntaxIdentifier LEVELY = new SyntaxIdentifier("UNOY", EdiConstants.ASCII_CHARSET, null);
    public static final Map<String,SyntaxIdentifier> EDIFACT_CHARSETS;
    static {
        EDIFACT_CHARSETS = new HashMap<>();
        EDIFACT_CHARSETS.put(LEVELA.syntaxCode, LEVELA);
        EDIFACT_CHARSETS.put(LEVELB.syntaxCode, LEVELB);
        EDIFACT_CHARSETS.put(LEVELC.syntaxCode, LEVELC);
        EDIFACT_CHARSETS.put(LEVELY.syntaxCode, LEVELY);
        addSyntax("UNOD", "ISO8859_2");
        addSyntax("UNOE", "ISO8859_5");
        addSyntax("UNOF", "ISO8859_7");
        addSyntax("UNOG", "ISO8859_3");
        addSyntax("UNOH", "ISO8859_4");
        addSyntax("UNOI", "ISO8859_6");
        addSyntax("UNOJ", "ISO8859_8");
        addSyntax("UNOK", "ISO8859_9");
        addSyntax("UNOX", "ISO2375");
    }
    public static void addSyntax(String code, String chname) {
        try {
            Charset chset = Charset.forName(chname);
            SyntaxIdentifier synid = new SyntaxIdentifier(code, chset, null);
            EDIFACT_CHARSETS.put(code, synid);
        } catch (Throwable e) { /* unsupported encoding, ignore */ }
    }
    
    // delimiters: data separator, subelement separator, repetition separator, segment terminator, release
    private static final String basicDelimiters = "+: '?";
    private static final String alternateDelimiters = "\035\037 \034 ";
    private static final String version4Delimiters = "+:*'?";
    
    public enum SyntaxVersion {
        VERSION2("2"), VERSION3("3"), VERSION4("4");
        
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
            if (sid == LEVELA) {
                return basicDelimiters;
            }
            return alternateDelimiters;
        }
    }
    
    public static final Map<String,SyntaxVersion> EDIFACT_VERSIONS = new HashMap<>();
    static {
        for (SyntaxVersion version: SyntaxVersion.values()) {
            EDIFACT_VERSIONS.put(version.code(), version);
        }
        EDIFACT_VERSIONS.put("1", SyntaxVersion.VERSION2);
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
    
    /**
     * Construct name for type.
     * 
     * @param type
     * @param minLength
     * @param maxLength
     * @return name
     */
    public static String typeName(String type, int minLength, int maxLength) {
        if (minLength == maxLength) {
            return type + maxLength;
        }
        if (minLength != 0) {
            throw new IllegalArgumentException("No support for non-zero based size range");
        }
        return type + ".." + maxLength;
    }
    
    /**
     * Build type instance.
     * 
     * @param type
     * @param minLength
     * @param maxLength
     * @return
     */
    public static TypeFormat buildType(String type, int minLength, int maxLength) {
        String norm = type.toLowerCase();
        if ("an".equals(norm)) {
            return new RestrictedCharacterStringFormat(type, minLength, maxLength, FillMode.LEFT,
                alphaNumerics, true);
        } if ("n".equals(norm)) {
            return new ExplicitDecimalFormat(type, minLength, maxLength, NumberSign.NEGATIVE_ONLY, false,
                FillMode.ZEROES, false, true, false, false);
        } else if ("a".equals(norm)) {
            return new RestrictedCharacterStringFormat(type, minLength, maxLength, FillMode.LEFT,
                plainAlphas, true);
        }
        throw new IllegalArgumentException("Unknown EDIFACT type code " + type);
    }
}