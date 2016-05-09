package com.mulesoft.flatfile.lexical;

import java.nio.charset.Charset;

import com.mulesoft.flatfile.lexical.TypeFormatConstants.*;
import com.mulesoft.flatfile.lexical.formats.*;

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
            return type + '(' + maxLength + ')';
        }
        return type + '(' + minLength + '-' + maxLength + ')';
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
        String norm = type.toUpperCase();
        if ("AN".equals(norm)) {
            return new GeneralStringFormat(type, minLength, maxLength, FillMode.LEFT);
        } if ("R".equals(norm)) {
            return new ExplicitDecimalFormat(type, minLength, maxLength, NumberSign.NEGATIVE_ONLY, false,
                FillMode.ZEROES, false, false, true, false);
        } if (norm.startsWith("N")) {
            if (norm.length() == 1 || "N0".equals(norm)) {
                return new IntegerFormat(type, minLength, maxLength, NumberSign.NEGATIVE_ONLY, false, FillMode.ZEROES);
            } else if (norm.length() == 2) {
                char chr = norm.charAt(1);
                if (chr > 0 && chr <= '9') {
                    return new ImpliedDecimalFormat(type, minLength, maxLength, NumberSign.NEGATIVE_ONLY, false,
                        FillMode.ZEROES, chr - '0');
                }
            }
        } else if ("ID".equals(norm)) {
            return new GeneralStringFormat(type, minLength, maxLength, FillMode.NONE);
        } else if ("DT".equals(norm)) {
            return new X12DateFormat(type, minLength, maxLength);
        } else if ("TM".equals(norm)) {
            return new MillisecondTimeFormat(type, minLength, maxLength);
        } else if ("B".equals(norm)) {
            // special X12 kludges for now
            return new GeneralStringFormat(type, minLength, maxLength, FillMode.LEFT);
        }
        throw new IllegalArgumentException("Unknown X12 type code " + type);
    }

    // value type definitions used for envelope segments
    public static final TypeFormat VALID1 = buildType("ID", 1, 1);
    public static final TypeFormat VALID2 = buildType("ID", 2, 2);
    public static final TypeFormat VALID5 = buildType("ID", 5, 5);
    public static final TypeFormat VALAN1 = buildType("AN", 1, 1);
    public static final TypeFormat VALAN10 = buildType("AN", 10, 10);
    public static final TypeFormat VALAN15 = buildType("AN", 15, 15);
    public static final TypeFormat VALDT6 = buildType("DT", 6, 6);
    public static final TypeFormat VALTM4 = buildType("TM", 4, 4);
    public static final TypeFormat VALN9 = buildType("N", 9, 9);
    public static final TypeFormat VALN1_5 = buildType("N", 1, 5);
}