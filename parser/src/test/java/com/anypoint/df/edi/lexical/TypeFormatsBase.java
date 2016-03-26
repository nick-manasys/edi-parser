package com.anypoint.df.edi.lexical;

import com.anypoint.df.edi.lexical.TypeFormatConstants.NumberPad;
import com.anypoint.df.edi.lexical.TypeFormatConstants.NumberSign;
import com.anypoint.df.edi.lexical.TypeFormatConstants.StringSpaceFill;
import com.anypoint.df.edi.lexical.formats.GeneralStringFormat;
import com.anypoint.df.edi.lexical.formats.ImpliedDecimalFormat;
import com.anypoint.df.edi.lexical.formats.IntegerFormat;
import com.anypoint.df.edi.lexical.formats.NumberFormat;
import com.anypoint.df.edi.lexical.formats.RestrictedCharacterStringFormat;
import com.anypoint.df.edi.lexical.formats.MillisecondTimeFormat;
import com.anypoint.df.edi.lexical.formats.X12DateFormat;
import com.anypoint.df.edi.lexical.formats.XmlDateFormat;
import com.anypoint.df.edi.lexical.formats.XmlDateFormat.Variation;

/**
 * Value type definitions used for testing both the actual value type implementations and the lexer/writer code. The
 * definitions use mostly X12 type codes, with some HL7 and a couple of EDIFACT types mixed in.
 */
public abstract class TypeFormatsBase
{
    /**
     * Construct name for type.
     * 
     * @param type
     * @param minLength
     * @param maxLength
     * @return name
     */
    private static String typeName(String type, int minLength, int maxLength) {
        if (minLength == maxLength) {
            return type + '(' + maxLength + ')';
        }
        return type + '(' + minLength + '-' + maxLength + ')';
    }
    
    /**
     * Build type instance.
     * 
     * @param code
     * @param minLength
     * @param maxLength
     * @param name
     * @return
     */
    private static TypeFormat buildType(String code, int minLength, int maxLength, String name) {
        if ("AN".equals(code)) {
            return new GeneralStringFormat(name, minLength, maxLength, StringSpaceFill.LEFT);
        } else if ("R".equals(code)) {
            return new NumberFormat(name, minLength, maxLength, NumberSign.NEGATIVE_ONLY, false,
                NumberPad.ZEROES, false, false, true, false);
        } else if ("NM".equals(code)) {
            return new NumberFormat(name, minLength, maxLength, NumberSign.OPTIONAL, true,
                NumberPad.ZEROES, true, false, false, false);
        } else if (code.startsWith("N")) {
            if (code.length() == 1 || "N0".equals(code)) {
                return new IntegerFormat(name, minLength, maxLength, NumberSign.NEGATIVE_ONLY, false,
                    NumberPad.ZEROES);
            } else if (code.length() == 2) {
                char chr = code.charAt(1);
                if (chr > 0 && chr <= '9') {
                    return new ImpliedDecimalFormat(name, minLength, maxLength, NumberSign.NEGATIVE_ONLY, false,
                        NumberPad.ZEROES, chr - '0');
                }
            }
        } else if ("DT".equals(code)) {
            return new X12DateFormat(name, minLength, maxLength);
        } else if ("TM".equals(code)) {
            return new MillisecondTimeFormat(name, minLength, maxLength);
        } else if ("SI".equals(code)) {
            return new IntegerFormat(name, minLength, maxLength, NumberSign.UNSIGNED, false, NumberPad.ZEROES);
        } else if ("DT7".equals(code)) {
            return new XmlDateFormat(name, minLength, maxLength, Variation.DATE);
        } else if ("DTM".equals(code)) {
            return new XmlDateFormat(name, minLength, maxLength, Variation.DATETIME);
        } else if ("TM7".equals(code)) {
            return new XmlDateFormat(name, minLength, maxLength, Variation.TIME);
        } else if ("n".equals(code)) {
            return new NumberFormat(name, minLength, maxLength, NumberSign.NEGATIVE_ONLY, false,
                NumberPad.ZEROES, false, true, false, false);
        } else if ("a".equals(code)) {
            return new RestrictedCharacterStringFormat(name, minLength, maxLength, StringSpaceFill.LEFT,
                EdifactConstants.plainAlphas, true);
        }
        throw new IllegalArgumentException("Unknown type code " + code);
    }
    
    /**
     * Build type instance.
     * 
     * @param type
     * @param minLength
     * @param maxLength
     * @return
     */
    private static TypeFormat buildType(String type, int minLength, int maxLength) {
        String name = typeName(type, minLength, maxLength);
        return buildType(type, minLength, maxLength, name);
    }
    
    // value type definitions used in tests
    public static final TypeFormat VALAN1 = buildType("AN", 1, 1);
    public static final TypeFormat VALAN1_10 = buildType("AN", 1, 10);
    public static final TypeFormat VALAN3 = buildType("AN", 3, 3);
    public static final TypeFormat VALAN5 = buildType("AN", 5, 5);
    public static final TypeFormat VALAN8 = buildType("AN", 8, 8);
    public static final TypeFormat VALAN10 = buildType("AN", 10, 10);
    public static final TypeFormat VALR1 = buildType("R", 1, 1);
    public static final TypeFormat VALR1_10 = buildType("R", 1, 10);
    public static final TypeFormat VALR5 = buildType("R", 5, 5);
    public static final TypeFormat VALR6 = buildType("R", 6, 6);
    public static final TypeFormat VALR8 = buildType("R", 8, 8);
    public static final TypeFormat VALR10 = buildType("R", 10, 10);
    public static final TypeFormat VALN1 = buildType("N", 1, 1);
    public static final TypeFormat VALN1_10 = buildType("N", 1, 10);
    public static final TypeFormat VALN5 = buildType("N", 5, 5);
    public static final TypeFormat VALN10 = buildType("N", 10, 10);
    public static final TypeFormat VALN2_1 = buildType("N2", 1, 1);
    public static final TypeFormat VALN2_1_10 = buildType("N2", 1, 10);
    public static final TypeFormat VALN2_10 = buildType("N2", 10, 10);
    public static final TypeFormat VALNM1 = buildType("NM", 1, 1);
    public static final TypeFormat VALNM1_10 = buildType("NM", 1, 10);
    public static final TypeFormat VALNM1_12 = buildType("NM", 1, 12);
    public static final TypeFormat VALNM5 = buildType("NM", 5, 5);
    public static final TypeFormat VALNM10 = buildType("NM", 10, 10);
    public static final TypeFormat VALSI1 = buildType("SI", 1, 1);
    public static final TypeFormat VALSI1_10 = buildType("SI", 1, 10);
    public static final TypeFormat VALSI10 = buildType("SI", 10, 10);
    public static final TypeFormat VALn1 = buildType("n", 1, 1);
    public static final TypeFormat VALn1_10 = buildType("n", 1, 10);
    public static final TypeFormat VALn10 = buildType("n", 10, 10);
    public static final TypeFormat VALa1 = buildType("a", 1, 1);
    public static final TypeFormat VALa1_10 = buildType("a", 1, 10);
    public static final TypeFormat VALa10 = buildType("a", 10, 10);
    // TODO: add more date/time value defs and tests
    public static final TypeFormat VALDT6 = buildType("DT", 6, 6);
    public static final TypeFormat VALDT8 = buildType("DT", 8, 8);
    public static final TypeFormat VALTM4 = buildType("TM", 4, 4);
    public static final TypeFormat VALTM6 = buildType("TM", 6, 6);
}