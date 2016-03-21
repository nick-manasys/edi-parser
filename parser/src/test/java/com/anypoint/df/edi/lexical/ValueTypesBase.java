package com.anypoint.df.edi.lexical;

import com.anypoint.df.edi.lexical.ValueTypeConstants.NumberPadType;
import com.anypoint.df.edi.lexical.ValueTypeConstants.NumberSignType;
import com.anypoint.df.edi.lexical.ValueTypeConstants.StringSpaceFill;
import com.anypoint.df.edi.lexical.types.GeneralStringValue;
import com.anypoint.df.edi.lexical.types.ImpliedDecimalValue;
import com.anypoint.df.edi.lexical.types.IntegerValue;
import com.anypoint.df.edi.lexical.types.NumberValue;
import com.anypoint.df.edi.lexical.types.RestrictedCharacterStringValue;
import com.anypoint.df.edi.lexical.types.TimeValue;
import com.anypoint.df.edi.lexical.types.X12DateValue;
import com.anypoint.df.edi.lexical.types.XmlDateValue;
import com.anypoint.df.edi.lexical.types.XmlDateValue.Variation;

/**
 * Value type definitions used for testing both the actual value type implementations and the lexer/writer code. The
 * definitions use mostly X12 type codes, with some HL7 and a couple of EDIFACT types mixed in.
 */
public abstract class ValueTypesBase
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
    private static ValueType buildType(String code, int minLength, int maxLength, String name) {
        if ("AN".equals(code)) {
            return new GeneralStringValue(name, minLength, maxLength, StringSpaceFill.RIGHT);
        } else if ("R".equals(code)) {
            return new NumberValue(name, minLength, maxLength, NumberSignType.NEGATIVE_ONLY, false,
                NumberPadType.ZEROES, false, false, true, false);
        } else if ("NM".equals(code)) {
            return new NumberValue(name, minLength, maxLength, NumberSignType.OPTIONAL, true,
                NumberPadType.ZEROES, true, false, false, false);
        } else if (code.startsWith("N")) {
            if (code.length() == 1 || "N0".equals(code)) {
                return new IntegerValue(name, minLength, maxLength, NumberSignType.NEGATIVE_ONLY, false,
                    NumberPadType.ZEROES);
            } else if (code.length() == 2) {
                char chr = code.charAt(1);
                if (chr > 0 && chr <= '9') {
                    return new ImpliedDecimalValue(name, minLength, maxLength, NumberSignType.NEGATIVE_ONLY, false,
                        NumberPadType.ZEROES, chr - '0');
                }
            }
        } else if ("DT".equals(code)) {
            return new X12DateValue(name, minLength, maxLength);
        } else if ("TM".equals(code)) {
            return new TimeValue(name, minLength, maxLength);
        } else if ("SI".equals(code)) {
            return new IntegerValue(name, minLength, maxLength, NumberSignType.UNSIGNED, false, NumberPadType.ZEROES);
        } else if ("DT7".equals(code)) {
            return new XmlDateValue(name, minLength, maxLength, Variation.DATE);
        } else if ("DTM".equals(code)) {
            return new XmlDateValue(name, minLength, maxLength, Variation.DATETIME);
        } else if ("TM7".equals(code)) {
            return new XmlDateValue(name, minLength, maxLength, Variation.TIME);
        } else if ("n".equals(code)) {
            return new NumberValue(name, minLength, maxLength, NumberSignType.NEGATIVE_ONLY, false,
                NumberPadType.ZEROES, false, true, false, false);
        } else if ("a".equals(code)) {
            return new RestrictedCharacterStringValue(name, minLength, maxLength, StringSpaceFill.RIGHT,
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
    private static ValueType buildType(String type, int minLength, int maxLength) {
        String name = typeName(type, minLength, maxLength);
        return buildType(type, minLength, maxLength, name);
    }
    
    // value type definitions used in tests
    public static final ValueType VALAN1 = buildType("AN", 1, 1);
    public static final ValueType VALAN1_10 = buildType("AN", 1, 10);
    public static final ValueType VALAN3 = buildType("AN", 3, 3);
    public static final ValueType VALAN5 = buildType("AN", 5, 5);
    public static final ValueType VALAN8 = buildType("AN", 8, 8);
    public static final ValueType VALAN10 = buildType("AN", 10, 10);
    public static final ValueType VALR1 = buildType("R", 1, 1);
    public static final ValueType VALR1_10 = buildType("R", 1, 10);
    public static final ValueType VALR5 = buildType("R", 5, 5);
    public static final ValueType VALR6 = buildType("R", 6, 6);
    public static final ValueType VALR8 = buildType("R", 8, 8);
    public static final ValueType VALR10 = buildType("R", 10, 10);
    public static final ValueType VALN1 = buildType("N", 1, 1);
    public static final ValueType VALN1_10 = buildType("N", 1, 10);
    public static final ValueType VALN5 = buildType("N", 5, 5);
    public static final ValueType VALN10 = buildType("N", 10, 10);
    public static final ValueType VALN2_1 = buildType("N2", 1, 1);
    public static final ValueType VALN2_1_10 = buildType("N2", 1, 10);
    public static final ValueType VALN2_10 = buildType("N2", 10, 10);
    public static final ValueType VALNM1 = buildType("NM", 1, 1);
    public static final ValueType VALNM1_10 = buildType("NM", 1, 10);
    public static final ValueType VALNM1_12 = buildType("NM", 1, 12);
    public static final ValueType VALNM5 = buildType("NM", 5, 5);
    public static final ValueType VALNM10 = buildType("NM", 10, 10);
    public static final ValueType VALSI1 = buildType("SI", 1, 1);
    public static final ValueType VALSI1_10 = buildType("SI", 1, 10);
    public static final ValueType VALSI10 = buildType("SI", 10, 10);
    public static final ValueType VALn1 = buildType("n", 1, 1);
    public static final ValueType VALn1_10 = buildType("n", 1, 10);
    public static final ValueType VALn10 = buildType("n", 10, 10);
    public static final ValueType VALa1 = buildType("a", 1, 1);
    public static final ValueType VALa1_10 = buildType("a", 1, 10);
    public static final ValueType VALa10 = buildType("a", 10, 10);
    // TODO: add more date/time value defs and tests
    public static final ValueType VALDT6 = buildType("DT", 6, 6);
    public static final ValueType VALDT8 = buildType("DT", 8, 8);
    public static final ValueType VALTM4 = buildType("TM", 4, 4);
    public static final ValueType VALTM6 = buildType("TM", 6, 6);
}