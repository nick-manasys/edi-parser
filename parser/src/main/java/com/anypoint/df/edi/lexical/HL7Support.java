package com.anypoint.df.edi.lexical;

import com.anypoint.df.edi.lexical.ValueTypeConstants.NumberPadType;
import com.anypoint.df.edi.lexical.ValueTypeConstants.NumberSignType;
import com.anypoint.df.edi.lexical.ValueTypeConstants.StringSpaceFill;
import com.anypoint.df.edi.lexical.types.GeneralStringValue;
import com.anypoint.df.edi.lexical.types.IntegerValue;
import com.anypoint.df.edi.lexical.types.NumberValue;
import com.anypoint.df.edi.lexical.types.XmlDateValue;
import com.anypoint.df.edi.lexical.types.XmlDateValue.Variation;

public final class HL7Support
{
    private HL7Support() {}
    
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
    public static ValueType buildType(String type, int minLength, int maxLength) {
        String norm = type.toUpperCase();
        if ("ST".equals(norm) || "VARIES".equals(norm) || "VAR".equals(norm)) {
            return new GeneralStringValue(type, minLength, maxLength, StringSpaceFill.RIGHT);
        } if ("NM".equals(norm)) {
            return new NumberValue(type, minLength, maxLength, NumberSignType.OPTIONAL, true,
                NumberPadType.ZEROES, true, false, false, false);
        } else if ("SI".equals(norm)) {
            return new IntegerValue(type, minLength, maxLength, NumberSignType.UNSIGNED, false, NumberPadType.ZEROES);
        } else if ("DT".equals(norm)) {
            return new XmlDateValue(type, minLength, maxLength, Variation.DATE);
        } else if ("DTM".equals(norm)) {
            return new XmlDateValue(type, minLength, maxLength, Variation.DATETIME);
        } else if ("TM".equals(norm)) {
            return new XmlDateValue(type, minLength, maxLength, Variation.TIME);
        } else if ("FT".equals(norm) || "GTS".equals(norm) || "ID".equals(norm) || "IS".equals(norm) || "SNM".equals(norm) || "TN".equals(norm) || "TX".equals(norm)) {
            return new GeneralStringValue(type, minLength, maxLength, StringSpaceFill.RIGHT);
        }
        throw new IllegalArgumentException("Unknown HL7 type code " + type);
    }
}