package com.mulesoft.flatfile.lexical;

import com.mulesoft.flatfile.lexical.TypeFormatConstants.*;
import com.mulesoft.flatfile.lexical.formats.*;
import com.mulesoft.flatfile.lexical.formats.XmlDateFormat.Variation;

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
    public static TypeFormat buildType(String type, int minLength, int maxLength) {
        String norm = type.toUpperCase();
        if ("ST".equals(norm) || "VARIES".equals(norm) || "VAR".equals(norm)) {
            return new GeneralStringFormat(type, minLength, maxLength, FillMode.LEFT);
        } if ("NM".equals(norm)) {
            return new ExplicitDecimalFormat(type, minLength, maxLength, NumberSign.OPTIONAL, true,
                FillMode.ZEROES, true, false, false, false);
        } else if ("SI".equals(norm)) {
            return new IntegerFormat(type, minLength, maxLength, NumberSign.UNSIGNED, false, FillMode.ZEROES);
        } else if ("DT".equals(norm)) {
            return new XmlDateFormat(type, minLength, maxLength, Variation.DATE);
        } else if ("DTM".equals(norm)) {
            return new XmlDateFormat(type, minLength, maxLength, Variation.DATETIME);
        } else if ("TM".equals(norm)) {
            return new XmlDateFormat(type, minLength, maxLength, Variation.TIME);
        } else if ("FT".equals(norm) || "GTS".equals(norm) || "ID".equals(norm) || "IS".equals(norm) || "SNM".equals(norm) || "TN".equals(norm) || "TX".equals(norm)) {
            return new GeneralStringFormat(type, minLength, maxLength, FillMode.LEFT);
        }
        throw new IllegalArgumentException("Unknown HL7 type code " + type);
    }
}