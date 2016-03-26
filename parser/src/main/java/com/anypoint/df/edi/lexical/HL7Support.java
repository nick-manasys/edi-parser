package com.anypoint.df.edi.lexical;

import com.anypoint.df.edi.lexical.TypeFormatConstants.NumberPad;
import com.anypoint.df.edi.lexical.TypeFormatConstants.NumberSign;
import com.anypoint.df.edi.lexical.TypeFormatConstants.StringSpaceFill;
import com.anypoint.df.edi.lexical.formats.GeneralStringFormat;
import com.anypoint.df.edi.lexical.formats.IntegerFormat;
import com.anypoint.df.edi.lexical.formats.NumberFormat;
import com.anypoint.df.edi.lexical.formats.XmlDateFormat;
import com.anypoint.df.edi.lexical.formats.XmlDateFormat.Variation;

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
            return new GeneralStringFormat(type, minLength, maxLength, StringSpaceFill.LEFT);
        } if ("NM".equals(norm)) {
            return new NumberFormat(type, minLength, maxLength, NumberSign.OPTIONAL, true,
                NumberPad.ZEROES, true, false, false, false);
        } else if ("SI".equals(norm)) {
            return new IntegerFormat(type, minLength, maxLength, NumberSign.UNSIGNED, false, NumberPad.ZEROES);
        } else if ("DT".equals(norm)) {
            return new XmlDateFormat(type, minLength, maxLength, Variation.DATE);
        } else if ("DTM".equals(norm)) {
            return new XmlDateFormat(type, minLength, maxLength, Variation.DATETIME);
        } else if ("TM".equals(norm)) {
            return new XmlDateFormat(type, minLength, maxLength, Variation.TIME);
        } else if ("FT".equals(norm) || "GTS".equals(norm) || "ID".equals(norm) || "IS".equals(norm) || "SNM".equals(norm) || "TN".equals(norm) || "TX".equals(norm)) {
            return new GeneralStringFormat(type, minLength, maxLength, StringSpaceFill.LEFT);
        }
        throw new IllegalArgumentException("Unknown HL7 type code " + type);
    }
}