
package com.anypoint.df.edi.lexical;

import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

/**
 * Constants used for all EDI variations.
 */
public abstract class EdiConstants
{
    protected EdiConstants() {}
    
    // standard character sets
    public static final Charset ASCII_CHARSET = Charset.forName("US-ASCII");
    
    /** Maximum year number accepted (otherwise wrapped to previous century). */
    public static final int maximumYear = 2070;
    
    /** Map from type name to type. */
    private static final Map<String,DataType> NAMETYPES = new HashMap<>();
    
    /**
     * Token delimiter types. The types used for separating values are explicitly ordered by granularity, so that the
     * next finer level of granularity can easily be found.
     */
    public enum ItemType {
        SEGMENT, END, REPETITION, DATA_ELEMENT, COMPONENT, SUB_COMPONENT;
        
        /**
         * Get next finer level of granularity for separators.
         * 
         * @return
         */
        public ItemType nextLevel() {
            if (ordinal() < DATA_ELEMENT.ordinal()) {
                throw new IllegalStateException("No granularity defined for item type " + toString());
            }
            if (ordinal() == values().length - 1) {
                return this;
            }
            return values()[ordinal() + 1];
        }
    }
    
    // flags for EDI forms using a data type
    private static final int X12_FLAG = 1;
    private static final int EDIFACT_FLAG = 2;
    private static final int HL7_FLAG = 3;
    
    /** Data types. */
    public enum DataType
    {
        ALPHA("A", X12_FLAG | EDIFACT_FLAG),
        ALPHANUMERIC("AN", X12_FLAG | EDIFACT_FLAG),
        DATE("DT", X12_FLAG | EDIFACT_FLAG | HL7_FLAG),
        BINARY("B", X12_FLAG),
        DATETIME("DTM", HL7_FLAG),
        DECIMAL1("N1", X12_FLAG, 1),
        DECIMAL2("N2", X12_FLAG, 2),
        DECIMAL3("N3", X12_FLAG, 3),
        DECIMAL4("N4", X12_FLAG, 4),
        DECIMAL5("N5", X12_FLAG, 5),
        DECIMAL6("N6", X12_FLAG, 6),
        DECIMAL7("N7", X12_FLAG, 7),
        DECIMAL8("N8", X12_FLAG, 8),
        DECIMAL9("N9", X12_FLAG, 9),
        ID("ID", X12_FLAG | EDIFACT_FLAG),
        INTEGER("N0", X12_FLAG),
        // TODO: the N type as used by X12 (basically an integer) is not compatible with that used by EDIFACT (general
        //  numeric value), so either the EDIFACT one should be changed to R or X12 should always use N0
        NUMBER("N", X12_FLAG | EDIFACT_FLAG),
        NUMERIC("NM", HL7_FLAG),
        REAL("R", X12_FLAG),
        SEQID("SI", HL7_FLAG),
        STRINGDATA("ST", HL7_FLAG),
        TIME("TM", X12_FLAG | HL7_FLAG),
        VARIES("varies", HL7_FLAG);
        
        private final String typeCode;
        private final int decimalPlaces;
        private final int formFlags;
        
        private DataType(String code, int flags, int places) {
            typeCode = code;
            formFlags = flags;
            decimalPlaces = places;
            NAMETYPES.put(code, this);
        }
        
        private DataType(String code, int flags) {
            this(code, flags, -1);
        }
        
        /**
         * Get code for type.
         *
         * @return code
         */
        public String code() {
            return typeCode;
        }
        
        /**
         * Check if a decimal type.
         *
         * @return <code>true</code> if decimal, <code>false</code> if not
         */
        public boolean isDecimal() {
            return decimalPlaces >= 0;
        }
        
        /**
         * Get number of decimal places for decimal type.
         *
         * @return decimal places, negative if not a decimal
         */
        public int decimalPlaces() {
            return decimalPlaces;
        }
    }
    
    /**
     * Get data type from X12 type code. This throws an exception if the type code is unknown.
     *
     * @param code
     * @return
     */
    public static DataType toX12Type(String code) {
        if (NAMETYPES.containsKey(code)) {
            DataType type = NAMETYPES.get(code);
            if ((type.formFlags & X12_FLAG) == 0) {
                throw new IllegalArgumentException("Not an X12 type code " + code);
            }
            return type;
        }
        throw new IllegalArgumentException("Unknown type code " + code);
    }
    
    /**
     * Get data type from EDIFACT type code. This throws an exception if the type code is unknown.
     *
     * @param code
     * @return
     */
    public static DataType toEdifactType(String code) {
        if (NAMETYPES.containsKey(code)) {
            DataType type = NAMETYPES.get(code);
            if ((type.formFlags & EDIFACT_FLAG) == 0) {
                throw new IllegalArgumentException("Not an EDIFACT type code " + code);
            }
            return type;
        }
        throw new IllegalArgumentException("Unknown type code " + code);
    }
    
    private static final Map<String, DataType> HL7Mapping = new HashMap<>();
    static {
        // treat specialized HL7 types as variations on string data types
        HL7Mapping.put("FT", DataType.STRINGDATA);
        HL7Mapping.put("GTS", DataType.ALPHANUMERIC);
        HL7Mapping.put("ID", DataType.STRINGDATA);
        HL7Mapping.put("IS", DataType.STRINGDATA);
        HL7Mapping.put("TX", DataType.STRINGDATA);
        HL7Mapping.put("DTM", DataType.STRINGDATA);
        HL7Mapping.put("var", DataType.VARIES);
    }
    
    /**
     * Get data type from HL7 type code. This throws an exception if the type code is unknown.
     *
     * @param code
     * @return
     */
    public static DataType toHL7Type(String code) {
        if (HL7Mapping.containsKey(code)) {
            return HL7Mapping.get(code);
        }
        if (NAMETYPES.containsKey(code)) {
            DataType type = NAMETYPES.get(code);
            if ((type.formFlags & HL7_FLAG) == 0) {
                throw new IllegalArgumentException("Not an HL7 type code " + code);
            }
            return type;
        }
        throw new IllegalArgumentException("Unknown type code " + code);
    }
   
    // combinations of types
    public static final DataType[] REAL_TYPES = new DataType[] {
        DataType.REAL, DataType.NUMBER, DataType.DECIMAL1, DataType.DECIMAL2, DataType.DECIMAL3, DataType.DECIMAL4,
        DataType.DECIMAL5, DataType.DECIMAL6, DataType.DECIMAL7, DataType.DECIMAL8, DataType.DECIMAL9, DataType.NUMERIC
    };
    public static final DataType[] INTEGER_TYPES = new DataType[] { DataType.INTEGER, DataType.TIME, DataType.SEQID };
    public static final DataType[] DATE_TYPES = new DataType[] { DataType.DATE };
    public static final DataType[] STRING_TYPES = new DataType[] {
        DataType.ID, DataType.ALPHANUMERIC, DataType.ALPHA, DataType.DATETIME, DataType.STRINGDATA
    };
    
    /**
     * Set flags for range of characters in array.
     *
     * @param from
     * @param to
     * @param flags
     */
    protected static void fillChars(char from, char to, boolean[] flags) {
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
    protected static void setChars(char[] chars, boolean[] flags) {
        for (int i = 0; i < chars.length; i++) {
            flags[chars[i]] = true;
        }
    }
}