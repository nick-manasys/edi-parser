
package com.anypoint.df.edi.lexical;

import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

/**
 * Constants used for all EDI variations.
 */
public final class EdiConstants
{
    private EdiConstants() {}
    
    // standard character sets
    public static final Charset ASCII_CHARSET = Charset.forName("US-ASCII");
    
    /** Maximum year number accepted (otherwise wrapped to previous century). */
    public static final int maximumYear = 2070;
    
    /** Map from type name to type. */
    private static final Map<String,DataType> NAMETYPES = new HashMap<>();
    
    /** Token delimiter types. */
    public enum ItemType {  SEGMENT, DATA_ELEMENT, QUALIFIER, REPETITION, END }
    
    /** Data types. */
    public enum DataType
    {
        REAL("R", false),
        ID("ID", true),
        ALPHANUMERIC("AN", true),
        ALPHA("A", true),
        DATE("DT", false),
        TIME("TM", false),
        BINARY("B", false),
        NUMBER("N", true),
        INTEGER("N0", false),
        DECIMAL1("N1", false, 1),
        DECIMAL2("N2", false, 2),
        DECIMAL3("N3", false, 3),
        DECIMAL4("N4", false, 4),
        DECIMAL5("N5", false, 5),
        DECIMAL6("N6", false, 6),
        DECIMAL7("N7", false, 7),
        DECIMAL8("N8", false, 8),
        DECIMAL9("N9", false, 9);
        
        private final String typeCode;
        private final int decimalPlaces;
        private final boolean edifactType;
        
        private DataType(String code, boolean edifact, int places) {
            typeCode = code;
            edifactType = edifact;
            decimalPlaces = places;
            NAMETYPES.put(code, this);
        }
        
        private DataType(String code, boolean edifact) {
            this(code, edifact, -1);
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
            return NAMETYPES.get(code);
        } else {
            throw new IllegalArgumentException("Unknown type code " + code);
        }
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
            if (!type.edifactType) {
                throw new IllegalArgumentException("Not an EDIFACT type code " + code);
            }
            return type;
        } else {
            throw new IllegalArgumentException("Unknown type code " + code);
        }
    }
    
    // combinations of types
    public static final DataType[] REAL_TYPES = new DataType[] {
        DataType.REAL, DataType.NUMBER, DataType.DECIMAL1, DataType.DECIMAL2, DataType.DECIMAL3, DataType.DECIMAL4,
        DataType.DECIMAL5, DataType.DECIMAL6, DataType.DECIMAL7, DataType.DECIMAL8, DataType.DECIMAL9
    };
    public static final DataType[] INTEGER_TYPES = new DataType[] { DataType.INTEGER, DataType.TIME};
    public static final DataType[] DATE_TYPES = new DataType[] { DataType.DATE };
    public static final DataType[] STRING_TYPES = new DataType[] { DataType.ID, DataType.ALPHANUMERIC, DataType.ALPHA };
}