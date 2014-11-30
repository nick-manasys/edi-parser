
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
    public static final Charset UTF8_CHARSET = Charset.forName("UTF-8");
    
    /** Maximum year number accepted (otherwise wrapped to previous century). */
    public static final int maximumYear = 2070;
    
    /** Map from type name to type. */
    private static final Map<String,DataType> NAMETYPES = new HashMap<>();
    
    /** Token delimiter types. */
    public enum ItemType {  SEGMENT, DATA_ELEMENT, QUALIFIER, REPETITION, END }
    
    /** Data types. */
    public enum DataType
    {
        REAL("R"),
        ID("ID"),
        ALPHANUMERIC("AN"),
        ALPHA("A"),
        DATE("DT"),
        TIME("TM"),
        BINARY("B"),
        NUMBER("N"),
        INTEGER("N0"),
        DECIMAL1("N1", 1),
        DECIMAL2("N2", 2),
        DECIMAL3("N3", 3),
        DECIMAL4("N4", 4),
        DECIMAL5("N5", 5),
        DECIMAL6("N6", 6),
        DECIMAL7("N7", 7),
        DECIMAL8("N8", 8),
        DECIMAL9("N9", 9);
        
        private final String typeCode;
        private final int decimalPlaces;
        
        private DataType(String code, int places) {
            typeCode = code;
            decimalPlaces = places;
            NAMETYPES.put(code, this);
        }
        
        private DataType(String code) {
            this(code, -1);
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
     * Get data type from type code. This throws an exception if the type code is unknown.
     *
     * @param type
     * @return
     */
    public static DataType toType(String type) {
        if (NAMETYPES.containsKey(type)) {
            return NAMETYPES.get(type);
        } else {
            throw new IllegalArgumentException("Unknown type code " + type);
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