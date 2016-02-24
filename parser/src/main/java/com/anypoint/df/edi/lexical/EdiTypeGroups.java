
package com.anypoint.df.edi.lexical;

import com.anypoint.df.edi.lexical.EdiConstants.DataType;

/**
 * Constants used for all EDI variations.
 */
public final class EdiTypeGroups
{
    private EdiTypeGroups() {}

    public static final DataType[] REAL_TYPES = new DataType[] {
        DataType.REAL, DataType.NUMBER, DataType.DECIMAL1, DataType.DECIMAL2, DataType.DECIMAL3, DataType.DECIMAL4,
        DataType.DECIMAL5, DataType.DECIMAL6, DataType.DECIMAL7, DataType.DECIMAL8, DataType.DECIMAL9, DataType.NUMERIC
    };
    public static final DataType[] INTEGER_TYPES = new DataType[] { DataType.INTEGER, DataType.TIME, DataType.SEQID };
    public static final DataType[] DATE_TYPES = new DataType[] { DataType.DATE };
    public static final DataType[] STRING_TYPES = new DataType[] {
        DataType.ID, DataType.ALPHANUMERIC, DataType.ALPHA, DataType.DATETIME, DataType.STRINGDATA
    };
}