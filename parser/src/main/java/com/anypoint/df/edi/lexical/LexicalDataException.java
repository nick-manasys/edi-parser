
package com.anypoint.df.edi.lexical;

import com.anypoint.df.edi.lexical.EdiConstants.DataType;
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;

/**
 * Exception caused by error in EDI data.
 */
public class LexicalDataException extends LexicalException
{
    private final DataType dataType;
    private final ErrorCondition errorCondition;
    
    /**
     * Constructor.
     * 
     * @param typ expected data type
     * @param err error condition
     * @param msg description
     */
    public LexicalDataException(DataType typ, ErrorCondition err, String msg) {
        super(msg);
        dataType = typ;
        errorCondition = err;
    }

    /**
     * Get data type.
     * 
     * @return data type, or <code>null</code> if not a data error
     */
    public DataType getDataType() {
        return dataType;
    }

    /**
     * Get error condition.
     * 
     * @return error condition, or <code>null</code> if not a data error
     */
    public ErrorCondition getErrorCondition() {
        return errorCondition;
    }
}