
package com.mulesoft.flatfile.lexical;

import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition;

/**
 * Exception caused by error in EDI data.
 */
public class LexicalDataException extends LexicalException
{
    private final TypeFormat dataType;
    private final ErrorCondition errorCondition;
    
    /**
     * Constructor.
     * 
     * @param typ expected value type
     * @param err error condition
     * @param msg description
     */
    public LexicalDataException(TypeFormat typ, ErrorCondition err, String msg) {
        super(msg);
        dataType = typ;
        errorCondition = err;
    }

    /**
     * Get data type.
     * 
     * @return data type, or <code>null</code> if not a data error
     */
    public TypeFormat getValueType() {
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