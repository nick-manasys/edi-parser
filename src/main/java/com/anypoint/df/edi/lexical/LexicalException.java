
package com.anypoint.df.edi.lexical;

import java.io.IOException;

/**
 * Exception caused by error in parsing EDI data.
 */
public class LexicalException extends IOException
{
    public enum ErrorCondition { TOO_SHORT, TOO_LONG, INVALID_CHARACTER, INVALID_CODE, INVALID_DATE, INVALID_TIME }
    
    /**
     * Constructor.
     *
     * @param condition
     * @param msg
     */
    public LexicalException(ErrorCondition condition, String msg) {
        super(msg);
    }
}
