
package com.anypoint.df.edi.lexical;

import java.io.IOException;

import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;

/**
 * Exception caused by error in parsing EDI data.
 */
public class LexicalException extends IOException
{
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
