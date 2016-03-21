
package com.anypoint.df.edi.lexical;

import java.io.IOException;

/**
 * Exception caused by error in parsing or writing EDI.
 */
public class LexicalException extends IOException
{
    /**
     * Constructor.
     *
     * @param msg
     */
    public LexicalException(String msg) {
        super(msg);
    }
    
    /**
     * Constructor.
     *
     * @param msg
     * @param t
     */
    public LexicalException(String msg, Throwable t) {
        super(msg, t);
    }
}
