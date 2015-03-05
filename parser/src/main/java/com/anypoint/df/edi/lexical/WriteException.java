
package com.anypoint.df.edi.lexical;

import java.io.IOException;

/**
 * Exception caused by error in writing EDI data.
 */
public class WriteException extends IOException
{
    /**
     * Constructor.
     *
     * @param msg
     */
    public WriteException(String msg) {
        super(msg);
    }
    
    /**
     * Constructor.
     *
     * @param msg
     * @param t
     */
    public WriteException(String msg, Throwable t) {
        super(msg, t);
    }
}
