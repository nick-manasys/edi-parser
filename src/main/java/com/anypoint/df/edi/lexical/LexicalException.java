
package com.anypoint.df.edi.lexical;

import java.io.IOException;

/**
 * Exception caused by error in parsing EDI data.
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
}
