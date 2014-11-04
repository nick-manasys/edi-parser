
package com.anypoint.df.edi.lexical;

import java.io.IOException;

/**
 * Exception caused by error in parsing EDI data.
 * TODO: add details of error position in input
 */
public class ParseException extends IOException
{
    /**
     * Constructor.
     *
     * @param msg
     */
    public ParseException(String msg) {
        super(msg);
    }
}
