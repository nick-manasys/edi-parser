
package com.anypoint.df.edi.lexical;

import java.nio.charset.Charset;

/**
 * TODO
 *
 */
public final class EdiConstants
{
    private EdiConstants() {}
    
    // standard character sets
    public static final Charset ASCII_CHARSET = Charset.forName("US-ASCII");
    public static final Charset UTF8_CHARSET = Charset.forName("UTF-8");
    
    /** Maximum year number accepted (otherwise wrapped to previous century). */
    public static final int maximumYear = 2070;
    
    /** Token delimiter types. */
    public enum ItemType {  SEGMENT, DATA_ELEMENT, QUALIFIER, REPETITION, END }
}
