
package com.anypoint.df.edi.lexical;

import java.nio.charset.Charset;

/**
 * Constants used for all EDI variations.
 */
public final class EdiConstants
{
    private EdiConstants() {}
    
    // standard character sets
    public static final Charset ASCII_CHARSET = Charset.forName("US-ASCII");
    public static final Charset ISO88591_CHARSET = Charset.forName("ISO-8859-1");
    
    /** Maximum year number accepted (otherwise wrapped to previous century). */
    public static final int maximumYear = 2070;
    
    /**
     * Token delimiter types. The types used for separating values are explicitly ordered by granularity, so that the
     * next finer level of granularity can easily be found.
     */
    public enum ItemType {
        SEGMENT, END, REPETITION, DATA_ELEMENT, COMPONENT, SUB_COMPONENT;
        
        /**
         * Get next finer level of granularity for separators.
         * 
         * @return
         */
        public ItemType nextLevel() {
            if (ordinal() < DATA_ELEMENT.ordinal()) {
                throw new IllegalStateException("No granularity defined for item type " + toString());
            }
            if (ordinal() == values().length - 1) {
                return this;
            }
            return values()[ordinal() + 1];
        }
    }
    
    /**
     * Set flags for range of characters in array.
     *
     * @param from
     * @param to
     * @param flags
     */
    protected static void fillChars(char from, char to, boolean[] flags) {
        for (int i = from; i <= to; i++) {
            flags[i] = true;
        }
    }
    
    /**
     * Set flags for specific characters in array.
     *
     * @param chars
     * @param flags
     */
    protected static void setChars(char[] chars, boolean[] flags) {
        for (int i = 0; i < chars.length; i++) {
            flags[chars[i]] = true;
        }
    }
}