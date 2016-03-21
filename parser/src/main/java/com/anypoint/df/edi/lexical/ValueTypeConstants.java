package com.anypoint.df.edi.lexical;

public final class ValueTypeConstants
{
    public enum StringSpaceFill { LEFT, RIGHT, NONE }
    
    public enum NumberSignType {
        UNSIGNED(false, false), NEGATIVE_ONLY(true, false), OPTIONAL(true, true), FORCED(true, true);
        
        private final boolean leadMinus;
        private final boolean leadPlus;
        
        NumberSignType(boolean minus, boolean plus) {
            leadMinus = minus;
            leadPlus = plus;
        }
        
        public boolean acceptLeadMinus() {
            return leadMinus;
        }
        
        public boolean acceptLeadPlus() {
            return leadPlus;
        }
    }

    public enum NumberPadType {
        UNPADDED, SPACE_LEFT, SPACE_RIGHT, ZEROES
    }

    private ValueTypeConstants() {}
}
