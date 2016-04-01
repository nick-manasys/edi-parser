package com.anypoint.df.edi.lexical;

public final class TypeFormatConstants
{
    public enum FillMode
    {
        LEFT("Left justify"), RIGHT("Right justify"), NONE("None"), ZEROES("Zero filled");
        
        private final String textName;
        
        private FillMode(String text) {
            textName = text;
        }
        
        public String toString() {
            return textName;
        }
    }
    
    public enum NumberSign
    {
        UNSIGNED("Unsigned", false, false, false, false),
        NEGATIVE_ONLY("Only if negative", true, false, false, false),
        OPTIONAL("Optional leading size", true, true, false, false),
        ALWAYS_LEFT("Always leading sign", true, true, true, false),
        ALWAYS_RIGHT("Always trailing sign", true, true, true, true);
        
        private final String textName;
        private final boolean useMinus;
        private final boolean acceptPlus;
        private final boolean requireSign;
        private final boolean trailingSign;
        
        NumberSign(String text, boolean minus, boolean plus, boolean require, boolean trail) {
            textName = text;
            useMinus = minus;
            acceptPlus = plus;
            requireSign = require;
            trailingSign = trail;
        }
        
        public boolean trailingSign() {
            return trailingSign;
        }
        
        public boolean forceSign() {
            return requireSign;
        }
        
        public boolean useMinus() {
            return useMinus;
        }
        
        public boolean acceptPlus() {
            return acceptPlus;
        }
        
        public String toString() {
            return textName;
        }
    }
    
    public enum BooleanRepresentation
    {
        ALPHA_LOWER("Lowercase alpha"), ALPHA_UPPER("Uppercase alpha"), NUMBER("Number (0/1)");
        
        private final String textName;
        
        private BooleanRepresentation(String text) {
            textName = text;
        }
        
        public String toString() {
            return textName;
        }
    }

    private TypeFormatConstants() {}
}
