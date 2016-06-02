package com.mulesoft.flatfile.lexical;

public final class TypeFormatConstants
{
    public enum GenericType
    {
        STRING, INTEGER, REAL, DATE, DATETIME, BOOLEAN
    }
    public enum FillMode
    {
        LEFT("Left justify", false), RIGHT("Right justify", false), NONE("None", false), ZEROES("Zero filled", true);
        
        private final String textName;
        private final boolean numberOnly;
        
        private FillMode(String text, boolean number) {
            textName = text;
            numberOnly = number;
        }
        
        public boolean numberOnly() {
            return numberOnly;
        }
        
        public String toString() {
            return textName;
        }
    }
    
    public enum NumberSign
    {
        UNSIGNED("Unsigned", false, false, false, false),
        NEGATIVE_ONLY("Only if negative", true, false, false, false),
        OPTIONAL("Optional leading sign", true, true, false, false),
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

    private TypeFormatConstants() {}
}
