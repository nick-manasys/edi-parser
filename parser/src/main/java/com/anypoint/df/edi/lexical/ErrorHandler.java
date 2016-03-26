package com.anypoint.df.edi.lexical;

/**
 * Handler for errors in lexical scanning of an EDI message.
 */
public interface ErrorHandler
{
    /** Error conditions reported by lexer. */
    enum ErrorCondition
    {
        // both input and output
        TOO_SHORT("less than minimum length"),
        TOO_LONG("more than maximum length"),
        INVALID_CHARACTER("invalid character for data type"),
        INVALID_CODE("invalid code value"),
        INVALID_DATE("invalid date"),
        INVALID_TIME("invalid time"),
        INVALID_FORMAT("invalid format for data type"),
        // output only
        WRONG_TYPE("unsupported value class"),
        INVALID_VALUE("value not allowed");
        
        private final String errorText;
        
        private ErrorCondition(String text) {
            errorText = text;
        }
        
        public String text() {
            return errorText;
        }
    }
    
    /**
     * Handle a lexical error. The implementation chooses whether to continue scanning or force an abort by throwing an
     * exception.
     * 
     * @param typ value type
     * @param err error condition
     * @param text error explanation
     * @throws LexicalException
     */
    void error(TypeFormat typ, ErrorCondition err, String text) throws LexicalException;
}