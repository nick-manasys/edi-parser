package com.anypoint.df.edi.lexical;

import com.anypoint.df.edi.lexical.EdiConstants.DataType;

/**
 * Handler for errors in lexical scanning of an EDI message.
 */
public interface ErrorHandler
{
    /** Error conditions reported by lexer. */
    enum ErrorCondition
    {
        TOO_SHORT("less than minimum length"),
        TOO_LONG("more than maximum length"),
        INVALID_CHARACTER("invalid character for data type"),
        INVALID_CODE("invalid code value"),
        INVALID_DATE("invalid date"),
        INVALID_TIME("invalid time");
        
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
     * @param lexer
     * @param typ data type
     * @param err error condition
     * @param explain optional supplemental explanation text (<code>null</code> if none)
     * @throws LexicalException
     */
    void error(LexerBase lexer, DataType typ, ErrorCondition err, String explain) throws LexicalException;
}