package com.mulesoft.flatfile.lexical;

import java.io.IOException;

/**
 * Data type definition. Each data type defines the handling of a value of that type
 */
public interface TypeFormat
{
    /**
     * Type code used in schemas.
     * 
     * @return
     */
    String typeCode();
    
    /**
     * Convert input text to value.
     * 
     * @param lexer
     * @return
     * @throws LexicalException 
     */
    Object parse(LexerBase lexer) throws LexicalException;
    
    /**
     * Write value to output.
     * 
     * @param value
     * @param writer
     * @throws IOException 
     */
    void write(Object value, WriterBase writer) throws IOException;
    
    /**
     * Get minimum length of value.
     * 
     * @return
     */
    int minLength();
    
    /**
     * Get maximum length of value.
     * 
     * @return
     */
    int maxLength();
}
