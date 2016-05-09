package com.mulesoft.flatfile.lexical.formats;

import com.mulesoft.flatfile.lexical.LexerBase;
import com.mulesoft.flatfile.lexical.LexicalException;
import com.mulesoft.flatfile.lexical.ErrorHandler.ErrorCondition;
import com.mulesoft.flatfile.lexical.TypeFormatConstants.FillMode;
import com.mulesoft.flatfile.lexical.WriterBase;

/**
 * String with no content restrictions aside from those imposed by the parser/writer.
 */
public class GeneralStringFormat extends StringFormatBase {
    
    public GeneralStringFormat(String code, int min, int max, FillMode fill) {
        super(code, min, max, fill);
    }

    @Override
    public Object parseToken(LexerBase lexer) throws LexicalException {
        return lexer.token();
    }

    @Override
    public String buildToken(Object value, WriterBase writer) throws LexicalException {
        if (value instanceof String) {
            return (String)value;
        } else {
            writer.error(this, ErrorCondition.WRONG_TYPE, "wrong value type " + value.getClass().getName());
            return value.toString();
        }
    }
}
