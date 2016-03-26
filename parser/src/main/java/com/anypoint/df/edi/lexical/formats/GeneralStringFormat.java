package com.anypoint.df.edi.lexical.formats;

import com.anypoint.df.edi.lexical.LexerBase;
import com.anypoint.df.edi.lexical.LexicalException;
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;
import com.anypoint.df.edi.lexical.TypeFormatConstants.StringSpaceFill;
import com.anypoint.df.edi.lexical.WriterBase;

/**
 * String with no content restrictions aside from those imposed by the parser/writer.
 */
public class GeneralStringFormat extends StringFormatBase {
    
    public GeneralStringFormat(String code, int min, int max, StringSpaceFill fill) {
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
