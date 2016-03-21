package com.anypoint.df.edi.lexical.types;

import com.anypoint.df.edi.lexical.LexerBase;
import com.anypoint.df.edi.lexical.ValueTypeConstants.StringSpaceFill;
import com.anypoint.df.edi.lexical.WriterBase;

/**
 * String with no content restrictions aside from those imposed by the parser/writer.
 */
public class GeneralStringValue extends StringValueBase {
    
    public GeneralStringValue(String code, int min, int max, StringSpaceFill fill) {
        super(code, min, max, fill);
    }

    @Override
    public void validate(LexerBase lexer) {
    }

    @Override
    public String validate(String token, WriterBase writer) {
        return token;
    }
}
