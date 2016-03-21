package com.anypoint.df.edi.lexical.types;

import java.io.IOException;

import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;
import com.anypoint.df.edi.lexical.LexerBase;
import com.anypoint.df.edi.lexical.LexicalException;
import com.anypoint.df.edi.lexical.ValueTypeConstants.StringSpaceFill;
import com.anypoint.df.edi.lexical.WriterBase;

/**
 * Base class for simple strings, with optional space padding to minimum length. Values are written using checks for
 * output reserved characters, if any.
 */
public abstract class StringValueBase extends ValueTypeBase {
    
    protected final StringSpaceFill spaceFill;
    
    public StringValueBase(String code, int min, int max, StringSpaceFill fill) {
        super(code, min, max);
        spaceFill = fill;
    }
    
    /**
     * Validate characters in current lexer token, modifying if necessary.
     * 
     * @param lexer
     * @throws LexicalException 
     */
    public abstract void validate(LexerBase lexer) throws LexicalException;
    
    /**
     * Validate characters in output token, modifying if necessary.
     * 
     * @param token
     * @param writer
     */
    public abstract String validate(String token,  WriterBase writer) throws LexicalException;

    @Override
    public Object parse(LexerBase lexer) throws LexicalException {
        checkInputLength(lexer);
        switch (spaceFill) {
            case LEFT:
                stripSpaceLeft(lexer.tokenBuilder());
                break;
            case RIGHT:
                stripSpaceRight(lexer.tokenBuilder());
                break;
            case NONE:
        }
        validate(lexer);
        return lexer.token();
    }

    @Override
    public void write(Object value, WriterBase writer) throws IOException {
        String text;
        if (value instanceof String) {
            text = (String)value;
        } else {
            writer.error(this, ErrorCondition.WRONG_TYPE, "wrong value type " + value.getClass().getName());
            text = value.toString();
        }
        text = validate(text, writer);
        writer.startToken();
        switch (spaceFill) {
            case LEFT:
                text = checkOutputMaxLength(text, writer);
                writePadding(minLength - text.length(), SPACES, writer);
                writer.writeEscaped(text);
                break;
            case RIGHT:
                text = checkOutputMaxLength(text, writer);
                writer.writeEscaped(text);
                writePadding(minLength - text.length(), SPACES, writer);
                break;
            case NONE:
                text = checkOutputLength(text, writer);
                writer.writeEscaped(text);
        }
    }
}
