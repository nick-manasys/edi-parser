package com.mulesoft.flatfile.lexical.formats;

import java.io.IOException;

import com.mulesoft.flatfile.lexical.LexerBase;
import com.mulesoft.flatfile.lexical.LexicalException;
import com.mulesoft.flatfile.lexical.TypeFormatConstants.FillMode;
import com.mulesoft.flatfile.lexical.WriterBase;

/**
 * Base class for objects formatted as simple strings, with optional space padding to minimum length. Values are written
 * using checks for output reserved characters, if any.
 */
public abstract class StringFormatBase extends TypeFormatBase {
    
    protected final FillMode spaceFill;
    
    public StringFormatBase(String code, int min, int max, FillMode fill) {
        super(code, min, max);
        spaceFill = fill;
        if (spaceFill.numberOnly()) {
            throw new IllegalArgumentException("Numeric fill option not supported");
        }
    }

    /**
     * Validate characters and parse current lexer token.
     * 
     * @param lexer
     * @return value
     * @throws LexicalException 
     */
    public abstract Object parseToken(LexerBase lexer) throws LexicalException;
    
    /**
     * Validate characters and generate output token, modifying characters if necessary.
     * 
     * @param value
     * @param writer
     */
    public abstract String buildToken(Object value,  WriterBase writer) throws LexicalException;

    @Override
    public Object parse(LexerBase lexer) throws LexicalException {
        checkInputLength(lexer);
        switch (spaceFill) {
            case LEFT:
                stripSpaceRight(lexer.tokenBuilder());
                break;
            case RIGHT:
                stripSpaceLeft(lexer.tokenBuilder());
                break;
            case NONE:
                break;
            default:
                throw new IllegalStateException("Invalid string format fill setting");
        }
        return parseToken(lexer);
    }

    @Override
    public void write(Object value, WriterBase writer) throws IOException {
        String text = buildToken(value, writer);
        writer.startToken();
        switch (spaceFill) {
            case LEFT:
                text = checkOutputMaxLength(text, writer);
                writer.writeEscaped(text);
                writePadding(minLength - text.length(), SPACES, writer);
                break;
            case RIGHT:
                text = checkOutputMaxLength(text, writer);
                writePadding(minLength - text.length(), SPACES, writer);
                writer.writeEscaped(text);
                break;
            case NONE:
                text = checkOutputLength(text, writer);
                writer.writeEscaped(text);
                break;
            default:
                throw new IllegalStateException("Invalid string format fill setting");
        }
    }
}
