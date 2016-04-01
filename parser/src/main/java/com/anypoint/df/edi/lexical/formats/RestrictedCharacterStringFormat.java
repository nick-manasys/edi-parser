package com.anypoint.df.edi.lexical.formats;

import com.anypoint.df.edi.lexical.LexerBase;
import com.anypoint.df.edi.lexical.LexicalException;
import com.anypoint.df.edi.lexical.ErrorHandler.ErrorCondition;
import com.anypoint.df.edi.lexical.TypeFormatConstants.FillMode;
import com.anypoint.df.edi.lexical.WriterBase;

/**
 * String with restricted character set. Checks that all characters of the string are flagged as valid in the supplied
 * table. If an invalid character is found and processing is not aborted by an exception from the error callback, the
 * lexer/writer in use is checked for a substitution character. If the substitution is defined, that character replaces
 * the invalid one in the data supplied to the application (lexer) or output (writer). If the character restriction is
 * open-ended character codes beyond the range of values in the table will be accepted; otherwise, characters beyond
 * the end of hte table will be rejected.
 */
public class RestrictedCharacterStringFormat extends StringFormatBase {
    
    private final boolean[] allowedChars;
    private final boolean openEnded;
    
    public RestrictedCharacterStringFormat(String code, int min, int max, FillMode fill, boolean[] allowed,
        boolean open) {
        super(code, min, max, fill);
        allowedChars = allowed;
        openEnded = open;
    }

    @Override
    public Object parseToken(LexerBase lexer) throws LexicalException {
        if (allowedChars != null) {
            StringBuilder builder = lexer.tokenBuilder();
            for (int i = 0; i < builder.length(); i++) {
                char chr = builder.charAt(i);
                boolean invalid = false;
                if (chr >= allowedChars.length) {
                    invalid = !openEnded;
                } else if (!allowedChars[chr]) {
                    invalid = true;
                }
                if (invalid) {
                    invalidCharacter(chr, lexer);
                    int subst = lexer.getSubstitutionChar();
                    if (subst > 0) {
                        builder.setCharAt(i, (char)lexer.getSubstitutionChar());
                    } else if (subst == 0) {
                        builder.deleteCharAt(i--);
                    }
                }
            }
        }
        return lexer.token();
    }

    @Override
    public String buildToken(Object value, WriterBase writer) throws LexicalException {
        if (!(value instanceof String)) {
            writer.error(this, ErrorCondition.WRONG_TYPE, "wrong value type " + value.getClass().getName());
        }
        String text = value.toString();
        if (allowedChars != null) {
            
            for (int i = 0; i < text.length(); i++) {
                char chr = text.charAt(i);
                boolean invalid = false;
                if (chr >= allowedChars.length) {
                    invalid = !openEnded;
                } else if (!allowedChars[chr]) {
                    invalid = true;
                }
                if (invalid) {
                    invalidCharacter(chr, writer);
                    int subst = writer.getSubstitutionChar();
                    if (subst > 0) {
                        text = text.substring(0, i) + (char)subst + text.substring(i + 1);
                    } else if (subst == 0) {
                        text = text.substring(0, i) + text.substring(i + 1);
                    }
                }
            }
        }
        return text;
    }
}
