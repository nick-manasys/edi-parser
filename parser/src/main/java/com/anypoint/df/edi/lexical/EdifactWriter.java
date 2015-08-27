
package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.EdifactConstants.charNonBlank;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Map;

import com.anypoint.df.edi.lexical.EdifactConstants.SyntaxIdentifier;
import com.anypoint.df.edi.lexical.EdifactConstants.SyntaxVersion;

/**
 * Writer variation for EDIFACT.
 */
public class EdifactWriter extends WriterBase
{
    private final boolean needUna;
    private final String overrideDelimiters;
    
    /**
     * Constructor.
     *
     * @param os output
     * @param encoding override character set encoding (<code>null</code> if none)
     * @param version syntax version
     * @param syntax syntax identifier
     * @param enforce character set restrictions for syntax level UNOA and UNOB enforced flag
     * @param delims override delimiters (<code>null</code> if none)
     * @param segsep inter-segment separator (following segment terminator; <code>null</code> if none)
     * @param subst substitution character for invalid character in string (-1 if unused)
     * @param mark decimal mark character
     */
    public EdifactWriter(OutputStream os, Charset encoding, SyntaxVersion version, SyntaxIdentifier syntax,
        boolean enforce, String delims, String segsep, int subst, char mark) {
        super(os, encodingOrDefault(encoding, syntax), delimsOrDefault(delims, version, syntax).charAt(0),
            delimsOrDefault(delims, version, syntax).charAt(1), -1,
            charNonBlank(delimsOrDefault(delims, version, syntax).charAt(2)),
            delimsOrDefault(delims, version, syntax).charAt(3), segsep,
            charNonBlank(delimsOrDefault(delims, version, syntax).charAt(4)), subst, mark,
            enforce ? syntax.flags() : null);
        String dfltdelims = version.defaultDelimiters(syntax);
        needUna = delims != null && !delims.equals(dfltdelims);
        overrideDelimiters = delims;
    }
    
    /**
     * Get character encoding to be used.
     * 
     * @param encoding override encoding
     * @param syntax syntax identifier
     * @return encoding
     */
    private static Charset encodingOrDefault(Charset encoding, SyntaxIdentifier syntax) {
        if (encoding != null) {
            return encoding;
        }
        return syntax.defaultCharSet();
    }
    
    /**
     * Get delimiters to be used.
     * 
     * @param delims override delimiters
     * @param version syntax version
     * @param syntax syntax identifier
     * @return delimiters (data, sub, rep, seg, rel)
     */
    private static String delimsOrDefault(String delims, SyntaxVersion version, SyntaxIdentifier syntax) {
        if (delims != null) {
            return delims;
        }
        return version.defaultDelimiters(syntax);
    }

    /**
     * @param props
     * @throws IOException 
     * @see com.anypoint.df.edi.lexical.WriterBase#init(java.util.Map)
     */
    public void init(Map<String, Object> props) throws IOException {
        
        // write UNA if needed
        if (needUna || Boolean.TRUE.equals(props.get(EdifactConstants.FORCE_UNA))) {
            writer.write("UNA");
            writer.write(overrideDelimiters.charAt(1));
            writer.write(overrideDelimiters.charAt(0));
            writer.write(decimalMark);
            writer.write(overrideDelimiters.charAt(4));
            writer.write(overrideDelimiters.charAt(2));
            writer.write(overrideDelimiters.charAt(3));
            if (segmentSeparator != null) {
                writer.write(segmentSeparator);
            }
        }
        groupCount = 0;
    }

    /**
     * @param props
     * @throws IOException
     * @see com.anypoint.df.edi.lexical.WriterBase#term(java.util.Map)
     */
    public void term(Map<String, Object> props) throws IOException {
        // unused, to be eliminated
    }
}