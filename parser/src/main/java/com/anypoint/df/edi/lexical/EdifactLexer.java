package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.EdiConstants.ASCII_CHARSET;
import static com.anypoint.df.edi.lexical.EdifactConstants.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.Map;

import com.anypoint.df.edi.lexical.EdiConstants.ItemType;
import com.anypoint.df.edi.lexical.EdifactConstants.SyntaxIdentifier;
import com.anypoint.df.edi.lexical.EdifactConstants.SyntaxVersion;

/**
 * Lexer variation for EDIFACT.
 */
public class EdifactLexer extends DelimiterLexer
{
    /** Status returned by {@link EDIFACT#term(Map)} method. */
    public enum InterchangeEndStatus {
        VALID, GROUP_COUNT_ERROR, CONTROL_NUMBER_ERROR
    }
    
    private final String delimiterDefaults;
    
    private boolean enforceCharacterSet;
    
    /**
     * Constructor.
     *
     * @param is input
     * @param delims default delimiter characters string (data sep, comp sep, rep sep, seg term, release),
     * <code>null</code> if using standard defaults)
     */
    public EdifactLexer(InputStream is, String delims) {
        super(is);
        componentSeparator = ':';
        dataSeparator = '+';
        releaseIndicator = '?';
        segmentTerminator = '\'';
        repetitionSeparator = '*';
        subCompSeparator = -1;
        delimiterDefaults = delims;
        altDecimalMark = ',';
    }
    
    /**
     * Configure character processing differences for partners. This is intended for use once the specifics of an
     * interchange are known.
     * 
     * @param subst substitution character for invalid character in string (-1 if unused)
     * @param enforce character set restrictions for syntax level UNOA and UNOB enforced flag
     */
    public void configure(int subst, boolean enforce) {
        substitutionChar = subst;
        enforceCharacterSet = enforce;
    }
    
    /**
     * First time interchange header read. This determines the character encoding used for all subsequent interchanges.
     * 
     * @param props
     * @return
     * @throws IOException
     */
    private SyntaxVersion firstTimeInit(Map<String,Object> props) throws IOException {
        
        // check the segment tag for optional UNA
        byte[] byts = readBytes(3);
        String tag = new String(byts, ASCII_CHARSET);
        boolean unaseen = false;
        if ("UNA".equals(tag)) {
            
            // get delimiter and other characters directly from segment
            byts = readBytes(6);
            componentSeparator = (char)byts[0];
            dataSeparator = (char)byts[1];
            releaseIndicator = (char)byts[3];
            repetitionSeparator = charNonBlank((char)byts[4]);
            segmentTerminator = (char)byts[5];
            unaseen = true;
            
            // skip any whitespace following segment
            char chr;
            while ((chr = (char)stream.read()) == '\n' || chr == '\r' || chr == ' ');
            segmentNumber++;
            
            // get the next segment tag
            byts = new byte[3];
            byts[0] = (byte)chr;
            readArray(byts, 1);
            tag = new String(byts, ASCII_CHARSET);
       }
        
        // must be at a UNB Interchange Header
        if (!"UNB".equals(tag)) {
            throw new LexicalException("Message is missing UNB segment (starts with " + tag + ")");
        }
        
        // process first part of syntax identifier (required identifier and version number)
        int ds = stream.read();
        String synid = new String(readBytes(4), ASCII_CHARSET);
        props.put(SYNTAX_IDENTIFIER, synid);
        SyntaxIdentifier syntax = EDIFACT_CHARSETS.get(synid);
        int cs = stream.read();
        SyntaxVersion version;
        int verch = stream.read();
        switch (verch) {
            case '1':
            case '2':
                version = SyntaxVersion.VERSION2;
                break;
            case '3':
                version = SyntaxVersion.VERSION3;
                break;
            case '4':
                version = SyntaxVersion.VERSION4;
                break;
            default:
                throw new LexicalException("Unsupported syntax version '" + verch + '\'');
        }
        props.put(SYNTAX_VERSION_NUMBER, Character.toString((char)verch));
        
        // make sure delimiter characters are set
        if (!unaseen) {
            String delims = delimiterDefaults == null ? version.defaultDelimiters(syntax) : delimiterDefaults;
            dataSeparator = delims.charAt(0);
            componentSeparator = delims.charAt(1);
            repetitionSeparator = charNonBlank(delims.charAt(2));
            segmentTerminator = delims.charAt(3);
            releaseIndicator = charNonBlank(delims.charAt(4));
        }
        
        // verify delimiter character used so far
        if (ds != dataSeparator) {
            throw new LexicalException("Wrong delimiter for syntax version (expected '" + dataSeparator + "', found '" + ds + '\'');
        } else if (cs != componentSeparator) {
            throw new LexicalException("Wrong delimiter for syntax version (expected '" + componentSeparator + "', found '" + cs + '\'');
        }
        
        // check for character encoding specified
        int chr = stream.read();
        String codelist = null;
        String charenc = null;
        Charset charset = syntax.defaultCharSet();
        if (version == SyntaxVersion.VERSION4 && chr == componentSeparator) {
            
            // ignore service code list directory version number
            StringBuilder builder = new StringBuilder();
            while ((chr = stream.read()) >= 0 && chr != componentSeparator && chr != dataSeparator
                && chr != segmentTerminator) {
                builder.append(chr);
            }
            codelist = builder.toString();
            props.put(SERVICE_CODE_LIST, codelist);
            if (chr == componentSeparator) {
                builder.setLength(0);
                while ((chr = stream.read()) >= 0 && chr != componentSeparator && chr != dataSeparator
                    && chr != segmentTerminator) {
                    builder.append(chr);
                }
                charenc = builder.toString();
                props.put(CHARACTER_ENCODING, charenc);
                if (charenc.length() == 1) {
                    char code = charenc.charAt(0);
                    try {
                        switch (code) {
                            case '1':
                            case '2':
                                charset = ASCII_CHARSET;
                                break;
                            case '3':
                                charset = Charset.forName("Cp1148");
                                break;
                            case '4':
                                charset = Charset.forName("Cp858");
                                break;
                            case '7':
                                charset = UTF8;
                                break;
                            case '8':
                                charset = Charset.forName("UTF-16");
                                break;
                            default:
                                throw new RuntimeException();
                        }
                    } catch (Exception e) {
                        throw new LexicalException("Unsupported character encoding '" + code + '\'', e);
                    }
                } else if (!"ZZZ".equals(charenc)) {
                    throw new LexicalException("Unknown character encoding code '" + charenc + '\'');
                }
            }
        }
        if (charset == null) {
            throw new LexicalException("Unsupported syntax identifier '" + synid + "' (character encoding not supported)");
        }
        
        // turn stream into reader with appropriate character set
        allowedChars = enforceCharacterSet ? syntax.flags() : null;
        reader = new BufferedReader(new InputStreamReader(stream, charset));
        advance(ItemType.DATA_ELEMENT);
        return version;
    }
    
    /**
     * Interchange header read after the first time.
     * 
     * @param props
     * @return
     * @throws IOException
     */
    private SyntaxVersion subsequentInit(Map<String,Object> props) throws IOException {
        
        // check the segment tag for optional UNA
        if ("UNA".equals(segmentTag())) {
            throw new LexicalException("UNA is only allowed for first interchange");
        }
        
        // must be at a UNB Interchange Header
        if (!"UNB".equals(segmentTag())) {
            throw new LexicalException("Message is missing UNB segment (starts with " + segmentTag() + ")");
        }
        
        // process first part of syntax identifier (required identifier and version number)
        advance();
        String synid = tokenBuilder.toString();
        props.put(SYNTAX_IDENTIFIER, synid);
        SyntaxIdentifier syntax = EDIFACT_CHARSETS.get(synid);
        SyntaxVersion version;
        advance();
        int verch = tokenBuilder.charAt(0);
        switch (verch) {
            case '1':
            case '2':
                version = SyntaxVersion.VERSION2;
                break;
            case '3':
                version = SyntaxVersion.VERSION3;
                break;
            case '4':
                version = SyntaxVersion.VERSION4;
                break;
            default:
                throw new LexicalException("Unsupported syntax version '" + verch + '\'');
        }
        props.put(SYNTAX_VERSION_NUMBER, Character.toString((char)verch));
        
        // skip to identification information
        while (currentType() != ItemType.DATA_ELEMENT) {
            advance();
        }
        
        // turn stream into reader with appropriate character set
        allowedChars = enforceCharacterSet ? syntax.flags() : null;
        return version;
    }
    
    /**
     * Initialize document parse. This checks the start of the document to find the separator characters used in
     * parsing, along with the character encoding. Returns with the parser positioned past the first component of the
     * UNB Interchange Header segment.
     *
     * @param props store for property values from interchange
     * @return version
     * @throws LexicalException
     */
    public SyntaxVersion init(Map<String,Object> props) throws LexicalException {
        try {
            if (reader == null) {
                return firstTimeInit(props);
            } else {
                return subsequentInit(props);
            }
        } catch (IOException e) {
            throw new LexicalException("Interchange aborted due to error reading header", e);
        }
    }

    /**
     * Finish interchange parse. The end of this interchange may be followed by another interchange, so this only parses
     * up to and including the UNZ segment terminator.
     * 
     * @param props
     * @throws IOException
     */
    public void term(Map<String, Object> props) throws IOException {
        if (!"UNZ".equals(segmentTag())) {
            throw new IllegalStateException("not at trailer");
        }
        if (nextType() == ItemType.DATA_ELEMENT) {
            advance();
            props.put(INTER_CONTROL_COUNT, parseInteger(1, 6));
            if (nextType() == ItemType.DATA_ELEMENT) {
                advance();
                props.put(INTER_CONTROL_REF, parseAlphaNumeric(0, 14));
            }
        }
    }
}