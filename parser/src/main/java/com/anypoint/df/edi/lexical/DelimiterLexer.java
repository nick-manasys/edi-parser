package com.anypoint.df.edi.lexical;

import java.io.IOException;
import java.io.InputStream;

import org.apache.log4j.Logger;

import com.anypoint.df.edi.lexical.EdiConstants.ItemType;

/**
 * Base EDI token scanner. The scanner supplies input tokens to consumers along with token delimiter types, with three
 * properties exposed: the delimiter at the start of the current input token ({@link #currentType}), the delimiter at
 * the end of the current input token ({@link #nextType}), and the actual current token ({@link LexerBase#token()}).
 * Various typed parseXXX methods work with the current token as typed data.
 */
public abstract class DelimiterLexer extends LexerBase
{
    protected final Logger logger = Logger.getLogger(getClass());
    
    /** Allowed character set for string data (<code>null</code> if unrestricted). */
    boolean[] allowedChars;
    
    /** Data element delimiter. */
    char dataSeparator;
    
    /** Repeated element delimiter (-1 if unused). */
    int repetitionSeparator;
    
    /** Component delimiter. */
    char componentSeparator;
    
    /** Sub-component delimiter (-1 if unused). */
    int subCompSeparator;
    
    /** Release character (-1 if unused). */
    int releaseIndicator;
    
    /** Segment terminator. */
    char segmentTerminator;
    
    /** Next token (empty if not yet scanned). */
    StringBuilder peekToken;
    
    /** Repetition number (from start of data element). */
    private int repetitionNumber;
    
    /** Component number (from start of composite). */
    private int componentNumber;
    
    /** Component number (from start of nested composite). */
    private int subCompNumber;
    
    /** Type of next token (ending delimiter of current token). */
    private ItemType nextType;
    
    /** Type of token following peeked token (ending delimiter of peek token, <code>null</code> if not yet scanned). */
    private ItemType peekType;
    
    /**
     * Constructor.
     *
     * @param is input
     * @param altmark alternative decimal mark (-1 if unused)
     */
    public DelimiterLexer(InputStream is, int altmark) {
    	super(is, altmark);
        repetitionSeparator = -1;
        subCompSeparator = -1;
        releaseIndicator = -1;
        peekToken = new StringBuilder();
    }
    
    /**
     * Get data separator character.
     *
     * @return separator
     */
    public char getDataSeparator() {
        return dataSeparator;
    }
    
    /**
     * Get repetition character.
     *
     * @return repetition character, or -1 if none
     */
    public int getRepetitionSeparator() {
        return repetitionSeparator;
    }
    
    /**
     * Get component separator character.
     *
     * @return separator
     */
    public char getComponentSeparator() {
        return componentSeparator;
    }
    
    /**
     * Get release (escape) character.
     *
     * @return release character, or -1 if none
     */
    public int getReleaseIndicator() {
        return releaseIndicator;
    }
    
    /**
     * Get segment terminator character.
     *
     * @return terminator
     */
    public char getSegmentTerminator() {
        return segmentTerminator;
    }
    
    /**
     * Get data element repetition number.
     *
     * @return number
     */
    public int getRepetitionNumber() {
        return repetitionNumber;
    }
    
    /**
     * Get component number within composite.
     *
     * @return number
     */
    public int getComponentNumber() {
        return componentNumber;
    }
    
    /**
     * Get subcomponent number within nested composite.
     *
     * @return number
     */
    public int getSubComponentNumber() {
        return subCompNumber;
    }
    
    /**
     * Check if current token is non-empty.
     * 
     * @return <code>true</code> if non-empty, <code>false</code> if empty
     */
    public boolean hasData() {
    	return tokenBuilder.length() > 0;
    }
    
    /**
     * Get the next token type (as determined by the trailing delimiter of the current token).
     *
     * @return type
     */
    public ItemType nextType() {
        return nextType;
    }
    
    /**
     * Handle lexical error. This passes off to the configured handler, but logs the error appropriately based on the
     * result.
     *
     * @param typ data type
     * @param err error condition
     * @param explain optional supplemental explanation text (<code>null</code> if none)
     * @throws LexicalException
     */
    public void error(ValueType typ, ErrorCondition err, String explain) throws LexicalException {
        boolean abort = false;
        String position = "element " + Integer.toString(elementNumber + 1);
        if (repetitionNumber > 0) {
            position = "repetition " + Integer.toString(repetitionNumber + 1) + " of " + position;
        }
        switch (currentType) {
            case SUB_COMPONENT:
            case COMPONENT:
                position = "component " + Integer.toString(componentNumber + 1) + " of " + position;
                if (currentType == ItemType.SUB_COMPONENT) {
                    position = "subcomponent " + Integer.toString(subCompNumber + 1) + " of " + position;
                }
            default:
                break;
        }
        String text = err.text() + " for data type " + typ.typeCode() + " at " + position  + ": '" + tokenBuilder + "'";
        if (explain != null) {
            text += " (" + explain + ")";
        }
        try {
            if (errorHandler == null) {
                throw new LexicalDataException(typ, err, text);
            } else {
                errorHandler.error(typ, err, explain);
            }
        } catch (LexicalException e) {
            abort = true;
            throw e;
        } finally {
            if (abort) {
                logger.error("Unrecoverable lexer error " + text);
            } else {
                logger.info("Recoverable lexer error " + text);
            }
        }
    }
    
    /**
     * Process escape character in input.
     * @throws IOException 
     */
    abstract void handleEscape() throws IOException;
    
    /**
     * Parse token beyond the current token.
     *
     * @return token
     * @throws IOException
     */
    public void peek() throws IOException {
        if (peekType == null) {
            
            // start by skipping whitespace, if necessary
            int value = reader.read();
            if (nextType == ItemType.SEGMENT) {
                while (value == '\n' || value == '\r' || value == ' ') {
                    value = reader.read();
                }
            }
            
            // exit if now at end
            peekToken.setLength(0);
            if (value < 0) {
                peekType = ItemType.END;
            } else {
                
                // accumulate value text to next delimiter
                char chr = (char)value;
                while (true) {
                    if (chr == subCompSeparator) {
                        peekType = ItemType.SUB_COMPONENT;
                        break;
                    } else if (chr == componentSeparator) {
                        peekType = ItemType.COMPONENT;
                        break;
                    } else if (chr == dataSeparator) {
                        peekType = ItemType.DATA_ELEMENT;
                        break;
                    } else if (chr == segmentTerminator) {
                        peekType = ItemType.SEGMENT;
                        break;
                    } else if (chr == repetitionSeparator) {
                        peekType = ItemType.REPETITION;
                        break;
                    } else if (chr == releaseIndicator) {
                        handleEscape();
                    } else if (value == -1) {
                        peekType = ItemType.END;
                        break;
                    } else {
                        peekToken.append(chr);
                    }
                    value = reader.read();
                    chr = (char)value;
                }
                
            }
        }
    }
    
    /**
     * Parse next item from input and advance. This sets the current state to the pending state, and sets the new
     * pending state to the item parsed.
     *
     * @return token
     * @throws IOException
     */
    public void advance() throws IOException {
        
        // scan next token and set state
        peek();
        currentType = nextType;
        
        // update counters and state
        switch (currentType) {
        
            case DATA_ELEMENT:
                elementNumber++;
                componentNumber = 0;
                repetitionNumber = 0;
                break;
            
            case SEGMENT:
                segmentNumber++;
                segmentTag = peekToken.toString();
            case END:
                elementNumber = 0;
                componentNumber = 0;
                repetitionNumber = 0;
                break;
            
            case SUB_COMPONENT:
                subCompNumber++;
                break;
            
            case COMPONENT:
                componentNumber++;
                break;
            
            case REPETITION:
                repetitionNumber++;
                componentNumber = 0;
                break;
        
        }
        
        // advance to next token
        StringBuilder hold = tokenBuilder;
        tokenBuilder = peekToken;
        hold.setLength(0);
        peekToken = hold;
        nextType = peekType;
        peekType = null;
    }
    
    /**
     * Peek ahead at next token.
     * 
     * @return
     * @throws IOException
     */
    public String peekToken() throws IOException {
        peek();
        return  peekType == ItemType.END ? null : peekToken.toString();
    }
    
    /**
     * Advance with next token type specified. This is used by lexer implementations during initialization.
     *
     * @param type
     * @return token
     * @throws IOException
     */
    void advance(ItemType type) throws IOException {
        nextType = type;
        advance();
    }
    
    /**
     * Discard to specified item type.
     * 
     * @param typ
     * @throws IOException
     */
    @Override
    public void discardTo(ItemType typ) throws IOException {
        advance();
        while (currentType != typ && currentType != ItemType.END) {
            advance();
        }
    }
}