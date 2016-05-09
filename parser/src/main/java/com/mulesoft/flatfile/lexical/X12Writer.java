
package com.mulesoft.flatfile.lexical;

import static com.mulesoft.flatfile.lexical.X12Constants.*;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Map;

import com.mulesoft.flatfile.lexical.X12Constants.CharacterRestriction;

/**
 * Writer variation for X12.
 */
public class X12Writer extends DelimiterWriter
{
    /**
     * Constructor.
     *
     * @param os output
     * @param encoding character set encoding
     * @param datasep data separator character
     * @param subsep sub-element separator character
     * @param repsep repetition separator character (-1 if unused)
     * @param segterm segment terminator character
     * @param segsep inter-segment separator (following segment terminator; <code>null</code> if none)
     * @param subst substitution character for invalid character in string (-1 if unused)
     * @param chset character set selection
     */
    public X12Writer(OutputStream os, Charset encoding, char datasep, char subsep, int repsep, char segterm,
        String segsep, int subst, CharacterRestriction chset) {
        super(os, encoding, datasep, subsep, -1, repsep, segterm, segsep, -1, subst, '.', chset.flags());
    }
    
    /**
     * Convert escaped character.
     * 
     * @param chr
     * @return escape
     * @throws WriteException 
     */
    String convertEscape(char chr) throws WriteException {
        throw new WriteException("unsupported character in data " + chr);
    }

    /**
     * Write the interchange control number value from properties.
     *
     * @param props
     * @throws IOException
     */
    private void writeInterchangeControlNumber(Map<String, Object> props) throws IOException {
        X12Constants.VALN9.write(getRequired(INTER_CONTROL, props), this);
    }

    /**
     * @param props
     * @throws IOException 
     * @see com.mulesoft.flatfile.lexical.WriterBase#init(java.util.Map)
     */
    public void init(Map<String, Object> props) throws IOException {
        
        // write the segment tag
        writer.write("ISA");
        writeDataSeparator();
        
        // write interchange properties
        writeProperty(AUTHORIZATION_QUALIFIER, props, "00", X12Constants.VALID2);
        writeProperty(AUTHORIZATION_INFO, props, "", X12Constants.VALAN10);
        writeProperty(SECURITY_QUALIFIER, props, "00", X12Constants.VALID2);
        writeProperty(SECURITY_INFO, props, "", X12Constants.VALAN10);
        writeProperty(SENDER_ID_QUALIFIER, props, "00", X12Constants.VALID2);
        writeProperty(SENDER_ID, props, null, X12Constants.VALAN15);
        writeProperty(RECEIVER_ID_QUALIFIER, props, "00", X12Constants.VALID2);
        writeProperty(RECEIVER_ID, props, null, X12Constants.VALAN15);
        Calendar calendar = (Calendar)props.get(INTERCHANGE_DATE);
        if (calendar == null) {
            calendar = new GregorianCalendar();
        }
        X12Constants.VALDT6.write(calendar, this);
        writeDataSeparator();
        Integer time = (Integer)props.get(INTERCHANGE_TIME);
        int millis;
        if (time == null) {
            millis = (calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000;
        } else {
            millis = time.intValue();
        }
        X12Constants.VALTM4.write(Integer.valueOf(millis), this);
        writeDataSeparator();
        Object version = getRequired(VERSION_ID, props);
        if (!(version instanceof String)) {
            throw new WriteException("expected String value for property '" + VERSION_ID + "'");
        }
        if ("00402".compareTo(version.toString()) <= 0) {
            writer.write(repetitionSeparator > 0 ? repetitionSeparator : 'U');
        } else {
            writer.write('U');
        }
        writeDataSeparator();
        X12Constants.VALID5.write(version.toString(), this);
        writeDataSeparator();
        writeInterchangeControlNumber(props);
        writeDataSeparator();
        writeProperty(ACK_REQUESTED, props, "1", X12Constants.VALID1);
        writeProperty(TEST_INDICATOR, props, "P", X12Constants.VALID1);
        writeComponentSeparator();
        writeSegmentTerminator();
        groupCount = 0;
    }

    /**
     * @param props
     * @throws IOException
     * @see com.mulesoft.flatfile.lexical.WriterBase#term(java.util.Map)
     */
    public void term(Map<String, Object> props) throws IOException {
        writeToken("IEA");
        writeDataSeparator();
        X12Constants.VALN1_5.write(Integer.valueOf(groupCount), this);
        writeDataSeparator();
        writeInterchangeControlNumber(props);
        writeSegmentTerminator();
    }
}