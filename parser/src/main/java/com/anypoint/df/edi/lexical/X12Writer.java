
package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.X12Constants.ACK_REQUESTED;
import static com.anypoint.df.edi.lexical.X12Constants.AUTHORIZATION_INFO;
import static com.anypoint.df.edi.lexical.X12Constants.AUTHORIZATION_QUALIFIER;
import static com.anypoint.df.edi.lexical.X12Constants.INTER_CONTROL;
import static com.anypoint.df.edi.lexical.X12Constants.RECEIVER_ID;
import static com.anypoint.df.edi.lexical.X12Constants.RECEIVER_ID_QUALIFIER;
import static com.anypoint.df.edi.lexical.X12Constants.SECURITY_INFO;
import static com.anypoint.df.edi.lexical.X12Constants.SECURITY_QUALIFIER;
import static com.anypoint.df.edi.lexical.X12Constants.SENDER_ID;
import static com.anypoint.df.edi.lexical.X12Constants.SENDER_ID_QUALIFIER;
import static com.anypoint.df.edi.lexical.X12Constants.TEST_INDICATOR;
import static com.anypoint.df.edi.lexical.X12Constants.VERSION_ID;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Map;

import com.anypoint.df.edi.lexical.X12Constants.CharacterSet;

/**
 * Writer variation for X12.
 */
public class X12Writer extends WriterBase
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
        String segsep, int subst, CharacterSet chset) {
        super(os, encoding, datasep, subsep, repsep, segterm, segsep, -1, subst, chset.flags());
    }
    
    /**
     * Get required property value, throwing an exception if the value is not defined.
     *
     * @param key
     * @param props
     * @return value
     * @throws WriteException
     */
    private static Object getRequired(String key, Map<String, Object> props) throws WriteException {
        Object value = props.get(key);
        if (value == null) {
            throw new WriteException("missing required property value '" + key + "'");
        }
        return value;
    }
    
    /**
     * Write property value as alphanumeric token.
     *
     * @param key
     * @param props
     * @param dflt default value (<code>null</code> if none)
     * @param minl minimum length
     * @param maxl maximum length
     * @throws IOException 
     */
    private void writeProperty(String key, Map<String, Object> props, String dflt, int minl, int maxl)
        throws IOException {
        String text;
        if (dflt == null) {
            writeAlphaNumeric(getRequired(key, props).toString(), minl, maxl);
        } else {
            text = (String)props.get(key);
            if (text == null) {
                text = dflt;
            }
            writeAlphaNumeric(text, minl, maxl);
        }
        writeDataSeparator();
    }

    /**
     * Write the interchange control number value from properties.
     *
     * @param props
     * @throws IOException
     */
    private void writeInterchangeControlNumber(Map<String, Object> props) throws IOException {
        Object version = getRequired(INTER_CONTROL, props);
        if (!(version instanceof Integer)) {
            throw new WriteException("expected Integer for property value '" + INTER_CONTROL + "'");
        }
        writeInt(((Integer)version).intValue(), 9, 9);
    }

    /**
     * @param props
     * @throws IOException 
     * @see com.anypoint.df.edi.lexical.WriterBase#init(java.util.Map)
     */
    public void init(Map<String, Object> props) throws IOException {
        
        // write the segment tag
        writer.write("ISA");
        writeDataSeparator();
        
        // write interchange properties
        writeProperty(AUTHORIZATION_QUALIFIER, props, "00", 2, 2);
        writeProperty(AUTHORIZATION_INFO, props, "", 10, 10);
        writeProperty(SECURITY_QUALIFIER, props, "00", 2, 2);
        writeProperty(SECURITY_INFO, props, "", 10, 10);
        writeProperty(SENDER_ID_QUALIFIER, props, "00", 2, 2);
        writeProperty(SENDER_ID, props, null, 15, 15);
        writeProperty(RECEIVER_ID_QUALIFIER, props, "00", 2, 2);
        writeProperty(RECEIVER_ID, props, null, 15, 15);
        Calendar calendar = new GregorianCalendar();
        writeDate(calendar, 6, 6);
        writeDataSeparator();
        int time = (calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000;
        writeTime(time, 4, 4);
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
        writeAlphaNumeric(version.toString(), 5, 5);
        writeDataSeparator();
        writeInterchangeControlNumber(props);
        writeDataSeparator();
        writeProperty(ACK_REQUESTED, props, "1", 1, 1);
        writeProperty(TEST_INDICATOR, props, "P", 1, 1);
        writeSubDelimiter();
        writeSegmentTerminator();
        
        groupCount = 0;
    }

    /**
     * @param props
     * @throws IOException
     * @see com.anypoint.df.edi.lexical.WriterBase#term(java.util.Map)
     */
    public void term(Map<String, Object> props) throws IOException {
        writer.write("IEA");
        writeDataSeparator();
        writeInt(groupCount, 1, 5);
        writeDataSeparator();
        writeInterchangeControlNumber(props);
        writeSegmentTerminator();
    }
}