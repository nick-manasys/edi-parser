
package com.anypoint.df.edi.lexical;

import static com.anypoint.df.edi.lexical.X12Constants.*;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Map;

/**
 * Writer variation for X12.
 */
public class X12Writer extends WriterBase
{
    /**
     * Configure writer for use.
     *
     * @param os
     * @param encoding
     * @param datasep
     * @param subsep
     * @param repsep
     * @param segterm
     */
    public void configureX12(OutputStream os, Charset encoding, char datasep, char subsep, int repsep, char segterm) {
        configure(os, encoding, datasep, subsep, repsep, segterm, -1);
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
        Date date = (Date)props.get(INTERCHANGE_DATE);
        if (date == null) {
            date = new Date();
        }
        writeDate(date, 6, 6);
        writeDataSeparator();
        Integer value = (Integer)props.get(INTERCHANGE_TIME);
        int time;
        if (value == null) {
            GregorianCalendar calendar =  new GregorianCalendar();
            time = (calendar.get(Calendar.HOUR_OF_DAY) * 24 + calendar.get(Calendar.MINUTE)) * 60 * 1000;
        } else {
            time = ((Integer)value).intValue();
        }
        writeTime(time, 4, 4);
        writeDataSeparator();
        writer.write(repetitionSeparator > 0 ? repetitionSeparator : 'U');
        writeDataSeparator();
        writeProperty(VERSION_ID, props, "00000", 5, 5);
        writeInterchangeControlNumber(props);
        writeDataSeparator();
        writeProperty(ACK_REQUESTED, props, "0", 1, 1);
        writeProperty(TEST_INDICATOR, props, "P", 1, 1);
        writeSubDelimiter();
        writeSegmentTerminator();
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
        writer.close();
    }
}