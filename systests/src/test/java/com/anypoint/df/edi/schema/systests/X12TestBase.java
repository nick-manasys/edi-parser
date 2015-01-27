package com.anypoint.df.edi.schema.systests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.anypoint.df.edi.schema.tools.DocumentTest;

public abstract class X12TestBase extends TestBase {
    
    /**
     * Mask variable values in an ISA segment by replacing them with 'X' characters.
     * 
     * @param offset
     * @param datasep
     * @param message
     * @return masked
     */
    protected String maskIsaVariableValues(int offset, char datasep, String message) {
        String text = message;
        int isaDate = nthOffset(datasep, offset, 9, text);
        int isaTime = nthOffset(datasep, isaDate, 1, text);
        int isaLimit = nthOffset(datasep, isaTime, 1, text);
        text = replaceRange('X', isaDate + 1, isaTime, text);
        text = replaceRange('X', isaTime + 1, isaLimit, text);
        int isaControlNum = nthOffset(datasep, isaLimit, 2, text);
        int isaAckRequested = nthOffset('*', isaControlNum, 1, text);
        text = replaceRange('X', isaControlNum + 1, isaAckRequested, text);
        return replaceRange('X', isaAckRequested + 1, isaAckRequested + 2, text);
    }
    
    /**
     * Mask variable values in a GS segment by replacing them with 'X' characters, or in the case of the
     * variable-length group control number just deleting the value.
     * 
     * @param offset
     * @param datasep
     * @param message
     * @return masked
     */
    protected String maskGsVariableValues(int offset, char datasep, String message) {
        String text = message;
        int gsDate = nthOffset(datasep, offset, 4, text);
        int gsTime = nthOffset(datasep, gsDate, 1, text);
        int gsControlNum = nthOffset(datasep, gsTime, 1, text);
        text = replaceRange('X', gsDate + 1, gsTime, text);
        text = replaceRange('X', gsTime + 1, gsControlNum, text);
        int numEnd = nthOffset(datasep, gsControlNum, 1, text);
        return text.substring(0, gsControlNum + 1) + text.substring(numEnd);
    }
    
    /**
     * Mask variable values in a segment by deleting a particular data value (for ST, SE, GE, and IEA segments).
     * 
     * @param offset
     * @param datasep
     * @param message
     * @return masked
     */
    protected String maskNthValue(int offset, int count, char datasep, String message) {
        String text = message;
        int controlNum = nthOffset(datasep, offset, count, text) + 1;
        int scan = controlNum;
        while (Character.isLetterOrDigit(text.charAt(scan)) || Character.isWhitespace(text.charAt(scan))) scan++;
        return text.substring(0, controlNum) + text.substring(scan);
    }
    
    /**
     * Prepare segments for comparison. This returns a list of segments with all the variable values masked out.
     * 
     * @param segterm
     * @param datasep
     * @param message
     * @return segs
     */
    protected List<String> prepareSegments(char segterm, char datasep, String message) {
        List<String> segs = new ArrayList<>();
        int base = 0;
        int split;
        while ((split = message.indexOf(segterm, base)) > 0) {
            String seg = message.substring(base, split + 1);
            if (seg.startsWith("ISA" + datasep)) {
                seg = maskIsaVariableValues(0, datasep, seg);
            } else if (seg.startsWith("GS" + datasep)) {
                seg = maskGsVariableValues(0, datasep, seg);
            } else if (seg.startsWith("ST" + datasep) || seg.startsWith("SE" + datasep) |
                seg.startsWith("GE" + datasep) | seg.startsWith("IEA" + datasep)) {
                seg = maskNthValue(0, 2, datasep, seg);
            }
            segs.add(seg);
            base = split + 1;
            while (base < message.length() && Character.isWhitespace(message.charAt(base))) base++;
        }
        return segs;
    }
    
    /**
     * Parse a document, write out the parsed data as a new document, and compare the original and the written document.
     * 
     * @param path
     * @throws IOException
     */
    protected void parseAndCheckWrite(String path) throws IOException {
        DocumentTest test = new DocumentTest(schema);
        String text = readAsString(path);
        Map<String, Object> result = test.parse(new ByteArrayInputStream(text.getBytes("ASCII")));
        String output = test.printDoc(result);
        List<String> segsin = prepareSegments('~', '*', text);
        List<String> segsout = prepareSegments('~', '*', output);
        Iterator<String> iterin = segsin.iterator();
        Iterator<String> iterout = segsout.iterator();
        while (iterin.hasNext() && iterout.hasNext()) {
            String is = iterin.next();
            String os = iterout.next();
            assertEquals(is, os);
        }
        assertFalse(iterin.hasNext());
        assertFalse(iterout.hasNext());
    }
    
    /**
     * Replace date/time fields in an acknowledgment with X characters, allowing it to be compared with a static string.
     * 
     * @param ack
     * @return replaced
     */
    protected String stripAckDates(String ack) {
        String working = maskIsaVariableValues(0, '*', ack);
        int scan = 0;
        while ((scan = working.indexOf("~GS*", scan)) > 0) {
            scan++;
            working = maskGsVariableValues(scan, '*', working);
        }
        return working;
    }
    
    /**
     * Parse a document, then write out and return the generated 997 acknowledgment.
     * 
     * @param path
     * @throws IOException
     */
    protected String parseAndReturnAck(String path) {
        DocumentTest test = new DocumentTest(schema);
        InputStream is = BiztalkTest.class.getResourceAsStream(path);
        try {
        	Map<String, Object> result = test.parse(is);
        	return test.printAck(result);
        } catch (Exception e ){
        	return e.getMessage(); 
        }
        
    }    
    
    
    
}
