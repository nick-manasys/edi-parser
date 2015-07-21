package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.anypoint.df.edi.schema.SchemaJavaValues;
import com.anypoint.df.edi.schema.EdifactIdentityInformation;
import com.anypoint.df.edi.schema.EdifactParserConfig;
import com.anypoint.df.edi.schema.EdifactSchemaDefs;
import com.anypoint.df.edi.schema.EdiSchema.*;
import com.anypoint.df.edi.schema.systests.TestBase;
import com.anypoint.df.edi.schema.systests.YamlSupport;
import com.anypoint.df.edi.schema.tools.DecodeContrl;
import com.anypoint.df.edi.schema.tools.DocumentTest;
import com.anypoint.df.edi.schema.tools.DocumentTestEdifact;

public abstract class EdifactTestBase extends TestBase {

    private static String deleteRange(int from, int to, String text) {
        return text.substring(0, from) + text.substring(to);
    }
    
    /**
     * Mask date/time variable values in a UNB segment by replacing them with 'X' characters, and delete the control
     * number, to allow comparisons.
     * 
     * @param offset
     * @param datasep
     * @param segterm
     * @param message
     * @return masked
     */
    protected String maskUnbVariableValues(int offset, char datasep, char segterm, String message) {
        String text = message;
        int unbDatetime = nthOffset(datasep, false, offset, 4, text);
        int unbControlNum = nthOffset(datasep, false, unbDatetime, 1, text);
        text = replaceRange('X', unbDatetime + 1, unbControlNum, text);
        // handle either data separator or segment terminator following the control number field
        int next = text.indexOf(datasep, unbControlNum + 1);
        int end = text.indexOf(segterm, unbControlNum + 1);
        int limit = next < 0 || next > end ? end : next;
        return deleteRange(unbControlNum + 1, limit, text);
    }

    /**
     * Mask variable values in a UNG segment by replacing them with 'X' characters.
     * 
     * @param offset
     * @param datasep
     * @param message
     * @return masked
     */
    protected String maskUngVariableValues(int offset, char datasep, String message) {
        String text = message;
        int ungDatetime = nthOffset(datasep, false, offset, 4, text);
        int ungControlNum = nthOffset(datasep, false, ungDatetime, 1, text);
        text = replaceRange('X', ungDatetime + 1, ungControlNum, text);
        int ungLimit = nthOffset(datasep, true, ungControlNum, 1, text);
        return deleteRange(ungControlNum + 1, ungLimit, text);
    }

    /**
     * Mask variable values in a segment by deleting a particular data value.
     * 
     * @param offset
     * @param datasep
     * @param message
     * @return masked
     */
    protected String maskNthValue(int offset, int count, char datasep, String message) {
        String text = message;
        int controlNum = nthOffset(datasep, true, offset, count, text) + 1;
        int scan = controlNum;
        while (Character.isLetterOrDigit(text.charAt(scan)) || Character.isWhitespace(text.charAt(scan))) {
            scan++;
        }
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
            if (seg.startsWith("UNB" + datasep)) {
                seg = maskUnbVariableValues(0, datasep, segterm, seg);
            } else if (seg.startsWith("UNG" + datasep)) {
                seg = maskUngVariableValues(0, datasep, seg);
            } else if (seg.startsWith("UNH" + datasep)) {
                seg = maskNthValue(0, 1, datasep, seg);
            } else if (seg.startsWith("UNT" + datasep) || seg.startsWith("UNE" + datasep) || seg.startsWith("UNZ" + datasep)) {
                seg = maskNthValue(0, 2, datasep, seg);
            }
            segs.add(seg);
            base = split + 1;
            while (base < message.length() && Character.isWhitespace(message.charAt(base)))
                base++;
        }
        return segs;
    }

    /**
     * Write a document from supplied map, and compare result with original document. This is normally used from
     * {@link #parseAndCheckWrite(String)}, but is provided as a separate method for when special handling is required.
     * 
     * @param test
     * @param text
     * @param result
     */
    protected void checkWrite(DocumentTest test, String text, Map<String, Object> result) {
        String output = test.printDoc(result);
        System.out.println(output);
        List<String> segsin = prepareSegments('\'', '+', text);
        List<String> segsout = prepareSegments('\'', '+', output);
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
     * Parse a document from text, write out the parsed data as a new document, and compare the original and the written
     * text.
     * 
     * @param path
     * @throws IOException
     */
    protected void parseAndCheckWrite(String text, DocumentTest test) throws IOException {
        Map<String, Object> result = test.parse(new ByteArrayInputStream(text.getBytes("ASCII")));
//        StringWriter writer = new StringWriter();
//        YamlSupport.writeMap(result, writer);
//        System.out.println(writer.toString());
        checkWrite(test, text, result);
    }

    /**
     * Parse a document using default parser configuration, write out the parsed data as a new document, and compare the
     * original and the written document.
     * 
     * @param path
     * @throws IOException
     */
    protected void parseAndCheckWrite(String path) throws IOException {
        DocumentTest test = new DocumentTestEdifact(schema);
        String text = readAsString(path);
        parseAndCheckWrite(text, test);
    }
    
    /**
     * Replace date/time fields in an acknowledgment with X characters, allowing it to be compared with a static string.
     * 
     * @param ack
     * @return replaced
     */
    protected String stripAckDates(String ack) {
        String working = maskUnbVariableValues(0, '+', '\'', ack);
        int scan = 0;
        while ((scan = working.indexOf("'UNH+", scan)) > 0) {
            scan++;
            working = maskNthValue(scan, 1, '+', working);
        }
        return working;
    }

    /**
     * Print acknowledgment information from parse result map.
     *
     * @param result
     */
    protected void printAcknowledgments(Map<String, Object> result) {
        List<Map<String, Object>> acks = (List<Map<String, Object>>)
            result.get(SchemaJavaValues.functionalAcksGenerated());
        if (acks != null) {
            Map<String, Object> inter = new HashMap<>();
            inter.put(((CompositeComponent)EdifactSchemaDefs.segUNBv4().components().head()).composite().components().apply(1).key(), "4");
            for (Map<String, Object> ack : acks) {
                ack.put(SchemaJavaValues.interchangeKey(), inter);
                System.out.println(DecodeContrl.decode(ack));
            }
        }
    }

    /**
     * Parse an input stream using supplied test configuration, then write out and return the generated CONTRL
     * acknowledgment with sender/receiver information reversed (as it would be sent back to the sender of the original
     * message).
     * 
     * @param test
     * @param is
     * @return acknowledgment
     */
    protected String parseAndReturnAck(DocumentTest test, InputStream is) {
        try {
            Map<String, Object> result = test.parse(is);
            test.prepareOutput(result);
            printAcknowledgments(result);
            return test.printAck(result);
        } catch (Exception e) {
            e.printStackTrace();
            return e.getMessage();
        }
    }

    /**
     * Parse a document from path with default parser configuration, then write out and return the generated CONTRL
     * acknowledgment with sender/receiver information reversed (as it would be sent back to the sender of the original
     * message).
     * 
     * @param path
     * @return acknowledgment
     */
    protected String parseAndReturnAck(String path) {
        InputStream is = EdifactTestBase.class.getResourceAsStream(path);
        if (is == null) {
            throw new IllegalArgumentException("File " + path + " not found");
        }
        DocumentTest test = new DocumentTestEdifact(schema);
        return parseAndReturnAck(test, is);
    }

    /**
     * Parse a document with sender identity information specified.
     * 
     * @param inputFilePath
     * @param interchangeId
     * @param interchangeQualifier
     * @return acknowledgment
     */
    protected String parseWithSenderIdentityInformation(String inputFilePath, String interchangeId,
        String interchangeQualifier) {
        EdifactIdentityInformation identity =
            new EdifactIdentityInformation(interchangeId, interchangeQualifier, null, null);
        EdifactParserConfig config = new EdifactParserConfig(true, true, true, true, true, true, true, false, -1, null,
            new EdifactIdentityInformation[0], new EdifactIdentityInformation[] { identity });

        DocumentTest test = new DocumentTestEdifact(schema, config);
        InputStream is = EdifactTestBase.class.getResourceAsStream(inputFilePath);
        Map<String, Object> result = test.parse(is);
        test.prepareOutput(result);
        printAcknowledgments(result);
        return test.printAck(result);
    }
    
    /**
     * Write document as string.
     * 
     * @param map
     * @return text
     */
    public String testWrite(Map<String, Object> map) {
        EdifactParserConfig config = new EdifactParserConfig(true, true, true, true, true, true, true, false, -1, null,
            new EdifactIdentityInformation[0], new EdifactIdentityInformation[0]);
        DocumentTest test = new DocumentTestEdifact(schema, config);
        return test.printDoc(map);
    }
}