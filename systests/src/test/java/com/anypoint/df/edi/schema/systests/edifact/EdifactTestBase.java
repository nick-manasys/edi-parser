package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.anypoint.df.edi.lexical.EdiConstants;
import com.anypoint.df.edi.lexical.EdifactConstants.SyntaxVersion;
import com.anypoint.df.edi.schema.SchemaJavaValues;
import com.anypoint.df.edi.schema.EdifactIdentityInformation;
import com.anypoint.df.edi.schema.EdifactParserConfig;
import com.anypoint.df.edi.schema.EdifactSchemaDefs;
import com.anypoint.df.edi.schema.EdifactVersionDefs;
import com.anypoint.df.edi.schema.EdiSchema.*;
import com.anypoint.df.edi.schema.IdentityInformation;
import com.anypoint.df.edi.schema.systests.TestBase;
import com.anypoint.df.edi.schema.tools.DecodeContrl;
import com.anypoint.df.edi.schema.tools.DocumentTest;
import com.anypoint.df.edi.schema.tools.DocumentTestEdifact;

public abstract class EdifactTestBase extends TestBase {

    /**
     * Mask variable values in a UNB segment by replacing them with 'X' characters.
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
     * Mask variable values in a segment by deleting a particular data value.
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
        while (Character.isLetterOrDigit(text.charAt(scan)) || Character.isWhitespace(text.charAt(scan)))
            scan++;
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
//            if (seg.startsWith("ISA" + datasep)) {
//                seg = maskIsaVariableValues(0, datasep, seg);
//            } else if (seg.startsWith("GS" + datasep)) {
//                seg = maskGsVariableValues(0, datasep, seg);
//            } else if (seg.startsWith("ST" + datasep) || seg.startsWith("SE" + datasep)
//                | seg.startsWith("GE" + datasep) | seg.startsWith("IEA" + datasep)) {
//                seg = maskNthValue(0, 2, datasep, seg);
//            }
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
        List<String> segsin = prepareSegments('~', '*', text);
        List<String> segsout = prepareSegments('~', '*', output);
        Iterator<String> iterin = segsin.iterator();
        Iterator<String> iterout = segsout.iterator();
        while (iterin.hasNext() && iterout.hasNext()) {
            String is = iterin.next();
            String os = iterout.next();
            assertEquals(is, os);
            System.out.println(is + " = " + os);
        }
        assertFalse(iterin.hasNext());
        assertFalse(iterout.hasNext());
    }

    /**
     * Parse a document, write out the parsed data as a new document, and compare the original and the written document.
     * 
     * @param path
     * @throws IOException
     */
    protected void parseAndCheckWrite(String path) throws IOException {
        DocumentTest test = new DocumentTestEdifact(schema);
        String text = readAsString(path);
        Map<String, Object> result = test.parse(new ByteArrayInputStream(text.getBytes("ASCII")));
        checkWrite(test, text, result);
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
//            working = maskGsVariableValues(scan, '*', working);
        }
        return working;
    }

    /**
     * Print acknowledgment information from parse result map.
     *
     * @param result
     */
    protected void printAcknowledgments(Map<String, Object> result) {
        List<Map<String, Object>> acks = (List<Map<String, Object>>) result.get(SchemaJavaValues
            .functionalAcksGenerated());
        if (acks != null) {
            Map<String, Object> inter = new HashMap<>();
            EdifactVersionDefs verdefs = EdifactSchemaDefs.versions().apply(SyntaxVersion.VERSION4);
            inter.put(((CompositeComponent)verdefs.segUNB().components().head()).composite().components().apply(1).key(), "4");
            for (Map<String, Object> ack : acks) {
                ack.put(SchemaJavaValues.interchangeKey(), inter);
                System.out.println(DecodeContrl.decode(ack));
            }
        }
    }

    /**
     * Parse a document, then write out and return the generated CONTRL acknowledgment.
     * 
     * @param path
     * @throws IOException
     */
    protected String parseAndReturnAck(String path) {
        DocumentTest test = new DocumentTestEdifact(schema);
        InputStream is = EdifactTestBase.class.getResourceAsStream(path);
        if (is == null) {
            throw new IllegalArgumentException("File " + path + " not found");
        }
        try {
            Map<String, Object> result = test.parse(is);
            printAcknowledgments(result);
            return test.printAck(result);
        } catch (Exception e) {
            e.printStackTrace();
            return e.getMessage();
        }

    }

    protected String parseWithSenderIdentityInformation(String inputFilePath, String interchangeQualifier,
        String interchangeId, String interchangeType) {
        IdentityInformation identity = new IdentityInformation(interchangeQualifier, interchangeId, interchangeType);
        IdentityInformation[] senders = new IdentityInformation[1];
        senders[0] = identity;
        EdifactParserConfig config = new EdifactParserConfig(true, true, true, true, true, true, true, -1,
            EdiConstants.ASCII_CHARSET, new EdifactIdentityInformation[0], new EdifactIdentityInformation[0]);

        DocumentTest test = new DocumentTestEdifact(schema, config);
        InputStream is = EdifactTestBase.class.getResourceAsStream(inputFilePath);
        try {
            Map<String, Object> result = test.parse(is);
            return test.printAck(result);
        } catch (Exception e) {
            return e.getMessage();
        }
    }
}