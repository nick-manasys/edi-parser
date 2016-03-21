package com.anypoint.df.edi.schema.systests.hl7;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.anypoint.df.edi.schema.systests.TestBase;
import com.anypoint.df.edi.schema.tools.DocumentTest;
import com.anypoint.df.edi.schema.tools.DocumentTestHL7;
import com.anypoint.df.edi.schema.tools.YamlSupport;

public class HL7TestBase extends TestBase
{

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
            if (seg.startsWith("MSH" + datasep)) {
                int mshDatetime = nthOffset(datasep, false, 0, 6, seg);
                int mshSecurity = nthOffset(datasep, false, mshDatetime, 1, seg);
                seg = replaceRange('X', mshDatetime + 1, mshSecurity, seg);
                int mshControl = nthOffset(datasep, false, mshSecurity, 2, seg);
                int mshProcessing = nthOffset(datasep, false, mshControl, 1, seg);
                seg = deleteRange(mshControl + 1, mshProcessing, seg);
            }
            segs.add(seg);
            base = split + 1;
            while (base < message.length() && Character.isWhitespace(message.charAt(base)))
                base++;
        }
        assertEquals(message.length(), base);
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
        List<String> segsin = prepareSegments('\r', '|', text);
        List<String> segsout = prepareSegments('\r', '|', output);
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
        StringWriter writer = new StringWriter();
        YamlSupport.writeMap(result, writer);
        System.out.println(writer.toString());
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
        DocumentTest test = new DocumentTestHL7(schema);
        String text = readAsString(path);
        parseAndCheckWrite(text, test);
    }
}