package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.util.Map;

import org.junit.Test;

import com.anypoint.df.edi.schema.EdifactParserConfig;
import com.anypoint.df.edi.schema.tools.DocumentTest;
import com.anypoint.df.edi.schema.tools.DocumentTestEdifact;

public class StandardD96aTest extends EdifactTestBase {
    
    @Test
    public void verifyDESADV() throws Exception {
        loadSchema("/edifact/d96a/DESADV.esl");
        parseAndCheckWrite("/edifact/d96a/DESADV.edi");
    }
    
    @Test
    public void verifyInvalidCharacterHandling() throws Exception {
        loadSchema("/edifact/d96a/DESADV.esl");
        parseAndCheckWrite("/edifact/d96a/DESADV-invalid-characters.edi");
        EdifactParserConfig pconfig = new EdifactParserConfig(true, true, true, true, true, true, true, true, -1);
        DocumentTest test = new DocumentTestEdifact(schema, pconfig);
        String text = readAsString("/edifact/d96a/DESADV-invalid-characters.edi");
        assertEquals("UNB+UNOA:3+MODUS:ZZZ+MULESOFT:ZZZ+XXXXXXXXXXX+++DESADV'UNH++CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UCM+00001+DESADV:D:96A:UN:A01051+4'UCS+10'UCD+21+3:1'UNT+6+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck(test, new ByteArrayInputStream(text.getBytes("ASCII")))));
        pconfig = new EdifactParserConfig(true, true, true, true, true, true, true, true, '_');
        test = new DocumentTestEdifact(schema, pconfig);
        Map<String, Object> result = test.parse(new ByteArrayInputStream(text.getBytes("ASCII")));
        checkWrite(test, text.replace((char)0x0E, '_'), result);
    }
    
    @Test
    public void verifyINVRPT() throws Exception {
        loadSchema("/edifact/d96a/INVRPT.esl");
        parseAndCheckWrite("/edifact/d96a/INVRPT.edi");
    }
    
    @Test
    public void verifySLSRPT() throws Exception {
        loadSchema("/edifact/d96a/SLSRPT.esl");
        parseAndCheckWrite("/edifact/d96a/SLSRPT.edi");
    }
    
    @Test
    public void verifyORDERS() throws Exception {
        loadSchema("/edifact/d96a/ORDERS.esl");
        parseAndCheckWrite("/edifact/d96a/ORDERS_D96A.edi");
    }
    
    @Test
    public void verifyGeneratedDESADVAcks() {
        loadSchema("/edifact/d96a/DESADV.esl");
        assertEquals("UNB+UNOA:3+TNT001:ZZ+LGEAP:ZZ+XXXXXXXXXXX+'UNH++CONTRL:D:96A:UN'UCI+15566+LGEAP:ZZ+TNT001:ZZ+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/DESADV.edi")));
        assertEquals("UNB+UNOA:3+TNT001:ZZ+LGEAP:ZZ+XXXXXXXXXXX+'UNH++CONTRL:D:96A:UN'UCI+15566+LGEAP:ZZ+TNT001:ZZ+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/DESADV.edi")));
    }
    
    @Test
    public void verifyGeneratedORDERSAcks() {
        loadSchema("/edifact/d96a/ORDERS.esl");
        assertEquals("UNB+UNOA:3+MODUS:ZZZ+MULESOFT:ZZZ+XXXXXXXXXXX+++ORDERS'UNH++CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A.edi")));
        assertEquals("UNB+UNOA:3+MODUS:ZZZ+MULESOFT:ZZZ+XXXXXXXXXXX+++ORDERS'UNH++CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UCM+6424+ORDERS:D:96A:UN:EAN008+4'UCS+15+15'UNT+5+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A-no-UNS.edi")));
        assertEquals("UNB+UNOA:3+MODUS:ZZZ+MULESOFT:ZZZ+XXXXXXXXXXX+++ORDERS'UNH++CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UCM+6424+ORDERS:D:96A:UN:EAN008+4'UCS+3+35'UNT+5+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A_duplicate-BGM.edi")));
        assertEquals("UNA:+.?*'UNB+UNOA:3+MODUS:ZZZ+XXXXXXXXXXXX++1++ORDERS'UNH++CONTRL:D:96A:UN'UCI+14+MULESOFT:ZZZ+MODUS:ZZZ+7'UCM+1+ORDERS:D:96A:UN:EAN008+4'UCS+3'UCD+13+1'UCS+4'UCD+13+1'UCS+15+15'UNT+9+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A-empty-DTM-and-RFF.edi")));
        assertEquals("UNB+UNOA:3+MODUS:ZZZ+MULESOFT:ZZZ+XXXXXXXXXXX+++ORDERS'UNH++CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UCM+6424+ORDERS:D:96A:UN:EAN008+4'UCS+9+35'UNT+5+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A_repeated-ALI.edi")));
        assertEquals("UNB+UNOA:3+MODUS:ZZZ+MULESOFT:ZZZ+XXXXXXXXXXX+++ORDERS'UNH++CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UCM+6424+ORDERS:D:96A:UN:EAN008+4'UCS+3+35'UCS+10+35'UNT+6+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A-too-many-segments-repetitions.edi")));
    }
}