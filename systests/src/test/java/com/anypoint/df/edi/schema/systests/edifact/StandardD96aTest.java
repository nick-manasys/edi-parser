package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.*;

import org.junit.Test;

import com.anypoint.df.edi.schema.EdifactInterchangeException;

public class StandardD96aTest extends EdifactTestBase {
    
    @Test
    public void verifyDESADV() throws Exception {
        loadSchema("/edifact/d96a/DESADV.esl");
        parseAndCheckWrite("/edifact/d96a/DESADV.edi");
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
        assertEquals("UNB+UNOC:3+TNT001:ZZ+LGEAP:ZZ+XXXXXXXXXXX+'UNH++CONTRL:D:96A:UN'UCI+15566+LGEAP:ZZ+TNT001:ZZ+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/DESADV.edi")));
        assertEquals("UNB+UNOC:3+TNT001:ZZ+LGEAP:ZZ+XXXXXXXXXXX+'UNH++CONTRL:D:96A:UN'UCI+15566+LGEAP:ZZ+TNT001:ZZ+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseWithSenderIdentityInformation("/edifact/d96a/DESADV.edi", "LGEAP", "ZZ")));
        assertEquals("UNB+UNOC:3+TNT001:ZZ+LGEAP:ZZ+XXXXXXXXXXX+'UNH++CONTRL:D:96A:UN'UCI+15566+LGEAP:ZZ+TNT001:ZZ+4+23'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseWithSenderIdentityInformation("/edifact/d96a/DESADV.edi", "GEAPL", "XX")));
    }
    
    @Test
    public void verifyGeneratedORDERSAcks() {
        loadSchema("/edifact/d96a/ORDERS.esl");
        assertEquals("UNB+UNOC:3+MODUS:ZZZ+MULESOFT:ZZZ+XXXXXXXXXXX+++ORDERS'UNH++CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A.edi")));
        assertEquals("UNB+UNOC:3+MODUS:ZZZ+MULESOFT:ZZZ+XXXXXXXXXXX+++ORDERS'UNH++CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A-no-UNS.edi")));
        assertEquals("UNB+UNOC:3+MODUS:ZZZ+MULESOFT:ZZZ+XXXXXXXXXXX+++ORDERS'UNH++CONTRL:D:96A:UN'UCI+582+MULESOFT:ZZZ+MODUS:ZZZ+7'UCM+6424+ORDERS:D:96A:UN:EAN008+4'UCS+4+35'UNT+5+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A_duplicate-BGM.edi")));
        assertEquals("UNA:+.?*'UNB+UNOC:3+MODUS:ZZZ+XXXXXXXXXXXX++1++ORDERS'UNH++CONTRL:D:96A:UN'UCI+14+MULESOFT:ZZZ+MODUS:ZZZ+7'UCM+1+ORDERS:D:96A:UN:EAN008+4'UCS+3'UCD+13+1'UCS+4'UCD+13+1'UNT+8+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/ORDERS_D96A-empty-DTM-and-RFF.edi")));
    }
}