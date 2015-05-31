package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.*;

import org.junit.Test;

import com.anypoint.df.edi.schema.EdifactInterchangeException;

public class StandardD96a extends EdifactTestBase {
    
    @Test
    public void verifyDESADV() throws Exception {
        loadSchema("/edifact/d96a/DESADV.esl");
        parseAndCheckWrite("/edifact/d96a/DESADV.edi");
        parseWithSenderIdentityInformation("/edifact/d96a/DESADV.edi", "LGEAP", "ZZ");
        try {
            parseWithSenderIdentityInformation("/edifact/d96a/DESADV.edi", "GEAPL", "XX");
            fail();
        } catch (EdifactInterchangeException e) {
            assertTrue(e.getMessage().contains("Interchange sender information does not match configuration"));
        }
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
    public void verifyGeneratedAcks() {
        loadSchema("/edifact/d96a/DESADV.esl");
        assertEquals("UNB+UNOC:3+TNT001:ZZ+LGEAP:ZZ+XXXXXXXXXXX++CONTRL+CONTRL:D:96A:UN'UCI+15566+LGEAP:ZZ+TNT001:ZZ+1'UNT+3+CONTRL'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d96a/DESADV.edi")));
    }
}