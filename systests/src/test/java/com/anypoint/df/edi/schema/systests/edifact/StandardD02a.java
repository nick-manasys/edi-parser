package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class StandardD02a extends EdifactTestBase {
    
    @Test
    public void verifyCOPARN() throws Exception {
        loadSchema("/edifact/d02a/COPARN.esl");
        parseAndCheckWrite("/edifact/d02a/BOOKING-COPARN.edi");
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+1'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-COPARN.edi")));
    }
    
    @Test
    public void verifyIFTMCS() throws Exception {
        loadSchema("/edifact/d02a/IFTMCS.esl");
        parseAndCheckWrite("/edifact/d02a/BOOKING-IFTMCS.edi");
        assertEquals("UNB+UNOC:3+1STOP+APL+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+160315143316+APL+1STOP+1'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-IFTMCS.edi")));
    }
}