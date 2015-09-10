package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class StandardD03aTest extends EdifactTestBase {
    
    @Test
    public void verifyCOPARN() throws Exception {
        loadSchema("/edifact/d03a/COPARN.esl");
        parseAndCheckWrite("/edifact/d03a/BOOKING-COPARN.edi");
        assertEquals("UNB+UNOA:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:3:1:UN'UCI+1853296+MSK+1STOP+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d03a/BOOKING-COPARN.edi")));
    }
    
    @Test
    public void verifyIFTMCS() throws Exception {
        loadSchema("/edifact/d03a/IFTMCS.esl");
        parseAndCheckWrite("/edifact/d03a/BOOKING-IFTMCS.edi");
        assertEquals("UNB+UNOA:3+1STOP+APL+XXXXXXXXXXX+'UNH++CONTRL:3:1:UN'UCI+160315143316+APL+1STOP+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d03a/BOOKING-IFTMCS.edi")));
    }
}