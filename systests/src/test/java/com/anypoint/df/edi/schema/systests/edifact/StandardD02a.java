package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class StandardD02a extends EdifactTestBase {
    
    @Test
    public void verifyCOPARN() throws Exception {
        loadSchema("/edifact/d02a/COPARN.esl");
        parseAndCheckWrite("/edifact/d02a/BOOKING-COPARN.edi");
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-COPARN.edi")));
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+4+28'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-interchange-number-mismatch.edi")));
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+7'UCM+185329600001+COPARN:D:03A:UN+4+17'UNT+4+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-wrong-version.edi")));
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+7'UCM+185329600001+COPARN:D:02A:UN+4+29'UNT+4+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-wrong-segment-count.edi")));
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+7'UCM+185329600001+COPARN:D:02A:UN+4'UCS+3'UCD+13+2'UNT+6+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-empty-value.edi")));
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+7'UCM+185329600001+COPARN:D:02A:UN+4'UCS+10+15'UCS+11+13'UNT+6+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-missing-required-segment.edi")));
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+7'UCM+185329600001+COPARN:D:02A:UN+4'UCS+4'UCD+13+1'UNT+6+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-missing-value.edi")));
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+7'UCM+185329600001+COPARN:D:02A:UN+4'UCS+3'UCD+16+2:4'UNT+6+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-too-many-components.edi")));
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+7'UCM+185329600001+COPARN:D:02A:UN+4'UCS+5+33'UNT+5+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-unknown-segment.edi")));
        assertEquals("UNB+UNOC:3+1STOP+MSK+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+1853296+MSK+1STOP+7'UCM+185329600001+COPARN:D:02A:UN+4'UCS+3'UCD+39+2:3'UNT+6+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-value-too-long.edi")));
    }
    
    @Test
    public void verifyIFTMCS() throws Exception {
        loadSchema("/edifact/d02a/IFTMCS.esl");
        parseAndCheckWrite("/edifact/d02a/BOOKING-IFTMCS.edi");
        assertEquals("UNB+UNOC:3+1STOP+APL+XXXXXXXXXXX+'UNH++CONTRL:D:02A:UN'UCI+160315143316+APL+1STOP+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d02a/BOOKING-IFTMCS.edi")));
    }
}