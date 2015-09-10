package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;

import org.junit.Ignore;
import org.junit.Test;

public class StandardD95bTest extends EdifactTestBase {
    
    @Test
    @Ignore
    public void verifyBAPLIE() throws Exception {
        // TODO: this causes problems with segments reused within a loop
        loadSchema("/edifact/d95b/BAPLIE.esl");
        parseAndCheckWrite("/edifact/d95b/HPA-BAYPLAN.edi");
        assertEquals("UNB+UNOA:3+1STOP+FACTT+XXXXXXXXXXX+++COARRI'UNH++CONTRL:3:1:UN'UCI+15731968+FACTT+1STOP+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d95b/HPA-BAYPLAN.edi")));
    }
    
    @Test
    public void verifyCOARRI() throws Exception {
        loadSchema("/edifact/d95b/COARRI.esl");
        parseAndCheckWrite("/edifact/d95b/COARRI.edi");
        assertEquals("UNB+UNOA:3+1STOP+FACTT+XXXXXXXXXXX+++COARRI'UNH++CONTRL:3:1:UN'UCI+15731968+FACTT+1STOP+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d95b/COARRI.edi")));
    }
    
    @Test
    public void verifyCODECO() throws Exception {
        loadSchema("/edifact/d95b/CODECO.esl");
        parseAndCheckWrite("/edifact/d95b/CODECO.edi");
        assertEquals("UNB+UNOA:3+1STOP+FACTT+XXXXXXXXXXX+++CODECO'UNH++CONTRL:3:1:UN'UCI+15733961+FACTT+1STOP+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d95b/CODECO.edi")));
        parseAndCheckWrite("/edifact/d95b/DEHIRE-CODECO.edi");
        assertEquals("UNB+UNOA:3+MAEU+APPLINK+XXXXXXXXXXX+'UNH++CONTRL:3:1:UN'UCI+0023286999+APPLINK+MAEU+7'UNT+3+1'UNZ+1+1'",
            stripAckDates(parseAndReturnAck("/edifact/d95b/DEHIRE-CODECO.edi")));
    }
}