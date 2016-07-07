package com.mulesoft.flatfile.schema.systests.edifact;

import static org.junit.Assert.assertEquals;

import org.junit.Ignore;
import org.junit.Test;

public class StandardD95bTest extends EdifactTestBase {
    
    // TODO: check handling of this - the current code reports different errors from the old code
    @Test
    @Ignore
    public void verifyBAPLIE() throws Exception {
        loadSchema("/edifact/d95b/BAPLIE.esl");
        assertEquals("UNB+UNOA:3+1STOP+HPAFI+XXXXXXXXXXX+'UNH++CONTRL:3:1:UN'UCI+99+HPAFI+1STOP+7'UCM+99+BAPLIE:D:95B:UN:SMDG20+4'UCS+1842'UCD+39+3:1'UCS+2144'UCD+39+3:1'UCS+2736'UCD+39+3:1'UCS+2746'UCD+39+3:1'UCS+3044'UCD+39+3:1'UCS+3644'UCD+21+5'UCS+3990'UCD+39+3:1'UNT+18+1'UNZ+1+1'",
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