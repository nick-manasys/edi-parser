package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class StandardD96a extends EdifactTestBase {
    
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
    @Ignore
    public void verifySLSRPT() throws Exception {
        loadSchema("/edifact/d96a/SLSRPT.esl");
        parseAndCheckWrite("/edifact/d96a/SLSRPT.edi");
    }
    
    @Test
    public void verifyGeneratedAcks() {
//        loadSchema("/edifact/d96a/DESADV.esl");
//        assertEquals("ISA*00*          *00*          *ZZ*9177           *01*000099999      *XXXXXX*XXXX*U*00400*XXXXXXXXX*X*P*>~GS*FA*9177*000099999*XXXXXXXX*XXXX**X*004010~ST*997*0001~AK1*PO*966~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
//            stripAckDates(parseAndReturnAck("/edifact/d96a/DESADV.edi")));
    }
}