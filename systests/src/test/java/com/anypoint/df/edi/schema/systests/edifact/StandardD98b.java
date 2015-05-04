package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class StandardD98b extends EdifactTestBase {
    
    @Test
    public void verifyIFCSUM() throws Exception {
        loadSchema("/edifact/d98b/IFCSUM.esl");
        parseAndCheckWrite("/edifact/d98b/EIDO-IFCSUM.edi");
    }
    
    @Test
    public void verifyIFTERA() throws Exception {
        loadSchema("/edifact/d98b/IFTERA.esl");
        parseAndCheckWrite("/edifact/d98b/PRA-IFTERA.edi");
//        parseAndCheckWrite("/edifact/d98b/IFTERA_original.edi");
    }
    
//    @Test
//    public void verifyGeneratedAcks() {
//        assertEquals("ISA*00*          *00*          *ZZ*9177           *01*000099999      *XXXXXX*XXXX*U*00400*XXXXXXXXX*X*P*>~GS*FA*9177*000099999*XXXXXXXX*XXXX**X*004010~ST*997*0001~AK1*PO*966~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
//            stripAckDates(parseAndReturnAck("/x12/004010/sample850s/850_9177_1.edi")));
//    }
}