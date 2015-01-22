package com.anypoint.df.edi.schema.systests;

import static org.junit.Assert.assertEquals;

import org.junit.BeforeClass;
import org.junit.Test;

public class TestOG extends X12TestBase {
    
    @BeforeClass
    public static void setUpClass() {
        loadSchema("/x12/005010/875.esl");
    }
    
    @Test
    public void verifyGeneratedAcks() {
        assertEquals("ISA*00*          *00*          *08*9262390000     *ZZ*08             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*8052495336*8136881188*XXXXXXXX*XXXX**X*005010~ST*997*1   ~AK1*OG*47*005010UCS~AK9*R*5*5*0*2~SE*4*1   ~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/testOG/testOG.edi")));
    }
}
