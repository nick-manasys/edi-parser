package com.mulesoft.flatfile.schema.systests.x12;

import static org.junit.Assert.assertEquals;

import org.junit.BeforeClass;
import org.junit.Test;

public class Standard_004010_850Test extends X12TestBase {
    
    @BeforeClass
    public static void setUpClass() {
        loadSchema("/x12/004010/850.esl");
    }

    @Test
    public void verifyWrite() throws Exception {
        parseAndCheckWrite("/x12/004010/sample850s/850_9177_1.edi");
        parseAndCheckWrite("/x12/004010/sample850s/Academy_850_1.edi");
    }
    
    @Test
    public void verifyGeneratedAcks() {
        assertEquals("ISA*00*          *00*          *ZZ*9177           *01*000099999      *XXXXXX*XXXX*U*00400*XXXXXXXXX*X*P*>~GS*FA*9177*000099999*XXXXXXXX*XXXX**X*004010~ST*997*0001~AK1*PO*966~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/004010/sample850s/850_9177_1.edi", false)));
        assertEquals("ISA*00*          *00*          *ZZ*IAIYUCAFOO     *01*006927180      *XXXXXX*XXXX*U*00401*XXXXXXXXX*X*P*>~GS*FA*IAIYUCAFOO*006927180*XXXXXXXX*XXXX**X*004010~ST*997*0001~AK1*PO*168~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/004010/sample850s/850EDIInput.txt.edi", false)));
        assertEquals("ISA*00*          *00*          *ZZ*IAIYUCAFOO     *01*006927180      *XXXXXX*XXXX*U*00401*XXXXXXXXX*X*P*>~GS*FA*IAIYUCAFOO*006927180*XXXXXXXX*XXXX**X*004010~ST*997*0001~AK1*PO*168~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/004010/sample850s/850EDIInput_Modified.edi", false)));
        assertEquals("ISA*00*          *00*          *ZZ*8869           *12*2816465200     *XXXXXX*XXXX*U*00400*XXXXXXXXX*X*P*>~GS*FA*8869*2816465200*XXXXXXXX*XXXX**X*004010~ST*997*0001~AK1*PO*477~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/004010/sample850s/Academy_850_1.edi", false).replace('', '*').replace((char)10, '~')));
        assertEquals("ISA*00*          *00*          *12*QSTEST         *ZZ*BECKER         *XXXXXX*XXXX*U*00401*XXXXXXXXX*X*P*>~GS*FA*QSTEST*BECKER*XXXXXXXX*XXXX**X*004010VICS~ST*997*0001~AK1*PO*210000004~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/004010/sample850s/Becker_LiveTest_850.edi", false)));
        assertEquals("ISA*00*          *00*          *01*128576449      *09*001288364I     *XXXXXX*XXXX*U*00401*XXXXXXXXX*X*P*:~GS*FA*128576449137*001288364*XXXXXXXX*XXXX**X*004010~ST*997*0001~AK1*PO*1925~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/004010/sample850s/edi_850_generic_test.edi", false).replace((char)10, '~')));
        assertEquals("ISA*00*          *00*          *ZZ*3897           *12*6048882079     *XXXXXX*XXXX*U*00401*XXXXXXXXX*X*P*>~GS*FA*3897*6048882079*XXXXXXXX*XXXX**X*004010~ST*997*0001~AK1*PO*77~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/004010/sample850s/Overtea-850.4010.edi", false)));
        // TODO: investigate and correct
//        assertEquals("ISA*00*          *00*          *ZZ*9256           *14*185122629OMX   *XXXXXX*XXXX*U*00401*XXXXXXXXX*X*P*>~GS*FA*9256*185122629OMX*XXXXXXXX*XXXX**X*004010VICS~ST*997*0001~AK1*PO*11~AK9*A*3*3*3~SE*4*0001~GE*1*1~IEA*1*000000001~",
//            stripAckDates(parseAndReturnAck("/x12/004010/sample850s/OfficeMax_850.edi").replace((char)13, '~')));
    }
}
