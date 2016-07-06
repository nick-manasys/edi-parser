package com.mulesoft.flatfile.schema.systests.x12;

import static org.junit.Assert.assertEquals;

import org.junit.BeforeClass;
import org.junit.Test;

public class Overlay_005010_834Test extends X12TestBase {
    
    @BeforeClass
    public static void setUpClass() {
        loadSchema("/x12/005010/test834/hcs-834-overlay.esl");
    }

    @Test
    public void verifyWrite() throws Exception {
        parseAndCheckWrite("/x12/005010/test834/834-per-added-contact.edi");
    }
    
    @Test
    public void verifyGeneratedAcks() {
        assertEquals("ISA*00*          *00*          *ZZ*2222222222222  *30*111111111      *XXXXXX*XXXX*^*00501*XXXXXXXXX*X*T*:~GS*FA*2222222222222*111111111*XXXXXXXX*XXXX**X*005010X220A1~ST*997*0001~AK1*BE*1*005010X220A1~AK2*834*0001*005010X220A1~AK3*PER*19**3~AK5*R*5~AK9*R*1*1*0~SE*7*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/test834/834-no-per.edi", false)));
        assertEquals("ISA*00*          *00*          *ZZ*2222222222222  *30*111111111      *XXXXXX*XXXX*^*00501*XXXXXXXXX*X*T*:~GS*FA*2222222222222*111111111*XXXXXXXX*XXXX**X*005010X220A1~ST*997*0001~AK1*BE*1*005010X220A1~AK2*834*0001*005010X220A1~AK3*PER*16**8~AK4*7*365*1~AK4*8*365*1~AK5*R*5~AK9*R*1*1*0~SE*9*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/test834/834-per-no-contact.edi", false)));
        assertEquals("ISA*00*          *00*          *ZZ*2222222222222  *30*111111111      *XXXXXX*XXXX*^*00501*XXXXXXXXX*X*T*:~GS*FA*2222222222222*111111111*XXXXXXXX*XXXX**X*005010X220A1~ST*997*0001~AK1*BE*1*005010X220A1~AK2*834*0001*005010X220A1~AK3*PER*16**8~AK4*9*443*5*twitMe~AK5*R*5~AK9*R*1*1*0~SE*8*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/test834/834-per08-too-long.edi", false)));
    }
}