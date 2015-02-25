package com.anypoint.df.edi.schema.systests;

import static org.junit.Assert.assertEquals;

import org.junit.BeforeClass;
import org.junit.Test;

public class Standard_005010_850 extends X12TestBase {
    
    @BeforeClass
    public static void setUpClass() {
        loadSchema("/x12/005010/850.esl");
    }

    @Test
    public void verifyWrite() throws Exception {
        parseAndCheckWrite("/x12/005010/biztalk-interop/850x1.edi");
        parseAndCheckWrite("/x12/005010/biztalk-interop/850-biztalk.edi");
    }
    
    @Test
    public void verifyGeneratedAcks() {
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*4*00501~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850-badbiztalk.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*223456789*005010~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x2.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK2*850*000000123~AK5*R*3*4~AK9*R*1*1*0~SE*6*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1a.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK2*850*000000123~AK5*R*3*4~AK9*R*1*1*0*4~SE*6*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1b.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK2*850*000000123~AK5*R*4~AK9*R*1*1*0~SE*6*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1c.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK2*850*000000123~AK5*R*4~AK9*R*2*1*0*5~SE*6*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1d.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*4*005010~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850-biztalk.edi")));
    }
}