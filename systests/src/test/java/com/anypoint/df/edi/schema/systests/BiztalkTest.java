package com.anypoint.df.edi.schema.systests;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import org.junit.BeforeClass;
import org.junit.Test;

import com.anypoint.df.edi.schema.SchemaJavaValues;
import com.anypoint.df.edi.schema.tools.DocumentTest;

public class BiztalkTest extends X12TestBase {

    @BeforeClass
    public static void setUpClass() {
        loadSchema("/x12/005010/biztalk-interop/850.esl");
    }

    @Test
    public void verifyWrite() throws Exception {
        parseAndCheckWrite("/x12/005010/biztalk-interop/850x1.edi");
    }

    @Test
    public void verifyWriteUsingDate() throws Exception {
        DocumentTest test = new DocumentTest(schema);
        String text = readAsString("/x12/005010/biztalk-interop/850x1.edi");
        Map<String, Object> result = test.parse(new ByteArrayInputStream(text.getBytes("ASCII")));
        Map<String, Object> transmap = (Map<String, Object>)result.get(SchemaJavaValues.transactionsMap());
        assertNotNull(transmap);
        List<Map<String, Object>> translist = (List<Map<String, Object>>)transmap.get("850");
        assertNotNull(translist);
        assertEquals(1, translist.size());
        Map<String, Object> transset = translist.get(0);
        Map<String, Object> heading = (Map<String, Object>)transset.get(SchemaJavaValues.transactionHeading());
        assertNotNull(heading);
        Map<String, Object> begseg = (Map<String, Object>)heading.get("BEG");
        assertNotNull(begseg);
        Calendar calendar = (Calendar)begseg.get("BEG05");
        assertNotNull(calendar);
        begseg.put("BEG05", calendar.getTime());
        checkWrite(test, text, result);
    }

    @Test
    public void verifyGeneratedAcks() {
        assertEquals(
            "ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**T*00501~ST*997*0001~AK1*PO*4*00501~AK2*850*0004~AK3*CUR*3**2~AK3*REF*4**2~AK3*PER*5**2~AK3*TAX*6**2~AK3*FOB*7**2~AK3*CTP*8**2~AK3*PAM*9**2~AK3*TC2*11**2~AK3*SAC*12*SAC*2~AK3*DIS*15**2~AK3*INC*16**2~AK3*LIN*18**2~AK3*SI*19**2~AK3*PID*20**2~AK3*MEA*21**2~AK3*PWK*22**2~AK3*PKG*23**2~AK3*TD1*24**2~AK3*TD3*26**2~AK3*TD4*27**2~AK3*MAN*28**2~AK3*PCT*29**2~AK3*CTB*30**2~AK3*TXI*31**2~AK3*LDT*32*LDT*2~AK3*AMT*36*AMT*2~AK3*DTM*43*N9*2~AK3*PWK*45*N9*2~AK3*EFI*46*N9*2~AK3*N2*48*N1*2~AK3*IN2*49*N1*2~AK3*NX2*52*N1*2~AK3*PER*54*N1*2~AK3*SI*55*N1*2~AK3*FOB*56*N1*2~AK3*TD1*57*N1*2~AK3*TD5*58*N1*2~AK3*TD3*59*N1*2~AK3*TD4*60*N1*2~AK3*PKG*61*N1*2~AK3*LM*62*LM*2~AK3*SPI*64*SPI*2~AK3*ADV*80*ADV*2~AK3*LIN*84*PO1*2~AK3*SI*85*PO1*2~AK3*CUR*86*PO1*2~AK3*CN1*87*PO1*2~AK3*PO3*88*PO1*2~AK3*CTP*89*CTP*2~AK3*PAM*91*PO1*2~AK3*MEA*92*PO1*2~AK3*MEA*94*PID*2~AK3*PWK*95*PO1*2~AK3*REF*97*PO1*2~AK3*PER*98*PO1*2~AK3*SAC*99*SAC*2~AK3*IT8*102*PO1*2~AK3*CSH*103*PO1*2~AK3*ITD*104*PO1*2~AK3*DIS*105*PO1*2~AK3*INC*106*PO1*2~AK3*TAX*107*PO1*2~AK3*FOB*108*PO1*2~AK3*SDQ*109*PO1*2~AK3*IT3*110*PO1*2~AK3*DTM*111*PO1*2~AK3*TC2*112*PO1*2~AK3*TD1*113*PO1*2~AK3*TD5*114*PO1*2~AK3*TD3*115*PO1*2~AK3*TD4*116*PO1*2~AK3*PCT*117*PO1*2~AK3*MAN*118*PO1*2~AK3*MTX*119*PO1*2~AK3*SPI*120*PO1*2~AK3*TXI*121*PO1*2~AK3*CTB*122*PO1*2~AK3*QTY*123*QTY*2~AK3*SCH*125*SCH*2~AK3*PKG*131*PKG*2~AK3*LS*133**1~AK3*LDT*134*LDT*2~AK3*LE*140**1~AK3*N9*141*N9*2~AK3*N1*147*N1*2~AK3*SLN*170*SLN*2~AK3*AMT*200*AMT*2~AK3*LM*203*LM*2~AK5*R*5~AK9*R*1*1*0~SE*94*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850-badbiztalk.edi")));
        assertEquals(
            "ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK9*A*1*1*1~SE*4*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1.edi")));
        assertEquals(
            "ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*223456789*005010~AK2*850*000000123~AK3*CUR*3**2~AK3*REF*4**2~AK5*R*5~AK9*R*1*1*0~SE*8*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x2.edi")));
        assertEquals(
            "ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK2*850*000000123~AK5*R*3*4~AK9*R*1*1*0~SE*6*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1a.edi")));
        assertEquals(
            "ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK2*850*000000123~AK5*R*3*4~AK9*R*1*1*0*4~SE*6*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1b.edi")));
        assertEquals(
            "ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK2*850*000000123~AK5*R*4~AK9*R*1*1*0~SE*6*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1c.edi")));
        assertEquals(
            "ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*XXXXXXXXX*X*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX**X*005010~ST*997*0001~AK1*PO*123456789*005010~AK2*850*000000123~AK5*R*4~AK9*R*2*1*0*5~SE*6*0001~GE*1*1~IEA*1*000000001~",
            stripAckDates(parseAndReturnAck("/x12/005010/biztalk-interop/850x1d.edi")));
    }
}