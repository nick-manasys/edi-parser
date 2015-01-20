package com.anypoint.df.edi.schema.systests;

import static org.junit.Assert.*;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;

import org.junit.BeforeClass;
import org.junit.Test;

import com.anypoint.df.edi.schema.EdiSchema;
import com.anypoint.df.edi.schema.YamlReader;
import com.anypoint.df.edi.schema.tools.DocumentTest;

public class BiztalkTest {
    
    private static final String schemaPath = "/x12/5010/biztalk-interop/850.esl";

    private static EdiSchema schema;
    
    @BeforeClass
    public static void setUpClass() {
        try {
            InputStream is = BiztalkTest.class.getResourceAsStream(schemaPath);
            schema = YamlReader.loadYaml(new InputStreamReader(is, "ASCII"), new String[0]);
        } catch (Exception e) {
            throw new RuntimeException("Could not load file " + schemaPath, e);
        }
    }
    
    private String parseAndReturnAck(String path) {
        DocumentTest test = new DocumentTest(schema);
        InputStream is = BiztalkTest.class.getResourceAsStream(path);
        Map<String, Object> result = test.parse(is);
        return test.printAck(result);
    }
    
    private int nthOffset(char ch, int start, int count, String str) {
        int scan = start;
        for (int i = 0; i < count; i++) {
            scan = str.indexOf(ch, scan + 1);
            if (scan < 0) {
                throw new IllegalArgumentException("Expected " + i + "th occurrence of '" + ch + "' not found");
            }
        }
        return scan;
    }
    
    private String replaceRange(char replace, int start, int limit, String str) {
        StringBuilder builder = new StringBuilder(str);
        for (int i = start; i < limit; i++) {
            builder.setCharAt(i, replace);
        }
        return builder.toString();
    }
    
    /**
     * Replace date/time fields in an acknowledgment with X characters, allowing it to be compared with a static string.
     * 
     * @param ack
     * @return replaced
     */
    private String stripDates(String ack) {
        String working = ack;
        int isaDate = nthOffset('*', 0, 9, working);
        int isaTime = nthOffset('*', isaDate, 1, working);
        int isaLimit = nthOffset('*', isaTime, 1, working);
        working = replaceRange('X', isaDate + 1, isaTime, working);
        working = replaceRange('X', isaTime + 1, isaLimit, working);
        int scan = isaLimit;
        while ((scan = working.indexOf("~GS*", scan)) > 0) {
            int gsDate = nthOffset('*', scan, 4, working);
            int gsTime = nthOffset('*', gsDate, 1, working);
            int gsLimit = nthOffset('*', gsTime, 1, working);
            working = replaceRange('X', gsDate + 1, gsTime, working);
            working = replaceRange('X', gsTime + 1, gsLimit, working);
            scan = gsLimit;
        }
        return working;
    }
    
    @Test
    public void test() {
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*000000001*0*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX*1*X*005010~ST*997*1   ~AK1*PO*4*00501~AK9*R*1*1*0*2~SE*4*1   ~GE*1*1~IEA*1*000000001~",
            stripDates(parseAndReturnAck("/x12/5010/biztalk-interop/850-badbiztalk.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*000000001*0*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX*1*X*005010~ST*997*1   ~AK1*PO*123456789*005010~AK9*A*1*1*1~SE*4*1   ~GE*1*1~IEA*1*000000001~",
            stripDates(parseAndReturnAck("/x12/5010/biztalk-interop/850x1.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*000000001*0*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX*1*X*005010~ST*997*1   ~AK1*PO*223456789*005010~AK9*A*1*1*1~SE*4*1   ~GE*1*1~IEA*1*000000001~",
            stripDates(parseAndReturnAck("/x12/5010/biztalk-interop/850x2.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*000000001*0*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX*1*X*005010~ST*997*1   ~AK1*PO*123456789*005010~AK9*A*1*1*1~SE*4*1   ~GE*1*1~IEA*1*000000001~",
            stripDates(parseAndReturnAck("/x12/5010/biztalk-interop/850x1a.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*000000001*0*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX*1*X*005010~ST*997*1   ~AK1*PO*123456789*005010~AK9*R*1*1*1*4~SE*4*1   ~GE*1*1~IEA*1*000000001~",
            stripDates(parseAndReturnAck("/x12/5010/biztalk-interop/850x1b.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*000000001*0*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX*1*X*005010~ST*997*1   ~AK1*PO*123456789*005010~AK9*A*1*1*1~SE*4*1   ~GE*1*1~IEA*1*000000001~",
            stripDates(parseAndReturnAck("/x12/5010/biztalk-interop/850x1c.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*000000001*0*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX*1*X*005010~ST*997*1   ~AK1*PO*123456789*005010~AK9*R*2*1*1*5~SE*4*1   ~GE*1*1~IEA*1*000000001~",
            stripDates(parseAndReturnAck("/x12/5010/biztalk-interop/850x1d.edi")));
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*000000001*0*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX*1*X*005010~ST*997*1   ~AK1*PO*4*005010~AK2*850*0004~AK3*CUR*3**2~AK3*REF*4**2~AK3*PER*5**2~AK3*TAX*6**2~AK3*FOB*7**2~AK3*CTP*8**2~AK3*PAM*9**2~AK3*TC2*11**2~AK3*DIS*15**2~AK3*INC*16**2~AK3*LIN*18**2~AK3*SI*19**2~AK3*PID*20**2~AK3*MEA*21**2~AK3*PWK*22**2~AK3*PKG*23**2~AK3*TD1*24**2~AK3*TD3*26**2~AK3*TD4*27**2~AK3*MAN*28**2~AK3*PCT*29**2~AK3*CTB*30**2~AK3*TXI*31**2~AK3*DTM*43*N9*2~AK3*PWK*45*N9*2~AK3*EFI*46*N9*2~AK3*N2*48*N1*2~AK3*IN2*49*N1*2~AK3*NX2*52*N1*2~AK3*PER*54*N1*2~AK3*SI*55*N1*2~AK3*FOB*56*N1*2~AK3*TD1*57*N1*2~AK3*TD5*58*N1*2~AK3*TD3*59*N1*2~AK3*TD4*60*N1*2~AK3*PKG*61*N1*2~AK3*LIN*84*PO1*2~AK3*SI*85*PO1*2~AK3*CUR*86*PO1*2~AK3*CN1*87*PO1*2~AK3*PO3*88*PO1*2~AK3*PAM*91*PO1*2~AK3*MEA*92*PO1*2~AK3*MEA*94*PID*2~AK3*PWK*95*PO1*2~AK3*REF*97*PO1*2~AK3*PER*98*PO1*2~AK3*IT8*102*PO1*2~AK3*CSH*103*PO1*2~AK3*ITD*104*PO1*2~AK3*DIS*105*PO1*2~AK3*INC*106*PO1*2~AK3*TAX*107*PO1*2~AK3*FOB*108*PO1*2~AK3*SDQ*109*PO1*2~AK3*IT3*110*PO1*2~AK3*DTM*111*PO1*2~AK3*TC2*112*PO1*2~AK3*TD1*113*PO1*2~AK3*TD5*114*PO1*2~AK3*TD3*115*PO1*2~AK3*TD4*116*PO1*2~AK3*PCT*117*PO1*2~AK3*MAN*118*PO1*2~AK3*MTX*119*PO1*2~AK3*SPI*120*PO1*2~AK3*TXI*121*PO1*2~AK3*CTB*122*PO1*2~AK5*R*5~AK9*R*1*1*0~SE*75*1   ~GE*1*1~IEA*1*000000001~",
            stripDates(parseAndReturnAck("/x12/5010/biztalk-interop/850-biztalk.edi")));
    }

}