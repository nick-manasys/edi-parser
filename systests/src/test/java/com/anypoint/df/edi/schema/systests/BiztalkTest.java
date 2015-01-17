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
        assertEquals("ISA*00*          *00*          *ZZ*MODUS          *ZZ*ZZ             *XXXXXX*XXXX*U*00501*000000001*0*P*>~GS*FA*MULESOFT*MODUS*XXXXXXXX*XXXX*1*X*005010~ST*997*1   ~AK1*PO*4*00501~AK9*R*1*1*0~SE*4*1   ~GE*1*1~IEA*1*000000001~",
            stripDates(parseAndReturnAck("/x12/5010/biztalk-interop/850-biztalk.edi")));
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
    }

}