package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;

import org.junit.Ignore;
import org.junit.Test;

import com.anypoint.df.edi.lexical.WriteException;
import com.anypoint.df.edi.schema.EdiSchema;
import com.anypoint.df.edi.schema.EdiSchemaVersion;
import com.anypoint.df.edi.schema.EdiSchema.EdiForm;
import com.anypoint.df.edi.schema.systests.TestBase;
import com.anypoint.df.edi.schema.systests.YamlSupport;
import com.anypoint.df.edi.schema.tools.JsonReader;

public class WriterTest extends EdifactTestBase {
    @Test
    public void test1Stop() throws Exception {
        String path = "/edifact/write/1stop.json";
        InputStream is = TestBase.class.getResourceAsStream(path);
        if (is == null) {
            File file = new File(path);
            if (file.exists()) {
                is = new FileInputStream(file);
            } else {
                throw new IllegalArgumentException("Path " + path + " not found");
            }
        }
        InputStreamReader reader = new InputStreamReader(is, "ASCII");
        JsonReader json = new JsonReader();
        Map<String, Object> map = json.read(reader);
        schema = new EdiSchema(new EdiSchemaVersion((EdiForm)EdiSchema.convertEdiForm("EDIFACT"), "D93A"));
        System.out.println(testWrite(map));
    }
    
    public boolean matchWriteException(Exception e, String lead) {
        return (e instanceof WriteException) && e.getMessage().startsWith(lead);
    }
    
    @Test
    public void testAPERAK() throws Exception {
        loadSchema("/edifact/d00a/APERAK.esl");
        String text = readAsString("/edifact/write/APERAK-base.yaml");
        Map<String, Object> map = YamlSupport.readMap(text);
        testWrite(map);
        text = readAsString("/edifact/write/APERAK-missing-required-seg.yaml");
        map = YamlSupport.readMap(text);
        try {
            testWrite(map);
            fail();
        } catch (Exception e) {
            assertTrue(matchWriteException(e, "missing required value"));
        }
    }
    
    @Test
    public void testORDERS() throws Exception {
        loadSchema("/edifact/d96a/ORDERS.esl");
        String text = readAsString("/edifact/write/ORDERS96a-base.yaml");
        Map<String, Object> map = YamlSupport.readMap(text);
        String baseout = testWrite(map);
        text = readAsString("/edifact/write/ORDERS96a-numbers.yaml");
        map = YamlSupport.readMap(text);
        String numsout = testWrite(map);
        assertEquals(baseout, numsout);
        text = readAsString("/edifact/write/ORDERS96a-rff-too-long.yaml");
        map = YamlSupport.readMap(text);
        try {
            testWrite(map);
            fail();
        } catch (Exception e) {
            assertTrue(matchWriteException(e, "length outside of allowed range"));
        }
        text = readAsString("/edifact/write/ORDERS96a-empty-rff-and-dtm.yaml");
        map = YamlSupport.readMap(text);
        try {
            testWrite(map);
            fail();
        } catch (Exception e) {
            assertTrue(matchWriteException(e, "missing required value"));
        }
    }
}