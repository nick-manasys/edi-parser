package com.anypoint.df.edi.schema.systests.edifact;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.Map;

import org.junit.Test;

import com.anypoint.df.edi.schema.EdiSchema;
import com.anypoint.df.edi.schema.EdiSchema.Structure;
import com.anypoint.df.edi.schema.EdifactSchemaDefs;
import com.anypoint.df.edi.schema.SchemaJavaValues;
import com.anypoint.df.edi.schema.tools.YamlSupport;

public class WriterTest extends EdifactTestBase {
    
    public boolean matchWriteException(Exception e, String lead) {
        return e.getMessage().startsWith(lead);
    }
    
    @Test
    public void testAPERAK() throws Exception {
        // TODO: need to add link to structure schema
        loadSchema("/edifact/d00a/APERAK.esl");
        YamlSupport yaml = new YamlSupport(schema);
        String text = readAsString("/edifact/write/APERAK-base.yaml");
        Map<String, Object> map = yaml.readMap(text);
        testWrite(map);
        text = readAsString("/edifact/write/APERAK-missing-required-seg.yaml");
        map = yaml.readMap(text);
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
        YamlSupport yaml = new YamlSupport(schema);
        String text = readAsString("/edifact/write/ORDERS96a-base.yaml");
        Map<String, Object> map = yaml.readMap(text);
        String baseout = testWrite(map);
        text = readAsString("/edifact/write/ORDERS96a-numbers.yaml");
        map = yaml.readMap(text);
        String numsout = testWrite(map);
        assertEquals(baseout, numsout);
        text = readAsString("/edifact/write/ORDERS96a-rff-too-long.yaml");
        map = yaml.readMap(text);
        try {
            testWrite(map);
            fail();
        } catch (Exception e) {
            assertTrue(matchWriteException(e, "length outside of allowed range"));
        }
        text = readAsString("/edifact/write/ORDERS96a-empty-rff-and-dtm.yaml");
        map = yaml.readMap(text);
        try {
            testWrite(map);
            fail();
        } catch (Exception e) {
            assertTrue(matchWriteException(e, "missing required value"));
        }
        text = readAsString("/edifact/write/ORDERS96a-too-many-cux.yaml");
        map = yaml.readMap(text);
        try {
            testWrite(map);
            fail();
        } catch (Exception e) {
            assertTrue(matchWriteException(e, "too many values present for group/loop"));
        }
        text = readAsString("/edifact/write/ORDERS96a-badchar.yaml");
        map = yaml.readMap(text);
        try {
            testWrite(map);
            fail();
        } catch (Exception e) {
            assertTrue(matchWriteException(e, "invalid character ^ in data"));
        }
    }
}