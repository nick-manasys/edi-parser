package com.mulesoft.flatfile.schema.systests.x12;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.Map;

import org.junit.BeforeClass;
import org.junit.Test;

import com.mulesoft.flatfile.schema.tools.DocumentTest;
import com.mulesoft.flatfile.schema.tools.DocumentTestX12;

public class Standard_005010_834Test extends X12TestBase {
    
    @BeforeClass
    public static void setUpClass() {
        loadSchema("/x12/005010/834.esl");
    }

    @Test
    public void verifyWrite() throws Exception {
        DocumentTest test = new DocumentTestX12(schema, false);
        String text = readAsString("/x12/005010/test834/834.edi");
        Map<String, Object> result = test.parse(new ByteArrayInputStream(text.getBytes("ASCII")));
        checkWrite(test, text, result);
        
        // verify name used for loop structures
        assertTrue(result.containsKey("TransactionSets"));
        Map<String, Object> versions = (Map<String, Object>)result.get("TransactionSets");
        assertTrue(versions.containsKey("v005010"));
        Map<String, Object> types = (Map<String, Object>)versions.get("v005010");
        assertTrue(types.containsKey("834"));
        List<Map<String, Object>> trans = (List<Map<String, Object>>)types.get("834");
        assertEquals(1, trans.size());
        Map<String, Object> root = trans.get(0);
        assertTrue(root.containsKey("Heading"));
        Map<String, Object> header = (Map<String, Object>)root.get("Heading");
        assertTrue(header.containsKey("1000_Loop"));
        List<Map<String, Object>> loops = (List<Map<String, Object>>)header.get("1000_Loop");
        assertEquals(2, loops.size());
        
        parseAndCheckWrite("/x12/005010/test834/834-2.edi");
    }
}