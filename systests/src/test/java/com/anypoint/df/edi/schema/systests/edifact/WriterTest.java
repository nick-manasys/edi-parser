package com.anypoint.df.edi.schema.systests.edifact;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;

import org.junit.Ignore;
import org.junit.Test;

import com.anypoint.df.edi.schema.EdiSchema;
import com.anypoint.df.edi.schema.EdiSchema.EdiForm;
import com.anypoint.df.edi.schema.systests.TestBase;
import com.anypoint.df.edi.schema.tools.JsonReader;

public class WriterTest extends EdifactTestBase {
    @Test
    public void test1Stop() throws Exception {
        String path = "/edifact/general/1stop.json";
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
        schema = new EdiSchema((EdiForm)EdiSchema.convertEdiForm("EDIFACT"), "D93A");
        System.out.println(testWrite(map));
    }
}