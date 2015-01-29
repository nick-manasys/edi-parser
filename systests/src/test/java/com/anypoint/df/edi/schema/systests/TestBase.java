package com.anypoint.df.edi.schema.systests;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;

import com.anypoint.df.edi.schema.EdiSchema;
import com.anypoint.df.edi.schema.YamlReader;

public abstract class TestBase {

    protected static EdiSchema schema;
    
    protected static void loadSchema(String path) {
        try {
            InputStream is = YamlReader.findSchema(path, new String[] { "" });
            schema = YamlReader.loadYaml(new InputStreamReader(is, "ASCII"), new String[0]);
        } catch (Exception e) {
            throw new RuntimeException("Could not load schema file " + path, e);
        }
    }
    
    protected int nthOffset(char ch, int start, int count, String str) {
        int scan = start;
        for (int i = 0; i < count; i++) {
            scan = str.indexOf(ch, scan + 1);
            if (scan < 0) {
                throw new IllegalArgumentException("Expected " + i + "th occurrence of '" + ch + "' not found");
            }
        }
        return scan;
    }
    
    protected String replaceRange(char replace, int start, int limit, String str) {
        StringBuilder builder = new StringBuilder(str);
        for (int i = start; i < limit; i++) {
            builder.setCharAt(i, replace);
        }
        return builder.toString();
    }
    
    protected String readAsString(String path) throws IOException {
        InputStream is = TestBase.class.getResourceAsStream(path);
        InputStreamReader reader = new InputStreamReader(is, "ASCII");
        StringWriter writer = new StringWriter();
        char[] buff = new char[2048];
        int actual;
        while ((actual = reader.read(buff)) > 0) {
            writer.write(buff, 0, actual);
        }
        is.close();
        return writer.toString();
    }
}
