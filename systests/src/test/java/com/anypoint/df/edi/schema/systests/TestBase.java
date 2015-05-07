package com.anypoint.df.edi.schema.systests;

import java.io.File;
import java.io.FileInputStream;
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
            YamlReader yamlrdr = new YamlReader();
            InputStream is = yamlrdr.findSchema(path, new String[] { "" });
            schema = yamlrdr.loadYaml(new InputStreamReader(is, "ASCII"), new String[0]);
        } catch (Exception e) {
            throw new RuntimeException("Could not load schema file " + path, e);
        }
    }
    
    protected int nthOffset(char ch, boolean last, int start, int count, String str) {
        int scan = start;
        for (int i = 0; i < count; i++) {
            scan = str.indexOf(ch, scan + 1);
            if (scan < 0) {
                if (last && i == count - 1) {
                    return str.length() - 1;
                }
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
        if (is == null) {
            File file = new File(path);
            if (file.exists()) {
                is = new FileInputStream(file);
            } else {
                throw new IllegalArgumentException("Path " + path + " not found");
            }
        }
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
