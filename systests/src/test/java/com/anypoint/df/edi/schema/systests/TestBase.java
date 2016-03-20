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
    
    /**
     * Load schema from file or resource path.
     * 
     * @param path
     */
    protected static void loadSchema(String path) {
        try {
            YamlReader yamlrdr = new YamlReader();
            InputStream is = yamlrdr.findSchema(path, new String[] { "" });
            schema = yamlrdr.loadYaml(new InputStreamReader(is, "ASCII"), new String[0]);
        } catch (Exception e) {
            throw new RuntimeException("Could not load schema file " + path, e);
        }
    }

    /**
     * Load file as input stream.
     * 
     * @param path
     * @return stream
     */
    protected static InputStream loadFile(String path) {
        InputStream is = TestBase.class.getResourceAsStream(path);
        if (is == null) {
            throw new IllegalArgumentException("File " + path + " not found");
        }
        return is;
    }
    
    /**
     * Find nth occurrence of character after start position. Throws an exception if the required count of the character
     * is not found, unless the count is one short and the last flag is set (useful when following data in segment is
     * optional).
     * 
     * @param ch character to be found
     * @param last may be last field of segment flag
     * @param start position before start of search
     * @param count number of occurrences to be found
     * @param str
     * @return position
     */
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
    
    /**
     * Replace substring with substitution character.
     * 
     * @param replace
     * @param start
     * @param limit
     * @param str
     * @return
     */
    protected String replaceRange(char replace, int start, int limit, String str) {
        StringBuilder builder = new StringBuilder(str);
        for (int i = start; i < limit; i++) {
            builder.setCharAt(i, replace);
        }
        return builder.toString();
    }
    
    /**
     * Delete substring.
     * 
     * @param start
     * @param limit
     * @param str
     * @return
     */
    protected static String deleteRange(int from, int to, String text) {
        return text.substring(0, from) + text.substring(to);
    }
    
    /**
     * Read a file as a string.
     * 
     * @param path file or resource path
     * @return
     * @throws IOException
     */
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
