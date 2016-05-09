package com.mulesoft.flatfile.lexical;

import static com.mulesoft.flatfile.lexical.EdifactConstants.SYNTAX_IDENTIFIER;
import static com.mulesoft.flatfile.lexical.EdifactConstants.SYNTAX_VERSION_NUMBER;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

import com.mulesoft.flatfile.lexical.EdiConstants.*;
import com.mulesoft.flatfile.lexical.EdifactLexer;

/**
 * EdiFactParser test
 */
public class EdifactLexerTest
{
    private static final String EDI1 = "UNA:+.? 'UNB+UNOB:4+STYLUSSTUDIO:1+DATADIRECT:1+20051107:1159+6002'UNZ+1+6002'";
    private static final Map<String, Object> EXPECT1;
    static {
        EXPECT1 = new HashMap<>();
        EXPECT1.put(SYNTAX_IDENTIFIER, "UNOB");
        EXPECT1.put(SYNTAX_VERSION_NUMBER, "4");
//        EXPECT1.put(SENDER_IDENTIFICATION, "STYLUSSTUDIO");
//        EXPECT1.put(SENDER_IDENTIFICATION_CODE_QUALIFIER, "1");
//        EXPECT1.put(RECIPIENT_IDENTIFICATION, "DATADIRECT");
//        EXPECT1.put(RECIPIENT_IDENTIFICATION_CODE_QUALIFIER, "1");
//        EXPECT1.put(PREPARATION_DATE, "20051107");
//        EXPECT1.put(PREPARATION_TIME, "1159");
//        EXPECT1.put(INTERCHANGE_CONTROL_REFERENCE, "6002");
    }
    
    private static final String EDI1_FULL =
        "UNA:+.? '\n" +
        "UNB+UNOB:4+STYLUSSTUDIO:1+DATADIRECT:1+20051107:1159+6002'\n" +
        "UNH+SSDD1+ORDERS:D:03B:UN:EAN008'\n" +
        "BGM+220+BKOD99+9'\n" +
        "DTM+137:20051107:102'\n" +
        "NAD+BY+5412345000176::9'\n" +
        "NAD+SU+4012345000094::9'\n" +
        "LIN+1+1+0764569104:IB'\n" +
        "QTY+1:25'\n" +
        "FTX+AFM+1++XPath 2.0 Programmer?'s Reference'\n" +
        "LIN+2+1+0764569090:IB'\n" +
        "QTY+1:25'\n" +
        "FTX+AFM+1++XSLT 2.0 Programmer?'s Reference'\n" +
        "LIN+3+1+1861004656:IB'\n" +
        "QTY+1:16'\n" +
        "FTX+AFM+1++Java Server Programming'\n" +
        "LIN+4+1+0596006756:IB'\n" +
        "QTY+1:10'\n" +
        "FTX+AFM+1++Enterprise Service Bus'\n" +
        "UNS+S'\n" +
        "CNT+2:4'\n" +
        "UNZ+1+6002'\n";
    
//    @Test
//    public void simpleEnvelope() throws Exception {
//        EdifactLexer parser = new EdifactLexer(new ByteArrayInputStream(EDI1.getBytes("US-ASCII")), -1);
//        Map<String, Object> props = new HashMap<>();
//        parser.init(Collections.EMPTY_MAP, props);
//        Assert.assertEquals(EXPECT1, props);
//        Assert.assertEquals(ItemType.SEGMENT, parser.nextType());
//        Assert.assertEquals("UNZ", parser.advance());
//        Assert.assertEquals(ItemType.DATA_ELEMENT, parser.nextType());
//        Assert.assertEquals("1", parser.advance());
//        Assert.assertEquals(ItemType.DATA_ELEMENT, parser.nextType());
//        Assert.assertEquals("6002", parser.advance());
//        Assert.assertEquals(ItemType.SEGMENT, parser.nextType());
//        Assert.assertEquals("", parser.advance());
//        Assert.assertEquals(ItemType.END, parser.nextType());
//    }
//    
//    public static void main(String[] args) throws Exception {
//        InputStream is = new ByteArrayInputStream(EDI1_FULL.getBytes("US-ASCII"));
//        EdifactLexer parser = new EdifactLexer(is, -1);
//        Map<String, Object> props = new HashMap<>();
//        parser.init(Collections.EMPTY_MAP, props);
//        while (ItemType.END != parser.nextType()) {
//            switch (parser.nextType()) {
//                case DATA_ELEMENT:
//                    System.out.print(" ");
//                    break;
//                default:
//                    System.out.print("  ");
//            }
//            System.out.println(parser.nextType() + ": " + parser.advance());
//        }
//    }
}