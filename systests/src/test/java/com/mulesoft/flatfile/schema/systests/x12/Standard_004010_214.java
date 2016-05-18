package com.mulesoft.flatfile.schema.systests.x12;

import org.junit.BeforeClass;
import org.junit.Test;

public class Standard_004010_214 extends X12TestBase {
    
    @BeforeClass
    public static void setUpClass() {
        loadSchema("/x12/004010/214.esl");
    }

    @Test
    public void verifyWrite() throws Exception {
        parseAndCheckWrite("/x12/004010/support/214-line-ending.edi");
    }
}