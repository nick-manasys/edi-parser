package com.mulesoft.flatfile.schema.systests.hl7;

import org.junit.Test;

public class Standard23Test extends HL7TestBase {
    
    @Test
    public void verifyADT_A01() throws Exception {
        loadSchema("/hl7/v2_3/ADT_A01.esl");
        parseAndCheckWrite("/hl7/v23/ADT_A01-1.hl7");
    }
}
