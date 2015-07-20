package com.anypoint.df.edi.schema.systests.edifact;

import org.junit.Test;

public class StandardD00aTest extends EdifactTestBase {
    
    @Test
    public void verifyAPERAK() throws Exception {
        loadSchema("/edifact/d00a/APERAK.esl");
        parseAndCheckWrite("/edifact/d00a/PRA-RESPONSE.edi");
    }
}