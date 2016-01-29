package com.anypoint.df.edi.schema.systests.x12;

import org.junit.BeforeClass;
import org.junit.Test;

public class Standard_005010_834Test extends X12TestBase {
    
    @BeforeClass
    public static void setUpClass() {
        loadSchema("/x12/005010/834.esl");
    }

    @Test
    public void verifyWrite() throws Exception {
        parseAndCheckWrite("/x12/005010/test834/834.edi");
    }
}