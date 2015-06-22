package com.anypoint.df.edi.schema.systests.edifact;

import org.junit.Test;

public class StandardD98bTest extends EdifactTestBase {
    
    @Test
    public void verifyIFCSUM() throws Exception {
        loadSchema("/edifact/d98b/IFCSUM.esl");
        parseAndCheckWrite("/edifact/d98b/EIDO-IFCSUM.edi");
    }
    
    @Test
    public void verifyIFTERA() throws Exception {
        loadSchema("/edifact/d98b/IFTERA.esl");
        parseAndCheckWrite("/edifact/d98b/PRA-IFTERA.edi");
        parseAndCheckWrite("/edifact/d98b/IFTERA_attached-corrected.edi");
        parseAndCheckWrite("/edifact/d98b/IFTERA_fixed.edi");
        parseAndCheckWrite("/edifact/d98b/IFTERA_hazardous.edi");
        parseAndCheckWrite("/edifact/d98b/IFTERA_original.edi");
        // TODO: fails because of comma decimal marker on input written as decimal point on output, need to configure output
//        parseAndCheckWrite("/edifact/d98b/IFTERA_refrigerated.edi");
    }
}