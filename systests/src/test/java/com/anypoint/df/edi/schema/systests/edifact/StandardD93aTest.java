package com.anypoint.df.edi.schema.systests.edifact;

import org.junit.Ignore;
import org.junit.Test;

public class StandardD93aTest extends EdifactTestBase {
    
    @Test
    public void verifyDESADV() throws Exception {
        loadSchema("/edifact/d93a/DESADV.esl");
        parseAndCheckWrite("/edifact/d93a/DESADV.edi");
    }
    
    // needs 8859-1 encoding
    @Test
    @Ignore
    public void verifyINVOIC() throws Exception {
        loadSchema("/edifact/d93a/INVOIC.esl");
        parseAndCheckWrite("/edifact/d93a/INVOIC-german.edi");
        parseAndCheckWrite("/edifact/d93a/invoic-d93a.edi");
        parseAndCheckWrite("/edifact/d93a/multiple-invoic-d93a.edi");
    }
    
    // needs 8859-1 encoding
    @Test
    @Ignore
    public void verifyORDERS() throws Exception {
        loadSchema("/edifact/d93a/ORDERS.esl");
        parseAndCheckWrite("/edifact/d93a/ORDERS_D93A.edi");
    }
}