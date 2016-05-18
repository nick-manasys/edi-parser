package com.mulesoft.flatfile.schema.systests.x12;

import static org.junit.Assert.assertEquals;

import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Test documents which are expected to throw parse exceptions.
 */
public class TA1Test extends X12TestBase
{
    private static final String SCHEMA = "/x12/005010/850.esl";
    
    @BeforeClass
    public static void setUpClass() {
        loadSchema(SCHEMA);
    }

    @Test
    public void verifyGeneratedAcks() {
        assertEquals("TA1*123456789*150106*1201*A*000~TA1*223456790*150106*1201*A*000~",
            parseAndReturnInterchangeAcks("/x12/005010/ta1-tests/850-multiple.edi"));
        // tests all use document with two interchanges, errors in the first to check recovery
        assertEquals("TA1*123456789*150106*1201*R*011~TA1*223456790*150106*1201*A*000~",
            parseAndReturnInterchangeAcks("/x12/005010/ta1-tests/850-bad-isa.edi"));
        assertEquals("TA1*123456789*150106*1201*E*024~TA1*223456790*150106*1201*A*000~",
            parseAndReturnInterchangeAcks("/x12/005010/ta1-tests/850-group-header-error.edi"));
        assertEquals("TA1*123456789*150106*1201*A*000~TA1*223456790*150106*1201*A*000~",
            parseAndReturnInterchangeAcks("/x12/005010/ta1-tests/850-group-number-mismatch.edi"));
        assertEquals("TA1*123456789*150106*1201*E*021~TA1*223456790*150106*1201*A*000~",
            parseAndReturnInterchangeAcks("/x12/005010/ta1-tests/850-interchange-count-error.edi"));
        assertEquals("TA1*123456789*150106*1201*E*001~TA1*223456790*150106*1201*A*000~",
            parseAndReturnInterchangeAcks("/x12/005010/ta1-tests/850-interchange-number-mismatch.edi"));
        assertEquals("TA1*123456789*150106*1201*E*022~TA1*223456790*150106*1201*A*000~",
            parseAndReturnInterchangeAcks("/x12/005010/ta1-tests/850-missing-group-header.edi"));
        assertEquals("TA1*123456789*150106*1201*A*000~TA1*223456790*150106*1201*A*000~",
            parseAndReturnInterchangeAcks("/x12/005010/ta1-tests/850-missing-group-trailer.edi"));
    }
}