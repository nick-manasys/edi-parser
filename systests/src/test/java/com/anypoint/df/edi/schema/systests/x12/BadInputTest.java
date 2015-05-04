package com.anypoint.df.edi.schema.systests.x12;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.Assert;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.anypoint.df.edi.schema.systests.x12.files.BadInputFiles;

/**
 * Test documents which are expected to throw parse exceptions.
 */
@RunWith(Parameterized.class)
public class BadInputTest extends X12TestBase
{
    
    private static final String SCHEMA = "/x12/005010/850.esl";
    
    private String badFilePath;
    private String badFileException;
    
    public BadInputTest(String badFilePath, String badFileException) {
        setBadFilePath(badFilePath);
        setBadFileException(badFileException);
    }
    
    @BeforeClass
    public static void setUpClass() {
        loadSchema(SCHEMA);
    }
    
    @Test
    public void badInput() throws Exception {
        String ediParseException = parseAndReturnAck(getBadFilePath());
        Assert.assertNotNull(ediParseException);
        if (!ediParseException.contains(getBadFileException())) {
            Assert.fail("Expected exception text '" + getBadFileException() + "', got '" + ediParseException + "'");
        }
    }
    
    @Parameters
    public static Collection<Object[]> badEdi850Files() {
        
        return Arrays.asList(new Object[][] { { BadInputFiles.RANDOM_FILE, BadInputFiles.RANDOM_FILE_EX },
            // TODO: this next one results in a TA1 rejection, not an exception, so add a different test for that
            //{ BadInputFiles.MISSING_GROUP, BadInputFiles.MISSING_GROUP_EX },
            { BadInputFiles.WRONG_HEADER, BadInputFiles.WRONG_HEADER_EX } });
    }
    
    public String getBadFilePath() {
        return badFilePath;
    }
    
    public void setBadFilePath(String badFilePath) {
        this.badFilePath = badFilePath;
    }
    
    public String getBadFileException() {
        return badFileException;
    }
    
    public void setBadFileException(String badFileException) {
        this.badFileException = badFileException;
    }
    
}