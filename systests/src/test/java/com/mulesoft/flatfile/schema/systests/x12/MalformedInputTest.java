package com.mulesoft.flatfile.schema.systests.x12;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.mulesoft.flatfile.schema.SchemaJavaValues;
import com.mulesoft.flatfile.schema.systests.x12.files.BadInputFiles;
import com.mulesoft.flatfile.schema.tools.Decode997;
import com.mulesoft.flatfile.schema.tools.DocumentTest;
import com.mulesoft.flatfile.schema.tools.DocumentTestX12;

/**
 * Tests documents generating 997 errors.
 */
@RunWith(Parameterized.class)
public class MalformedInputTest extends X12TestBase {

	private static final String SCHEMA = "/x12/005010/850.esl";
	
	private String badFilePath; 
	private String expected997; 
	
	public MalformedInputTest(String badFilePath, String expected997){
		setBadFilePath(badFilePath);
		setExpected997(expected997);
	}
	
	@BeforeClass
	public static void setUpClass() {
		loadSchema(SCHEMA);
	}

	@Test
	public void badInput() throws Exception {
        DocumentTest test = new DocumentTestX12(schema, false);
        InputStream is = BiztalkTest.class.getResourceAsStream(badFilePath);
        Map<String, Object> result = test.parse(is);
        List<Map<String, Object>> acks = (List<Map<String, Object>>)result.
            get(SchemaJavaValues.functionalAcksGenerated());
        assertNotNull(acks);
        assertTrue(acks.size() > 0);
        String text = Decode997.decode(acks.get(0));
        // TODO: restore tests broken by UHG changes
//        System.out.println(text);
//        System.out.println("matching '" + expected997 + "'");
//        assertTrue(text.contains(expected997));
	}

	@Parameters
	public static Collection<Object[]> badEdi850Files(){ 
		
		return Arrays.asList(new Object[][]{
				{BadInputFiles.MISSING_ST, BadInputFiles.MISSING_ST_EX}, 
				{BadInputFiles.MISSING_BEG, BadInputFiles.MISSING_BEG_EX}, 
				{BadInputFiles.MISSING_PO1, BadInputFiles.MISSING_PO1_EX}, 
				{BadInputFiles.MISSING_SE, BadInputFiles.MISSING_SE_EX},
                {BadInputFiles.BAD_REPLACES_SE, BadInputFiles.BAD_REPLACES_SE_EX},
                {BadInputFiles.MISSING_GE, BadInputFiles.MISSING_GE_EX}
		});
	}
	
	
	public String getBadFilePath() {
		return badFilePath;
	}


	public void setBadFilePath(String badFilePath) {
		this.badFilePath = badFilePath;
	}


	public String getExpected997() {
		return expected997;
	}


	public void setExpected997(String expected997) {
		this.expected997 = expected997;
	}

}
