package com.anypoint.df.edi.schema.systests;

import static org.junit.Assert.*;

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

import com.anypoint.df.edi.schema.SchemaJavaValues;
import com.anypoint.df.edi.schema.systests.files.BadInputFiles;
import com.anypoint.df.edi.schema.tools.Decode997;
import com.anypoint.df.edi.schema.tools.DocumentTest;

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
        DocumentTest test = new DocumentTest(schema);
        InputStream is = BiztalkTest.class.getResourceAsStream(badFilePath);
        Map<String, Object> result = test.parse(is);
        List<Map<String, Object>> acks = (List<Map<String, Object>>)result.
            get(SchemaJavaValues.functionalAcknowledgments());
        assertNotNull(acks);
        assertTrue(acks.size() > 0);
        String text = Decode997.decode(acks.get(0));
        assertTrue(text.contains(expected997));
	}

	@Parameters
	public static Collection<Object[]> badEdi850Files(){ 
		
		return Arrays.asList(new Object[][]{
				{BadInputFiles.MISSING_ST, BadInputFiles.MISSING_ST_EX}, 
//				{BadInputFiles.MISSING_GROUP, BadInputFiles.MISSING_GROUP_EX},
				{BadInputFiles.MISSING_BEG, BadInputFiles.MISSING_BEG_EX}, 
				{BadInputFiles.MISSING_PO1, BadInputFiles.MISSING_PO1_EX}, 
				{BadInputFiles.MISSING_SE, BadInputFiles.MISSING_SE_EX},
                {BadInputFiles.MISSING_GE, BadInputFiles.MISSING_GE_EX},
//				{BadInputFiles.WRONG_HEADER, BadInputFiles.WRONG_HEADER_EX}
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