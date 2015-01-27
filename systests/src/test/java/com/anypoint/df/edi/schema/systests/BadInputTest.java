package com.anypoint.df.edi.schema.systests;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.Assert;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.anypoint.df.edi.schema.systests.files.BadInputFiles;

@RunWith(Parameterized.class)
public class BadInputTest extends X12TestBase {

	private static final String SCHEMA = "/x12/005010/850.esl";
	
	private String badFilePath; 
	private String badFileException; 
	
	public BadInputTest(String badFilePath, String badFileException){
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
		Assert.assertTrue(ediParseException.contains(getBadFileException()));
	}

	@Parameters
	public static Collection<Object[]> badEdi850Files(){ 
		
		return Arrays.asList(new Object[][]{
				{BadInputFiles.MISSING_ST, BadInputFiles.MISSING_ST_EX}, 
				{BadInputFiles.RANDOM_FILE, BadInputFiles.RANDOM_FILE_EX}, 
				{BadInputFiles.MISSING_GROUP, BadInputFiles.MISSING_GROUP_EX},
//				{BadInputFiles.MISSING_BEG, BadInputFiles.MISSING_BEG_EX}, 
//				{BadInputFiles.MISSING_PO1, BadInputFiles.MISSING_PO1_EX}, 
//				{BadInputFiles.MISSING_SE, BadInputFiles.MISSING_SE_EX},
				{BadInputFiles.WRONG_HEADER, BadInputFiles.WRONG_HEADER_EX}
		});
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
