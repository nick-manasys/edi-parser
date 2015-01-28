package com.anypoint.df.edi.schema.systests;

import junit.framework.Assert;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class SenderQualifierTest extends X12TestBase {

	private static final String SCHEMA = "/x12/005010/850.esl";
	
	private String inputFilePath;
	private String exceptionText;
	
	public SenderQualifierTest(){
		setInputFilePath("/x12/005010/biztalk-interop/850x1.edi");
		setExceptionText("Interchange sender infromation does not match configuration");
	}
	
	
	@BeforeClass
	public static void setUpClass() {
		loadSchema(SCHEMA);
	}
	
	@Test
	public void qualifierMissmatch() throws Exception {
		String ediParseResult = parsWithSenderIdentityInformation(getInputFilePath(), "AA", "BB", "TEST");
		Assert.assertTrue(ediParseResult.contains(getExceptionText()));
	}
	
	@Ignore
	@Test 
	public void qualifierMatch() throws Exception {
		String ediParseResult = parsWithSenderIdentityInformation(getInputFilePath(), "ZZ", "MULESOFT", "");
		Assert.assertFalse(ediParseResult.contains(getExceptionText()));
	}

	
	private String getInputFilePath(){
		return this.inputFilePath; 
	}
	
	private void setInputFilePath(String filePath){
		this.inputFilePath = filePath; 
	}
	
	private String getExceptionText(){
		return this.exceptionText; 
	}
	
	private void setExceptionText(String text){
		this.exceptionText = text; 
	}
}

