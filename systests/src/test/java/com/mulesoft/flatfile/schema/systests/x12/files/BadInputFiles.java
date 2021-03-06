package com.mulesoft.flatfile.schema.systests.x12.files;

public final class BadInputFiles {
	
	/** Common Text file not related to EDI **/ 
	public static final String RANDOM_FILE = "/x12/005010/bad-input/random-file.edi";
	public static final String RANDOM_FILE_EX = "missing ISA segment"; 
	
	/** Missing GS line **/ 
	public static final String MISSING_GROUP = "/x12/005010/bad-input/850-group.edi"; 
	public static final String MISSING_GROUP_EX = "not at trailer";
	
	/** Missing ST Segment**/
	public static final String MISSING_ST = "/x12/005010/bad-input/850-missing-st.edi"; 
	public static final String MISSING_ST_EX = "Rejected, contained 1 transaction set(s) with 0 received and 0 accepted";
	
	/** Missing BEG Segment**/
	public static final String MISSING_BEG = "/x12/005010/bad-input/850-missing-beg.edi"; 
	public static final String MISSING_BEG_EX = "Segment BEG at position 15 has syntax error Mandatory segment missing";
	
	/** Missing PO1 Segment**/
	public static final String MISSING_PO1= "/x12/005010/bad-input/850-missing-po1.edi"; 
	public static final String MISSING_PO1_EX = "Segment PO1 at position 21 has syntax error Mandatory segment missing";
	
	/** Missing SE Segment**/
	public static final String MISSING_SE= "/x12/005010/bad-input/850-missing-se.edi"; 
	public static final String MISSING_SE_EX = "Error codes:  Transaction Set Trailer Missing";
    
    /** Missing SE Segment**/
    public static final String BAD_REPLACES_SE= "/x12/005010/bad-input/850-unknown-segment-se.edi"; 
    public static final String BAD_REPLACES_SE_EX = "Error codes:  Transaction Set Trailer Missing";
    
    /** Missing GE Segment**/
    public static final String MISSING_GE= "/x12/005010/bad-input/850-missing-ge.edi"; 
    public static final String MISSING_GE_EX = "Error codes:  Functional Group Trailer Missing";
	
}
