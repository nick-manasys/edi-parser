
package com.anypoint.df.edi.parser;

/**
 * Constants for EDIFACT documents.
 */
public final class EdiFactConstants
{
    private EdiFactConstants() {}
    
    public static final String SYNTAX_IDENTIFIER = "Syntax identifier";
    public static final String SYNTAX_VERSION_NUMBER = "Syntax version number";
    public static final String SENDER_IDENTIFICATION = "Sender identification";
    public static final String SENDER_IDENTIFICATION_CODE_QUALIFIER = "Sender partner identification code qualifier";
    public static final String REVERSE_ROUTING_ADDRESS = "Address for reverse routing";
    public static final String RECIPIENT_IDENTIFICATION = "Recipient identification";
    public static final String RECIPIENT_IDENTIFICATION_CODE_QUALIFIER =
        "Recipient partner identification code qualifier";
    public static final String RECIPIENT_ROUTING_ADDRESS = "Onward routing address";
    public static final String PREPARATION_DATE = "Date of preparation";
    public static final String PREPARATION_TIME = "Time of preparation";
    public static final String INTERCHANGE_CONTROL_REFERENCE = "Interchange control reference";
    public static final String RECIPIENT_REFERENCE = "Recipient's reference/password";
    public static final String RECIPIENT_REFERENCE_QUALIFIER = "Recipient's reference/password qualifier";
    public static final String APPLICATION_REFERENCE = "Application reference";
    public static final String PROCESSING_PRIORITY_CODE = "Processing priority code";
    public static final String ACKNOWLEDGEMENT_REQUEST = "Acknowledgement request";
    public static final String COMMUNICATIONS_AGREEMENT_ID = "Communications agreement id";
    public static final String TEST_INDICATOR = "Test indicator";
}
