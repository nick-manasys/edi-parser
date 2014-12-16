
package com.anypoint.df.edi.lexical;

/**
 * Constants for X12 documents.
 */
public final class X12Constants
{
    private X12Constants() {}
    
    // configuration properties
    public static final String CHAR_SET = "Character set";
    
    // interchange properties from ISA segment
    public static final String AUTHORIZATION_QUALIFIER = "Authorization qualifier";
    public static final String AUTHORIZATION_INFO = "Authorization info";
    public static final String SECURITY_QUALIFIER = "Security qualifier";
    public static final String SECURITY_INFO = "Security info";
    public static final String SENDER_ID_QUALIFIER = "Sender identification qualifier";
    public static final String SENDER_ID = "Sender identification";
    public static final String RECEIVER_ID_QUALIFIER = "Receiver identification qualifier";
    public static final String RECEIVER_ID = "Receiver identification";
    public static final String INTERCHANGE_DATE = "Interchange date";
    public static final String INTERCHANGE_TIME = "Interchange time";
    public static final String VERSION_ID = "Version identification";
    public static final String INTER_CONTROL = "Interchange control";
    public static final String ACK_REQUESTED = "Ack requested";
    public static final String TEST_INDICATOR = "Test indicator";
}
