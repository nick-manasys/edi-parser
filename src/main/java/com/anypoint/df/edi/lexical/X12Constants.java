
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
    
    // group properties from GS segment
    public static final String FUNCTIONAL_IDENTIFIER_CODE = "Functional identifier code";
    public static final String APPLICATION_SENDERS_CODE = "Application sender's code";
    public static final String APPLICATION_RECEIVERS_CODE = "Application receiver's code";
    public static final String GROUP_DATE = "Group date";
    public static final String GROUP_TIME = "Group time";
    public static final String GROUP_CONTROL_NUMBER = "Group control number";
    public static final String RESPONSIBLE_AGENCY_CODE = "Responsible agency code";
    public static final String VERSION_IDENTIFIER_CODE = "Version / release / industry identifier code";
    
    // group properties from GE segment
    public static final String NUMBER_OF_TRANSACTION_SETS = "Number of transaction sets included";
//    public static final String GROUP_CONTROL_NUMBER = "Group control number";
    
    // transaction set properties from ST segment
    public static final String TRANSACTION_SET_IDENTIFIER_CODE = "Transaction set identifier code";
    public static final String TRANSACTION_SET_CONTROL_NUMBER = "Transaction set control number";
    public static final String IMPLEMENTATION_CONVENTION_REFERENCE = "Implementation convention reference";
    
    // transaction set properties from SE segment
    public static final String NUMBER_OF_INCLUDED_SEGMENTS = "Number of included segments";
//    public static final String TRANSACTION_SET_CONTROL_NUMBER = "Transaction set control number";
}
