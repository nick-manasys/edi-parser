TO: 	ASC X12 Diskette Purchasers
FROM: 	Data Interchange Standards Association, Inc.
DATE:	October 2003
RE: 	ASC X12 005010 - Interactive Standards
     	Copyright 2003, Data Interchange Standards Association, Inc.
     	All Rights Reserved.      
==============================================================================

INTRODUCTION
============
Enclosed please find the nine (9) data files which represent the variable 
length format of ASC X12 005010 Interactive Standards. These 
data files are inert raw data intended to be imported into existing 
applications. This diskette does not contain application programs and is not 
intended to replace the X12 standards documentation.

X12 SECRETARIAT
===============
Following is the address and phone number of the ASC X12 Secretariat, 
Data Interchange Standards Association, Inc. (DISA). DISA publishes this 
diskette and comments and questions should be directed here:

  DISA
  X12 Operations Department
  7600 Leesburg Pike, Suite 430
  Falls Church, VA 22043
  Voice: (703) 970-4480
  Fax: (703) 970-4488

  
STANDARDS CHANGE SUMMARY
========================
X12 supports a continual data maintenance process which results in 
thousands of changes to the X12 standards each year. Each year's 
publication -- the release -- is not frontward or backwards compatible 
with other releases. For a complete standards change summary, please 
refer to the X12 standards documentation.

     COMMONLY ASKED QUESTIONS
     ========================

  Q: I found an error in the data. What should I do?
  A: Contact the Secretariat's X12 Operations Department at (703) 548-7005.

  Q: I cannot find the code definitions. Where are they?
  A: The ASC X12 interactive directory does not, for the most part, contain 
     any code values. Instead, data elements that require codes point to 
     the ASC X12 batch directory and its respective code lists.
     
     However, the syntax rules specific to interactive EDI (I-EDI) and 
     corresponding I-EDI service directories (segments, composites, data
     elements) that this version/release uses now contains several codes in
     in data elements used in the messages' header and trailer segments.

     The data structures are clearly defined below. You will find these 
     code definitions in the data file FREEFORM.TXT.


INCLUDED FILES
==============
The following nine (9) files comprise this variable length representation:

  1. SETHEAD.INT
  2. SETDETL.INT
  3. SEGHEAD.INT
  4. SEGDETL.INT
  5. COMHEAD.INT
  6. COMDETL.INT
  7. ELEHEAD.INT
  8. ELEDETL.INT
  9. FREEFORM.INT

The core syntactical data is represented in data files 1-8. All other 
free-form textual data is represented in data file 9.

FILE STRUCTURE FOR CORE SYNTACTICAL DATA
========================================
An explanation of the file layouts for the core syntactical data follows. 
Note that the core syntactical data is quote-comma delimited. Lengths 
expressed are current maximum and may change as the standards require.

  SETHEAD.INT
  -----------
   Fields: Message ID, Message Name
  Lengths: 6, 80, 2
  Example: "IHCEBI","Interactive Health Care Eligibility/Benefit Inquiry
                     Message"

  SETDETL.INT
  -----------
   Fields: Message ID, Area, Sequence, Segment ID, Requirement, 
           Maximum Use, Loop Level, Loop Repeat, Loop Identifier
  Lengths: 6, 1, 3, 3, 1, 6, 1, 6, 4
  Example: "IHCEBI","1","050","PVD","C","1","0","",""

  SEGHEAD.INT
  -----------
   Fields: Segment ID, Segment Name
  Lengths: 3, 80
  Example: "COO","Coordination of Benefits"

  SEGDETL.INT
  -----------
   Fields: Segment ID, Sequence, Data Element Number, Requirement, Repeat
  Lengths: 3, 2, 4, 1, 2
  Example: "COO","01","I001","C","1"

  COMHEAD.INT
  -----------
   Fields: Composite Data Element Number, Composite Name
  Lengths: 4, 80
  Example: "I001","Reference Number"

  COMDETL.INT
  -----------
   Fields: Composite Data Element Number, Sequence, Data Element
           Number, Requirement
  Lengths: 4, 2, 4, 1
  Example: "I001","01","1154","M"

  ELEHEAD.INT
  -----------
   Fields: Data Element Number, Data Element Name
  Lengths: 4, 80
  Example: "0017","Date"

  ELEDETL.INT
  -----------
   Fields: Data Element Number, Data Element Type, Minimum Length, 
           Maximum Length
  Lengths: 4, 2, 6, 6
  Example: "0017","N","6","6"

FILE STRUCTURE FOR FREE-FORM TEXTUAL DATA
=========================================
An explanation of the file layouts for the free-form textual data 
(FREEFORM.INT) follows. Note that the free-form textual data is 
represented in a simple custom format. The rules for this format 
are described below.

  FREEFORM.INT RULES
  ------------------
  - This file contains all of the free-form text within the standard.

  - Lines of data are terminated with a carriage return/line feed.

  - Lines of data in which the first character is an asterisk (*),
    indicate the beginning of a new piece of free-form data. A six
    character tag indicates the type of free-form data that follows.

  - There are currently 9 different types of data (described below)
    in this file.

  - The line with the asterisk (the tagged line) indicates the type of
    free-form data. The line immediately following the tagged line (the
    key line) specifies to what standards entity the free form data
    belongs. The key line may have one or more entries separated by
    commas.

  - Lines of data may end with a space character followed by a carriage
    return/line feed. This indicates that the next line is the
    continuation of the preceding line.

  - If a line of data ends simply with a carriage return, this may
    indicate that a carriage return is required within this piece of free-
    form data, or that this piece of data ends on this line. The first
    character of the next line indicates which of these two choices is
    applicable. If the next line of data does not begin with an asterisk,
    the preceding line is a continuation of the data. If the next line
    begins with an asterisk, a new piece of free form data is beginning.

  INCLUDED FREE-FORM TEXT
  -----------------------

  1. Message Purpose/Scope
  --------------------------------
  Tag Line: *SETPUR
  Key Line: Message ID
   Lengths: 6
   Example: *SETPUR
            IHCEBI
            This standard provides the format and establishes the data 
            contents of an interactive health care eligibility ...

  2. Message Notes/Comments
  ---------------------------------
  Tag Line: *SETNTE
  Key Line: Message ID, Area, Sequence, Note Type, Paragraph Number
   Lengths: 6, 1, 3, 1, 1
     Notes: See ISO 9735 for a description of the coded syntax notes.
   Example: *SETNTE
            IHCEBI,1,070,C,1
            If there is no PTT, then the patient is the subscriber.

  3. Segment Purpose
  ------------------
  Tag Line: *SEGPUR
  Key Line: Segment ID
   Lengths: 3
   Example: *SEGPUR
            COO
            To provide coordination of benefit information.

  4. Segment Notes/Comments
  -------------------------
  Tag Line: *SEGNTE
  Key Line: Segment ID, Sequence, Note Type, Paragraph Number
   Lengths: 3, 2, 1, 1
     Notes: See ISO 9735 for a description of the coded syntax notes.
   Example: *SEGNTE
            COO,01,C,1
            This is the payor identification number.
            *SEGNTE
            COO,01,N,1
            D3(01,02)

  5. Composite Data Element Purpose
  ---------------------------------
  Tag Line: *COMPUR
  Key Line: Composite ID
   Lengths: 4
   Example: *COMPUR
            I001
            To identify a reference number.

  6. Composite Data Element Notes/Comments
  ----------------------------------------
  Tag Line: *COMNTE
  Key Line: Composite ID, Sequence, Note Type, Paragraph Number
   Lengths: 3, 2, 1, 1
     Notes: See ISO 9735 for a description of the coded syntax notes.
   Example: *COMNTE
            I001,01,N,1
            D3(01,02)

  7. Simple Data Element Definitions
  ----------------------------------
  Tag Line: *ELEDEF
  Key Line: Data Element Number
   Lengths: 4
   Example: *ELEDEF
            0017
            Local date when an interchange or a functional group was 
            prepared.

  8. Simple Data Element Expanded Definitions
  -------------------------------------------
  Tag Line: *ELEEXP
  Key Line: Data Element Number
   Lengths: 4
   Example: *ELEEXP
            0017
            Format is YYMMDD.

  9. Simple Data Element Code Definitions
  ---------------------------------------
  Tag Line: *ELECOD
  Key Line: Data Element Number, Partition Number, Code Value, 
            Paragraph Number
   Lengths: 4, 1, 6, 1
     Notes: 1) "Partition Number" is used for ASC X12 multi-part codes,
            however this is not used in the Interactive directory.

            2) "Paragraph Number" is used when there is more than one 
            definition for any one code value. As there is no instance of 
            this in the interactive directory, the paragraph number is 
            always "1".
   Example: *ELECOD
            0322, ,F,1
            First message
            *ELECOD
            0322, ,I,1
            Intermediate

==============================================================================
<end of readme-i.txt>
