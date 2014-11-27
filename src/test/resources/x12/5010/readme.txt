TO: 	ASC X12 Diskette Purchasers
FROM: 	Data Interchange Standards Association, Inc.
DATE: 	October 2003
RE:	ASC X12 005010 - Batch Standards
     	Copyright 2003, Data Interchange Standards Association, Inc.
      	All Rights Reserved.
==============================================================================

INTRODUCTION
============
Enclosed please find the twelve (12) data files which represent the variable 
length format of ASC X12 005010 Batch Standards. These data files are inert 
raw data intended to be imported into existing applications. This diskette 
does not contain application programs and is not intended to replace the 
X12 standards documentation.

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
  A: The data structures are clearly defined below. You will find the 
     code definitions in the data file FREEFORM.TXT.

  Q: Why is the segment requirement "C" in the data and "X" in the book?
  A: The representation for the conditional segment requirement in the 
     paper documentation changed from "C" to "X" in Version 003 Release 
     020. Rather than disrupt the installed user base we continued to 
     use "C" to represent a conditional requirement in the data file 
     SEGDETL.TXT. If you wish, you may change every "C" to "X" to be 
     consistent with the paper documentation.

  Q: Is the information for the external code sources referenced from
     the X12 standards available as importable data in these ASC X12 
     variable length data files?
  A: Yes, beginning at version 004021 this information has been added to 
     the ASC X12 Table Data files with the addition of three new data 
     files: CSHEAD.TXT, CS_DE.TXT and CS_CODE.TXT and five (5) new types 
     of free-form textual data being added to the FREEFORM.TXT file.  An 
     explanation of these file layouts and free-form textual data types 
     may be found below in the descriptions for file structures for core 
     syntactical data and free-form textual data, respectively.
     
INCLUDED FILES
==============
The following twelve (12) files comprise this variable length representation:

  1.  SETHEAD.TXT
  2.  SETDETL.TXT
  3.  SEGHEAD.TXT
  4.  SEGDETL.TXT
  5.  COMHEAD.TXT
  6.  COMDETL.TXT
  7.  ELEHEAD.TXT
  8.  ELEDETL.TXT
  9.  CSHEAD.TXT
  10. CS_DE.TXT
  11. CS_CV.TXT
  12. FREEFORM.TXT

The core syntactical data is represented in data files 1-11. All other 
free-form textual data is represented in data file 12.

FILE STRUCTURE FOR CORE SYNTACTICAL DATA
========================================
An explanation of the file layouts for the core syntactical data follows. 
Note that the core syntactical data is quote-comma delimited. Lengths 
expressed are current maximum and may change as the standards require.

  SETHEAD.TXT
  -----------
   Fields: Transaction Set ID, Transaction Set Name, Functional Group ID
  Lengths: 3, 80, 2
  Example: "810","Invoice","IN"

  SETDETL.TXT
  -----------
   Fields: Transaction Set ID, Area, Sequence, Segment ID, Requirement, 
           Maximum Use, Loop Level, Loop Repeat, Loop Identifier
  Lengths: 3, 1, 4, 3, 1, 6, 1, 6, 4
  Example: "810","2","0100","IT1","O","1","1","200000","IT1"

  SEGHEAD.TXT
  -----------
   Fields: Segment ID, Segment Name
  Lengths: 3, 80
  Example: "A1","Rejection"

  SEGDETL.TXT
  -----------
   Fields: Segment ID, Sequence, Data Element Number, Requirement, 
           Repetition Count
  Lengths: 3, 2, 4, 1, 2
  Example: "A1","01","131","M","1"

  COMHEAD.TXT
  -----------
   Fields: Composite Data Element Number, Composite Name
  Lengths: 4, 80
  Example: "C001","Composite Unit of Measure"

  COMDETL.TXT
  -----------
   Fields: Composite Data Element Number, Sequence, Data Element
           Number, Requirement
  Lengths: 4, 2, 4, 1
  Example: "C001","01","355","M"

  ELEHEAD.TXT
  -----------
   Fields: Data Element Number, Data Element Name
  Lengths: 4, 80
  Example: "1","Route Code"

  ELEDETL.TXT
  -----------
   Fields: Data Element Number, Data Element Type, Minimum Length, 
           Maximum Length
  Lengths: 4, 2, 6, 6
  Example: "1","AN","1","13"

  CSHEAD.TXT
  ----------
   Fields: Code Source Number, Code Source Name
  Lengths: 4, 150
  Example: "5","Countries, Currencies and Funds"

  CS_DE.TXT
  ---------
   Fields: Code Source Number, Simple Data Element Referencing Code 
           Source
  Lengths: 4, 4
  Example: "11","59"

  CS_CV.TXT
  -----------
   Fields: Code Source Number, Data Element Number, Data Element Code 
           Referencing Code Source
  Lengths: 4, 4, 6
  Example: "11","1270","NK"
  
FILE STRUCTURE FOR FREE-FORM TEXTUAL DATA
=========================================
An explanation of the file layouts for the free-form textual data 
(FREEFORM.TXT) follows. Note that the free-form textual data is 
represented in a simple custom format. The rules for this format 
are described below.

  FREEFORM.TXT RULES
  ------------------
  - This file contains all of the free-form text within the standard.

  - Lines of data are terminated with a carriage return/line feed.

  - Lines of data in which the first character is an asterisk (*),
    indicate the beginning of a new piece of free-form data. A six
    character tag indicates the type of free-form data that follows.

  - There are currently 14 different types of data (described below)
    in this file.

  - The line with the asterisk (the tagged line) indicates the type of
    free-form data. The line immediately following the tagged line (the
    key line) specifies to what standards entity the free-form data
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
    begins with an asterisk, a new piece of free-form data is beginning.

  INCLUDED FREE-FORM TEXT
  -----------------------

  1. Transaction Set Purpose/Scope
  --------------------------------
  Tag Line: *SETPUR
  Key Line: Transaction Set ID
   Lengths: 3
   Example: *SETPUR
            810
            This Draft Standard for Trial Use contains the format and
            establishes the data contents of the Invoice Transaction Set
            (810) for use within the context of an Electronic Data
            Interchange (EDI) environment. The transaction set can be used
            for customary and established business and industry practice
            relative to the billing for goods and services provided.

  2. Transaction Set Notes/Comments
  ---------------------------------
  Tag Line: *SETNTE
  Key Line: Transaction Set ID, Area, Sequence, Note Type, Paragraph Number
   Lengths: 3, 1, 4, 1, 1
     Notes: 1) "Note Type" has two possible values, N = Note, C = Comment.
            2) "Paragraph Number" is used when there is more than one note 
            of the same sequence and type. It is a sequential number 
            starting at one and incrementing by one.
   Example: *SETNTE
            810,3,0700,N,1
            Number of line items (CTT01) is the accumulation of the number 
            of IT1 segments. If used, hash total (CTT02) is the sum of the 
            value of quantities invoiced (IT102) for each IT1 segment.

  3. Segment Purpose
  ------------------
  Tag Line: *SEGPUR
  Key Line: Segment ID
   Lengths: 3
   Example: *SEGPUR
            A1
            To identify elements not meeting the EDI edit criteria

  4. Segment Notes/Comments
  -------------------------
  Tag Line: *SEGNTE
  Key Line: Segment ID, Sequence, Note Type, Paragraph Number
   Lengths: 3, 2, 1, 1
     Notes: 1) "Note Type" has three possible values, N = Syntax Note, S = 
            Semantic Note, and C = Comment. 2) "Paragraph Number" is used 
            when there is more than one note of the same sequence and type. 
            It is a sequential number starting at one and incrementing by 
            one. 3) Beginning with Version 003011, the segment syntax notes 
            are codified according to rules prescribed in DSTU X12.6-1989. 
            This codification allows for automatic syntax checking of 
            syntax notes. The textual equivalent of the codified note 
            no longer appears on the diskette because it is no longer 
            supported by ASC X12 and is easily generated. 4) Beginning with 
            Version 003020, segment semantic notes are included according to 
            rules prescribed in DSTU X12.6-1989. Many notes which were 
            "Comments" in past releases have now been changed to "Semantic 
            Notes".
   Example: *SEGNTE
            A1,01,C,1
            The rejected-set identifier contains up to the first 19 
            characters of the first segment in the transaction set with all 
            asterisks converted to spaces and excluding the new line 
            character.
            *SEGNTE
            A3,06,N,1
            P0607

  5. Composite Data Element Purpose
  ---------------------------------
  Tag Line: *COMPUR
  Key Line: Composite ID
   Lengths: 4
   Example: *COMPUR
            C001
            To identify a composite unit of measure

            (See Figures Appendix for examples of use)
  
  6. Composite Data Element Notes/Comments
  ----------------------------------------
  Tag Line: *COMNTE
  Key Line: Composite ID, Sequence, Note Type, Paragraph Number
   Lengths: 3, 2, 1, 1
     Notes: See notes for Segment Notes/Comments
   Example: *COMNTE
            C001,02,C,1
            If C001-02 is not used, its value is to be interpreted as 1.

  7. Simple Data Element Definitions
  ----------------------------------
  Tag Line: *ELEDEF
  Key Line: Data Element Number
   Lengths: 4
   Example: *ELEDEF
            1
            Mutually defined route code

  8. Simple Data Element Code Definitions
  ---------------------------------------
  Tag Line: *ELECOD
  Key Line: Data Element Number, Partition Number, Code Value, 
            Paragraph Number
   Lengths: 4, 1, 6, 1
     Notes: 1) "Partition Number" is used for multi-part codes. For 
            instance, data element 103 (Packaging Code) is a minimum 5, 
            maximum 5 data element. The first 3 characters specify a 
            packaging form, like box. The last 2 characters specify a 
            packaging material, such as wood. The values for packaging 
            form are all 3 characters and belong to partition number 1. 
            The values for packaging material are all 2 characters and 
            belong to partition number 2. For the most part, partition 
            number is the space character. 2) "Paragraph Number" is used 
            when there is more than one definition for any one code value. 
            This used to occur on data element 479, but does not in this 
            release. Therefore, paragraph number is always "1".
   Example: *ELECOD
            8, ,E,1
            Payee
            *ELECOD
            103,1,BAG,1
            Bag
            *ELECOD
            103,2,94,1
            Wood
            *ELECOD
            479, ,CO,1
            Cooperative Advertising Agreements (290)

  9. Simple Data Element Code Explanations
  ----------------------------------------
  Tag Line: *ELENTE
  Key Line: Data Element Number, Partition Number, Code Value, 
            Paragraph Number
   Lengths: 4, 1, 6, 1
     Notes: 1) "Partition Number" is used for multi-part codes. See above.
            2) "Paragraph Number" is used when there is more than one 
            definition for any one code value. See above. 3) In this 
            release data element code definitions and data element code 
            explanations have a one-to-one correspondence. In future 
            releases, there may be multiple code explanations for a single 
            code definition.
   Example: *ELENTE
            336, ,18,1
            Sales terms specifying a past due date, and a late payment 
            percentage penalty applies to unpaid balances past this due date
            *ELENTE
            346, ,LT,1
            Used by a shipper to inform carrier that a particular load is 
            available or becoming available for movement; also signifies an 
            advance pick-up notification
            *ELENTE
            355, ,16,1
            A cylindrical container whose contents weigh 115 kilograms when 
            full

  10. Source of Referenced Code List
  ----------------------------------
  Tag Line: *CSSRCE
  Key Line: Code Source Number
   Lengths: 4
   Example: *CSSRCE
            478
            Congressional Staff Directory
            *CSSRCE
            479
            Financial Management Service Cash-Link Code List
            *CSSRCE
            480
            Real Estate Inspection Code List

  11. Available From Address for Code Source Maintainer
  -----------------------------------------------------
  Tag Line: *CSFROM
  Key Line: Code Source Number
   Lengths: 4
   Example: *CSFROM
            478
            Staff Directories, LTD
            P.O. Box 62
            Mount Vernon, VA 22121-0062
            *CSFROM
            479
            Banking Operations Branch
            U.S. Treasury Dept./FMS
            Prince Georges Center II, Room 501A
            3700 East-West Highway
            Hyattsville, MD 20782
            *CSFROM
            480
            Director of Technology Initiatives
            Mortgage Bankers Association
            1125 15th Street, N.W.
            Washington, DC 20005-2766

  12. Internet Address of Code Source Maintainer
  ----------------------------------------------
  Tag Line: *CSINET
  Key Line: Code Source Number
   Lengths: 4
   Example: *CSINET
            89
            http://www.atmi.org
            *CSINET
            95
            http://www.truckline.com
            *CSINET
            105
            http://www.NFPA.org

  13. Abstract for Code List
  --------------------------
  Tag Line: *CSABST
  Key Line: Code Source Number
   Lengths: 4
   Example: *CSABST
            478
            This directory provides a listing of Congressional Districts 
            including a list of zip codes and maps of the division lines.
            *CSABST
            479
            Code list maintained by the Financial Management Service, 
            Banking Operations Branch.
            *CSABST
            480
            List that identifies unique codes representing types of Real 
            Estate inspections.

  14. Code Source Notes
  ---------------------
  Tag Line: *CSNOTE
  Key Line: Code Source Number
   Lengths: 4
   Example: *CSNOTE
            2
            The air carrier data element is assigned a field length of 3 
            because of planned expansion by IATA.
            *CSNOTE
            3
            The airport code data element is assigned a field length of 
            5 because of planned expansion by IATA.
            *CSNOTE
            21
            The SPLC data element is assigned a field length of 9 to 
            allow for a subcode when necessary to specify a rating 
            point, switching point, or pier number.
          
==============================================================================
<end of readme.txt>
