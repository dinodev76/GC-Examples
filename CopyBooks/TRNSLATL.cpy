      *=========================== TRNSLATL ===========================*
      * Authors: Brian D Pead
      *
      * Description: Control parameter for subroutine TRNSLAT which 
      *     converts the character-set of specified fields in a COBOL
      *     record.
      *
      * License: MIT
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-18  0.1      First release
      *================================================================*

      *01  TRNSLATL-CONTROL.

      **** Input fields:
      ****     TC-FIELD-CNT:
      ****         Numbers of fields to convert.
      ****     TC-FIELD-POS: 
      ****         Starting position of each field in record.
      ****     TC-FIELD-LEN: 
      ****         Length of each field in record. 

           05  TC-FIELD-CNT        PIC S9(09) COMP VALUE 0.
           05  TC-FIELD-SPEC                       OCCURS 256
                                                   INDEXED TCF-DX.
               10  TC-FIELD-POS    PIC S9(09) COMP. 
               10  TC-FIELD-LEN    PIC S9(09) COMP. 

      **** Output fields:
      ****     TC-RESPONSE-CODE
      ****         Use 88 levels to determine result of calls.
      ****     TC-RESPONSE-MSG
      ****         Non-space if bad response.

           05  TC-RESPONSE-CODE    PIC 9(4). 
               88  TC-RESPONSE-GOOD                VALUE 0.
               88  TC-RESPONSE-TABLE-ERROR         VALUE 10.
               88  TC-RESPONSE-RECLEN-ERROR        VALUE 20.

           05  TC-RESPONSE-MSG     PIC X(80). 
