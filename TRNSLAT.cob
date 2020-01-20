      *=========================== TRNSLAT ============================*
      * Authors: Brian D Pead
      *
      * Description: Subroutine to convert character-set of specified 
      *     fields in a COBOL record.
      *
      * License: MIT
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-12  0.1      First release
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.             TRNSLAT.

       ENVIRONMENT DIVISION.
      *=====================

       CONFIGURATION SECTION.
      *----------------------

       SOURCE-COMPUTER.
           IBM-Z15.
      *    IBM-Z15 DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
      *---------------------

       FILE-CONTROL.
      /
       DATA DIVISION.
      *==============

       FILE SECTION.
      *-------------

       WORKING-STORAGE SECTION.
      *------------------------

       01  W-SUB-1                 PIC S9(04) COMP.
       01  W-SUB-2                 PIC S9(04) COMP.

       01  FILLER                  PIC X(01)       VALUE 'Y'.
           88  W-FIRST-CALL                        VALUE 'Y'.
           88  W-NOT-FIRST-CALL                    VALUE 'N'.

       01  W-COMPILED-DATE.
           05  W-COMPILED-DATE-YYYY
                                   PIC X(04).
           05  W-COMPILED-DATE-MM  PIC X(02).
           05  W-COMPILED-DATE-DD  PIC X(02).
           05  W-COMPILED-TIME-HH  PIC X(02).
           05  W-COMPILED-TIME-MM  PIC X(02).
           05  W-COMPILED-TIME-SS  PIC X(02).
           05  FILLER              PIC X(07).

       01  W-CHAR-BIN              PIC S9(04)      COMP.
       01  FILLER REDEFINES W-CHAR-BIN.
           05  FILLER              PIC X(01)       VALUE X'00'.
           05  W-CHAR-BIN-2        PIC X(01).

       01  W-CONVERSION-TABLE-HEX                  VALUE LOW-VALUES.
           05  FILLER                              OCCURS 256.
               10  FILLER                          OCCURS 2.
                   15  W-CONV-HEX  PIC X(01).
                   15  W-CONV-DIG REDEFINES W-CONV-HEX
                                   PIC 9(01).

       01  FILLER.
           05  W-HEX-VALUE         PIC S9(04) COMP OCCURS 2.

       01  W-CONVERSION-TABLE-BIN.
           05  FILLER                              OCCURS 256.
               10  W-CONV-BIN      PIC S9(04)      COMP.
               10  FILLER REDEFINES W-CONV-BIN.
                   15  FILLER      PIC X(01).
                   15  W-CONV-CHAR PIC X(01).
      /
       LINKAGE SECTION.
      *----------------

       01  L-CONTROL.              COPY TRNSLATL.

       01  L-RECORD-TO-TRANSLATE.
           05  L-REC-CHAR          PIC X(01)       OCCURS 32768
                                                   INDEXED LR-DX.

       01  L-CONVERSION-TABLE.
           05  FILLER                              OCCURS 256
                                                   INDEXED LC-DX.
               10  L-CONV-HEX-1    PIC X(01).
               10  L-CONV-HEX-2    PIC X(01).
      /
       PROCEDURE DIVISION 
      *==================
           USING L-CONTROL
                 L-RECORD-TO-TRANSLATE
                 L-CONVERSION-TABLE.

       MAIN.
      *-----

           PERFORM SUB-1000-START-UP THRU SUB-1000-EXIT

           IF      NOT TC-RESPONSE-GOOD
               GO TO MAIN-EXIT
           END-IF

           PERFORM SUB-2000-PROCESS THRU SUB-2000-EXIT
               VARYING TCF-DX FROM 1 BY 1
                 UNTIL TCF-DX > TC-FIELD-CNT
                 OR    NOT TC-RESPONSE-GOOD
           .
       MAIN-EXIT.
           GOBACK.
      /
       SUB-1000-START-UP.
      *------------------

           SET  TC-RESPONSE-GOOD   TO TRUE
           MOVE SPACES             TO TC-RESPONSE-MSG

           IF      W-CONVERSION-TABLE-HEX NOT = L-CONVERSION-TABLE
               MOVE L-CONVERSION-TABLE
                                   TO W-CONVERSION-TABLE-HEX 

               PERFORM SUB-1100-CONVERT-HEX-TO-BIN THRU SUB-1100-EXIT
                   VARYING W-SUB-1 FROM 1 BY 1
                     UNTIL W-SUB-1 > 256
           END-IF

           IF      W-NOT-FIRST-CALL
               GO TO SUB-1000-EXIT
           END-IF

           SET W-NOT-FIRST-CALL    TO TRUE
           MOVE FUNCTION WHEN-COMPILED 
                                   TO W-COMPILED-DATE

           DISPLAY 'TRNSLAT compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-1100-CONVERT-HEX-TO-BIN.
      *----------------------------
           
           PERFORM VARYING W-SUB-2 FROM 1 BY 1
                     UNTIL W-SUB-2 > 2
                     OR    NOT TC-RESPONSE-GOOD
               IF      W-CONV-HEX(W-SUB-1 W-SUB-2) NUMERIC
                   MOVE W-CONV-DIG(W-SUB-1 W-SUB-2)
                                   TO W-HEX-VALUE(W-SUB-2)
               ELSE 
                   EVALUATE W-CONV-HEX(W-SUB-1 W-SUB-2)
                     WHEN 'A'
                       MOVE 10     TO W-HEX-VALUE(W-SUB-2) 
                     WHEN 'B'
                       MOVE 11     TO W-HEX-VALUE(W-SUB-2) 
                     WHEN 'C'
                       MOVE 12     TO W-HEX-VALUE(W-SUB-2) 
                     WHEN 'D'
                       MOVE 13     TO W-HEX-VALUE(W-SUB-2) 
                     WHEN 'E'
                       MOVE 14     TO W-HEX-VALUE(W-SUB-2) 
                     WHEN 'F'
                       MOVE 15     TO W-HEX-VALUE(W-SUB-2)
                     WHEN OTHER
                       SET  TC-RESPONSE-TABLE-ERROR
                                   TO TRUE
                      MOVE 'Invalid hex character in conv table: '  
                                   TO TC-RESPONSE-MSG
                      MOVE W-CONV-HEX(W-SUB-1 W-SUB-2)
                                   TO TC-RESPONSE-MSG(38 : 1)
                   END-EVALUATE
               END-IF
           END-PERFORM

           IF      NOT TC-RESPONSE-GOOD
               GO TO SUB-1100-EXIT
           END-IF

           COMPUTE W-CONV-BIN(W-SUB-1)
                                   =  W-HEX-VALUE(1)
                                      * 16
                                      + W-HEX-VALUE(2)

      D    DISPLAY 'W-CONV-BIN('
      D            W-SUB-1
      D            ') = '
      D            W-CONV-BIN(W-SUB-1)    
           .
       SUB-1100-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS.
      *-----------------

           IF      TC-FIELD-POS(TCF-DX) + TC-FIELD-LEN(TCF-DX) - 1 >
                   LENGTH OF L-RECORD-TO-TRANSLATE
               SET  TC-RESPONSE-RECLEN-ERROR
                                   TO TRUE
                      MOVE 'Field pos + len > 32K'  
                                   TO TC-RESPONSE-MSG
               GO TO SUB-2000-EXIT
           END-IF

           SET  LR-DX              TO TC-FIELD-POS(TCF-DX)

           PERFORM TC-FIELD-LEN(TCF-DX) TIMES
               MOVE L-REC-CHAR(LR-DX)
                                   TO W-CHAR-BIN-2
               ADD  1
                    W-CHAR-BIN GIVING W-SUB-2
               MOVE W-CONV-CHAR(W-SUB-2)
                                   TO L-REC-CHAR(LR-DX)
               SET  LR-DX       UP BY 1
           END-PERFORM
           .
       SUB-2000-EXIT.
           EXIT.
