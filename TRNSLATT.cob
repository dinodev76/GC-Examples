      *=========================== TRNSLATT ===========================*
      * Authors: Brian D Pead
      *
      * Description: Program to test subroutine TRNSLAT.
      *
      * License: MIT
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-12  0.1      First release
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.             TRNSLATT.

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
           SELECT TESTIN               ASSIGN 'Data\TRNSLATT.Input.bin'
                                       ORGANIZATION SEQUENTIAL. 

           SELECT TESTOUT              ASSIGN 'Data\TRNSLATT.Output.bin'
                                       ORGANIZATION SEQUENTIAL. 
      /
       DATA DIVISION.
      *==============

       FILE SECTION.
      *-------------

       FD  TESTIN
           RECORD VARYING 25 TO 15000
           DEPENDING W-RECLEN.

       01  TESTIN-REC.
           05  TI-CHAR             PIC X           OCCURS 25 TO 15000
                                                   DEPENDING W-RECLEN
                                                   INDEXED TI-DX.

       FD  TESTOUT
           RECORD VARYING 25 TO 15000
           DEPENDING W-RECLEN.

       01  TESTOUT-REC.
           05  TO-CHAR             PIC X           OCCURS 25 TO 15000
                                                   DEPENDING W-RECLEN
                                                   INDEXED TO-DX.
      /
       WORKING-STORAGE SECTION.
      *------------------------

       01  W-TESTIN-RECS           PIC 9(09)  COMP VALUE 0.
       01  W-TESTOUT-RECS          PIC 9(09)  COMP VALUE 0.
       01  W-RECLEN                PIC S9(09) COMP.
       01  W-TRNSLAT-PROG          PIC X(08)       VALUE 'TRNSLAT'.

       01  FILLER                  PIC X(01)       VALUE 'N'.
           88  W-EOF                               VALUE 'Y'.

       01  W-COMPILED-DATE.
           05  W-COMPILED-DATE-YYYY
                                   PIC X(04).
           05  W-COMPILED-DATE-MM  PIC X(02).
           05  W-COMPILED-DATE-DD  PIC X(02).
           05  W-COMPILED-TIME-HH  PIC X(02).
           05  W-COMPILED-TIME-MM  PIC X(02).
           05  W-COMPILED-TIME-SS  PIC X(02).
           05  FILLER              PIC X(07).

       01  W-TRNSLAT-CONTROL.      COPY TRNSLATL.

      * COPY ASC2EBC.

      * COPY EBC2ASC.

      * COPY I037T437.

       COPY I437T037.
      /
       PROCEDURE DIVISION.
      *===================

       MAIN.
      *-----

           PERFORM SUB-1000-START-UP THRU SUB-1000-EXIT

           PERFORM SUB-9100-READ-TESTIN THRU SUB-9100-EXIT

           PERFORM SUB-2000-PROCESS THRU SUB-2000-EXIT
               UNTIL W-EOF

           PERFORM SUB-3000-SHUT-DOWN THRU SUB-3000-EXIT
           .
       MAIN-EXIT.
           STOP RUN.
      /
       SUB-1000-START-UP.
      *------------------

           MOVE FUNCTION WHEN-COMPILED 
                                   TO W-COMPILED-DATE

           DISPLAY 'TRNSLATT compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS

           OPEN INPUT  TESTIN
                OUTPUT TESTOUT
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS.
      *-----------------
           
           MOVE 1                  TO TC-FIELD-CNT
                                      TC-FIELD-POS(1)
           MOVE W-RECLEN           TO TC-FIELD-LEN(1)
           MOVE TESTIN-REC         TO TESTOUT-REC

           CALL W-TRNSLAT-PROG  USING W-TRNSLAT-CONTROL
                                      TESTOUT-REC
      *                                W-ASCII-TO-EBCDIC-TABLE
      *                                W-EBCDIC-TO-ASCII-TABLE
      *                                W-IBM037-TO-IBM437-TABLE
                                      W-IBM437-TO-IBM037-TABLE

           IF      TC-RESPONSE-GOOD
               PERFORM SUB-9200-WRITE-TESTOUT THRU SUB-9200-EXIT
           ELSE
               DISPLAY 'Bad response from TRNSLAT: '
                       TC-RESPONSE-CODE
                       ' - '
                       TC-RESPONSE-MSG
           END-IF
           .
       SUB-2000-READ.

           PERFORM SUB-9100-READ-TESTIN THRU SUB-9100-EXIT
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------
      
           CLOSE TESTIN
                 TESTOUT

           DISPLAY 'TESTIN  records read:    '
                   W-TESTIN-RECS
           DISPLAY 'TESTOUT records written: '
                   W-TESTOUT-RECS
           DISPLAY 'TRNSLATT completed'
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9100-READ-TESTIN.
      *---------------------
      
           READ TESTIN
               AT END
                   SET  W-EOF      TO TRUE
               NOT AT END
                   ADD  1          TO W-TESTIN-RECS
           END-READ
           .
       SUB-9100-EXIT.
           EXIT.
      /
       SUB-9200-WRITE-TESTOUT.
      *-----------------------
      
           WRITE TESTOUT-REC

           ADD  1                  TO W-TESTOUT-RECS
           .
       SUB-9200-EXIT.
           EXIT.
