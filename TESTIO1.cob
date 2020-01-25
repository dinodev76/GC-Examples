      *=========================== TESTIO1 ============================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Description: Test reading a line sequential file. This shows
      *     that GnuCOBOL treats CR/LF (on Windows) as end of record
      *     markers, whereas those are ignored on regular sequential
      *     files. Try removing the LINE from "ORGANIZATION LINE 
      *     SEQUENTIAL" to see the difference with the provided input
      *     file.
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-18  0.1      First release
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.                 TESTIO1.

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
           SELECT TESTIO1I         ASSIGN 'Data\TESTIO1.Input.txt'
      *                             ORGANIZATION SEQUENTIAL. 
                                   ORGANIZATION LINE SEQUENTIAL. 
      /
       DATA DIVISION.
      *==============

       FILE SECTION.
      *-------------

       FD  TESTIO1I
           RECORD VARYING 1 TO 80  *> Comment out if switching to
           DEPENDING W-RECLEN      *> ORGANIZATION SEQUENTIAL
           .

       01  TESTIO1I-REC            PIC X(80).

       WORKING-STORAGE SECTION.
      *------------------------

       01  W-TESTIO1I-RECS         PIC 9(09)  COMP VALUE 0.
       01  W-RECLEN                PIC S9(09) COMP.
       01  W-DISP-NUM-1            PIC Z,ZZ9.
       01  W-DISP-NUM-2            PIC Z9.

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
      /
       PROCEDURE DIVISION.
      *===================

       MAIN.
      *-----

           PERFORM SUB-1000-START-UP THRU SUB-1000-EXIT

           PERFORM SUB-9100-READ-TESTIO1I THRU SUB-9100-EXIT

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

           DISPLAY 'TESTIO1 compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS

           OPEN INPUT TESTIO1I
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS.
      *-----------------

           MOVE W-TESTIO1I-RECS    TO W-DISP-NUM-1
           MOVE W-RECLEN           TO W-DISP-NUM-2

           DISPLAY 'Record '
                   W-DISP-NUM-1
                   ' ('
                   W-DISP-NUM-2
                   ' bytes): '
                   TESTIO1I-REC

           PERFORM SUB-9100-READ-TESTIO1I THRU SUB-9100-EXIT
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------
      
           CLOSE TESTIO1I

           MOVE W-TESTIO1I-RECS    TO W-DISP-NUM-1
           DISPLAY 'TESTIO1I records read: '
                   W-DISP-NUM-1
           DISPLAY 'TESTIO1 Completed'
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9100-READ-TESTIO1I.
      *-----------------------
      
           READ TESTIO1I
               AT END
                   SET  W-EOF      TO TRUE
               NOT AT END
                   ADD  1          TO W-TESTIO1I-RECS
           END-READ
           .
       SUB-9100-EXIT.
           EXIT.
