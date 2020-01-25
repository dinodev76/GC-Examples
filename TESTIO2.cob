      *========================== TESTIO2 =============================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Description: Test writing a variable length record 
      *     sequential file. This shows that GnuCOBOL creates RDWs 
      *     similar to the IBM mainframe (when COB_VARSEQ_FORMAT=0, the
      *     default), but the RDW record length does not include itself,
      *     i.e. it is 4 less than the mainframe.
      *
      *     Environment name: COB_VARSEQ_FORMAT
      *     Parameter name:   varseq_format
      *     Purpose:          Declare format used for variable length
      *                       sequential files
      *                       - different types and lengths precede each
      *                         record
      *                       - length is the data length, does not
      *                         include the prefix
      *     Type: 0 means 2 byte record length (big-endian) + 2 NULs
      *           1 means 4 byte record length (big-endian)
      *           2 means 4 byte record length (local machine int)
      *           3 means 2 byte record length (big-endian)
      *     Default: 0
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-18  0.1      First release
      *
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.                 TESTIO2.

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
           SELECT TESTIO2O         ASSIGN 'Data\TESTIO2.Output.bin'
                                   ORGANIZATION SEQUENTIAL. 
      /
       DATA DIVISION.
      *==============

       FILE SECTION.
      *-------------

       FD  TESTIO2O
           RECORD VARYING 25 TO 15000
           DEPENDING W-RECLEN.

       01  TESTIO2O-REC.
           05  TO-CHAR             PIC X           OCCURS 25 TO 15000
                                                   DEPENDING W-RECLEN
                                                   INDEXED TO-DX.

       WORKING-STORAGE SECTION.
      *------------------------

       01  W-TESTIO2O-RECS         PIC 9(09)  COMP VALUE 0.
       01  W-RANDOM-NO             PIC V9(09) COMP.
       01  W-RECLEN                PIC S9(09) COMP.
       01  W-RECLEN-MIN            PIC S9(09) COMP VALUE 25.
       01  W-RECLEN-MAX            PIC S9(09) COMP VALUE 15000.
       01  W-OUTPUT-MIN            PIC 9(09)  COMP VALUE 999999999.
       01  W-OUTPUT-MAX            PIC 9(09)  COMP VALUE 0.
       01  W-QUOTIENT              PIC 9(09)  COMP.
       01  W-REMAINDER             PIC 9(09)  COMP.
       01  W-DISP-NUM              PIC ZZ,ZZZ,ZZ9.

       01  W-ERROR-MSG             PIC X(21)       VALUE
           '**** TESTIO2 error: '.

       01  W-COMPILED-DATE.
           05  W-COMPILED-DATE-YYYY
                                   PIC X(04).
           05  W-COMPILED-DATE-MM  PIC X(02).
           05  W-COMPILED-DATE-DD  PIC X(02).
           05  W-COMPILED-TIME-HH  PIC X(02).
           05  W-COMPILED-TIME-MM  PIC X(02).
           05  W-COMPILED-TIME-SS  PIC X(02).
           05  FILLER              PIC X(07).

       01  W-CHAR-BIN              PIC S9(04) COMP.
       01  FILLER REDEFINES W-CHAR-BIN.
           05  FILLER              PIC X.
           05  W-CHAR              PIC X.
      /
       PROCEDURE DIVISION.
      *===================

       MAIN.
      *-----

           PERFORM SUB-1000-START-UP THRU SUB-1000-EXIT

           PERFORM SUB-2000-PROCESS THRU SUB-2000-EXIT
               UNTIL W-TESTIO2O-RECS >= 12345

           PERFORM SUB-3000-SHUT-DOWN THRU SUB-3000-EXIT
           .
       MAIN-EXIT.
           STOP RUN.
      /
       SUB-1000-START-UP.
      *------------------

           MOVE FUNCTION WHEN-COMPILED 
                                   TO W-COMPILED-DATE

           DISPLAY 'TESTIO2 compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS

           OPEN OUTPUT TESTIO2O
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS.
      *-----------------

           MOVE FUNCTION RANDOM    TO W-RANDOM-NO
           COMPUTE W-RECLEN        =  W-RANDOM-NO
                                      * (W-RECLEN-MAX
                                         - W-RECLEN-MIN
                                         + 1)
                                      + W-RECLEN-MIN     

      *    DISPLAY 'W-RECLEN = '
      *            W-RECLEN

           IF      W-RECLEN < W-OUTPUT-MIN
               MOVE W-RECLEN       TO W-OUTPUT-MIN
           END-IF

           IF      W-RECLEN > W-OUTPUT-MAX
               MOVE W-RECLEN       TO W-OUTPUT-MAX
           END-IF

           PERFORM SUB-2100-POPULATE-REC THRU SUB-2100-EXIT

           PERFORM SUB-9100-WRITE-TESTIO2O THRU SUB-9100-EXIT
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-2100-POPULATE-REC.
      *----------------------

           COMPUTE W-CHAR-BIN      =  FUNCTION RANDOM * 256
                             
           PERFORM VARYING TO-DX FROM 1 BY 1 
                     UNTIL TO-DX > W-RECLEN
               MOVE W-CHAR         TO TO-CHAR(TO-DX)
           END-PERFORM
           .
       SUB-2100-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------
      
           CLOSE TESTIO2O

           MOVE W-OUTPUT-MIN       TO W-DISP-NUM
           DISPLAY 'Shortest record length:   '
                   W-DISP-NUM

           MOVE W-OUTPUT-MAX       TO W-DISP-NUM
           DISPLAY 'Longest  record length:   '
                   W-DISP-NUM

           MOVE W-TESTIO2O-RECS    TO W-DISP-NUM
           DISPLAY 'TESTIO2O records written: '
                   W-DISP-NUM

           DISPLAY 'TESTIO2 completed'
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9100-WRITE-TESTIO2O.
      *------------------------
      
           WRITE TESTIO2O-REC

           ADD  1                  TO W-TESTIO2O-RECS

           DIVIDE W-TESTIO2O-RECS  BY 10000
                               GIVING W-QUOTIENT
                            REMAINDER W-REMAINDER

           IF      W-REMAINDER = 0
               MOVE W-TESTIO2O-RECS
                                   TO W-DISP-NUM
               DISPLAY '    Records written: '
                       W-DISP-NUM
           END-IF
           .
       SUB-9100-EXIT.
           EXIT.
