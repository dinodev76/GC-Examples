      *=========================== BLDFILES ===========================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Description: Test reading a line sequential file which is used
      *              to populate indexed and relative output files.
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-24  0.1      First release
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.                 BLDFILES.

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
           SELECT NAMADDSQ         ASSIGN 'Data\NAMEADDR.Seq.dat'
                                   ORGANIZATION LINE SEQUENTIAL. 

           SELECT NAMADDIX         ASSIGN "Data\NAMEADDR.Idx.dat"
                                   ORGANIZATION INDEXED   
                                   ACCESS RANDOM
                                   RECORD KEY NA-TAXID
                                                   IN NAMADDIX-REC
                                   ALTERNATE KEY NA-LAST-NAME
                                                   IN NAMADDIX-REC
                                       WITH DUPLICATES
                                   FILE STATUS W-FILE-STATUS.

           SELECT NAMADDRL         ASSIGN "Data\NAMEADDR.Rel.dat"
                                   ORGANIZATION RELATIVE   
                                   ACCESS RANDOM
                                   RELATIVE KEY W-NAMADDRL-KEY
                                   FILE STATUS W-FILE-STATUS.
      /
       DATA DIVISION.
      *==============

       FILE SECTION.
      *-------------

       FD  NAMADDSQ.

       01  NAMADDSQ-REC.           COPY NAMEADDR.

       FD  NAMADDIX.

       01  NAMADDIX-REC.           COPY NAMEADDR.

       FD  NAMADDRL.

       01  NAMADDRL-REC.           COPY NAMEADDR.

       WORKING-STORAGE SECTION.
      *------------------------

       01  W-NAMADDSQ-RECS         PIC 9(09)  COMP VALUE 0.
       01  W-NAMADDIX-RECS         PIC 9(09)  COMP VALUE 0.
       01  W-NAMADDRL-RECS         PIC 9(09)  COMP VALUE 0.
       01  W-RETURN-CODE           PIC S9(04) COMP.
       01  W-NAMADDRL-KEY          PIC 9(09)  COMP.
       01  W-DISP-NUM              PIC ZZ,ZZ9.

       01  W-ERROR-MSG             PIC X(21)       VALUE
           '**** BLDFILES error: '.

       01  FILLER                  PIC X(01)       VALUE 'N'.
           88  W-EOF                               VALUE 'Y'.

       01  W-FILE-STATUS           PIC X(02).
           88  W-FILE-STATUS-GOOD                  VALUE '00'.

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

           IF      W-RETURN-CODE NOT = 0
               GO TO MAIN-EXIT
           END-IF

           PERFORM SUB-9100-READ-NAMADDSQ THRU SUB-9100-EXIT

           PERFORM SUB-2000-PROCESS THRU SUB-2000-EXIT
               UNTIL W-EOF
               OR    W-RETURN-CODE NOT = 0

           PERFORM SUB-3000-SHUT-DOWN THRU SUB-3000-EXIT
           .
       MAIN-EXIT.
           STOP RUN.
      /
       SUB-1000-START-UP.
      *------------------

           MOVE 0                  TO W-RETURN-CODE
           MOVE FUNCTION WHEN-COMPILED 
                                   TO W-COMPILED-DATE

           DISPLAY 'BLDFILES compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS

           OPEN INPUT  NAMADDSQ
                OUTPUT NAMADDIX

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' opening NAMADDIX'
               MOVE 10             TO W-RETURN-CODE
               GO TO SUB-1000-EXIT
           END-IF

           OPEN OUTPUT NAMADDRL

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' opening NAMADDRL'
               MOVE 20             TO W-RETURN-CODE
               GO TO SUB-1000-EXIT
           END-IF

           MOVE 1                  TO W-NAMADDRL-KEY
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS.
      *-----------------

           MOVE NAMADDSQ-REC       TO NAMADDIX-REC

           PERFORM SUB-9200-WRITE-NAMADDIX THRU SUB-9200-EXIT

           IF      W-RETURN-CODE NOT = 0
               GO TO SUB-2000-EXIT
           END-IF

           MOVE NAMADDSQ-REC       TO NAMADDRL-REC

           PERFORM SUB-9300-WRITE-NAMADDRL THRU SUB-9300-EXIT

           IF      W-RETURN-CODE NOT = 0
               GO TO SUB-2000-EXIT
           END-IF

           PERFORM SUB-9100-READ-NAMADDSQ THRU SUB-9100-EXIT
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------
      
           CLOSE NAMADDSQ
                 NAMADDIX
                 NAMADDRL

           MOVE W-NAMADDSQ-RECS    TO W-DISP-NUM
           DISPLAY 'NAMADDSQ records read:    '
                   W-DISP-NUM

           MOVE W-NAMADDIX-RECS    TO W-DISP-NUM
           DISPLAY 'NAMADDIX records written: '
                   W-DISP-NUM

           MOVE W-NAMADDRL-RECS    TO W-DISP-NUM
           DISPLAY 'NAMADDRL records written: '
                   W-DISP-NUM

           DISPLAY 'BLDFILES Completed'
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9100-READ-NAMADDSQ.
      *-----------------------
      
           READ NAMADDSQ
               AT END
                   SET  W-EOF      TO TRUE
               NOT AT END
                   ADD  1          TO W-NAMADDSQ-RECS
           END-READ
           .
       SUB-9100-EXIT.
           EXIT.
      /
       SUB-9200-WRITE-NAMADDIX.
      *------------------------
      
           WRITE NAMADDIX-REC

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' writing NAMADDIX-REC'
               MOVE 30             TO W-RETURN-CODE
               GO TO SUB-9200-EXIT
           END-IF

           ADD  1                  TO W-NAMADDIX-RECS
           .
       SUB-9200-EXIT.
           EXIT.
      /
       SUB-9300-WRITE-NAMADDRL.
      *------------------------
      
           WRITE NAMADDRL-REC

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' writing NAMADDRL-REC'
               MOVE 40             TO W-RETURN-CODE
               GO TO SUB-9300-EXIT
           END-IF

           ADD  1                  TO W-NAMADDRL-RECS

      **** Add 2 to key to show resulting space left in output file
      **** for missing records:     
           ADD  2                  TO W-NAMADDRL-KEY
           .
       SUB-9300-EXIT.
           EXIT.
