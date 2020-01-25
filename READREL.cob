      *=========================== READREL ============================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Description: Test reading the relative file created by program
      *              BLDFILES.
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-24  0.1      First release
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.                 READREL.

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
           SELECT NAMADDRL         ASSIGN "Data\NAMEADDR.Rel.dat"
                                   ORGANIZATION RELATIVE   
                                   ACCESS DYNAMIC
                                   RELATIVE KEY W-RELATIVE-REC
                                   FILE STATUS W-FILE-STATUS.
      /
       DATA DIVISION.
      *==============

       FILE SECTION.
      *-------------

       FD  NAMADDRL.

       01  NAMADDRL-REC.           COPY NAMEADDR.

       WORKING-STORAGE SECTION.
      *------------------------

       01  W-NAMADDRL-RECS         PIC 9(09)  COMP VALUE 0.
       01  W-RETURN-CODE           PIC S9(04) COMP.
       01  W-DISP-NUM              PIC ZZ,ZZ9.

       01  W-ERROR-MSG             PIC X(20)       VALUE
           '**** READREL error: '.

       01  W-READ-METHOD           PIC X(01).
           88  W-READ-METHOD-RELATIVE              VALUE 'R'.
           88  W-READ-METHOD-SEQUENTIAL            VALUE 'S'.
           88  W-READ-METHOD-EXIT                  VALUE ' '.
           88  W-READ-METHOD-VALID                 VALUE 'R'
                                                         'S'
                                                         ' '.
       01  W-RELATIVE-REC          PIC 9(09).
           88  W-RELATIVE-REC-EXIT                 VALUE 0.

       01  FILLER                  PIC X(01).
           88  W-INVALID-KEY                       VALUE 'Y'.
           88  W-NOT-INVALID-KEY                   VALUE 'N'.

       01  FILLER                  PIC X(01).
           88  W-EOF                               VALUE 'Y'.
           88  W-NOT-EOF                           VALUE 'N'.

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

           PERFORM SUB-9100-GET-READ-METHOD THRU SUB-9100-EXIT

           PERFORM SUB-2000-PROCESS-READ-METHOD THRU SUB-2000-EXIT
               UNTIL W-READ-METHOD-EXIT
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

           DISPLAY 'READREL compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS

           OPEN INPUT NAMADDRL

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' opening NAMADDRL'
               MOVE 10             TO W-RETURN-CODE
           END-IF
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS-READ-METHOD.
      *-----------------------------

           IF      W-READ-METHOD-RELATIVE
               PERFORM SUB-9200-GET-RELATIVE-REC THRU SUB-9200-EXIT

               PERFORM SUB-2100-READ-RELATIVE THRU SUB-2100-EXIT
                   UNTIL W-RELATIVE-REC-EXIT
           ELSE
               SET  W-NOT-EOF      TO TRUE

               PERFORM SUB-2200-READ-SEQUENTIAL THRU SUB-2200-EXIT
                   UNTIL W-EOF
                   OR    W-RETURN-CODE NOT = 0
           END-IF

           IF      W-RETURN-CODE NOT = 0
               GO TO SUB-2000-EXIT
           END-IF

           PERFORM SUB-9100-GET-READ-METHOD THRU SUB-9100-EXIT
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-2100-READ-RELATIVE.
      *-----------------------

           PERFORM SUB-9300-READ-NAMADDRL-RELATIVE THRU SUB-9300-EXIT 

           IF      W-NOT-INVALID-KEY
               DISPLAY NAMADDRL-REC
           END-IF

           PERFORM SUB-9200-GET-RELATIVE-REC THRU SUB-9200-EXIT
           .
       SUB-2100-EXIT.
           EXIT.
      /
       SUB-2200-READ-SEQUENTIAL.
      *-------------------------

           PERFORM SUB-9400-START-NAMADDRL THRU SUB-9400-EXIT

           IF      W-RETURN-CODE NOT = 0
               GO TO SUB-2200-EXIT
           END-IF

           PERFORM SUB-9500-READ-NAMADDRL-SEQ THRU SUB-9500-EXIT

           PERFORM UNTIL W-EOF
                   OR    W-RETURN-CODE NOT = 0
               DISPLAY NAMADDRL-REC

               PERFORM SUB-9500-READ-NAMADDRL-SEQ THRU SUB-9500-EXIT
           END-PERFORM
           .
       SUB-2200-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------
      
           CLOSE NAMADDRL

           MOVE W-NAMADDRL-RECS    TO W-DISP-NUM
           DISPLAY 'NAMADDRL records read:    '
                   W-DISP-NUM

           DISPLAY 'READREL Completed'
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9100-GET-READ-METHOD.
      *-------------------------
       
           MOVE 'X'                TO W-READ-METHOD

           PERFORM UNTIL W-READ-METHOD-VALID        
               DISPLAY 'Enter R for relative reads, S for sequential, '
                       'space to exit: '
                   NO ADVANCING
           
               ACCEPT W-READ-METHOD
           END-PERFORM
           .
       SUB-9100-EXIT.
           EXIT.
      /
       SUB-9200-GET-RELATIVE-REC.
      *--------------------------
           
           DISPLAY 'Enter relative record number (odd number since we '
                   'left space in file for the even ones) or space to '
                   ' exit: '
               NO ADVANCING
           
           ACCEPT W-RELATIVE-REC
           .
       SUB-9200-EXIT.
           EXIT.
      /
       SUB-9300-READ-NAMADDRL-RELATIVE.
      *--------------------------------
      
           READ NAMADDRL
               INVALID KEY
                   SET  W-INVALID-KEY
                                   TO TRUE
                   DISPLAY 'Relative record not on file: '
                           W-RELATIVE-REC
                   GO TO SUB-9300-EXIT
           END-READ

           IF      W-FILE-STATUS-GOOD
               SET  W-NOT-INVALID-KEY
                                   TO TRUE
               ADD  1              TO W-NAMADDRL-RECS
           ELSE    
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' reading NAMADDRL key'
               MOVE 20             TO W-RETURN-CODE
           END-IF
           .
       SUB-9300-EXIT.
           EXIT.
      /
       SUB-9400-START-NAMADDRL.
      *------------------------

           START NAMADDRL
               FIRST
           END-START

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' starting NAMADDRL'
               MOVE 40             TO W-RETURN-CODE
           END-IF
           .
       SUB-9400-EXIT.
           EXIT.
      /
       SUB-9500-READ-NAMADDRL-SEQ.
      *---------------------------
      
           READ NAMADDRL NEXT
               AT END
                   SET  W-EOF      TO TRUE
                   GO TO SUB-9500-EXIT
           END-READ

           IF      W-FILE-STATUS-GOOD
               ADD  1              TO W-NAMADDRL-RECS
           ELSE    
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' reading NAMADDRL next record'
               MOVE 50             TO W-RETURN-CODE
           END-IF
           .
       SUB-9500-EXIT.
           EXIT.
