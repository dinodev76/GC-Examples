      *=========================== READIDX ============================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Description: Test reading the indexed file created by program
      *              BLDFILES.
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-24  0.1      First release
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.                 READIDX.

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
           SELECT NAMADDIX         ASSIGN "Data\NAMEADDR.Idx.dat"
                                   ORGANIZATION INDEXED   
                                   ACCESS DYNAMIC
                                   RECORD KEY NA-TAXID
                                   ALTERNATE KEY NA-LAST-NAME
                                       WITH DUPLICATES
                                   FILE STATUS W-FILE-STATUS.
      /
       DATA DIVISION.
      *==============

       FILE SECTION.
      *-------------

       FD  NAMADDIX.

       01  NAMADDIX-REC.           COPY NAMEADDR.

       WORKING-STORAGE SECTION.
      *------------------------

       01  W-NAMADDIX-RECS         PIC 9(09)  COMP VALUE 0.
       01  W-RETURN-CODE           PIC S9(04) COMP.
       01  W-DISP-NUM              PIC ZZ,ZZ9.

       01  W-ERROR-MSG             PIC X(20)       VALUE
           '**** READIDX error: '.

       01  W-READ-SEQUENCE         PIC X(01).
           88  W-READ-SEQUENCE-LAST-NAME           VALUE 'l'
                                                         'L'.

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

           PERFORM SUB-9200-READ-NAMADDIX THRU SUB-9200-EXIT

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

           DISPLAY 'READIDX compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS

           OPEN INPUT NAMADDIX

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' opening NAMADDIX'
               MOVE 10             TO W-RETURN-CODE
               GO TO SUB-1000-EXIT
           END-IF
       
           DISPLAY 'Enter read sequence '
                   '(T for Taxid, L for Last Name): '
               NO ADVANCING
           
           ACCEPT W-READ-SEQUENCE

           IF      NOT W-READ-SEQUENCE-LAST-NAME
               GO TO SUB-1000-EXIT
           END-IF

           PERFORM SUB-9100-START-NAMADDIX THRU SUB-9100-EXIT
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS.
      *-----------------

           DISPLAY NAMADDIX-REC

           PERFORM SUB-9200-READ-NAMADDIX THRU SUB-9200-EXIT
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------
      
           CLOSE NAMADDIX

           MOVE W-NAMADDIX-RECS    TO W-DISP-NUM
           DISPLAY 'NAMADDIX records read:    '
                   W-DISP-NUM

           DISPLAY 'READIDX Completed'
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9100-START-NAMADDIX.
      *------------------------
      
           MOVE SPACES             TO NA-LAST-NAME

           START NAMADDIX
               KEY >= NA-LAST-NAME
           END-START

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' starting NAMADDIX'
               MOVE 20             TO W-RETURN-CODE
           END-IF
           .
       SUB-9100-EXIT.
           EXIT.
      /
       SUB-9200-READ-NAMADDIX.
      *-----------------------
      
           READ NAMADDIX NEXT
               AT END
                   SET  W-EOF      TO TRUE
                   GO TO SUB-9200-EXIT
           END-READ

           ADD  1                  TO W-NAMADDIX-RECS

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' reading NAMADDIX'
               MOVE 30             TO W-RETURN-CODE
           END-IF
           .
       SUB-9200-EXIT.
           EXIT.
