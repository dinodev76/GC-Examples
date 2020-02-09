      *=========================== READIDX ============================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Description: Reads the indexed file created by program BLDFILES
      *              in the specfied manner, and displays the records
      *              read.
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-02-08  1.0      First release
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

       01  W-READ-METHOD           PIC X(01).
           88  W-READ-METHOD-RANDOM                VALUE 'R'.
           88  W-READ-METHOD-SEQUENTIAL            VALUE 'S'.
           88  W-READ-METHOD-EXIT                  VALUE ' '.
           88  W-READ-METHOD-VALID                 VALUE 'R'
                                                         'S'
                                                         ' '.
       01  W-READ-KEY-FIELD              PIC X(01).
           88  W-READ-KEY-FIELD-LAST-NAME          VALUE 'L'.
           88  W-READ-KEY-FIELD-TAXID              VALUE 'T'.
           88  W-READ-KEY-FIELD-EXIT               VALUE ' '.
           88  W-READ-KEY-FIELD-VALID              VALUE 'L'
                                                         'T' 
                                                         ' '.
       01  W-KEY-VALUE             PIC X(35).
           88  W-KEY-VALUE-EXIT                    VALUE SPACES.

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
           END-IF
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS-READ-METHOD.
      *-----------------------------

           PERFORM SUB-9200-GET-READ-KEY-FIELD THRU SUB-9200-EXIT

           IF      W-READ-METHOD-RANDOM
               PERFORM SUB-9300-GET-KEY-VALUE THRU SUB-9300-EXIT

               PERFORM SUB-2100-READ-RANDOM THRU SUB-2100-EXIT
                   UNTIL W-KEY-VALUE-EXIT
           ELSE
               PERFORM SUB-2200-READ-SEQUENTIAL THRU SUB-2200-EXIT
                   UNTIL W-READ-KEY-FIELD-EXIT
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
       SUB-2100-READ-RANDOM.
      *---------------------

           PERFORM SUB-9400-READ-NAMADDIX-RANDOM THRU SUB-9400-EXIT 

           IF      W-NOT-INVALID-KEY
               DISPLAY NAMADDIX-REC
           END-IF

           PERFORM SUB-9300-GET-KEY-VALUE THRU SUB-9300-EXIT
           .
       SUB-2100-EXIT.
           EXIT.
      /
       SUB-2200-READ-SEQUENTIAL.
      *-------------------------

           PERFORM SUB-9500-START-NAMADDIX THRU SUB-9500-EXIT

           IF      W-RETURN-CODE NOT = 0
               GO TO SUB-2200-EXIT
           END-IF

           SET  W-NOT-EOF          TO TRUE

           PERFORM SUB-9600-READ-NAMADDIX-SEQ THRU SUB-9600-EXIT

           PERFORM UNTIL W-EOF
                   OR    W-RETURN-CODE NOT = 0
               DISPLAY NAMADDIX-REC

               PERFORM SUB-9600-READ-NAMADDIX-SEQ THRU SUB-9600-EXIT
           END-PERFORM

           IF      W-RETURN-CODE NOT = 0
               GO TO SUB-2200-EXIT
           END-IF

           PERFORM SUB-9200-GET-READ-KEY-FIELD THRU SUB-9200-EXIT
           .
       SUB-2200-EXIT.
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
       SUB-9100-GET-READ-METHOD.
      *-------------------------
       
           MOVE 'X'                TO W-READ-METHOD

           PERFORM UNTIL W-READ-METHOD-VALID        
               DISPLAY 'Enter R for random reads, S for sequential, '
                       'space to exit: '
                   NO ADVANCING
           
               ACCEPT W-READ-METHOD

               MOVE FUNCTION UPPER-CASE(W-READ-METHOD)
                                   TO W-READ-METHOD
           END-PERFORM
           .
       SUB-9100-EXIT.
           EXIT.
      /
       SUB-9200-GET-READ-KEY-FIELD.
      *----------------------------
       
           MOVE 'X'                TO W-READ-KEY-FIELD

           PERFORM UNTIL W-READ-KEY-FIELD-VALID        
               DISPLAY 'Enter L for last name key, T for taxid, '
                       'space to exit: '
                   NO ADVANCING
           
               ACCEPT W-READ-KEY-FIELD

               MOVE FUNCTION UPPER-CASE(W-READ-KEY-FIELD)
                                   TO W-READ-KEY-FIELD
           END-PERFORM
           .
       SUB-9200-EXIT.
           EXIT.
      /
       SUB-9300-GET-KEY-VALUE.
      *-----------------------
           
           DISPLAY 'Enter key value or space to exit: '
               NO ADVANCING
           
           ACCEPT W-KEY-VALUE
           .
       SUB-9300-EXIT.
           EXIT.
      /
       SUB-9400-READ-NAMADDIX-RANDOM.
      *------------------------------
      
           IF      W-READ-KEY-FIELD-LAST-NAME
               MOVE W-KEY-VALUE    TO NA-LAST-NAME

               READ NAMADDIX
                   KEY NA-LAST-NAME
                   INVALID KEY
                       SET  W-INVALID-KEY
                                   TO TRUE
                       DISPLAY 'Key not on file: '
                               NA-LAST-NAME
                       GO TO SUB-9400-EXIT
               END-READ
           ELSE
               MOVE W-KEY-VALUE    TO NA-TAXID
               
               READ NAMADDIX
                   KEY NA-TAXID
                   INVALID KEY
                       SET  W-INVALID-KEY
                                   TO TRUE
                       DISPLAY 'Key not on file: '
                               NA-TAXID
                       GO TO SUB-9400-EXIT
               END-READ
           END-IF

           IF      W-FILE-STATUS-GOOD
               SET  W-NOT-INVALID-KEY
                                   TO TRUE
               ADD  1              TO W-NAMADDIX-RECS
           ELSE    
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' reading NAMADDIX key'
               MOVE 20             TO W-RETURN-CODE
           END-IF
           .
       SUB-9400-EXIT.
           EXIT.
      /
       SUB-9500-START-NAMADDIX.
      *------------------------

           IF      W-READ-KEY-FIELD-LAST-NAME
               MOVE LOW-VALUES     TO NA-LAST-NAME

               START NAMADDIX
                   KEY >= NA-LAST-NAME
               END-START
           ELSE
               MOVE LOW-VALUES     TO NA-TAXID

               START NAMADDIX
                   KEY >= NA-TAXID
               END-START
           END-IF

           IF      NOT W-FILE-STATUS-GOOD
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' starting NAMADDIX'
               MOVE 40             TO W-RETURN-CODE
           END-IF
           .
       SUB-9500-EXIT.
           EXIT.
      /
       SUB-9600-READ-NAMADDIX-SEQ.
      *---------------------------
      
           READ NAMADDIX NEXT
               AT END
                   SET  W-EOF      TO TRUE
                   GO TO SUB-9600-EXIT
           END-READ

           IF      W-FILE-STATUS-GOOD
               ADD  1              TO W-NAMADDIX-RECS
           ELSE    
               DISPLAY W-ERROR-MSG
                       'File status '
                       W-FILE-STATUS
                       ' reading NAMADDIX next record'
               MOVE 50             TO W-RETURN-CODE
           END-IF
           .
       SUB-9600-EXIT.
           EXIT.
