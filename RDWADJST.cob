      *========================== RDWADJST ============================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Description: This program adjusts the Record Descriptor Word 
      *     (RDW) on variable length record files, to support uploading 
      *     and downloading such files to/from an IBM mainframe.
      *     GnuCOBOL expects RDWs to have a record length excluding 
      *     the RDW while the mainframe RDWs have a record length 
      *     including the RDW, i.e. the GnuCOBOL RDW record length is
      *     4 less than the mainframe.
      *
      *     The program supports GnuCOBOL files read/written with 
      *     COB_VARSEQ_FORMAT = 0, i.e. RDWs have a 2 byte record
      *     length (big-endian) + 2 NULs.
      *
      *     This program must be compiled with option std=mf which  
      *     supports COMP-X fields and CBL_OPEN_FILE, etc.
      *
      *     This program requires 2 run-time arguments:
      *         1. File name containing RDWs to be updated (RDWs are
      *            updated in place).
      *         2. GC2MF or MF2GC to increase or decrease RDWs by 4:
      *                GC2MF is GnuCOBOL to Mainframe,
      *                MF2GC is Mainframe to GnuCOBOL.
      *
      *     When uploading/downloading mainframe files, FTP in binary
      *     mode and specify "quote site rdw" to have the mainframe 
      *     include the RDWs. 
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-18  0.1      First release
      *
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.                 RDWADJST.

       ENVIRONMENT DIVISION.
      *=====================

       CONFIGURATION SECTION.
      *----------------------

       SOURCE-COMPUTER.
           IBM-Z15.
      *    IBM-Z15 DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
      *---------------------

      /
       DATA DIVISION.
      *==============

       FILE SECTION.
      *-------------

       WORKING-STORAGE SECTION.
      *------------------------

       01  W-RDWFILE-RECS          PIC 9(09)  COMP.
       01  W-RDW-LEN-MIN           PIC S9(09) COMP VALUE 99999999.
       01  W-RDW-LEN-MAX           PIC S9(09) COMP VALUE 0.
       01  W-QUOTIENT              PIC 9(09)  COMP.
       01  W-REMAINDER             PIC 9(09)  COMP.
       01  W-DISP-NUM              PIC ZZ,ZZZ,ZZ9.
       01  W-DATA-LEN              PIC X(04)  COMP-X.
       01  W-FILE-HANDLE           PIC X(04)  COMP-X.
       01  W-OFFSET                PIC X(08)  COMP-X.
       01  W-NBYTES                PIC X(04)  COMP-X
                                                   VALUE 4.
       01  W-FLAG                  PIC X(01)  COMP-X 
                                                   VALUE 0.
       01  W-FILE-PATH             PIC X(256).

       01  W-FUNCTION              PIC X(05).
           88  W-FUNCTION-DECR-RDW                 VALUE 'MF2GC'.
           88  W-FUNCTION-INCR-RDW                 VALUE 'GC2MF'.
           88  W-FUNCTION-OK                       VALUE 'MF2GC'
                                                         'GC2MF'.

       01  FILLER                  PIC X(01).
           88  W-EOF                               VALUE 'Y'.
           88  W-NOT-EOF                           VALUE 'N'.

       01  FILLER                  PIC X(01).
           88  W-VALIDATE                          VALUE 'V'.
           88  W-UPDATE                            VALUE 'U'.

       01  W-ERROR-MSG             PIC X(21)       VALUE
           '**** RDWADJST error: '.

       01  W-COMPILED-DATE.
           05  W-COMPILED-DATE-YYYY
                                   PIC X(04).
           05  W-COMPILED-DATE-MM  PIC X(02).
           05  W-COMPILED-DATE-DD  PIC X(02).
           05  W-COMPILED-TIME-HH  PIC X(02).
           05  W-COMPILED-TIME-MM  PIC X(02).
           05  W-COMPILED-TIME-SS  PIC X(02).
           05  FILLER              PIC X(07).

       01  W-RDW.
           05  W-RDW-LEN           PIC X(02)  COMP-X.
           05  W-RDW-LV            PIC X(02).
       01  FILLER REDEFINES W-RDW.
           05  W-RDW-CHAR          PIC X(01)  COMP-X
                                                   OCCURS 4
                                                   INDEXED W-R-DX.

       01  W-HEX-TABLE.
           05  FILLER              PIC X(16)       VALUE
               '0123456789ABCDEF'.
       01  FILLER REDEFINES W-HEX-TABLE.
           05  W-HEX-CHAR          PIC X(01)       OCCURS 16.

       01  W-HEX-STRING.
           05  FILLER                              OCCURS 4
                                                   INDEXED W-H-DX.
               10  W-HEX-CHAR-1    PIC X(01).
               10  W-HEX-CHAR-2    PIC X(01).
               10  W-HEX-DASH      PIC X(01).
      /
       PROCEDURE DIVISION.
      *===================

       MAIN.
      *-----

           PERFORM SUB-1000-START-UP THRU SUB-1000-EXIT

           IF      RETURN-CODE = 0           
               SET  W-VALIDATE     TO TRUE

               PERFORM SUB-2000-PROCESS-RECS THRU SUB-2000-EXIT
           END-IF

           IF      RETURN-CODE = 0           
               SET  W-UPDATE       TO TRUE

               PERFORM SUB-2000-PROCESS-RECS THRU SUB-2000-EXIT
           END-IF

           IF      RETURN-CODE = 0           
               PERFORM SUB-3000-SHUT-DOWN THRU SUB-3000-EXIT
           END-IF
           .
       MAIN-EXIT.
           STOP RUN.
      /
       SUB-1000-START-UP.
      *------------------

           MOVE FUNCTION WHEN-COMPILED 
                                   TO W-COMPILED-DATE

           DISPLAY 'RDWADJST compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS

           PERFORM SUB-1100-GET-ARGUMENTS THRU SUB-1100-EXIT

           IF      RETURN-CODE NOT = 0
               GO TO SUB-1000-EXIT
           END-IF

           CALL 'CBL_OPEN_FILE' USING W-FILE-PATH
                                      3 *> INPUT AND OUTPUT
                                      0 *> FUTURE USE
                                      0 *> FUTURE USE
                                      W-FILE-HANDLE

           IF      RETURN-CODE NOT = 0            
               DISPLAY W-ERROR-MSG
                       ' return code '
                       RETURN-CODE
                       ' from CBL_OPEN_FILE'
           END-IF
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-1100-GET-ARGUMENTS.
      *-----------------------

           DISPLAY 1             UPON ARGUMENT-NUMBER

           ACCEPT W-FILE-PATH    FROM ARGUMENT-VALUE
               ON EXCEPTION
                   DISPLAY W-ERROR-MSG
                           'Argument 1 required - RDW file name'
                   MOVE 20         TO RETURN-CODE
                   GO TO SUB-1100-EXIT
           END-ACCEPT

           DISPLAY 'RDW file: '
                   W-FILE-PATH

           DISPLAY 2             UPON ARGUMENT-NUMBER

           ACCEPT W-FUNCTION     FROM ARGUMENT-VALUE
               ON EXCEPTION
                   DISPLAY W-ERROR-MSG
                           'Argument 2 required - function code'
                   MOVE 30         TO RETURN-CODE
                   GO TO SUB-1100-EXIT
           END-ACCEPT

           IF      W-FUNCTION-OK
               DISPLAY 'Function: '
                       W-FUNCTION
           ELSE
               DISPLAY W-ERROR-MSG
                       ' Argument 2 must be GC2MF or MF2GC'
               MOVE 40             TO RETURN-CODE
               GO TO SUB-1100-EXIT
           END-IF
           .
       SUB-1100-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS-RECS.
      *----------------------

           IF      W-VALIDATE
               DISPLAY 'Validating RDWs...'
           ELSE
               DISPLAY 'Updating RDWs...'
           END-IF

           SET  W-NOT-EOF          TO TRUE
           MOVE 0                  TO W-RDWFILE-RECS 
                                      W-OFFSET

           PERFORM SUB-9100-READ-RDWFILE THRU SUB-9100-EXIT

           PERFORM SUB-2100-PROCESS-REC THRU SUB-2100-EXIT
               UNTIL W-EOF
               OR    RETURN-CODE NOT = 0

           IF      W-VALIDATE
               MOVE W-RDW-LEN-MIN   TO W-DISP-NUM
               DISPLAY 'Shortest RDW length: '
                       W-DISP-NUM
               MOVE W-RDW-LEN-MAX   TO W-DISP-NUM
               DISPLAY 'Longest  RDW length: '
                       W-DISP-NUM
           END-IF
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-2100-PROCESS-REC.
      *---------------------

      D    DISPLAY 'W-RDW-LEN = '
      D            W-RDW-LEN

           IF      W-RDW-LEN < W-RDW-LEN-MIN
               MOVE W-RDW-LEN      TO W-RDW-LEN-MIN
           END-IF

           IF      W-RDW-LEN > W-RDW-LEN-MAX
               MOVE W-RDW-LEN      TO W-RDW-LEN-MAX
           END-IF

           IF      W-RDW-LV NOT = LOW-VALUES
               PERFORM SUB-9900-CONV-RDW-TO-HEX THRU SUB-9900-EXIT
               DISPLAY '  Bytes 3-4 of RDW not = LOW-VALUES at offset '
                       W-OFFSET
                       ': '
                       W-HEX-STRING(1 : 11)
               MOVE 110            TO RETURN-CODE
               GO TO SUB-2100-EXIT
           END-IF

           IF      W-RDW-LEN > 32 * 1024
               PERFORM SUB-9900-CONV-RDW-TO-HEX THRU SUB-9900-EXIT
               DISPLAY '  RDW len > 32K at offset '
                       W-OFFSET
                       ': '
                       W-HEX-STRING(1 : 11)
               MOVE 120            TO RETURN-CODE
               GO TO SUB-2100-EXIT
           END-IF

           IF      W-FUNCTION-DECR-RDW
               IF      W-RDW-LEN <= 4
                   PERFORM SUB-9900-CONV-RDW-TO-HEX THRU SUB-9900-EXIT
                   DISPLAY '  RDW len <= 4 at offset '
                           W-OFFSET
                           ': '
                           W-HEX-STRING(1 : 11)
                   MOVE 130        TO RETURN-CODE
                   GO TO SUB-2100-EXIT
               END-IF

               SUBTRACT 4        FROM W-RDW-LEN
                               GIVING W-DATA-LEN
               MOVE W-DATA-LEN     TO W-RDW-LEN
           ELSE
               MOVE W-RDW-LEN      TO W-DATA-LEN
               ADD  4
                    W-DATA-LEN GIVING W-RDW-LEN
           END-IF

           IF      W-UPDATE
               PERFORM SUB-9200-WRITE-RDWFILE THRU SUB-9200-EXIT
           END-IF

           ADD  4
                W-DATA-LEN         TO W-OFFSET

           PERFORM SUB-9100-READ-RDWFILE THRU SUB-9100-EXIT
           .
       SUB-2100-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------

           CALL 'CBL_CLOSE_FILE'
                                USING W-FILE-HANDLE

           DISPLAY 'RDWADJST completed'
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9100-READ-RDWFILE.
      *----------------------

           CALL 'CBL_READ_FILE' USING W-FILE-HANDLE
                                      W-OFFSET
                                      W-NBYTES
                                      W-FLAG
                                      W-RDW

           EVALUATE RETURN-CODE            
             WHEN 0
               CONTINUE

             WHEN 10
               PERFORM SUB-9110-DISPLAY-RECS THRU SUB-9110-EXIT
               SET  W-EOF          TO TRUE
               MOVE 0              TO RETURN-CODE
               GO TO SUB-9100-EXIT

             WHEN OTHER
               DISPLAY W-ERROR-MSG
                       ' return code '
                       RETURN-CODE
                       ' from CBL_READ_FILE'
               GO TO SUB-9100-EXIT
           END-EVALUATE

           ADD  1                  TO W-RDWFILE-RECS

           DIVIDE W-RDWFILE-RECS   BY 10000
                               GIVING W-QUOTIENT
                            REMAINDER W-REMAINDER

           IF      W-REMAINDER = 0
               PERFORM SUB-9110-DISPLAY-RECS THRU SUB-9110-EXIT
           END-IF
           .
       SUB-9100-EXIT.
           EXIT.
      /
       SUB-9110-DISPLAY-RECS.
      *----------------------

           MOVE W-RDWFILE-RECS     TO W-DISP-NUM

           IF      W-VALIDATE
               DISPLAY '  RDWs validated: '
                       W-DISP-NUM
           ELSE
               DISPLAY '  RDWs updated:   '
                       W-DISP-NUM
           END-IF
           .
       SUB-9110-EXIT.
           EXIT.
      /
       SUB-9200-WRITE-RDWFILE.
      *-----------------------

           CALL 'CBL_WRITE_FILE'
                                USING W-FILE-HANDLE
                                      W-OFFSET
                                      W-NBYTES
                                      W-FLAG
                                      W-RDW

           IF      RETURN-CODE = 0            
               CONTINUE
           ELSE
               DISPLAY W-ERROR-MSG
                       ' return code '
                       RETURN-CODE
                       ' from CBL_WRITE_FILE'
               GO TO SUB-9200-EXIT
           END-IF
           .
       SUB-9200-EXIT.
           EXIT.
      /
       SUB-9900-CONV-RDW-TO-HEX.
      *-------------------------

           PERFORM VARYING W-R-DX FROM 1 BY 1
                     UNTIL W-R-DX > 4
               
               DIVIDE W-RDW-CHAR(W-R-DX)
                                   BY 16
                               GIVING W-QUOTIENT
                            REMAINDER W-REMAINDER

               SET  W-H-DX         TO W-R-DX
               MOVE W-HEX-CHAR(W-QUOTIENT + 1)
                                   TO W-HEX-CHAR-1(W-H-DX)
               MOVE W-HEX-CHAR(W-REMAINDER + 1)
                                   TO W-HEX-CHAR-2(W-H-DX)
               MOVE '-'            TO W-HEX-DASH  (W-H-DX)
           END-PERFORM
           .
       SUB-9900-EXIT.
           EXIT.
