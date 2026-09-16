       ID DIVISION.
       PROGRAM-ID. EPSMLIST.
      *    THIS DEMONSTRATES CICS/DEBUG           - EPSDEMOS 2008
      *
      *    THIS PROGRAM WILL RECEIVE A DATE AND COVERT THE DATE TO
      *    AN INTEGER IN A CALLED PROGRAM TO DETERMINE DAYS FROM
      *    CURRENT DATE.
      *
      *    (C) 2008 IBM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-FLEX-ES.
       OBJECT-COMPUTER. IBM-FLEX-ES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  W-FLAGS.
           10  W-SEND-FLAG                    PIC X.
               88  SEND-ERASE                   VALUE '1'.
               88  SEND-DATAONLY                VALUE '2'.
               88  SEND-MAPONLY                 VALUE '3'.
               88  SEND-DATAONLY-ALARM          VALUE '4'.

       01 W-CONVERSIONS.
           05  W-PMT-CNVRT     PIC X(12).
           05  W-PMT-NUMBER
               REDEFINES W-PMT-CNVRT
                               PIC 9(10)V99.
           05  WS-FORMAT-NUMBER PIC Z,ZZZ,ZZ9.99.
           05  W-PRINC-CNVRT   PIC X(12).
           05  W-PRINC-NUMBER
               REDEFINES W-PRINC-CNVRT
                               PIC 9(10)V99.

       01 W-CALL-PROGRAM                      PIC X(8).
       01 RESPONSE                            PIC S9(8) COMP.
       01 INTERNAL-PROGRAM-VARIABLES.
           05 RID-LENGTH                      PIC S9(9) COMP.
           05 DISP-COUNT                      PIC S9(4) COMP.
           05 MAX-LOOP                        PIC S9(4) COMP
                                              VALUE IS 8.
           05 END-OF-FILE                     PIC X.
           05 CLOSE-FILE                      PIC X.
      *
       01 W-RETIREMENT-WA                     PIC 9(4).
       01 W-COMAREA-LENGTH                    PIC 9(4) COMP.

       01 SAVE-COMM-AREA.
          COPY EPSMTCOM.

       01  END-OF-TRANS-MSG                 PIC X(30)
             VALUE 'END OF TRANSACTION - THANK YOU'.
           COPY DFHAID.
      *    COPY DFHEIBLK.

           COPY EPSMLIS.
       01  OUTMAP REDEFINES EPSMLISI.
           03 FILLER PIC X(110).
           03 OUTMAP-REPEATE OCCURS 8 TIMES.
              05 FILLER                    PIC X(5).
              05 OUTMAP-COMPANY            PIC X(24).
              05 FILLER                    PIC X(5).
              05 OUTMAP-PHONE-NUM          PIC X(13).
              05 FILLER                    PIC X(5).
              05 OUTMAP-RATE               PIC X(5).
              05 FILLER                    PIC X(5).
              05 OUTMAP-LOAN               PIC X(12).
              05 FILLER                    PIC X(5).
              05 OUTMAP-YEARS              PIC X(2).
           03 FILLER                       PIC X(5).
           03 OUTMAP-MSG                   PIC X(40).

           COPY EPSNBRPM.

           COPY EPSMORTF.

       01  W-COMMUNICATION-AREA.
           COPY EPSMTCOM.

       LINKAGE SECTION.

       01 DFHCOMMAREA.
       COPY EPSMTCOM.

       PROCEDURE DIVISION USING DFHCOMMAREA.

      * JPH - DO I need this for files
      * EXEC CICS HANDLE CONDITION H900-NOT-FOUND


       EPSCMORT-MAINLINE.
           MOVE LENGTH OF DFHCOMMAREA to W-COMAREA-LENGTH.
           MOVE DFHCOMMAREA           TO SAVE-COMM-AREA.
           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
      * First time in - Show Screen
                   PERFORM A100-PROCESS-MAP
               WHEN EIBAID = DFHCLEAR
      * Process CLEAR key
                   EXEC CICS
                        RETURN
                   END-EXEC
               WHEN EIBAID = DFHPF3 OR DFHPF12
      * Process END/RETURN keys
                   EXEC CICS
                        RETURN
                   END-EXEC
               WHEN EIBAID = DFHENTER
      * Process ENTER Key
                   PERFORM A100-PROCESS-MAP
               WHEN OTHER
      * Present Invalid Key
                   PERFORM A100-PROCESS-MAP
           END-EVALUATE
           .
           MOVE SAVE-COMM-AREA TO DFHCOMMAREA.
           EXEC CICS RETURN END-EXEC.

       A100-PROCESS-MAP.

           PERFORM A310-ERASE-MAP.

           MOVE 0      TO RID-LENGTH.
           MOVE 'N'    TO CLOSE-FILE.
           MOVE 'N'    TO END-OF-FILE.

           EXEC CICS STARTBR DATASET('EPSMORTF')
                     RIDFLD(RID-LENGTH) RBA
                     EQUAL
                     RESP(RESPONSE) END-EXEC.

           IF (RESPONSE = DFHRESP(NORMAL))
              MOVE 'Y' TO CLOSE-FILE
              MOVE 1   TO DISP-COUNT
              PERFORM A150-PROCESS-FILE
                      UNTIL END-OF-FILE = 'Y'
                      OR    DISP-COUNT  > MAX-LOOP
           ELSE
              MOVE 'ERROR WITH START'         TO EPCMP1O
              MOVE RESPONSE                   TO EPLOAN1O
           END-IF
           .


           IF CLOSE-FILE = 'Y'
            EXEC CICS ENDBR FILE('EPSMORTF') END-EXEC
           END-IF
           .

           PERFORM A300-SEND-MAP.

       A150-PROCESS-FILE.

           EXEC CICS READNEXT FILE('EPSMORTF')
                    INTO(MORTGAGE-COMPANY-INFO)
                    RIDFLD(RID-LENGTH)
                    RBA RESP(RESPONSE)
           END-EXEC
           .
           IF (RESPONSE = DFHRESP(NORMAL))
              IF  EPSPCOM-PRINCIPLE-DATA OF SAVE-COMM-AREA
                                          < MORT-FILE-LOAN
              AND EPSPCOM-QUOTED-INTEREST-RATE OF SAVE-COMM-AREA
                                          > MORT-FILE-RATE
                 MOVE MORT-FILE-COMPANY
                                       TO OUTMAP-COMPANY(DISP-COUNT)
                 MOVE MORT-FILE-PHONE-NUM
                                       TO OUTMAP-PHONE-NUM(DISP-COUNT)
                 PERFORM A600-CALCULATE-MORTGAGE
                 MOVE MORT-FILE-RATE
                                       TO WS-FORMAT-NUMBER
                 MOVE WS-FORMAT-NUMBER(7:5)
                                       TO OUTMAP-RATE(DISP-COUNT)
                 MOVE EPSPCOM-RETURN-MONTH-PAYMENT OF DFHCOMMAREA
                                       TO WS-FORMAT-NUMBER
                 MOVE WS-FORMAT-NUMBER TO OUTMAP-LOAN(DISP-COUNT)
                 MOVE MORT-FILE-YEARS
                                       TO OUTMAP-YEARS(DISP-COUNT)
                 ADD 1                 TO DISP-COUNT
              END-IF
           ELSE
              IF (RESPONSE NOT = DFHRESP(ENDFILE))
                 MOVE 'ERROR WITH READ NEXT' TO EPCMP1O
                 MOVE RESPONSE               TO EPLOAN1O
              ELSE
                 MOVE 'Y' TO END-OF-FILE
              END-IF
           END-IF
           .

       A300-SEND-MAP.
                   EXEC CICS
                     SEND MAP ('EPSMLIS')
                         MAPSET('EPSMLIS')
                         FROM(EPSMLISO)
                   END-EXEC.

       A310-ERASE-MAP.
            MOVE LOW-VALUES TO EPSMLISO.
            EXEC CICS
                SEND MAP ('EPSMLIS')
                     MAPSET('EPSMLIS')
                     FROM(EPSMLISO)
                     ERASE
            END-EXEC.


       A600-CALCULATE-MORTGAGE.
           MOVE SAVE-COMM-AREA   TO DFHCOMMAREA.
           MOVE 'Y' TO EPSPCOM-YEAR-MONTH-IND
                                 OF DFHCOMMAREA.
           MOVE MORT-FILE-RATE   TO EPSPCOM-QUOTED-INTEREST-RATE
                                 OF DFHCOMMAREA.
           MOVE MORT-FILE-YEARS  TO EPSPCOM-NUMBER-OF-YEARS
                                 OF DFHCOMMAREA.
           MOVE 'EPSCSMRT' TO W-CALL-PROGRAM
           EXEC CICS LINK PROGRAM( W-CALL-PROGRAM )
                          COMMAREA( DFHCOMMAREA )
           END-EXEC
           MOVE EPSPCOM-RETURN-MONTH-PAYMENT
                                 OF DFHCOMMAREA
                                 TO WS-FORMAT-NUMBER.
           MOVE WS-FORMAT-NUMBER TO OUTMAP-LOAN(DISP-COUNT).
