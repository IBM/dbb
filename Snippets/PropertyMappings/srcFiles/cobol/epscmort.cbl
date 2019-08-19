      *****************************************************************
      * EPSCMORT.cbl  - original                                      *
      *****************************************************************
       ID DIVISION.
       PROGRAM-ID. EPSCMORT.
      *
      *    ON INITIAL ENTRY DISPLAY MAP
      *    ON SUBSEQUENT ENTRY:
      *       VALIDATE KEY. IF F3 AND MORT SCREEN, QUIT.
      *                     IF F3 AND RET FROM EPSMLIST, RESEND MAP
      *                     IF Enter, VALIDATE/CONVERT DATA THEN
      *                             LINK TO EPSCSMRT TO CALC MORT
      *                     IF F9, LINK TO EPSMLIST
      * CCC
      * XAV
      *    (C) 2008 IBM - JIM HILDNER RESERVED.
      *    (C) 2012 IBM - MODIFIED BY DAVE ELLIS.
      *    (C) 2012 IBM - MODIFIED BY AMY SILBERBAUER.
      *============== Xav change 04 Feb ==============================
      *============== Xav demo 05 Feb   ==============================
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
               88  SEND-ALL                     VALUE '5'.

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
      *
       01 W-RETIREMENT-WA                     PIC 9(4).
       01 W-COMAREA-LENGTH                    PIC 9(4) COMP.

        01  SQL-ERROR-MSG.
            03  FILLER              PIC X(11)      VALUE 'SQL ERROR: '.
            03  SQL-ERROR-CODE      PIC 9(5) DISPLAY.

            EXEC SQL
                INCLUDE SQLCA
            END-EXEC.

            EXEC SQL DECLARE SYSIBM.SYSDUMMY1 TABLE
            ( IBMREQD                        CHAR(1) NOT NULL
            ) END-EXEC.

       01 IBMREQD                           PIC X(1).
      *
       01  END-OF-TRANS-MSG                 PIC X(30)
             VALUE 'END OF TRANSACTION - THANK YOU'.
       01  BLANK-MSG                        PIC X(1) VALUE ' '.
           COPY DFHAID.
      *    COPY DFHEIBLK.
           COPY EPSMORT.

       01  W-COMMUNICATION-AREA.
           COPY EPSMTCOM.

       COPY EPSNBRPM.

       LINKAGE SECTION.

       01 DFHCOMMAREA.
       COPY EPSMTCOM.

       PROCEDURE DIVISION USING DFHCOMMAREA.

       A000-MAINLINE SECTION.
       A000-10.
      * Call procedure to do SQL call
      *    PERFORM A805-DUMMY-SQL-CALL
           INITIALIZE W-COMMUNICATION-AREA.
           MOVE LOW-VALUES TO W-COMMUNICATION-AREA.

           MOVE LENGTH OF DFHCOMMAREA to W-COMAREA-LENGTH.
           MOVE DFHCOMMAREA to W-COMMUNICATION-AREA.
           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
      * First time in - Show Screen
                   MOVE LOW-VALUES TO EPSMENUO
                   SET SEND-ERASE TO TRUE
                   PERFORM A300-SEND-MAP
                   MOVE '3' TO
                      PROCESS-INDICATOR OF W-COMMUNICATION-AREA
               WHEN EIBAID = DFHCLEAR
      * Process CLEAR key
                   MOVE LOW-VALUES TO EPSMENUO
                   SET SEND-ERASE TO TRUE
                   PERFORM A300-SEND-MAP
               WHEN EIBAID = DFHPF3
      * Process END/RETURN key - F3
                  IF PROCESS-INDICATOR OF W-COMMUNICATION-AREA = '3'
                      EXEC CICS
                         SEND TEXT FROM (END-OF-TRANS-MSG)
                         ERASE
                         FREEKB
                      END-EXEC
                      EXEC CICS
                           RETURN
                      END-EXEC
                   ELSE
                      SET SEND-ALL TO TRUE
                      EXEC CICS
                         SEND TEXT FROM (BLANK-MSG)
                         ERASE
                         FREEKB
                      END-EXEC
                      PERFORM A300-SEND-MAP
                      MOVE '3' TO
                          PROCESS-INDICATOR OF W-COMMUNICATION-AREA
                   END-IF
               WHEN EIBAID = DFHPF9
      * Process PF9 to compare rates
                IF EPSPARM-RULE-FLAG NOT = 1
                   MOVE LOW-VALUES TO EPSMENUO
                   MOVE 'PRESS ENTER FIRST, THEN F9.' TO MSGERRO
                   SET SEND-DATAONLY TO TRUE
                   PERFORM A300-SEND-MAP
                ELSE
                   MOVE '9' TO
                      PROCESS-INDICATOR OF W-COMMUNICATION-AREA
                   EXEC CICS LINK PROGRAM( 'EPSMLIST' )
                          COMMAREA( W-COMMUNICATION-AREA )
                   END-EXEC
                END-IF
               WHEN EIBAID = DFHENTER
                 IF PROCESS-INDICATOR OF W-COMMUNICATION-AREA NOT = '9'
      * Process Enter Key to calculate the loan amount
                   PERFORM A100-PROCESS-MAP
                 END-IF
               WHEN OTHER
      * Invalid key
                    MOVE LOW-VALUES TO EPSMENUO
                    MOVE 'INVALID KEY PRESSED.' TO MSGERRO
                    SET SEND-DATAONLY TO TRUE
                    PERFORM A300-SEND-MAP
           END-EVALUATE
           EXEC CICS
               RETURN TRANSID(EIBTRNID)
               COMMAREA(W-COMMUNICATION-AREA)
               LENGTH(W-COMAREA-LENGTH)
           END-EXEC.

      * NEVER EXECUTED BECAUSE OF ABOVE EXEC CICS RETURN
       A000-EXIT.
           GOBACK.

       A100-PROCESS-MAP SECTION.
       A100-10.
      *    INIT RET CODE
           MOVE 0 TO  EPSPARM-RETURN-ERROR-RC.
           PERFORM A400-RECEIVE-MAP.
      *    IF ANY ERROR IN INPUT, SEND MSG AND QUIT
           IF  EPSPARM-RETURN-ERROR-RC > 0
              MOVE EPSPARM-RETURN-ERROR-TEXT
                TO MSGERRO
              SET SEND-DATAONLY TO TRUE
              PERFORM A300-SEND-MAP
              GO TO A100-EXIT
           END-IF.
           PERFORM A600-CALCULATE-MORTGAGE.
           SET SEND-DATAONLY TO TRUE.
           PERFORM A300-SEND-MAP.
       A100-EXIT.
           EXIT.

       A300-SEND-MAP SECTION.
       A300-10.
           EVALUATE TRUE
              WHEN SEND-MAPONLY
                   EXEC CICS
                     SEND MAP ('EPSMENU')
                       MAPSET('EPSMORT')
                       MAPONLY
                       FREEKB
                       CURSOR
                   END-EXEC
              WHEN SEND-ERASE
                   EXEC CICS
                     SEND MAP ('EPSMENU')
                         MAPSET('EPSMORT')
                         FROM(EPSMENUO)
                         ERASE
                         FREEKB
                         CURSOR
                   END-EXEC
              WHEN SEND-DATAONLY
                   EXEC CICS
                     SEND MAP ('EPSMENU')
                         MAPSET('EPSMORT')
                         FROM(EPSMENUO)
                         DATAONLY
                         CURSOR
                         FREEKB
                   END-EXEC
              WHEN SEND-ALL
                   EXEC CICS
                     SEND MAP ('EPSMENU')
                         MAPSET('EPSMORT')
                         FROM(EPSMENUO)
                         FREEKB
                     END-EXEC.
       A300-EXIT.
           EXIT.

       A400-RECEIVE-MAP SECTION.
       A400-10.
           EXEC CICS
                RECEIVE MAP('EPSMENU')
                   MAPSET('EPSMORT')
                   INTO (EPSMENUI)
           END-EXEC.

           DISPLAY 'AFTER RECEIVE:'
      * VALIDATE LOAN AMOUNT

           MOVE EPLOANI        TO EPSPARM-VALIDATE-DATA.
           MOVE LENGTH OF EPLOANI
                               TO EPSPARM-MAX-LENGTH.
           MOVE 2 TO EPSPARM-RULE-FLAG.
           DISPLAY 'BEFORE EPSNBRVL'
           CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.
      *    CHECK RESULT BEFORE CONTINUING
           IF EPSPARM-RETURN-ERROR-RC > 0
              GO TO A400-EXIT
           END-IF.
           COMPUTE EPSPCOM-PRINCIPLE-DATA OF W-COMMUNICATION-AREA
                = EPSPARM-NUMBER + EPSPARM-DECIMAL.
           DISPLAY 'PRINCIPAL:'
                   EPSPCOM-PRINCIPLE-DATA OF W-COMMUNICATION-AREA

      * VALIDATE LOAN RATE
           DISPLAY 'EPRATEI' EPRATEI
           MOVE EPRATEI              TO EPSPARM-VALIDATE-DATA.
           MOVE LENGTH OF EPRATEI    TO EPSPARM-MAX-LENGTH.
           MOVE 0 TO EPSPARM-RULE-FLAG.
           CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.
      *    CHECK RESULT BEFORE CONTINUING
           IF EPSPARM-RETURN-ERROR-RC > 0
              GO TO A400-EXIT
           END-IF.
           COMPUTE EPSPCOM-QUOTED-INTEREST-RATE OF W-COMMUNICATION-AREA
                = EPSPARM-NUMBER + EPSPARM-DECIMAL.
           DISPLAY 'QUOTED INTEREST RATE:'
                 EPSPCOM-QUOTED-INTEREST-RATE OF W-COMMUNICATION-AREA

      * VALIDATE LOAN TERM
           MOVE EPYEARSI             TO EPSPARM-VALIDATE-DATA.
           MOVE LENGTH OF EPYEARSI   TO EPSPARM-MAX-LENGTH.
           MOVE 1 TO EPSPARM-RULE-FLAG.
           CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.
      *    CHECK RESULT BEFORE CONTINUING
           IF EPSPARM-RETURN-ERROR-RC > 0
              GO TO A400-EXIT
           END-IF.

           COMPUTE EPSPCOM-NUMBER-OF-YEARS OF W-COMMUNICATION-AREA
                = EPSPARM-NUMBER + EPSPARM-DECIMAL.

           DISPLAY 'PAN EPPANVI:' EPPANVI
           MOVE EPPANVI    TO EPSPCOM-PAN  OF
                             W-COMMUNICATION-AREA.

       A400-EXIT.
           EXIT.


       A600-CALCULATE-MORTGAGE SECTION.
       A600-10.
           MOVE 'Y' TO EPSPCOM-YEAR-MONTH-IND
                           OF W-COMMUNICATION-AREA.
           MOVE 'EPSCSMRT' TO W-CALL-PROGRAM
           EXEC CICS LINK PROGRAM( W-CALL-PROGRAM )
                          COMMAREA( W-COMMUNICATION-AREA )
           END-EXEC
           .
           MOVE EPSPCOM-RETURN-MONTH-PAYMENT
                             OF W-COMMUNICATION-AREA
                             TO WS-FORMAT-NUMBER.

           MOVE WS-FORMAT-NUMBER
                             TO EPPAYMNTO.
           MOVE EPSPCOM-ERRMSG
                             OF W-COMMUNICATION-AREA
                             TO MSGERRO.
       A600-EXIT.
           EXIT.

       A805-DUMMY-SQL-CALL.
      *     EXEC SQL
      *         SELECT IBMREQD
      *              INTO :IBMREQD
      *              FROM SYSIBM.SYSDUMMY1
      *     END-EXEC.
      *
      *     IF SQLCODE = 100
      *         MOVE 'No rows found on SYSDUMM1.' TO MSGERRO
      *     ELSE
      *         IF SQLCODE NOT = 0
      *             MOVE SQLCODE TO SQL-ERROR-CODE
      *             MOVE SQL-ERROR-MSG TO MSGERRO
      *         END-IF
      *     END-IF.
      *
