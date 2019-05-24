       ID DIVISION.
       PROGRAM-ID. EPSCMORT.
      *    THIS DEMONSTRATES CICS/DEBUG           - EPSDEMOS 2008
      *
      *    THIS PROGRAM WILL RECEIVE A DATE AND COVERT THE DATE TO
      *    AN INTEGER IN A CALLED PROGRAM TO DETERMINE DAYS FROM
      *    CURRENT DATE.
      *
      *    (C) 2017 IBM - JIM HILDNER RESERVED.
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
      *
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *
           EXEC SQL DECLARE SYSIBM.SYSDUMMY1 TABLE
           ( IBMREQD                        CHAR(1) NOT NULL
           ) END-EXEC.
      *
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

       EPSCMORT-MAINLINE.
      * Call procedure to do SQL call
           PERFORM A805-DUMMY-SQL-CALL
           MOVE LENGTH OF DFHCOMMAREA to W-COMAREA-LENGTH.
           MOVE DFHCOMMAREA to W-COMMUNICATION-AREA.
           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
      * First time in - Show Screen
                   MOVE LOW-VALUES TO EPMENUO
                   SET SEND-ERASE TO TRUE
                   PERFORM A300-SEND-MAP
                   MOVE '3' TO
                      PROCESS-INDICATOR OF W-COMMUNICATION-AREA
               WHEN EIBAID = DFHCLEAR
      * Process CLEAR key
                   MOVE LOW-VALUES TO EPMENUO
                   SET SEND-ERASE TO TRUE
                   PERFORM A300-SEND-MAP
               WHEN EIBAID = DFHPF3 OR DFHPF12
      * Process END/RETURN keys
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
                   MOVE '9' TO
                      PROCESS-INDICATOR OF W-COMMUNICATION-AREA
                   EXEC CICS LINK PROGRAM( 'EPSMLIST' )
                          COMMAREA( W-COMMUNICATION-AREA )
                   END-EXEC
               WHEN EIBAID = DFHENTER
      * Process ENTER Key
                   IF PROCESS-INDICATOR OF W-COMMUNICATION-AREA = '3'
                      PERFORM A100-PROCESS-MAP
                   ELSE
                      EXEC CICS LINK PROGRAM('EPSMLIST')
                             COMMAREA( W-COMMUNICATION-AREA )
                      END-EXEC
                   END-IF
               WHEN OTHER
      * Process Data
                    IF PROCESS-INDICATOR OF W-COMMUNICATION-AREA = '3'
                      PERFORM A600-CALCULATE-MORTGAGE
                      EXEC CICS RETURN END-EXEC
      *             ELSE
      *                MOVE X'E8' TO MSGERRA
      *                MOVE LOW-VALUES TO EPMENUO
      *                SET SEND-DATAONLY-ALARM TO TRUE
      *                PERFORM A300-SEND-MAP
                    END-IF
           END-EVALUATE
           EXEC CICS
               RETURN TRANSID('EPSP')
               COMMAREA(W-COMMUNICATION-AREA)
               LENGTH(W-COMAREA-LENGTH)
           END-EXEC.

       A100-PROCESS-MAP.
           PERFORM A400-RECEIVE-MAP.
           PERFORM A600-CALCULATE-MORTGAGE
           SET SEND-DATAONLY TO TRUE
           PERFORM A300-SEND-MAP
               .

       A300-SEND-MAP.
           EVALUATE TRUE
              WHEN SEND-MAPONLY
                   EXEC CICS
                     SEND MAP ('EPMENU')
                       MAPSET('EPSMORT')
                       MAPONLY
                       CURSOR
                   END-EXEC
              WHEN SEND-ERASE
                   EXEC CICS
                     SEND MAP ('EPMENU')
                         MAPSET('EPSMORT')
                         FROM(EPMENUO)
                         ERASE
                         CURSOR
                   END-EXEC
              WHEN SEND-DATAONLY
                   EXEC CICS
                     SEND MAP ('EPMENU')
                         MAPSET('EPSMORT')
                         FROM(EPMENUO)
                         DATAONLY
                         CURSOR
                   END-EXEC
              WHEN SEND-ALL
                   EXEC CICS
                     SEND MAP ('EPMENU')
                         MAPSET('EPSMORT')
                         FROM(EPMENUO)
                     END-EXEC.

       A400-RECEIVE-MAP.
           EXEC CICS
                RECEIVE MAP('EPMENU')
                   MAPSET('EPSMORT')
                   INTO (EPMENUI)
           END-EXEC.

           MOVE EPLOANI        TO EPSPARM-VALIDATE-DATA.
           MOVE LENGTH OF EPLOANI
                               TO EPSPARM-MAX-LENGTH.
           CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.
           COMPUTE EPSPCOM-PRINCIPLE-DATA
                OF W-COMMUNICATION-AREA
                = EPSPARM-NUMBER + EPSPARM-DECIMAL.

           MOVE EPYEARSI             TO EPSPARM-VALIDATE-DATA.
           MOVE LENGTH OF EPYEARSI   TO EPSPARM-MAX-LENGTH.
           CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.
           COMPUTE EPSPCOM-NUMBER-OF-YEARS
                OF W-COMMUNICATION-AREA
                = EPSPARM-NUMBER + EPSPARM-DECIMAL.

           MOVE EPRATEI              TO EPSPARM-VALIDATE-DATA.
           MOVE LENGTH OF EPRATEI    TO EPSPARM-MAX-LENGTH.
           CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.
           COMPUTE EPSPCOM-QUOTED-INTEREST-RATE
                OF W-COMMUNICATION-AREA
                = EPSPARM-NUMBER + EPSPARM-DECIMAL.


       A600-CALCULATE-MORTGAGE.
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

       A805-DUMMY-SQL-CALL.
           EXEC SQL
               SELECT IBMREQD
                    INTO :IBMREQD
                    FROM SYSIBM.SYSDUMMY1
           END-EXEC.
      *
           IF SQLCODE = 100
               MOVE 'No rows found on SYSDUMM1.' TO MSGERRO
           ELSE
               IF SQLCODE NOT = 0
                   MOVE SQLCODE TO SQL-ERROR-CODE
                   MOVE SQL-ERROR-MSG TO MSGERRO
               END-IF
           END-IF.
      *
