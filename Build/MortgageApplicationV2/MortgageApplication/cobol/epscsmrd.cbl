       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),OPT,CICS
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *         Web Services for CICS TS 3.x Converter Driver
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRD'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
      * *************************************************************
      *    xx       Coded Character Sets Configuration
      * *************************************************************
      *XML to Language Structure Input CCSID 1140
      *XML to Language Structure Output CCSID 1140
      *Language Structure to XML Output CCSID 1140
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CONVERTER-ERROR-7.
       2 PIC X(40) USAGE DISPLAY
           VALUE 'Language Environment Service Call Failed'.
       1 CONVERTER-ERROR-8.
       2 PIC X(35) USAGE DISPLAY
           VALUE 'Language Environment Message Number'.
       1 CONVERTER-ERROR-9.
       2 PIC X(31) USAGE DISPLAY
           VALUE 'XML Converter Is Terminating...'.
      * *************************************************************
      *             Vendor Program Container Definitions
      * *************************************************************
       1 DFH-BODY-CONTAINER PIC X(16) VALUE 'DFH-BODY'.
       1 DFH-DATA-CONTAINER PIC X(16) VALUE 'DFH-DATA'.
       LOCAL-STORAGE SECTION.
      * *************************************************************
      *             Storage Items For LE Error Handling
      * *************************************************************
       1 CONVERTER-RETURN-CODE PIC S9(9) BINARY.
       1 ROUTINE PROCEDURE-POINTER.
       1 TOKEN POINTER.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 OPTIONAL-FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 ERROR-RESPONSE.
       2 ERROR-OCCURRED PIC X.
       2 ERROR-MESSAGE-NUMBER PIC 9(9).
       2 ERROR-REASON-LENGTH PIC 9(9) BINARY.
       2 ERROR-REASON PIC X(512).
      * *************************************************************
      *                 Converter Metadata Variables
      * *************************************************************
       1 XML2LS-LANG-BUFFER-LENGTH PIC S9(9) COMP.
       1 LS2XML-LANG-BUFFER-LENGTH PIC S9(9) COMP.
       1 LS2XML-XML-BUFFER-LENGTH PIC S9(9) COMP.
       1 XML2LS-XML-CCSID PIC S9(9) COMP.
       1 HOST-LANG-CCSID PIC S9(9) COMP.
       1 LS2XML-XML-CCSID PIC S9(9) COMP.
      * *************************************************************
      *                 SOAP Pipeline Work Variables
      * *************************************************************
       1 SOAP-PIPELINE-WORK-VARIABLES.
       2 NEXT-CONTAINER PIC X(16).
       2 COMMAND-RESP PIC 9(9) BINARY.
       2 COMMAND-RESP2 PIC 9(9) BINARY.
       2 CONTAINER-BROWSE-TOKEN POINTER.
       2 DFH-BODY-PTR POINTER.
       2 DFH-BODY-LEN PIC 9(9) BINARY.
       2 DFH-DATA-PTR POINTER.
       2 DFH-DATA-LEN PIC 9(9) BINARY.
       2 WORK-AREA-PTR POINTER.
       2 WORK-AREA-LEN PIC 9(9) BINARY.
       LINKAGE SECTION.
       1 DFH-BODY PIC X.
       1 DFH-DATA PIC X.
      * *************************************************************
      *              Business Program Binary Interfaces
      * *************************************************************
       01 X00000175
           .
       10 PROCESS-INDICATOR
           PICTURE X
           USAGE DISPLAY
           .
       10 EPSPCOM-PRINCIPLE-DATA
           PICTURE S9(9)V9(2)
           USAGE COMP
           .
       10 EPSPCOM-NUMBER-OF-YEARS
           PICTURE S9(4)
           USAGE COMP
           .
       10 EPSPCOM-NUMBER-OF-MONTHS
           PICTURE S9(4)
           USAGE COMP
           .
       10 EPSPCOM-QUOTED-INTEREST-RATE
           PICTURE S9(2)V9(3)
           USAGE COMP
           .
       10 EPSPCOM-YEAR-MONTH-IND
           PICTURE X
           USAGE DISPLAY
           .
       10 EPSPCOM-RETURN-MONTH-PAYMENT
           PICTURE S9(7)V9(2)
           USAGE COMP
           .
       10 EPSPCOM-ERRMSG
           PICTURE X(80)
           USAGE DISPLAY
           .
       10 EPSPCOM-PROGRAM-RETCODE
           PICTURE 9(4)
           USAGE DISPLAY
           .
       88 EPS02-REQUEST-SUCCESS
           VALUE
           0
           .
       10 EPSPCOM-PROGRAM-RETCODE-RDF REDEFINES EPSPCOM-PROGRAM-RETCODE
           PICTURE X(4)
           USAGE DISPLAY
           .
       PROCEDURE DIVISION.
       MAINLINE SECTION.
      * -------------------------------------------------------------
      *            Initialize Storage and Browse Channel
      * -------------------------------------------------------------
           PERFORM REGISTER-EXCEPTION-HANDLER
           INITIALIZE SOAP-PIPELINE-WORK-VARIABLES
           PERFORM GET-CONVERTER-METADATA
           PERFORM BROWSE-VENDOR-CHANNEL
      * -------------------------------------------------------------
      *           Branch To Processing Logic For Container
      * -------------------------------------------------------------
           EVALUATE NEXT-CONTAINER
             WHEN DFH-BODY-CONTAINER
               PERFORM PROCESS-DFH-BODY
             WHEN DFH-DATA-CONTAINER
               PERFORM PROCESS-DFH-DATA
             WHEN OTHER
               EXEC CICS ABEND
               END-EXEC
           END-EVALUATE
      * -------------------------------------------------------------
      *                           Finished
      * -------------------------------------------------------------
           PERFORM FREE-WORK-AREA
           PERFORM UNREGISTER-EXCEPTION-HANDLER
           EXEC CICS RETURN
           END-EXEC
           .
       BROWSE-VENDOR-CHANNEL.
           EXEC CICS STARTBROWSE CONTAINER
             BROWSETOKEN (CONTAINER-BROWSE-TOKEN)
           END-EXEC
           PERFORM TEST AFTER UNTIL
             NEXT-CONTAINER EQUAL DFH-BODY-CONTAINER OR
             NEXT-CONTAINER EQUAL DFH-DATA-CONTAINER OR
             COMMAND-RESP2 NOT EQUAL ZERO
             EXEC CICS GETNEXT CONTAINER (NEXT-CONTAINER)
               BROWSETOKEN (CONTAINER-BROWSE-TOKEN)
               RESP(COMMAND-RESP)
               RESP2(COMMAND-RESP2)
             END-EXEC
           END-PERFORM
           .
       PROCESS-DFH-BODY.
           PERFORM RECEIVE-DFH-BODY
           PERFORM ALLOCATE-DFH-DATA-WORK-AREA
           MOVE 'N' TO ERROR-OCCURRED
           PERFORM INBOUND-CONVERSION
           IF ERROR-OCCURRED = 'Y'
             PERFORM SEND-SOAP-FAULT
           ELSE
             PERFORM SEND-DFH-DATA
           END-IF
           .
       PROCESS-DFH-DATA.
           PERFORM RECEIVE-DFH-DATA
           PERFORM ALLOCATE-DFH-BODY-WORK-AREA
           MOVE 'N' TO ERROR-OCCURRED
           PERFORM OUTBOUND-CONVERSION
           IF ERROR-OCCURRED = 'Y'
             PERFORM SEND-SOAP-FAULT
           ELSE
             PERFORM SEND-DFH-BODY
           END-IF
           .
       RECEIVE-DFH-BODY.
           MOVE 'DFHREQUEST' TO DFH-BODY-CONTAINER
           EXEC CICS GET CONTAINER(DFH-BODY-CONTAINER)
             SET(DFH-BODY-PTR)
             FLENGTH(DFH-BODY-LEN)
             INTOCCSID(1140)
           END-EXEC
           SET ADDRESS OF DFH-BODY
             TO DFH-BODY-PTR
           .
       SEND-DFH-BODY.
           EXEC CICS PUT CONTAINER(DFH-BODY-CONTAINER)
             FROM(DFH-BODY)
             FLENGTH(DFH-BODY-LEN)
             FROMCCSID(1140)
           END-EXEC
           .
       RECEIVE-DFH-DATA.
           EXEC CICS GET CONTAINER(DFH-DATA-CONTAINER)
             SET(DFH-DATA-PTR)
             FLENGTH(DFH-DATA-LEN)
           END-EXEC
           SET ADDRESS OF X00000175
             TO DFH-DATA-PTR
           .
       SEND-DFH-DATA.
           COMPUTE DFH-DATA-LEN =
             LENGTH OF X00000175
           EXEC CICS PUT CONTAINER(DFH-DATA-CONTAINER)
             FROM(X00000175)
             FLENGTH(DFH-DATA-LEN)
           END-EXEC
           .
       ALLOCATE-DFH-BODY-WORK-AREA.
           MOVE LS2XML-XML-BUFFER-LENGTH
             TO WORK-AREA-LEN
           EXEC CICS GETMAIN
             SET(WORK-AREA-PTR)
             FLENGTH(WORK-AREA-LEN)
           END-EXEC
           SET ADDRESS OF DFH-BODY
             TO WORK-AREA-PTR
           .
       ALLOCATE-DFH-DATA-WORK-AREA.
           MOVE XML2LS-LANG-BUFFER-LENGTH
             TO WORK-AREA-LEN
           EXEC CICS GETMAIN
             SET(WORK-AREA-PTR)
             FLENGTH(WORK-AREA-LEN)
           END-EXEC
           SET ADDRESS OF X00000175
             TO WORK-AREA-PTR
           .
       FREE-WORK-AREA.
           IF WORK-AREA-PTR NOT EQUAL NULL
             EXEC CICS FREEMAIN
               DATAPOINTER(WORK-AREA-PTR)
             END-EXEC
           END-IF
           .
       GET-CONVERTER-METADATA.
           CALL 'EPSCSMRX' USING
             XML2LS-LANG-BUFFER-LENGTH LS2XML-LANG-BUFFER-LENGTH
             LS2XML-XML-BUFFER-LENGTH XML2LS-XML-CCSID
             HOST-LANG-CCSID LS2XML-XML-CCSID
             OMITTED OMITTED
           .
       SEND-SOAP-FAULT.
           EXEC CICS SOAPFAULT CREATE CLIENT
             FAULTSTRING(ERROR-REASON)
             FAULTSTRLEN(ERROR-REASON-LENGTH)
           END-EXEC
           .
       INBOUND-CONVERSION.
           CALL 'EPSCSMRI'
             USING
               X00000175
               DFH-BODY-LEN
               DFH-BODY
               OMITTED
      *   OPTIONAL-FEEDBACK-CODE
             RETURNING
               CONVERTER-RETURN-CODE
           .
       OUTBOUND-CONVERSION.
           CALL 'EPSCSMRO'
             USING
               X00000175
               DFH-BODY-LEN
               DFH-BODY
               OMITTED
      *   OPTIONAL-FEEDBACK-CODE
             RETURNING
               CONVERTER-RETURN-CODE
           .
       REGISTER-EXCEPTION-HANDLER.
           SET ROUTINE TO ENTRY 'EPSCSMRF'
           SET TOKEN TO ADDRESS OF ERROR-RESPONSE
           CALL 'CEEHDLR' USING ROUTINE TOKEN FEEDBACK-CODE
           PERFORM CHECK-LE-SERVICE-FC
           .
       UNREGISTER-EXCEPTION-HANDLER.
           CALL 'CEEHDLU' USING ROUTINE FEEDBACK-CODE
           PERFORM CHECK-LE-SERVICE-FC
           .
       CHECK-LE-SERVICE-FC.
           IF NOT CEE000 OF FEEDBACK-CODE
             DISPLAY CONVERTER-ERROR-7
             DISPLAY CONVERTER-ERROR-8 ' '
               FACILITY OF FEEDBACK-CODE
               MSG-NO OF FEEDBACK-CODE
             DISPLAY CONVERTER-ERROR-9
             STOP RUN
           END-IF
           .
       END PROGRAM 'EPSCSMRD'.
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *                       Exception Handler
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),OPT,NOCICS
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRF'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CONVERTER-ERROR-5.
       2 PIC X(31) USAGE DISPLAY
           VALUE 'Failed To Get Exception Message'.
       LOCAL-STORAGE SECTION.
       1 MSG-PTR PIC S9(9) COMP.
       1 MSG-PART PIC X(80).
       1 MSG-OFFSET PIC 9(9) COMP.
       1 MSG-PART-LENGTH PIC 9(9) COMP.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       LINKAGE SECTION.
       1 TOKEN POINTER.
       1 RESULT PIC S9(9) BINARY.
       88 RESUME VALUE 10.
       1 CURRENT-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION PIC X(12).
       1 ERROR-CDATA-PTR PIC X(512).
       1 ERROR-RESPONSE.
       2 ERROR-OCCURRED PIC X.
       2 ERROR-MESSAGE-NUMBER PIC 9(9).
       2 ERROR-REASON-LENGTH PIC 9(9) BINARY.
       2 ERROR-REASON PIC X(512).
       PROCEDURE DIVISION USING CURRENT-CONDITION TOKEN
           RESULT NEW-CONDITION.
       MAINLINE SECTION.
      * -------------------------------------------------------------
      *             Storage For Saving Exception Details
      * -------------------------------------------------------------
           SET ADDRESS OF ERROR-RESPONSE TO TOKEN
      * -------------------------------------------------------------
      *                    Get Exception Message
      * -------------------------------------------------------------
           PERFORM FILL-DESCRIPTION-BUFFER
      * -------------------------------------------------------------
      *                  Display Exception Message
      * -------------------------------------------------------------
           PERFORM DISPLAY-MESSAGE-TEXT
      * -------------------------------------------------------------
      *        Recover From Exception To Produce XML Response
      * -------------------------------------------------------------
           MOVE 'Y' TO ERROR-OCCURRED
           SET RESUME TO TRUE
      * -------------------------------------------------------------
      *                           Finished
      * -------------------------------------------------------------
           GOBACK
           .
       FILL-DESCRIPTION-BUFFER.
           MOVE 0 TO MSG-PTR
           MOVE 512 TO ERROR-REASON-LENGTH
           MOVE SPACES TO MSG-PART ERROR-REASON
           CALL 'CEEMGET' USING
             CURRENT-CONDITION MSG-PART
             MSG-PTR FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE AND
              NOT CEE0E7 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-5
           END-IF
           IF NOT CEE0E7 OF FEEDBACK-CODE
            PERFORM COMPUTE-PART-LENGTH
            MOVE MSG-PART-LENGTH TO ERROR-REASON-LENGTH
            MOVE MSG-PART TO ERROR-REASON
           ELSE
            MOVE MSG-PART TO ERROR-REASON
            MOVE MSG-PTR TO MSG-OFFSET
            PERFORM UNTIL MSG-PTR = 0
             MOVE SPACES TO MSG-PART
             CALL 'CEEMGET' USING
              CURRENT-CONDITION MSG-PART
              MSG-PTR FEEDBACK-CODE
             IF NOT CEE000 OF FEEDBACK-CODE AND
                NOT CEE0E7 OF FEEDBACK-CODE
              DISPLAY CONVERTER-ERROR-5
             END-IF
             IF MSG-PTR NOT = 0
              MOVE MSG-PART TO
               ERROR-REASON(MSG-OFFSET + 1:MSG-PTR)
              ADD MSG-PTR TO MSG-OFFSET
             ELSE
              PERFORM COMPUTE-PART-LENGTH
              MOVE MSG-PART TO
               ERROR-REASON(MSG-OFFSET + 1:MSG-PART-LENGTH)
              ADD MSG-PART-LENGTH TO MSG-OFFSET
             END-IF
            END-PERFORM
           END-IF
           MOVE MSG-NO OF CURRENT-CONDITION TO
            ERROR-MESSAGE-NUMBER
           MOVE MSG-OFFSET TO ERROR-REASON-LENGTH
           .
       COMPUTE-PART-LENGTH.
           PERFORM VARYING MSG-PART-LENGTH FROM 80 BY -1
            UNTIL MSG-PART(MSG-PART-LENGTH:1) NOT = SPACE
            OR MSG-PART-LENGTH < 1
           END-PERFORM
           .
       DISPLAY-MESSAGE-TEXT.
           DISPLAY ERROR-REASON(1:ERROR-REASON-LENGTH)
           .
       END PROGRAM 'EPSCSMRF'.
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *             Compiled XML Conversion Properties API
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRX'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       1 XML2LS-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-XML-BUFFER-LENGTH PIC 9(9) COMP.
       1 XML2LS-XML-CCSID PIC 9(9) COMP.
       1 HOST-LANG-CCSID PIC 9(9) COMP.
       1 LS2XML-XML-CCSID PIC 9(9) COMP.
       1 XML2LS-PROPERTIES PIC X.
       1 LS2XML-PROPERTIES PIC X.
       PROCEDURE DIVISION USING
           XML2LS-LANG-BUFFER-LENGTH
           LS2XML-LANG-BUFFER-LENGTH
           LS2XML-XML-BUFFER-LENGTH
           XML2LS-XML-CCSID
           HOST-LANG-CCSID
           LS2XML-XML-CCSID
           XML2LS-PROPERTIES
           LS2XML-PROPERTIES
           .
       MAINLINE SECTION.
           IF ADDRESS OF XML2LS-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 106
              TO XML2LS-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 106
              TO LS2XML-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-XML-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 758
              TO LS2XML-XML-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF XML2LS-XML-CCSID
                         NOT EQUAL NULL
            MOVE 1140
              TO XML2LS-XML-CCSID
           END-IF
           IF ADDRESS OF HOST-LANG-CCSID
                         NOT EQUAL NULL
            MOVE 1140
              TO HOST-LANG-CCSID
           END-IF
           IF ADDRESS OF LS2XML-XML-CCSID
                         NOT EQUAL NULL
            MOVE 1140
              TO LS2XML-XML-CCSID
           END-IF
           IF ADDRESS OF XML2LS-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO XML2LS-PROPERTIES
           END-IF
           IF ADDRESS OF LS2XML-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO LS2XML-PROPERTIES
           END-IF
           GOBACK
           .
       END PROGRAM 'EPSCSMRX'.
       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),OPT,NOCICS,XMLPARSE(COMPAT)
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *              XML to Language Structure Converter
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRI'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 XML-ROOT-ELEMENT.
       2 PIC X(11) USAGE DISPLAY
           VALUE 'DFHCOMMAREA'.
       1 ELEMENT-HASH-ENTRIES.
       2 X00000064.
       3 PIC X(35) USAGE DISPLAY
           VALUE 'DFHCOMMAREA/epspcom_number_of_years'.
       3 PIC X(5) USAGE DISPLAY
           VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5 VALUE 3.
       3 PIC 9(4) USAGE COMP-5 VALUE 2.
       3 PIC 9(4) USAGE COMP-5 VALUE 2.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE 'N'.
       2 X00000065.
       3 PIC X(36) USAGE DISPLAY
           VALUE 'DFHCOMMAREA/epspcom_number_of_months'.
       3 PIC X(4) USAGE DISPLAY
           VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5 VALUE 4.
       3 PIC 9(4) USAGE COMP-5 VALUE 2.
       3 PIC 9(4) USAGE COMP-5 VALUE 3.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE 'N'.
       2 X00000066.
       3 PIC X(40) USAGE DISPLAY
           VALUE 'DFHCOMMAREA/epspcom_quoted_interest_rate'.
       3 PIC 9(4) USAGE COMP-5 VALUE 5.
       3 PIC 9(4) USAGE COMP-5 VALUE 3.
       3 PIC 9(4) USAGE COMP-5 VALUE 4.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE 'N'.
       2 X00000067.
       3 PIC X(34) USAGE DISPLAY
           VALUE 'DFHCOMMAREA/epspcom_year_month_ind'.
       3 PIC X(6) USAGE DISPLAY
           VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5 VALUE 6.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE 'X'.
       2 X00000068.
       3 PIC X(11) USAGE DISPLAY
           VALUE 'DFHCOMMAREA'.
       3 PIC X(29) USAGE DISPLAY
           VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5 VALUE 1.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5 VALUE 1.
       3 PIC X VALUE 'T'.
       2 X00000069.
       3 PIC X(34) USAGE DISPLAY
           VALUE 'DFHCOMMAREA/epspcom_principle_data'.
       3 PIC X(6) USAGE DISPLAY
           VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5 VALUE 2.
       3 PIC 9(4) USAGE COMP-5 VALUE 1.
       3 PIC 9(4) USAGE COMP-5 VALUE 1.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE 'N'.
       2 X0000006A.
       3 PIC X(40) VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE '-'.
       1 ELEMENT-HASH-TABLE REDEFINES ELEMENT-HASH-ENTRIES.
       2 EHT-ENTRIES OCCURS 7 TIMES.
       3 ELEMENT-NAME PIC X(40).
       3 ROUTING-CODE PIC 9(4) USAGE COMP-5.
       3 NUMERIC-TARGET-CODE PIC 9(4) USAGE COMP-5.
       3 NUMERIC-RESCUE-CODE PIC 9(4) USAGE COMP-5.
       3 LANG-STRUCT-ID PIC 9(4) USAGE COMP-5.
       3 CONTENT-TYPE PIC X.
       1 NUMERIC-SOURCES.
       2 PIC X(2) USAGE DISPLAY
           VALUE '.9'.
       2 PIC X(11) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(3) USAGE DISPLAY
           VALUE '.99'.
       2 PIC X(10) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(4) USAGE DISPLAY
           VALUE '.999'.
       2 PIC X(9) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(3) USAGE DISPLAY
           VALUE '-.9'.
       2 PIC X(10) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(4) USAGE DISPLAY
           VALUE '-.99'.
       2 PIC X(9) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(5) USAGE DISPLAY
           VALUE '-.999'.
       2 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(2) USAGE DISPLAY
           VALUE '-9'.
       2 PIC X(11) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(4) USAGE DISPLAY
           VALUE '-9.9'.
       2 PIC X(9) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(5) USAGE DISPLAY
           VALUE '-9.99'.
       2 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(6) USAGE DISPLAY
           VALUE '-9.999'.
       2 PIC X(7) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(3) USAGE DISPLAY
           VALUE '-99'.
       2 PIC X(10) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(5) USAGE DISPLAY
           VALUE '-99.9'.
       2 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(6) USAGE DISPLAY
           VALUE '-99.99'.
       2 PIC X(7) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(7) USAGE DISPLAY
           VALUE '-99.999'.
       2 PIC X(6) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(4) USAGE DISPLAY
           VALUE '-999'.
       2 PIC X(9) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(6) USAGE DISPLAY
           VALUE '-999.9'.
       2 PIC X(7) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(7) USAGE DISPLAY
           VALUE '-999.99'.
       2 PIC X(6) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(5) USAGE DISPLAY
           VALUE '-9999'.
       2 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(7) USAGE DISPLAY
           VALUE '-9999.9'.
       2 PIC X(6) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(8) USAGE DISPLAY
           VALUE '-9999.99'.
       2 PIC X(5) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(6) USAGE DISPLAY
           VALUE '-99999'.
       2 PIC X(7) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(8) USAGE DISPLAY
           VALUE '-99999.9'.
       2 PIC X(5) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(9) USAGE DISPLAY
           VALUE '-99999.99'.
       2 PIC X(4) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(7) USAGE DISPLAY
           VALUE '-999999'.
       2 PIC X(6) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(9) USAGE DISPLAY
           VALUE '-999999.9'.
       2 PIC X(4) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(10) USAGE DISPLAY
           VALUE '-999999.99'.
       2 PIC X(3) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(8) USAGE DISPLAY
           VALUE '-9999999'.
       2 PIC X(5) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(10) USAGE DISPLAY
           VALUE '-9999999.9'.
       2 PIC X(3) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(11) USAGE DISPLAY
           VALUE '-9999999.99'.
       2 PIC X(2) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(9) USAGE DISPLAY
           VALUE '-99999999'.
       2 PIC X(4) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(11) USAGE DISPLAY
           VALUE '-99999999.9'.
       2 PIC X(2) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(12) USAGE DISPLAY
           VALUE '-99999999.99'.
       2 PIC X(1) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(10) USAGE DISPLAY
           VALUE '-999999999'.
       2 PIC X(3) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(12) USAGE DISPLAY
           VALUE '-999999999.9'.
       2 PIC X(1) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(13) USAGE DISPLAY
           VALUE '-999999999.99'.
       2 PIC X(1) USAGE DISPLAY
           VALUE '9'.
       2 PIC X(12) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(3) USAGE DISPLAY
           VALUE '9.9'.
       2 PIC X(10) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(4) USAGE DISPLAY
           VALUE '9.99'.
       2 PIC X(9) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(5) USAGE DISPLAY
           VALUE '9.999'.
       2 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(2) USAGE DISPLAY
           VALUE '99'.
       2 PIC X(11) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(4) USAGE DISPLAY
           VALUE '99.9'.
       2 PIC X(9) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(5) USAGE DISPLAY
           VALUE '99.99'.
       2 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(6) USAGE DISPLAY
           VALUE '99.999'.
       2 PIC X(7) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(3) USAGE DISPLAY
           VALUE '999'.
       2 PIC X(10) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(5) USAGE DISPLAY
           VALUE '999.9'.
       2 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(6) USAGE DISPLAY
           VALUE '999.99'.
       2 PIC X(7) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(4) USAGE DISPLAY
           VALUE '9999'.
       2 PIC X(9) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(6) USAGE DISPLAY
           VALUE '9999.9'.
       2 PIC X(7) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(7) USAGE DISPLAY
           VALUE '9999.99'.
       2 PIC X(6) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(5) USAGE DISPLAY
           VALUE '99999'.
       2 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(7) USAGE DISPLAY
           VALUE '99999.9'.
       2 PIC X(6) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(8) USAGE DISPLAY
           VALUE '99999.99'.
       2 PIC X(5) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(6) USAGE DISPLAY
           VALUE '999999'.
       2 PIC X(7) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(8) USAGE DISPLAY
           VALUE '999999.9'.
       2 PIC X(5) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(9) USAGE DISPLAY
           VALUE '999999.99'.
       2 PIC X(4) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(7) USAGE DISPLAY
           VALUE '9999999'.
       2 PIC X(6) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(9) USAGE DISPLAY
           VALUE '9999999.9'.
       2 PIC X(4) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(10) USAGE DISPLAY
           VALUE '9999999.99'.
       2 PIC X(3) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(8) USAGE DISPLAY
           VALUE '99999999'.
       2 PIC X(5) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(10) USAGE DISPLAY
           VALUE '99999999.9'.
       2 PIC X(3) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(11) USAGE DISPLAY
           VALUE '99999999.99'.
       2 PIC X(2) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(9) USAGE DISPLAY
           VALUE '999999999'.
       2 PIC X(4) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(11) USAGE DISPLAY
           VALUE '999999999.9'.
       2 PIC X(2) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(12) USAGE DISPLAY
           VALUE '999999999.99'.
       2 PIC X(1) USAGE DISPLAY
           VALUE SPACES.
       1 NUMERIC-SOURCES-ARRAY REDEFINES NUMERIC-SOURCES.
       2 NPSA PIC X(13)
           OCCURS 64 TIMES
           ASCENDING NPSA INDEXED BY NPSA-NDX.
       1 NUMERIC-MOVE-RULES.
       2 X0000006B.
       3 PIC S9(4) COMP VALUE 1.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 2.
       2 X0000006C.
       3 PIC S9(4) COMP VALUE 3.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 4.
       2 X0000006D.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 5.
       2 X0000006E.
       3 PIC S9(4) COMP VALUE 6.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 7.
       2 X0000006F.
       3 PIC S9(4) COMP VALUE 8.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 9.
       2 X00000070.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 10.
       2 X00000071.
       3 PIC S9(4) COMP VALUE 11.
       3 PIC S9(4) COMP VALUE 12.
       3 PIC S9(4) COMP VALUE 13.
       2 X00000072.
       3 PIC S9(4) COMP VALUE 14.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 15.
       2 X00000073.
       3 PIC S9(4) COMP VALUE 16.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 17.
       2 X00000074.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 18.
       2 X00000075.
       3 PIC S9(4) COMP VALUE 19.
       3 PIC S9(4) COMP VALUE 20.
       3 PIC S9(4) COMP VALUE 21.
       2 X00000076.
       3 PIC S9(4) COMP VALUE 22.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 23.
       2 X00000077.
       3 PIC S9(4) COMP VALUE 24.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 25.
       2 X00000078.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 26.
       2 X00000079.
       3 PIC S9(4) COMP VALUE 27.
       3 PIC S9(4) COMP VALUE 28.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000007A.
       3 PIC S9(4) COMP VALUE 29.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000007B.
       3 PIC S9(4) COMP VALUE 30.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000007C.
       3 PIC S9(4) COMP VALUE 31.
       3 PIC S9(4) COMP VALUE 32.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000007D.
       3 PIC S9(4) COMP VALUE 33.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000007E.
       3 PIC S9(4) COMP VALUE 34.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000007F.
       3 PIC S9(4) COMP VALUE 35.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000080.
       3 PIC S9(4) COMP VALUE 36.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000081.
       3 PIC S9(4) COMP VALUE 37.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000082.
       3 PIC S9(4) COMP VALUE 38.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000083.
       3 PIC S9(4) COMP VALUE 39.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000084.
       3 PIC S9(4) COMP VALUE 40.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000085.
       3 PIC S9(4) COMP VALUE 41.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000086.
       3 PIC S9(4) COMP VALUE 42.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000087.
       3 PIC S9(4) COMP VALUE 43.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000088.
       3 PIC S9(4) COMP VALUE 44.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000089.
       3 PIC S9(4) COMP VALUE 45.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000008A.
       3 PIC S9(4) COMP VALUE 46.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000008B.
       3 PIC S9(4) COMP VALUE 47.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000008C.
       3 PIC S9(4) COMP VALUE 48.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000008D.
       3 PIC S9(4) COMP VALUE 49.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000008E.
       3 PIC S9(4) COMP VALUE 50.
       3 PIC S9(4) COMP VALUE 51.
       3 PIC S9(4) COMP VALUE 52.
       2 X0000008F.
       3 PIC S9(4) COMP VALUE 53.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 54.
       2 X00000090.
       3 PIC S9(4) COMP VALUE 55.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 56.
       2 X00000091.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 57.
       2 X00000092.
       3 PIC S9(4) COMP VALUE 58.
       3 PIC S9(4) COMP VALUE 59.
       3 PIC S9(4) COMP VALUE 60.
       2 X00000093.
       3 PIC S9(4) COMP VALUE 61.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 62.
       2 X00000094.
       3 PIC S9(4) COMP VALUE 63.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 64.
       2 X00000095.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE 65.
       2 X00000096.
       3 PIC S9(4) COMP VALUE 66.
       3 PIC S9(4) COMP VALUE 67.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000097.
       3 PIC S9(4) COMP VALUE 68.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000098.
       3 PIC S9(4) COMP VALUE 69.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X00000099.
       3 PIC S9(4) COMP VALUE 70.
       3 PIC S9(4) COMP VALUE 71.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000009A.
       3 PIC S9(4) COMP VALUE 72.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000009B.
       3 PIC S9(4) COMP VALUE 73.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000009C.
       3 PIC S9(4) COMP VALUE 74.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000009D.
       3 PIC S9(4) COMP VALUE 75.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000009E.
       3 PIC S9(4) COMP VALUE 76.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X0000009F.
       3 PIC S9(4) COMP VALUE 77.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A0.
       3 PIC S9(4) COMP VALUE 78.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A1.
       3 PIC S9(4) COMP VALUE 79.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A2.
       3 PIC S9(4) COMP VALUE 80.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A3.
       3 PIC S9(4) COMP VALUE 81.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A4.
       3 PIC S9(4) COMP VALUE 82.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A5.
       3 PIC S9(4) COMP VALUE 83.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A6.
       3 PIC S9(4) COMP VALUE 84.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A7.
       3 PIC S9(4) COMP VALUE 85.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A8.
       3 PIC S9(4) COMP VALUE 86.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000A9.
       3 PIC S9(4) COMP VALUE 87.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       2 X000000AA.
       3 PIC S9(4) COMP VALUE 88.
       3 PIC S9(4) COMP VALUE -1.
       3 PIC S9(4) COMP VALUE -1.
       1 NMAR-TABLE REDEFINES NUMERIC-MOVE-RULES.
       2 OCCURS 64 TIMES.
       3 NMAR-ENTRY PIC S9(4) COMP OCCURS 3 TIMES.
       1 ERROR-MESSAGES.
       2 CONVERTER-ERROR-3.
       3 PIC X(36) USAGE DISPLAY
           VALUE 'Failed To Register Exception Handler'.
       2 CONVERTER-ERROR-4.
       3 PIC X(38) USAGE DISPLAY
           VALUE 'Failed To Unregister Exception Handler'.
       2 CONVERTER-ERROR-7.
       3 PIC X(40) USAGE DISPLAY
           VALUE 'Language Environment Service Call Failed'.
       2 CONVERTER-ERROR-8.
       3 PIC X(35) USAGE DISPLAY
           VALUE 'Language Environment Message Number'.
       2 CONVERTER-ERROR-9.
       3 PIC X(31) USAGE DISPLAY
           VALUE 'XML Converter Is Terminating...'.
       1 XPATH-DELIM PIC X VALUE '/'.
       1 QNAME-DELIM PIC X VALUE ':'.
       LOCAL-STORAGE SECTION.
       1 NUMERIC-PICTURE-STORAGE.
       2 X000000AB PIC X(13).
       2 X000000AC PIC X(1).
       2 X000000AD PIC X(2).
       2 X000000AE PIC X(3).
       2 X000000AF PIC X(4).
       2 X000000B0 PIC X(5).
       2 X000000B1 PIC X(6).
       2 X000000B2 PIC X(7).
       2 X000000B3 PIC X(8).
       2 X000000B4 PIC X(9).
       2 X000000B5 PIC X(10).
       2 X000000B6 PIC X(11).
       2 X000000B7 PIC X(12).
       2 X000000B8 PIC X(13).
       1 NUMERIC-CHARACTER-STORAGE.
       2 X000000B9 PIC X(1).
       2 X000000BA PIC X(2).
       2 X000000BB PIC X(3).
       2 X000000BC PIC X(4).
       2 X000000BD PIC X(5).
       2 X000000BE PIC X(6).
       2 X000000BF PIC X(7).
       2 X000000C0 PIC X(8).
       2 X000000C1 PIC X(9).
       2 X000000C2 PIC X(10).
       2 X000000C3 PIC X(11).
       2 X000000C4 PIC X(12).
       2 X000000C5 PIC X(13).
       1 VSTRING.
       2 VSTRING-LENGTH PIC S9(4) COMP.
       2 VSTRING-DATA   PIC X(80).
       1 CEESRP-DATA.
       2 RECOVERY-POINT PIC S9(9) COMP.
       2 NUMVAL-ERROR PIC X.
       2 UNICODE-ERROR PIC X.
       2 OTHER-ERROR PIC X.
       2 SAVED-CONDITION PIC X(12).
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 ARRAY-SUBSCRIPTS.
       2 X00000012 PIC 9(9) COMP VALUE 0.
       1 HASH-TOKEN PIC X(40).
       1 REDEFINES HASH-TOKEN.
       2 OCCURS 10 TIMES INDEXED BY HASH-DIGIT-NDX.
       3 HASH-DIGIT PIC S9(9) COMP.
       1 HASH-VALUE PIC S9(9) COMP-5.
       1 LANG-STRUCT-HASH-VALUE PIC S9(9) COMP-5 VALUE 0.
       1 HASH-DISCARD PIC S9(9) COMP-5.
       1 INTEGER-PART PIC S9(20) COMP-3.
       1 FRACTION-PART PIC SV9(20) COMP-3.
       1 SKIP-ELEMENT PIC X DISPLAY VALUE 'Y'.
       1 ELE-NAME-LEN PIC 9(9) COMP VALUE 0.
       1 ELE-NAME PIC X(800).
       1 ELE-CON-LEN PIC 9(9) COMP VALUE 0.
       1 ELE-CON PIC X(128).
       1 ELE-CON-NDX PIC 9(9) COMP.
       1 ELE-CHAR-LIMIT PIC 9(9) COMP.
       1 CON-TXED PIC X DISPLAY.
       1 CMP-TMPA PIC S9(9) COMP.
       1 CMP-TMPB PIC S9(9) COMP.
       1 STOP-SEARCH PIC X DISPLAY.
       1 NPSAN PIC 9(9) COMP.
       1 ERROR-CODE PIC S9(9) COMP.
       1 ELEMENT-HITS PIC 9(9) COMP VALUE 0.
       1 SEV PIC S9(4) COMP.
       1 MSGNO PIC S9(4) COMP.
       1 CASE PIC S9(4) COMP.
       1 SEV2 PIC S9(4) COMP.
       1 CNTRL PIC S9(4) COMP.
       1 FACID PIC X(3) DISPLAY.
       1 ISINFO PIC S9(9) COMP.
       1 QDATA PIC S9(9) COMP.
       1 INSERTNO PIC S9(9) COMP.
       1 EEC PIC 9(9) DISPLAY.
       1 ROUTINE PROCEDURE-POINTER.
       1 TOKEN POINTER.
       1 XML2LS-LANG-BUFFER-POINTER POINTER.
       1 XML2LS-LANG-BUFFER-ADDRESS
           REDEFINES XML2LS-LANG-BUFFER-POINTER PIC 9(9) COMP.
       1 XML2LS-CONVERTED-LENGTH PIC 9(9) COMP VALUE 0.
       1 XPATH PIC X(400).
       1 XPOS PIC 9(9) COMP VALUE 1.
       1 XSTACK-DEPTH PIC 9(9) COMP VALUE 0.
       1 XML-ROOT-FOUND PIC X VALUE 'N'.
       1 LANG-STRUCT-NAME PIC X(30).
       1 LANG-STRUCT-NAME-LENGTH PIC 9(4) COMP.
       1 XML-NAT-CHAR PIC N USAGE NATIONAL.
       LINKAGE SECTION.
       1 X0000005E PIC .9.
       1 X00000060 PIC .9(2).
       1 X00000062 PIC .9(3).
       1 X0000005F PIC -.9.
       1 X00000061 PIC -.9(2).
       1 X00000063 PIC -.9(3).
       1 X00000017 PIC -9.
       1 X00000029 PIC -9.9.
       1 X0000002B PIC -9.9(2).
       1 X0000002D PIC -9.9(3).
       1 X00000019 PIC -9(2).
       1 X0000002F PIC -9(2).9.
       1 X00000031 PIC -9(2).9(2).
       1 X00000033 PIC -9(2).9(3).
       1 X0000001B PIC -9(3).
       1 X00000035 PIC -9(3).9.
       1 X00000037 PIC -9(3).9(2).
       1 X0000001D PIC -9(4).
       1 X0000003B PIC -9(4).9.
       1 X0000003D PIC -9(4).9(2).
       1 X0000001F PIC -9(5).
       1 X00000041 PIC -9(5).9.
       1 X00000043 PIC -9(5).9(2).
       1 X00000021 PIC -9(6).
       1 X00000047 PIC -9(6).9.
       1 X00000049 PIC -9(6).9(2).
       1 X00000023 PIC -9(7).
       1 X0000004D PIC -9(7).9.
       1 X0000004F PIC -9(7).9(2).
       1 X00000025 PIC -9(8).
       1 X00000053 PIC -9(8).9.
       1 X00000055 PIC -9(8).9(2).
       1 X00000027 PIC -9(9).
       1 X00000059 PIC -9(9).9.
       1 X0000005B PIC -9(9).9(2).
       1 X00000016 PIC 9.
       1 X00000028 PIC 9.9.
       1 X0000002A PIC 9.9(2).
       1 X0000002C PIC 9.9(3).
       1 X00000018 PIC 9(2).
       1 X0000002E PIC 9(2).9.
       1 X00000030 PIC 9(2).9(2).
       1 X00000032 PIC 9(2).9(3).
       1 X0000001A PIC 9(3).
       1 X00000034 PIC 9(3).9.
       1 X00000036 PIC 9(3).9(2).
       1 X0000001C PIC 9(4).
       1 X0000003A PIC 9(4).9.
       1 X0000003C PIC 9(4).9(2).
       1 X0000001E PIC 9(5).
       1 X00000040 PIC 9(5).9.
       1 X00000042 PIC 9(5).9(2).
       1 X00000020 PIC 9(6).
       1 X00000046 PIC 9(6).9.
       1 X00000048 PIC 9(6).9(2).
       1 X00000022 PIC 9(7).
       1 X0000004C PIC 9(7).9.
       1 X0000004E PIC 9(7).9(2).
       1 X00000024 PIC 9(8).
       1 X00000052 PIC 9(8).9.
       1 X00000054 PIC 9(8).9(2).
       1 X00000026 PIC 9(9).
       1 X00000058 PIC 9(9).9.
       1 X0000005A PIC 9(9).9(2).
       1 X00000013 COMP PIC S9(9)V9(2).
       1 X00000014 COMP PIC S9(4).
       1 X00000015 COMP PIC S9(2)V9(3).
       01 DFHCOMMAREA
           .
       10 PROCESS-INDICATOR
           PICTURE X
           USAGE DISPLAY
           .
       10 EPSPCOM-PRINCIPLE-DATA
           PICTURE S9(9)V9(2)
           USAGE COMP
           .
       10 EPSPCOM-NUMBER-OF-YEARS
           PICTURE S9(4)
           USAGE COMP
           .
       10 EPSPCOM-NUMBER-OF-MONTHS
           PICTURE S9(4)
           USAGE COMP
           .
       10 EPSPCOM-QUOTED-INTEREST-RATE
           PICTURE S9(2)V9(3)
           USAGE COMP
           .
       10 EPSPCOM-YEAR-MONTH-IND
           PICTURE X
           USAGE DISPLAY
           .
       10 EPSPCOM-RETURN-MONTH-PAYMENT
           PICTURE S9(7)V9(2)
           USAGE COMP
           .
       10 EPSPCOM-ERRMSG
           PICTURE X(80)
           USAGE DISPLAY
           .
       10 EPSPCOM-PROGRAM-RETCODE
           PICTURE 9(4)
           USAGE DISPLAY
           .
       88 EPS02-REQUEST-SUCCESS
           VALUE
           0
           .
       10 EPSPCOM-PROGRAM-RETCODE-RDF REDEFINES EPSPCOM-PROGRAM-RETCODE
           PICTURE X(4)
           USAGE DISPLAY
           .
       1 XML2LS-LANG-BUFFER PIC X(106).
       1 XML2LS-XML-BUFFER-LENGTH PIC 9(9) COMP.
       1 XML2LS-XML-BUFFER PIC X(33554436).
       1 OPTIONAL-FEEDBACK-CODE PIC X(12).
       1 CONVERTER-RETURN-CODE PIC 9(9) COMP.
       PROCEDURE DIVISION USING
           XML2LS-LANG-BUFFER
           XML2LS-XML-BUFFER-LENGTH
           XML2LS-XML-BUFFER
           OPTIONAL-FEEDBACK-CODE
           RETURNING
           CONVERTER-RETURN-CODE.
      * -------------------------------------------------------------
      *              Please Do Not Modify This Program
      * -------------------------------------------------------------
       MAINLINE SECTION.
           MOVE 'N'
             TO NUMVAL-ERROR UNICODE-ERROR OTHER-ERROR
           PERFORM CHECK-PARAMETERS
           PERFORM REGISTER-EXCEPTION-HANDLER
           PERFORM CHECK-INPUT-MESSAGE-LENGTH
           SET XML2LS-LANG-BUFFER-POINTER
            TO ADDRESS OF XML2LS-LANG-BUFFER
           XML PARSE XML2LS-XML-BUFFER (1:XML2LS-XML-BUFFER-LENGTH)
            PROCESSING PROCEDURE XML-HANDLER
            THRU GENERAL-LOGIC-EXIT
            ON EXCEPTION
             PERFORM UNREGISTER-EXCEPTION-HANDLER
             PERFORM SIGNAL-CONDITION
            NOT ON EXCEPTION
             PERFORM UNREGISTER-EXCEPTION-HANDLER
             MOVE ZERO TO CONVERTER-RETURN-CODE
           END-XML
           GOBACK
           .
       CHECK-PARAMETERS.
           IF ADDRESS OF XML2LS-LANG-BUFFER EQUAL NULL AND
              ADDRESS OF XML2LS-XML-BUFFER-LENGTH NOT EQUAL NULL
            CALL 'EPSCSMRJ' USING XML2LS-XML-BUFFER-LENGTH
            GOBACK
           END-IF
           IF ADDRESS OF XML2LS-LANG-BUFFER EQUAL NULL OR
              ADDRESS OF XML2LS-XML-BUFFER-LENGTH EQUAL NULL OR
              ADDRESS OF XML2LS-XML-BUFFER EQUAL NULL
            MOVE 294 TO MSGNO
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           .
       CHECK-INPUT-MESSAGE-LENGTH.
           IF XML2LS-XML-BUFFER-LENGTH > 33554436
            MOVE 285 TO MSGNO
            PERFORM UNREGISTER-EXCEPTION-HANDLER
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           .
       XML-HANDLER.
           EVALUATE XML-EVENT
           WHEN 'CONTENT-CHARACTERS'
            IF SKIP-ELEMENT = 'N'
             COMPUTE CMP-TMPB = FUNCTION LENGTH (XML-TEXT)
             COMPUTE CMP-TMPA = ELE-CON-LEN + CMP-TMPB
             IF CMP-TMPA <= 128
              MOVE XML-TEXT
               TO ELE-CON (ELE-CON-NDX:CMP-TMPB)
              ADD CMP-TMPB TO ELE-CON-LEN ELE-CON-NDX
             ELSE
              MOVE CMP-TMPA TO ELE-CON-LEN
              GO TO CHARACTER-BUFFER-OVERFLOW
             END-IF
            END-IF
           WHEN 'START-OF-ELEMENT'
            PERFORM PUSH-ELEMENT
            IF XSTACK-DEPTH > 0 AND
               (XPOS - 2) <= 40
             MOVE ALL X'00' TO HASH-TOKEN
             MOVE XPATH(2:XPOS - 2) TO HASH-TOKEN
             (40 - (XPOS - 3):XPOS - 2)
             MOVE 0 TO HASH-VALUE
             SET HASH-DIGIT-NDX TO 1
             PERFORM 10 TIMES
              ADD HASH-DIGIT (HASH-DIGIT-NDX) TO
              HASH-VALUE
              SET HASH-DIGIT-NDX UP BY 1
             END-PERFORM
             DIVIDE HASH-VALUE BY 7 GIVING HASH-DISCARD
              REMAINDER HASH-VALUE
             END-DIVIDE
             IF HASH-VALUE LESS THAN ZERO
              ADD 7 TO HASH-VALUE
             ELSE
              ADD 1 TO HASH-VALUE
             END-IF
             IF ELEMENT-NAME (HASH-VALUE)
                = XPATH(2:XPOS - 2)
              MOVE 'N' TO SKIP-ELEMENT
              ADD 1 TO ELEMENT-HITS
              MOVE 'N' TO CON-TXED
              MOVE ZERO TO ELE-CON-LEN
              MOVE 1 TO ELE-CON-NDX
              IF CONTENT-TYPE (HASH-VALUE) = 'R'
                 OR CONTENT-TYPE (HASH-VALUE) = 'T'
               GO TO ROUTE-ELEMENT
              END-IF
             ELSE
              MOVE 'Y' TO SKIP-ELEMENT
             END-IF
            ELSE
             MOVE 'Y' TO SKIP-ELEMENT
            END-IF
           WHEN 'END-OF-ELEMENT'
            IF XSTACK-DEPTH > 0
             IF LANG-STRUCT-HASH-VALUE > 0 AND
                ELEMENT-NAME(LANG-STRUCT-HASH-VALUE)
                = XPATH(2:XPOS - 2)
              EVALUATE LANG-STRUCT-ID(LANG-STRUCT-HASH-VALUE)
               WHEN 1
                COMPUTE CMP-TMPA =
                 LENGTH OF DFHCOMMAREA
                END-COMPUTE
                ADD CMP-TMPA
                 TO XML2LS-LANG-BUFFER-ADDRESS
                    XML2LS-CONVERTED-LENGTH
              END-EVALUATE
              INITIALIZE LANG-STRUCT-HASH-VALUE
              PERFORM POP-ELEMENT
              GO TO GENERAL-LOGIC-EXIT
             ELSE
              IF SKIP-ELEMENT = 'N' AND CON-TXED = 'N' AND
                 ELEMENT-NAME(HASH-VALUE)
                 = XPATH(2:XPOS - 2)
               PERFORM POP-ELEMENT
               GO TO ROUTE-ELEMENT
              END-IF
             END-IF
             PERFORM POP-ELEMENT
            END-IF
           WHEN 'CONTENT-CHARACTER'
            IF SKIP-ELEMENT = 'N'
             COMPUTE CMP-TMPB = FUNCTION LENGTH (XML-TEXT)
             COMPUTE CMP-TMPA = ELE-CON-LEN + CMP-TMPB
             IF CMP-TMPA <= 128
              MOVE XML-TEXT
               TO ELE-CON (ELE-CON-NDX:CMP-TMPB)
              ADD CMP-TMPB TO ELE-CON-LEN ELE-CON-NDX
             ELSE
              MOVE CMP-TMPA TO ELE-CON-LEN
              GO TO CHARACTER-BUFFER-OVERFLOW
             END-IF
            END-IF
           WHEN 'CONTENT-NATIONAL-CHARACTER'
            IF SKIP-ELEMENT = 'N'
             COMPUTE CMP-TMPB = FUNCTION LENGTH (XML-NTEXT)
             IF CMP-TMPB > 1
              MOVE SPACE TO XML-NAT-CHAR
             ELSE
              MOVE XML-NTEXT TO XML-NAT-CHAR
             END-IF
             COMPUTE CMP-TMPA = ELE-CON-LEN + 1
             IF CMP-TMPA <= 128
              COMPUTE CMP-TMPB = FUNCTION LENGTH (
               FUNCTION DISPLAY-OF (XML-NAT-CHAR))
              IF CMP-TMPB > 1
               MOVE SPACE
                TO ELE-CON (ELE-CON-NDX:1)
              ELSE
               MOVE FUNCTION DISPLAY-OF (XML-NAT-CHAR)
                TO ELE-CON (ELE-CON-NDX:1)
              END-IF
              ADD 1 TO ELE-CON-LEN ELE-CON-NDX
             ELSE
              MOVE CMP-TMPA TO ELE-CON-LEN
              GO TO CHARACTER-BUFFER-OVERFLOW
             END-IF
            END-IF
           WHEN 'START-OF-DOCUMENT'
            CALL 'CEE3SRP' USING RECOVERY-POINT FEEDBACK-CODE
            SERVICE LABEL
            IF NUMVAL-ERROR = 'Y'
             MOVE 284 TO MSGNO
             MOVE -1 TO XML-CODE
            END-IF
            IF UNICODE-ERROR = 'Y'
             MOVE 288 TO MSGNO
             MOVE -1 TO XML-CODE
            END-IF
            IF OTHER-ERROR = 'Y'
             MOVE -1 TO XML-CODE
            END-IF
           WHEN 'END-OF-DOCUMENT'
            IF ELEMENT-HITS = 0
             MOVE 282 TO MSGNO
             MOVE -1 TO XML-CODE
             GO TO GENERAL-LOGIC-EXIT
            END-IF
            IF X00000012 < 1
             MOVE 299 TO MSGNO
             MOVE -1 TO XML-CODE
             GO TO GENERAL-LOGIC-EXIT
            END-IF
           WHEN 'EXCEPTION'
            IF XML-CODE >= 50 AND XML-CODE <= 99
             MOVE 0 TO XML-CODE
            ELSE
             MOVE 280 TO MSGNO
             MOVE -1 TO XML-CODE
            END-IF
           END-EVALUATE
           GO TO GENERAL-LOGIC-EXIT
           .
       PUSH-ELEMENT.
           PERFORM PARSE-ELEMENT
           IF XML-ROOT-FOUND = 'N' AND
            ELE-NAME (1:ELE-NAME-LEN)
             = XML-ROOT-ELEMENT
            MOVE 'Y' TO XML-ROOT-FOUND
           END-IF
           IF XML-ROOT-FOUND = 'Y'
            COMPUTE CMP-TMPA = XPOS + (ELE-NAME-LEN + 1)
            IF CMP-TMPA <= 400
             MOVE XPATH-DELIM TO XPATH (XPOS:1)
             MOVE ELE-NAME (1:ELE-NAME-LEN)
              TO XPATH (XPOS + 1:ELE-NAME-LEN)
             COMPUTE XPOS = XPOS + (ELE-NAME-LEN + 1)
            ELSE
             COMPUTE ELE-NAME-LEN = FUNCTION LENGTH (XML-TEXT)
             MOVE XML-TEXT TO ELE-NAME
             MOVE 291 TO MSGNO
             MOVE -1 TO XML-CODE
            END-IF
            ADD 1 TO XSTACK-DEPTH
           END-IF
           .
       POP-ELEMENT.
           IF XSTACK-DEPTH > 0
            PERFORM PARSE-ELEMENT
            COMPUTE XPOS = XPOS - (ELE-NAME-LEN + 1)
            SUBTRACT 1 FROM XSTACK-DEPTH
           END-IF
           .
       PARSE-ELEMENT.
           COMPUTE ELE-NAME-LEN = FUNCTION LENGTH (XML-TEXT)
           MOVE 1 TO CMP-TMPB
           PERFORM VARYING CMP-TMPA FROM 1 BY 1
            UNTIL CMP-TMPA > ELE-NAME-LEN
            IF XML-TEXT (CMP-TMPA:1) = QNAME-DELIM
             COMPUTE CMP-TMPB = CMP-TMPA + 1
             MOVE ELE-NAME-LEN TO CMP-TMPA
            END-IF
           END-PERFORM
           IF CMP-TMPB > 1
            COMPUTE ELE-NAME-LEN
             = (ELE-NAME-LEN - CMP-TMPB) + 1
            IF ELE-NAME-LEN <= 28
             MOVE XML-TEXT (CMP-TMPB:ELE-NAME-LEN)
              TO ELE-NAME (1:ELE-NAME-LEN)
            ELSE
             MOVE 0 TO ELE-NAME-LEN
            END-IF
           ELSE
            IF ELE-NAME-LEN <= 28
             MOVE XML-TEXT (1:ELE-NAME-LEN)
              TO ELE-NAME (1:ELE-NAME-LEN)
            ELSE
             MOVE 0 TO ELE-NAME-LEN
            END-IF
           END-IF
           IF ELE-NAME-LEN = 0
            MOVE 1 TO ELE-NAME-LEN
            MOVE '0' TO ELE-NAME (1:ELE-NAME-LEN)
           END-IF
           .
       ROUTE-ELEMENT.
           GO TO
            X000000C6
            X000000C7
            X000000C8
            X000000C9
            X000000CA
            X000000CB
           DEPENDING ON ROUTING-CODE (HASH-VALUE)
           GO TO GENERAL-LOGIC-EXIT
           .
       CONTENT-PROCESSING SECTION.
       X000000C6.
           ADD 1 TO X00000012
           IF X00000012 > 1
            MOVE 300 TO MSGNO
            MOVE -1 TO XML-CODE
            GO TO GENERAL-LOGIC-EXIT
           END-IF
           SET ADDRESS OF DFHCOMMAREA
            TO XML2LS-LANG-BUFFER-POINTER
           MOVE 'DFHCOMMAREA'
             TO LANG-STRUCT-NAME
           MOVE 11
             TO LANG-STRUCT-NAME-LENGTH
           MOVE HASH-VALUE
             TO LANG-STRUCT-HASH-VALUE
           GO TO CONTENT-CONVERTED-EXIT
           .
       X000000C7.
           IF ELE-CON-LEN = 0
            MOVE ZEROS TO
             EPSPCOM-PRINCIPLE-DATA
             OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           SET ADDRESS OF X00000013 TO ADDRESS OF
            EPSPCOM-PRINCIPLE-DATA
             OF DFHCOMMAREA
           GO TO
            X000000CC
            X000000CD
            X000000CE
            X000000CF
            X000000D0
            X000000D1
            X000000D2
            X000000D3
            X000000D4
            X000000D5
            X000000D6
            X000000D7
            X000000D8
           DEPENDING ON ELE-CON-LEN
           GO TO NUMERIC-RESCUE
           .
       X000000C8.
           IF ELE-CON-LEN = 0
            MOVE ZEROS TO
             EPSPCOM-NUMBER-OF-YEARS
             OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           SET ADDRESS OF X00000014 TO ADDRESS OF
            EPSPCOM-NUMBER-OF-YEARS
             OF DFHCOMMAREA
           GO TO
            X000000CC
            X000000CD
            X000000CE
            X000000CF
            X000000D0
           DEPENDING ON ELE-CON-LEN
           GO TO NUMERIC-RESCUE
           .
       X000000C9.
           IF ELE-CON-LEN = 0
            MOVE ZEROS TO
             EPSPCOM-NUMBER-OF-MONTHS
             OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           SET ADDRESS OF X00000014 TO ADDRESS OF
            EPSPCOM-NUMBER-OF-MONTHS
             OF DFHCOMMAREA
           GO TO
            X000000CC
            X000000CD
            X000000CE
            X000000CF
            X000000D0
           DEPENDING ON ELE-CON-LEN
           GO TO NUMERIC-RESCUE
           .
       X000000CA.
           IF ELE-CON-LEN = 0
            MOVE ZEROS TO
             EPSPCOM-QUOTED-INTEREST-RATE
             OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           SET ADDRESS OF X00000015 TO ADDRESS OF
            EPSPCOM-QUOTED-INTEREST-RATE
             OF DFHCOMMAREA
           GO TO
            X000000CC
            X000000CD
            X000000CE
            X000000CF
            X000000D0
            X000000D1
            X000000D2
           DEPENDING ON ELE-CON-LEN
           GO TO NUMERIC-RESCUE
           .
       X000000CB.
           IF ELE-CON-LEN = 0
            MOVE SPACES TO
             EPSPCOM-YEAR-MONTH-IND
             OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           IF ELE-CON-LEN <= 1
            MOVE ELE-CON (1:ELE-CON-LEN) TO
             EPSPCOM-YEAR-MONTH-IND
              OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           MOVE 1 TO ELE-CHAR-LIMIT
           GO TO CHARACTER-CONTENT-OVERFLOW
           .
       CHARACTER-CONTENT-OVERFLOW.
           MOVE 283 TO MSGNO
           MOVE -1 TO XML-CODE
           GO TO GENERAL-LOGIC-EXIT
           .
       CHARACTER-BUFFER-OVERFLOW.
           MOVE 286 TO MSGNO
           MOVE -1 TO XML-CODE
           GO TO GENERAL-LOGIC-EXIT
           .
       COMPUTE-LOOKUP-SOURCE SECTION.
       X000000CC.
           MOVE ELE-CON (1:1) TO X000000B9 X000000AC
           INSPECT X000000AC REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000AC TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000CD.
           MOVE ELE-CON (1:2) TO X000000BA X000000AD
           INSPECT X000000AD REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000AD TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000CE.
           MOVE ELE-CON (1:3) TO X000000BB X000000AE
           INSPECT X000000AE REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000AE TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000CF.
           MOVE ELE-CON (1:4) TO X000000BC X000000AF
           INSPECT X000000AF REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000AF TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000D0.
           MOVE ELE-CON (1:5) TO X000000BD X000000B0
           INSPECT X000000B0 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000B0 TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000D1.
           MOVE ELE-CON (1:6) TO X000000BE X000000B1
           INSPECT X000000B1 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000B1 TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000D2.
           MOVE ELE-CON (1:7) TO X000000BF X000000B2
           INSPECT X000000B2 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000B2 TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000D3.
           MOVE ELE-CON (1:8) TO X000000C0 X000000B3
           INSPECT X000000B3 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000B3 TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000D4.
           MOVE ELE-CON (1:9) TO X000000C1 X000000B4
           INSPECT X000000B4 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000B4 TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000D5.
           MOVE ELE-CON (1:10) TO X000000C2 X000000B5
           INSPECT X000000B5 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000B5 TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000D6.
           MOVE ELE-CON (1:11) TO X000000C3 X000000B6
           INSPECT X000000B6 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000B6 TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000D7.
           MOVE ELE-CON (1:12) TO X000000C4 X000000B7
           INSPECT X000000B7 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000B7 TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       X000000D8.
           MOVE ELE-CON (1:13) TO X000000C5 X000000B8
           INSPECT X000000B8 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X000000B8 TO X000000AB
           GO TO LOOKUP-SOURCE
           .
       LOOKUP-SOURCE.
           SEARCH ALL NPSA
            AT END
             GO TO NUMERIC-RESCUE
            WHEN NPSA (NPSA-NDX) = X000000AB
             SET NPSAN TO NPSA-NDX
           END-SEARCH
           GO TO
            X000000D9
            X000000DA
            X000000DB
            X000000DC
            X000000DD
            X000000DE
            X000000DF
            X000000E0
            X000000E1
            X000000E2
            X000000E3
            X000000E4
            X000000E5
            X000000E6
            X000000E7
            X000000E8
            X000000E9
            X000000EA
            X000000EB
            X000000EC
            X000000ED
            X000000EE
            X000000EF
            X000000F0
            X000000F1
            X000000F2
            X000000F3
            X000000F4
            X000000F5
            X000000F6
            X000000F7
            X000000F8
            X000000F9
            X000000FA
            X000000FB
            X000000FC
            X000000FD
            X000000FE
            X000000FF
            X00000100
            X00000101
            X00000102
            X00000103
            X00000104
            X00000105
            X00000106
            X00000107
            X00000108
            X00000109
            X0000010A
            X0000010B
            X0000010C
            X0000010D
            X0000010E
            X0000010F
            X00000110
            X00000111
            X00000112
            X00000113
            X00000114
            X00000115
            X00000116
            X00000117
            X00000118
           DEPENDING ON NPSAN
           GO TO NUMERIC-RESCUE
           .
       SET-NUMERIC-SOURCE SECTION.
       X000000D9.
           SET ADDRESS OF X0000005E
            TO ADDRESS OF X000000BA
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000DA.
           SET ADDRESS OF X00000060
            TO ADDRESS OF X000000BB
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000DB.
           SET ADDRESS OF X00000062
            TO ADDRESS OF X000000BC
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000DC.
           SET ADDRESS OF X0000005F
            TO ADDRESS OF X000000BB
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000DD.
           SET ADDRESS OF X00000061
            TO ADDRESS OF X000000BC
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000DE.
           SET ADDRESS OF X00000063
            TO ADDRESS OF X000000BD
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000DF.
           SET ADDRESS OF X00000017
            TO ADDRESS OF X000000BA
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E0.
           SET ADDRESS OF X00000029
            TO ADDRESS OF X000000BC
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E1.
           SET ADDRESS OF X0000002B
            TO ADDRESS OF X000000BD
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E2.
           SET ADDRESS OF X0000002D
            TO ADDRESS OF X000000BE
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E3.
           SET ADDRESS OF X00000019
            TO ADDRESS OF X000000BB
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E4.
           SET ADDRESS OF X0000002F
            TO ADDRESS OF X000000BD
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E5.
           SET ADDRESS OF X00000031
            TO ADDRESS OF X000000BE
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E6.
           SET ADDRESS OF X00000033
            TO ADDRESS OF X000000BF
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E7.
           SET ADDRESS OF X0000001B
            TO ADDRESS OF X000000BC
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E8.
           SET ADDRESS OF X00000035
            TO ADDRESS OF X000000BE
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000E9.
           SET ADDRESS OF X00000037
            TO ADDRESS OF X000000BF
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000EA.
           SET ADDRESS OF X0000001D
            TO ADDRESS OF X000000BD
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000EB.
           SET ADDRESS OF X0000003B
            TO ADDRESS OF X000000BF
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000EC.
           SET ADDRESS OF X0000003D
            TO ADDRESS OF X000000C0
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000ED.
           SET ADDRESS OF X0000001F
            TO ADDRESS OF X000000BE
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000EE.
           SET ADDRESS OF X00000041
            TO ADDRESS OF X000000C0
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000EF.
           SET ADDRESS OF X00000043
            TO ADDRESS OF X000000C1
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F0.
           SET ADDRESS OF X00000021
            TO ADDRESS OF X000000BF
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F1.
           SET ADDRESS OF X00000047
            TO ADDRESS OF X000000C1
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F2.
           SET ADDRESS OF X00000049
            TO ADDRESS OF X000000C2
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F3.
           SET ADDRESS OF X00000023
            TO ADDRESS OF X000000C0
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F4.
           SET ADDRESS OF X0000004D
            TO ADDRESS OF X000000C2
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F5.
           SET ADDRESS OF X0000004F
            TO ADDRESS OF X000000C3
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F6.
           SET ADDRESS OF X00000025
            TO ADDRESS OF X000000C1
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F7.
           SET ADDRESS OF X00000053
            TO ADDRESS OF X000000C3
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F8.
           SET ADDRESS OF X00000055
            TO ADDRESS OF X000000C4
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000F9.
           SET ADDRESS OF X00000027
            TO ADDRESS OF X000000C2
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000FA.
           SET ADDRESS OF X00000059
            TO ADDRESS OF X000000C4
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000FB.
           SET ADDRESS OF X0000005B
            TO ADDRESS OF X000000C5
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000FC.
           SET ADDRESS OF X00000016
            TO ADDRESS OF X000000B9
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000FD.
           SET ADDRESS OF X00000028
            TO ADDRESS OF X000000BB
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000FE.
           SET ADDRESS OF X0000002A
            TO ADDRESS OF X000000BC
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X000000FF.
           SET ADDRESS OF X0000002C
            TO ADDRESS OF X000000BD
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000100.
           SET ADDRESS OF X00000018
            TO ADDRESS OF X000000BA
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000101.
           SET ADDRESS OF X0000002E
            TO ADDRESS OF X000000BC
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000102.
           SET ADDRESS OF X00000030
            TO ADDRESS OF X000000BD
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000103.
           SET ADDRESS OF X00000032
            TO ADDRESS OF X000000BE
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000104.
           SET ADDRESS OF X0000001A
            TO ADDRESS OF X000000BB
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000105.
           SET ADDRESS OF X00000034
            TO ADDRESS OF X000000BD
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000106.
           SET ADDRESS OF X00000036
            TO ADDRESS OF X000000BE
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000107.
           SET ADDRESS OF X0000001C
            TO ADDRESS OF X000000BC
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000108.
           SET ADDRESS OF X0000003A
            TO ADDRESS OF X000000BE
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000109.
           SET ADDRESS OF X0000003C
            TO ADDRESS OF X000000BF
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X0000010A.
           SET ADDRESS OF X0000001E
            TO ADDRESS OF X000000BD
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X0000010B.
           SET ADDRESS OF X00000040
            TO ADDRESS OF X000000BF
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X0000010C.
           SET ADDRESS OF X00000042
            TO ADDRESS OF X000000C0
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X0000010D.
           SET ADDRESS OF X00000020
            TO ADDRESS OF X000000BE
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X0000010E.
           SET ADDRESS OF X00000046
            TO ADDRESS OF X000000C0
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X0000010F.
           SET ADDRESS OF X00000048
            TO ADDRESS OF X000000C1
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000110.
           SET ADDRESS OF X00000022
            TO ADDRESS OF X000000BF
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000111.
           SET ADDRESS OF X0000004C
            TO ADDRESS OF X000000C1
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000112.
           SET ADDRESS OF X0000004E
            TO ADDRESS OF X000000C2
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000113.
           SET ADDRESS OF X00000024
            TO ADDRESS OF X000000C0
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000114.
           SET ADDRESS OF X00000052
            TO ADDRESS OF X000000C2
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000115.
           SET ADDRESS OF X00000054
            TO ADDRESS OF X000000C3
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000116.
           SET ADDRESS OF X00000026
            TO ADDRESS OF X000000C1
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000117.
           SET ADDRESS OF X00000058
            TO ADDRESS OF X000000C3
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000118.
           SET ADDRESS OF X0000005A
            TO ADDRESS OF X000000C4
           GO TO COMPLETE-NUMERIC-MOVE
           .
       MOVE-NUMERIC SECTION.
       COMPLETE-NUMERIC-MOVE.
           MOVE NUMERIC-TARGET-CODE (HASH-VALUE) TO CMP-TMPA
           GO TO
            X00000119
            X0000011A
            X0000011B
            X0000011C
            X0000011D
            X0000011E
            X0000011F
            X00000120
            X00000121
            X00000122
            X00000123
            X00000124
            X00000125
            X00000126
            X00000127
            X00000128
            X00000129
            X0000012A
            X0000012B
            X0000012C
            X0000012D
            X0000012E
            X0000012F
            X00000130
            X00000131
            X00000132
            X00000133
            X00000134
            X00000135
            X00000136
            X00000137
            X00000138
            X00000139
            X0000013A
            X0000013B
            X0000013C
            X0000013D
            X0000013E
            X0000013F
            X00000140
            X00000141
            X00000142
            X00000143
            X00000144
            X00000145
            X00000146
            X00000147
            X00000148
            X00000149
            X0000014A
            X0000014B
            X0000014C
            X0000014D
            X0000014E
            X0000014F
            X00000150
            X00000151
            X00000152
            X00000153
            X00000154
            X00000155
            X00000156
            X00000157
            X00000158
            X00000159
            X0000015A
            X0000015B
            X0000015C
            X0000015D
            X0000015E
            X0000015F
            X00000160
            X00000161
            X00000162
            X00000163
            X00000164
            X00000165
            X00000166
            X00000167
            X00000168
            X00000169
            X0000016A
            X0000016B
            X0000016C
            X0000016D
            X0000016E
            X0000016F
            X00000170
           DEPENDING ON NMAR-ENTRY (NPSAN, CMP-TMPA)
           GO TO NUMERIC-RESCUE
           .
       X00000119.
           MOVE X0000005E TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000011A.
           MOVE X0000005E TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000011B.
           MOVE X00000060 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000011C.
           MOVE X00000060 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000011D.
           MOVE X00000062 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000011E.
           MOVE X0000005F TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000011F.
           MOVE X0000005F TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000120.
           MOVE X00000061 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000121.
           MOVE X00000061 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000122.
           MOVE X00000063 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000123.
           MOVE X00000017 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000124.
           MOVE X00000017 TO X00000014
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000125.
           MOVE X00000017 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000126.
           MOVE X00000029 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000127.
           MOVE X00000029 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000128.
           MOVE X0000002B TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000129.
           MOVE X0000002B TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000012A.
           MOVE X0000002D TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000012B.
           MOVE X00000019 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000012C.
           MOVE X00000019 TO X00000014
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000012D.
           MOVE X00000019 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000012E.
           MOVE X0000002F TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000012F.
           MOVE X0000002F TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000130.
           MOVE X00000031 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000131.
           MOVE X00000031 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000132.
           MOVE X00000033 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000133.
           MOVE X0000001B TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000134.
           MOVE X0000001B TO X00000014
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000135.
           MOVE X00000035 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000136.
           MOVE X00000037 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000137.
           MOVE X0000001D TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000138.
           MOVE X0000001D TO X00000014
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000139.
           MOVE X0000003B TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000013A.
           MOVE X0000003D TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000013B.
           MOVE X0000001F TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000013C.
           MOVE X00000041 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000013D.
           MOVE X00000043 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000013E.
           MOVE X00000021 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000013F.
           MOVE X00000047 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000140.
           MOVE X00000049 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000141.
           MOVE X00000023 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000142.
           MOVE X0000004D TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000143.
           MOVE X0000004F TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000144.
           MOVE X00000025 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000145.
           MOVE X00000053 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000146.
           MOVE X00000055 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000147.
           MOVE X00000027 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000148.
           MOVE X00000059 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000149.
           MOVE X0000005B TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000014A.
           MOVE X00000016 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000014B.
           MOVE X00000016 TO X00000014
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000014C.
           MOVE X00000016 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000014D.
           MOVE X00000028 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000014E.
           MOVE X00000028 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000014F.
           MOVE X0000002A TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000150.
           MOVE X0000002A TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000151.
           MOVE X0000002C TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000152.
           MOVE X00000018 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000153.
           MOVE X00000018 TO X00000014
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000154.
           MOVE X00000018 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000155.
           MOVE X0000002E TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000156.
           MOVE X0000002E TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000157.
           MOVE X00000030 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000158.
           MOVE X00000030 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000159.
           MOVE X00000032 TO X00000015
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000015A.
           MOVE X0000001A TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000015B.
           MOVE X0000001A TO X00000014
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000015C.
           MOVE X00000034 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000015D.
           MOVE X00000036 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000015E.
           MOVE X0000001C TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000015F.
           MOVE X0000001C TO X00000014
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000160.
           MOVE X0000003A TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000161.
           MOVE X0000003C TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000162.
           MOVE X0000001E TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000163.
           MOVE X00000040 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000164.
           MOVE X00000042 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000165.
           MOVE X00000020 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000166.
           MOVE X00000046 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000167.
           MOVE X00000048 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000168.
           MOVE X00000022 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000169.
           MOVE X0000004C TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000016A.
           MOVE X0000004E TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000016B.
           MOVE X00000024 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000016C.
           MOVE X00000052 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000016D.
           MOVE X00000054 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000016E.
           MOVE X00000026 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X0000016F.
           MOVE X00000058 TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000170.
           MOVE X0000005A TO X00000013
           GO TO CONTENT-CONVERTED-EXIT
           .
       NUMERIC-RESCUE-PROCESSING SECTION.
       NUMERIC-RESCUE.
           GO TO
            X00000171
            X00000172
            X00000173
            X00000174
           DEPENDING ON NUMERIC-RESCUE-CODE (HASH-VALUE)
           .
       X00000171.
           COMPUTE EPSPCOM-PRINCIPLE-DATA
             OF DFHCOMMAREA
             = FUNCTION NUMVAL-C(ELE-CON (1:ELE-CON-LEN))
            ON SIZE ERROR
             GO TO NUMERIC-RESCUE-FAILED
            NOT ON SIZE ERROR
             GO TO CONTENT-CONVERTED-EXIT
           END-COMPUTE
           .
       X00000172.
           COMPUTE EPSPCOM-NUMBER-OF-YEARS
             OF DFHCOMMAREA
             = FUNCTION NUMVAL-C(ELE-CON (1:ELE-CON-LEN))
            ON SIZE ERROR
             GO TO NUMERIC-RESCUE-FAILED
            NOT ON SIZE ERROR
             GO TO CONTENT-CONVERTED-EXIT
           END-COMPUTE
           .
       X00000173.
           COMPUTE EPSPCOM-NUMBER-OF-MONTHS
             OF DFHCOMMAREA
             = FUNCTION NUMVAL-C(ELE-CON (1:ELE-CON-LEN))
            ON SIZE ERROR
             GO TO NUMERIC-RESCUE-FAILED
            NOT ON SIZE ERROR
             GO TO CONTENT-CONVERTED-EXIT
           END-COMPUTE
           .
       X00000174.
           COMPUTE EPSPCOM-QUOTED-INTEREST-RATE
             OF DFHCOMMAREA
             = FUNCTION NUMVAL-C(ELE-CON (1:ELE-CON-LEN))
            ON SIZE ERROR
             GO TO NUMERIC-RESCUE-FAILED
            NOT ON SIZE ERROR
             GO TO CONTENT-CONVERTED-EXIT
           END-COMPUTE
           .
       NUMERIC-RESCUE-FAILED.
           MOVE 284 TO MSGNO
           MOVE -1 TO XML-CODE
           GO TO GENERAL-LOGIC-EXIT
           .
       CONTENT-CONVERTED-EXIT.
           MOVE 'Y' TO CON-TXED
           .
       GENERAL-LOGIC-EXIT.
           CONTINUE
           .
       CONDITION-SIGNALER SECTION.
       SIGNAL-CONDITION.
           IF OTHER-ERROR = 'N'
            MOVE 3 TO SEV SEV2
            MOVE 1 TO CASE CNTRL
            MOVE 0 TO ISINFO
            MOVE 0 TO INSERTNO
            MOVE 'IGZ' TO FACID
            CALL 'CEENCOD' USING
              SEV MSGNO CASE SEV2
              CNTRL FACID ISINFO
              NEW-CONDITION FEEDBACK-CODE
            PERFORM CHECK-LE-SERVICE-FC
            MOVE 8 TO VSTRING-LENGTH
            MOVE 'EPSCSMRI' TO VSTRING-DATA (1:8)
            PERFORM INSERT-VSTRING
            EVALUATE MSGNO
             WHEN 280
      *IGZ_PARSER_ERROR_MSG_NO
              PERFORM CEECMI-IGZ0280S
             WHEN 281
      *IGZ_SUBSCRIPT_RANGE_EXCEEDED_ERROR_MSG_NO
              PERFORM CEECMI-IGZ0281S
             WHEN 282
      *IGZ_NO_KNOWN_ELEMENTS_FOUND_ERROR_MSG_NO
              PERFORM CEECMI-IGZ0282S
             WHEN 283
      *IGZ_CHARACTER_CONTENT_OVERFLOW_ERROR_MSG_NO
              PERFORM CEECMI-IGZ0283S
             WHEN 284
      *IGZ_NUMERIC_TX_FAILURE_ERROR_MSG_NO
              PERFORM CEECMI-IGZ0284S
             WHEN 285
      *IGZ_MESSAGE_TOO_LARGE_ERROR_MSG_NO
              PERFORM CEECMI-IGZ0285S
             WHEN 286
      *IGZ_CHARACTER_CONTENT_BUFFER_OVERFLOW_ERROR_MSG_NO
              PERFORM CEECMI-IGZ0286S
             WHEN 288
      *IGZ_UNICODE_RUNTIME_ERROR
              PERFORM CEECMI-IGZ0288S
             WHEN 291
      *IGZ_XPATH_OVERFLOW_ERROR_MSG_NO
              PERFORM CEECMI-IGZ0291S
             WHEN 294
      *IGZ_INVALID_PARAMETERS_MSG_NO
              PERFORM CEECMI-IGZ0294S
             WHEN 299
      *IGZ_XML2LS_LANGUAGE_STRUCTURE_MIN_COUNT_NOT_MET
              PERFORM CEECMI-IGZ0299S
             WHEN 300
      *IGZ_XML2LS_LANGUAGE_STRUCTURE_MAX_COUNT_EXCEEDED
              PERFORM CEECMI-IGZ0300S
            END-EVALUATE
           ELSE
            MOVE SAVED-CONDITION TO NEW-CONDITION
            MOVE MSG-NO OF NEW-CONDITION TO ERROR-CODE
           END-IF
           MOVE 0 TO QDATA
           IF ADDRESS OF OPTIONAL-FEEDBACK-CODE = NULL
            CALL 'CEESGL' USING NEW-CONDITION QDATA OMITTED
           ELSE
            MOVE NEW-CONDITION TO OPTIONAL-FEEDBACK-CODE
           END-IF
           IF MSGNO NOT EQUAL 294
            MOVE ERROR-CODE TO CONVERTER-RETURN-CODE
           END-IF
           .
       CEECMI-IGZ0280S.
           MOVE XML-CODE TO ERROR-CODE
           MOVE ERROR-CODE TO EEC
           PERFORM INSERT-NUMBER
           PERFORM INSERT-ELE-NAME
           PERFORM INSERT-ELE-CON
           .
       CEECMI-IGZ0281S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-ELE-NAME
           .
       CEECMI-IGZ0282S.
           MOVE MSGNO TO ERROR-CODE
           .
       CEECMI-IGZ0283S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-ELE-NAME
           MOVE ELE-CHAR-LIMIT TO EEC
           PERFORM INSERT-NUMBER
           .
       CEECMI-IGZ0284S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-ELE-NAME
           PERFORM INSERT-ELE-CON
           .
       CEECMI-IGZ0285S.
           MOVE MSGNO TO ERROR-CODE
           MOVE XML2LS-XML-BUFFER-LENGTH TO EEC
           PERFORM INSERT-NUMBER
           MOVE 33554436 TO EEC
           PERFORM INSERT-NUMBER
           .
       CEECMI-IGZ0286S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-ELE-NAME
           MOVE ELE-CON-LEN TO EEC
           PERFORM INSERT-NUMBER
           .
       CEECMI-IGZ0288S.
           MOVE MSGNO TO ERROR-CODE
           MOVE 1200 TO EEC
           PERFORM INSERT-NUMBER
           MOVE 1140 TO EEC
           PERFORM INSERT-NUMBER
           .
       CEECMI-IGZ0291S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-ELE-NAME
           PERFORM INSERT-ELE-CON
           .
       CEECMI-IGZ0294S.
           MOVE MSGNO TO ERROR-CODE
           .
       CEECMI-IGZ0299S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-STRUCT-NAME
           .
       CEECMI-IGZ0300S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-STRUCT-NAME
           .
       INSERT-ELE-CON.
           IF ELE-CON-LEN > 80
            MOVE 80 TO ELE-CON-LEN
            MOVE '...' TO ELE-CON (78:3)
           END-IF
           IF ELE-CON-LEN <= 0
            MOVE 1 TO ELE-CON-LEN
            MOVE '?' TO ELE-CON
           END-IF
           MOVE ELE-CON-LEN TO VSTRING-LENGTH
           MOVE ELE-CON
             TO VSTRING-DATA (1:80)
           PERFORM INSERT-VSTRING
           .
       INSERT-ELE-NAME.
           IF ELE-NAME-LEN > 80
            MOVE 80 TO ELE-NAME-LEN
            MOVE '...' TO ELE-NAME (78:3)
           END-IF
           IF ELE-NAME-LEN <= 0
            MOVE 1 TO ELE-NAME-LEN
            MOVE '?' TO ELE-NAME
           END-IF
           MOVE ELE-NAME-LEN TO VSTRING-LENGTH
           MOVE ELE-NAME
             TO VSTRING-DATA (1:80)
           PERFORM INSERT-VSTRING
           .
       INSERT-NUMBER.
           MOVE ZERO TO CMP-TMPA
           INSPECT EEC TALLYING CMP-TMPA FOR LEADING '0'
           COMPUTE CMP-TMPB = 9 - CMP-TMPA
           MOVE CMP-TMPB TO VSTRING-LENGTH
           MOVE EEC (CMP-TMPA + 1:CMP-TMPB) TO VSTRING-DATA
           PERFORM INSERT-VSTRING
           .
       INSERT-STRUCT-NAME.
           MOVE LANG-STRUCT-NAME-LENGTH TO VSTRING-LENGTH
           MOVE LANG-STRUCT-NAME TO VSTRING-DATA(1:80)
           PERFORM INSERT-VSTRING
           .
       INSERT-VSTRING.
           ADD 1 TO INSERTNO
           CALL 'CEECMI' USING
            NEW-CONDITION INSERTNO
            VSTRING FEEDBACK-CODE
           PERFORM CHECK-LE-SERVICE-FC
           .
       CHECK-LE-SERVICE-FC.
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-7
            DISPLAY CONVERTER-ERROR-8 ' '
             FACILITY OF FEEDBACK-CODE
             MSG-NO OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-9
            STOP RUN
           END-IF
           .
       REGISTER-EXCEPTION-HANDLER.
           SET ROUTINE TO ENTRY 'EPSCSMRA'
           SET TOKEN TO ADDRESS OF CEESRP-DATA
           CALL 'CEEHDLR' USING ROUTINE TOKEN FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-3
            STOP RUN
           END-IF
           .
       UNREGISTER-EXCEPTION-HANDLER.
           CALL 'CEEHDLU' USING ROUTINE FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-4
            STOP RUN
           END-IF
           .
       END PROGRAM 'EPSCSMRI'.
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *  Required Length in Bytes of the XML to Language Structure Ou
      *  tput Buffer
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRJ'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       1 XML2LS-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 XML2LS-PROPERTIES PIC X.
       PROCEDURE DIVISION USING
           XML2LS-LANG-BUFFER-LENGTH
           XML2LS-PROPERTIES
           .
       MAINLINE SECTION.
           IF ADDRESS OF XML2LS-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 106
              TO XML2LS-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF XML2LS-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO XML2LS-PROPERTIES
           END-IF
           GOBACK
           .
       END PROGRAM 'EPSCSMRJ'.
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *          XML to Language Structure Exception Handler
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRA'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CONVERTER-ERROR-6.
       2 PIC X(39) USAGE DISPLAY
           VALUE 'Failed To Resume Execution Of Converter'.
       LOCAL-STORAGE SECTION.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       LINKAGE SECTION.
       1 CEESRP-DATA.
       2 RECOVERY-POINT PIC S9(9) COMP.
       2 NUMVAL-ERROR PIC X.
       2 UNICODE-ERROR PIC X.
       2 OTHER-ERROR PIC X.
       2 SAVED-CONDITION PIC X(12).
       1 TOKEN  POINTER.
       1 RESULT PIC S9(9) COMP.
       88 RESUME VALUE 10.
       1 CURRENT-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       PROCEDURE DIVISION USING CURRENT-CONDITION TOKEN
           RESULT NEW-CONDITION.
       MAINLINE SECTION.
      * -------------------------------------------------------------
      *              Please Do Not Modify This Program
      * -------------------------------------------------------------
           SET ADDRESS OF CEESRP-DATA TO TOKEN
           CALL 'CEEMRCE' USING RECOVERY-POINT FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-6
           END-IF
           SET RESUME TO TRUE
           IF FACILITY OF CURRENT-CONDITION = 'IGZ'
            EVALUATE MSG-NO OF CURRENT-CONDITION
             WHEN 97
             WHEN 151
             WHEN 152
             WHEN 155
             WHEN 196
              MOVE 'Y' TO NUMVAL-ERROR
             WHEN 272
              MOVE 'Y' TO UNICODE-ERROR
             WHEN OTHER
              MOVE 'Y' TO OTHER-ERROR
            END-EVALUATE
           ELSE
            MOVE 'Y' TO OTHER-ERROR
           END-IF
           MOVE CURRENT-CONDITION TO SAVED-CONDITION
           GOBACK
           .
       END PROGRAM 'EPSCSMRA'.
       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),OPT,NOCICS
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *              Language Structure to XML Converter
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRO'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 ERROR-MESSAGES.
       2 CONVERTER-ERROR-3.
       3 PIC X(36) USAGE DISPLAY
           VALUE 'Failed To Register Exception Handler'.
       2 CONVERTER-ERROR-4.
       3 PIC X(38) USAGE DISPLAY
           VALUE 'Failed To Unregister Exception Handler'.
       2 CONVERTER-ERROR-7.
       3 PIC X(40) USAGE DISPLAY
           VALUE 'Language Environment Service Call Failed'.
       2 CONVERTER-ERROR-8.
       3 PIC X(35) USAGE DISPLAY
           VALUE 'Language Environment Message Number'.
       2 CONVERTER-ERROR-9.
       3 PIC X(31) USAGE DISPLAY
           VALUE 'XML Converter Is Terminating...'.
       1 X0000000B.
       2 PIC 9(4) COMP VALUE 26.
       2 PIC X(26) USAGE DISPLAY
           VALUE '</epspcom_program_retcode>'.
       1 X00000005.
       2 PIC 9(4) COMP VALUE 31.
       2 PIC X(31) USAGE DISPLAY
           VALUE '</epspcom_return_month_payment>'.
       1 X00000001.
       2 PIC 9(4) COMP VALUE 30.
       2 PIC X(30) USAGE DISPLAY
           VALUE '</DFHCOMMAREA></SOAP-ENV:Body>'.
       1 X00000008.
       2 PIC 9(4) COMP VALUE 17.
       2 PIC X(17) USAGE DISPLAY
           VALUE '</epspcom_errmsg>'.
       1 X00000004.
       2 PIC 9(4) COMP VALUE 30.
       2 PIC X(30) USAGE DISPLAY
           VALUE '<epspcom_return_month_payment>'.
       1 X00000000.
       2 PIC 9(4) COMP VALUE 88.
       2 PIC X(48) USAGE DISPLAY
           VALUE '<SOAP-ENV:Body><DFHCOMMAREA xmlns="http://www.EP'.
       2 PIC X(40) USAGE DISPLAY
           VALUE 'SCSMRTO.com/schemas/EPSCSMRTOInterface">'.
       1 X0000000A.
       2 PIC 9(4) COMP VALUE 25.
       2 PIC X(25) USAGE DISPLAY
           VALUE '<epspcom_program_retcode>'.
       1 X00000007.
       2 PIC 9(4) COMP VALUE 16.
       2 PIC X(16) USAGE DISPLAY
           VALUE '<epspcom_errmsg>'.
       LOCAL-STORAGE SECTION.
       1 CEESRP-DATA.
       2 RECOVERY-POINT PIC S9(9) COMP.
       2 NUMVAL-ERROR PIC X.
       2 UNICODE-ERROR PIC X.
       2 OTHER-ERROR PIC X.
       2 SAVED-CONDITION PIC X(12).
       1 ROUTINE PROCEDURE-POINTER.
       1 TOKEN POINTER.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 VSTRING.
       2 VSTRING-LENGTH PIC S9(4) COMP.
       2 VSTRING-DATA PIC X(80).
       1 SEV PIC S9(4) COMP.
       1 MSGNO PIC S9(4) COMP.
       1 CASE PIC S9(4) COMP.
       1 SEV2 PIC S9(4) COMP.
       1 CNTRL PIC S9(4) COMP.
       1 FACID PIC X(3) DISPLAY.
       1 ISINFO PIC S9(9) COMP.
       1 QDATA PIC S9(9) COMP.
       1 INSERTNO PIC S9(9) COMP.
       1 EEC PIC 9(9) DISPLAY.
       1 CMP-TMPA PIC 9(9) COMP.
       1 CMP-TMPB PIC 9(9) COMP.
       1 ERROR-CODE PIC S9(9) COMP.
       1 MSG-VAR-PART-LEN PIC 9(9) COMP.
       1 MSGBLD-RETURN-CODE PIC S9(9) COMP.
       1 LAST-INSTRUCTION PIC 9(9) COMP.
       1 LS2XML-LANG-BUFFER-POINTER POINTER.
       1 LS2XML-LANG-BUFFER-ADDRESS
           REDEFINES LS2XML-LANG-BUFFER-POINTER PIC 9(9) COMP.
       1 LANG-STRUCT-NAME PIC X(30).
       1 LANG-STRUCT-NAME-LENGTH PIC 9(4) COMP.
       1 LS2XML-XML-TEMPLATE-BUFFER PIC X(95).
       1 ARRAY-SUBSCRIPTS.
       2 X0000000C PIC 9(9) COMP VALUE 0.
       1 INSTRUCTIONS.
       2 INSTRUCT OCCURS 22 TIMES
            INDEXED BY INSTRUCT-NDX.
       3 MBOPCODE PIC X VALUE X'FF'.
       3 MBWSPOPT PIC X.
       3 MBDNMPTR POINTER.
       3 MBDATPTR POINTER.
       3 MBDATLEN PIC 9(9) COMP.
       3 MBDATYPE PIC X.
       3 MBSTGPTR POINTER.
       3 MBETGPTR POINTER.
       LINKAGE SECTION.
       01 DFHCOMMAREA
           .
       10 PROCESS-INDICATOR
           PICTURE X
           USAGE DISPLAY
           .
       10 EPSPCOM-PRINCIPLE-DATA
           PICTURE S9(9)V9(2)
           USAGE COMP
           .
       10 EPSPCOM-NUMBER-OF-YEARS
           PICTURE S9(4)
           USAGE COMP
           .
       10 EPSPCOM-NUMBER-OF-MONTHS
           PICTURE S9(4)
           USAGE COMP
           .
       10 EPSPCOM-QUOTED-INTEREST-RATE
           PICTURE S9(2)V9(3)
           USAGE COMP
           .
       10 EPSPCOM-YEAR-MONTH-IND
           PICTURE X
           USAGE DISPLAY
           .
       10 EPSPCOM-RETURN-MONTH-PAYMENT
           PICTURE S9(7)V9(2)
           USAGE COMP
           .
       10 EPSPCOM-ERRMSG
           PICTURE X(80)
           USAGE DISPLAY
           .
       10 EPSPCOM-PROGRAM-RETCODE
           PICTURE 9(4)
           USAGE DISPLAY
           .
       88 EPS02-REQUEST-SUCCESS
           VALUE
           0
           .
       10 EPSPCOM-PROGRAM-RETCODE-RDF REDEFINES EPSPCOM-PROGRAM-RETCODE
           PICTURE X(4)
           USAGE DISPLAY
           .
       1 X0000003D.
       10 X00000003 PIC -9(7).9(2).
       10 X00000006 PIC X(80).
       10 X00000009 PIC 9(4).
       1 LS2XML-XML-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-XML-BUFFER PIC X(758).
       1 LS2XML-LANG-BUFFER PIC X(106).
       1 OPTIONAL-FEEDBACK-CODE PIC X(12).
       1 CONVERTER-RETURN-CODE PIC S9(9) COMP.
       PROCEDURE DIVISION USING
           LS2XML-LANG-BUFFER
           LS2XML-XML-BUFFER-LENGTH
           LS2XML-XML-BUFFER
           OPTIONAL-FEEDBACK-CODE
           RETURNING
           CONVERTER-RETURN-CODE.
      * -------------------------------------------------------------
      *              Please Do Not Modify This Program
      * -------------------------------------------------------------
       MAINLINE SECTION.
           MOVE 'N' TO NUMVAL-ERROR UNICODE-ERROR OTHER-ERROR
           PERFORM CHECK-PARAMETERS
           PERFORM REGISTER-EXCEPTION-HANDLER
           CALL 'CEE3SRP' USING RECOVERY-POINT FEEDBACK-CODE
           SERVICE LABEL
           IF UNICODE-ERROR = 'Y'
            MOVE 288 TO MSGNO
            PERFORM UNREGISTER-EXCEPTION-HANDLER
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           IF OTHER-ERROR = 'Y'
            PERFORM UNREGISTER-EXCEPTION-HANDLER
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           SET LS2XML-LANG-BUFFER-POINTER
            TO ADDRESS OF LS2XML-LANG-BUFFER
           INITIALIZE LS2XML-XML-BUFFER-LENGTH
           SET INSTRUCT-NDX TO 1
           MOVE 'DFHCOMMAREA'
             TO LANG-STRUCT-NAME
           MOVE 11
             TO LANG-STRUCT-NAME-LENGTH
           SET ADDRESS OF DFHCOMMAREA
            TO LS2XML-LANG-BUFFER-POINTER
           SET ADDRESS OF X0000003D
            TO ADDRESS OF LS2XML-XML-TEMPLATE-BUFFER
           INITIALIZE X0000003D
           MOVE X'E0' TO MBOPCODE(INSTRUCT-NDX)
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000000
           SET INSTRUCT-NDX UP BY 1
           MOVE X'B0' TO MBOPCODE(INSTRUCT-NDX)
           MOVE X'C3' TO MBWSPOPT(INSTRUCT-NDX)
           MOVE 11 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000003
           MOVE 'N' TO MBDATYPE(INSTRUCT-NDX)
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000004
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000005
           SET INSTRUCT-NDX UP BY 1
           MOVE EPSPCOM-RETURN-MONTH-PAYMENT
             OF DFHCOMMAREA
            TO X00000003
           MOVE X'A0' TO MBOPCODE(INSTRUCT-NDX)
           MOVE X'C3' TO MBWSPOPT(INSTRUCT-NDX)
           MOVE 80 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000006
           MOVE 'X' TO MBDATYPE(INSTRUCT-NDX)
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000007
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000008
           SET INSTRUCT-NDX UP BY 1
           MOVE EPSPCOM-ERRMSG
             OF DFHCOMMAREA
            TO X00000006
           MOVE X'B0' TO MBOPCODE(INSTRUCT-NDX)
           MOVE X'C3' TO MBWSPOPT(INSTRUCT-NDX)
           MOVE 4 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000009
           MOVE 'N' TO MBDATYPE(INSTRUCT-NDX)
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000A
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000B
           SET INSTRUCT-NDX UP BY 1
           IF EPSPCOM-PROGRAM-RETCODE
           OF DFHCOMMAREA
            IS NOT NUMERIC
            MOVE ZEROS TO X00000009
           ELSE
            MOVE EPSPCOM-PROGRAM-RETCODE
              OF DFHCOMMAREA
             TO X00000009
           END-IF
           MOVE X'E1' TO MBOPCODE(INSTRUCT-NDX)
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000001
           SET INSTRUCT-NDX UP BY 1
           PERFORM INVOKE-MESSAGE-BUILDER
           PERFORM UNREGISTER-EXCEPTION-HANDLER
           GOBACK
           .
       CHECK-PARAMETERS.
           IF ADDRESS OF LS2XML-LANG-BUFFER EQUAL NULL AND
              ADDRESS OF LS2XML-XML-BUFFER-LENGTH NOT EQUAL NULL
            CALL 'EPSCSMRL' USING LS2XML-XML-BUFFER-LENGTH
            GOBACK
           ELSE
            IF ADDRESS OF LS2XML-XML-BUFFER EQUAL NULL AND
               ADDRESS OF LS2XML-XML-BUFFER-LENGTH NOT EQUAL NULL
             CALL 'EPSCSMRK' USING LS2XML-XML-BUFFER-LENGTH
             GOBACK
           END-IF
           IF ADDRESS OF LS2XML-LANG-BUFFER EQUAL NULL OR
              ADDRESS OF LS2XML-XML-BUFFER-LENGTH EQUAL NULL OR
              ADDRESS OF LS2XML-XML-BUFFER EQUAL NULL
            MOVE 294 TO MSGNO
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           .
       INVOKE-MESSAGE-BUILDER.
           CALL 'EPSCSMRC' USING
            INSTRUCTIONS LS2XML-XML-BUFFER-LENGTH
            LS2XML-XML-BUFFER LAST-INSTRUCTION
            RETURNING
             MSGBLD-RETURN-CODE
           IF MSGBLD-RETURN-CODE NOT EQUAL ZERO
            MOVE MSGBLD-RETURN-CODE
             TO MSGNO CONVERTER-RETURN-CODE
            PERFORM UNREGISTER-EXCEPTION-HANDLER
            PERFORM SIGNAL-CONDITION
            GOBACK
           ELSE
            MOVE ZERO TO CONVERTER-RETURN-CODE
           END-IF
           SET INSTRUCT-NDX TO 1
           MOVE ALL X'FF' TO INSTRUCTIONS
           .
       SIGNAL-CONDITION.
           IF OTHER-ERROR = 'N'
            MOVE 3 TO SEV SEV2
            MOVE 1 TO CASE CNTRL
            MOVE 0 TO ISINFO
            MOVE 0 TO INSERTNO
            MOVE 'IGZ' TO FACID
            CALL 'CEENCOD' USING
             SEV MSGNO CASE SEV2
             CNTRL FACID ISINFO
             NEW-CONDITION FEEDBACK-CODE
            PERFORM CHECK-LE-SERVICE-FC
            MOVE 8 TO VSTRING-LENGTH
            MOVE 'EPSCSMRO'
             TO VSTRING-DATA (1:8)
            PERFORM INSERT-VSTRING
            MOVE MSGNO TO ERROR-CODE
            EVALUATE MSGNO
             WHEN 287
              MOVE 758 TO EEC
              PERFORM INSERT-NUMBER
             WHEN 288
              MOVE 1140 TO EEC
              PERFORM INSERT-NUMBER
              MOVE 1140 TO EEC
              PERFORM INSERT-NUMBER
            END-EVALUATE
           ELSE
            MOVE SAVED-CONDITION TO NEW-CONDITION
            MOVE MSG-NO OF NEW-CONDITION TO ERROR-CODE
           END-IF
           MOVE 0 TO QDATA
           IF ADDRESS OF OPTIONAL-FEEDBACK-CODE = NULL
            CALL 'CEESGL' USING
             NEW-CONDITION QDATA OMITTED
           ELSE
            MOVE NEW-CONDITION TO OPTIONAL-FEEDBACK-CODE
           END-IF
           IF MSGNO NOT EQUAL 294
            MOVE ERROR-CODE TO CONVERTER-RETURN-CODE
           END-IF
           .
       INSERT-NUMBER.
           MOVE ZERO TO CMP-TMPA
           INSPECT EEC
            TALLYING CMP-TMPA FOR LEADING ZEROS
           COMPUTE CMP-TMPB = 9 - CMP-TMPA
           MOVE CMP-TMPB TO VSTRING-LENGTH
           MOVE EEC (CMP-TMPA + 1:CMP-TMPB)
            TO VSTRING-DATA
           PERFORM INSERT-VSTRING
           .
       INSERT-VSTRING.
           ADD 1 TO INSERTNO
           CALL 'CEECMI' USING
            NEW-CONDITION INSERTNO
            VSTRING FEEDBACK-CODE
           PERFORM CHECK-LE-SERVICE-FC
           .
       CHECK-LE-SERVICE-FC.
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-7
            DISPLAY CONVERTER-ERROR-8 ' '
             FACILITY OF FEEDBACK-CODE
             MSG-NO OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-9
            STOP RUN
           END-IF
           .
       REGISTER-EXCEPTION-HANDLER.
           SET ROUTINE
            TO ENTRY 'EPSCSMRE'
           SET TOKEN TO ADDRESS OF CEESRP-DATA
           CALL 'CEEHDLR' USING
            ROUTINE TOKEN FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-3
            STOP RUN
           END-IF
           .
       UNREGISTER-EXCEPTION-HANDLER.
           CALL 'CEEHDLU' USING
            ROUTINE FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-4
            STOP RUN
           END-IF
           .
       END PROGRAM 'EPSCSMRO'.
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *           Language Structure to XML Markup Generator
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRC'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       1 CMP-TMPA PIC 9(9) COMP.
       1 CMP-TMPB PIC 9(9) COMP.
       1 VALID-TEXT-FLAG PIC X VALUE 'Y'.
       1 NEXT-CHAR PIC X.
       LINKAGE SECTION.
       1 INSTRUCTIONS.
       2 INSTRUCT OCCURS 22 TIMES
            INDEXED BY INSTRUCT-NDX.
       3 MBOPCODE PIC X.
       3 MBWSPOPT PIC X.
       3 MBDNMPTR POINTER.
       3 MBDATPTR POINTER.
       3 MBDATLEN PIC 9(9) COMP.
       3 MBDATYPE PIC X.
       3 MBSTGPTR POINTER.
       3 MBETGPTR POINTER.
       1 XML-BUFFER-OFFSET PIC 9(9) COMP.
       1 XML-BUFFER PIC X(758).
       1 MSGBLD-RETURN-CODE PIC S9(9) COMP.
       1 XML-TAG-DESCRIPTOR.
       2 XML-TAG-LEN PIC 9(4) COMP.
       2 XML-TAG PIC X(88).
       1 CONTENT-TXT PIC X(128).
       1 LAST-INSTRUCTION PIC 9(9) COMP.
       PROCEDURE DIVISION USING
            INSTRUCTIONS XML-BUFFER-OFFSET
            XML-BUFFER LAST-INSTRUCTION
           RETURNING
            MSGBLD-RETURN-CODE.
       MAINLINE SECTION.
      * -------------------------------------------------------------
      *              Please Do Not Modify This Program
      * -------------------------------------------------------------
           SET INSTRUCT-NDX TO 1
           PERFORM UNTIL MBOPCODE(INSTRUCT-NDX) = X'FF'
            EVALUATE MBOPCODE(INSTRUCT-NDX)
            WHEN X'E0'
             PERFORM START-TAG
            WHEN X'E1'
             PERFORM END-TAG
            WHEN X'B0'
             PERFORM START-TAG
             PERFORM CONVERT-NUMERIC
             PERFORM END-TAG
            WHEN X'A0'
             PERFORM START-TAG
             PERFORM CONVERT-ALPHANUMERIC
             PERFORM END-TAG
            WHEN X'F0'
             PERFORM START-TAG
             PERFORM CONVERT-FLOAT
             PERFORM END-TAG
            END-EVALUATE
            SET INSTRUCT-NDX UP BY 1
           END-PERFORM
           GOBACK
           .
       START-TAG.
           SET ADDRESS OF XML-TAG-DESCRIPTOR
            TO MBSTGPTR(INSTRUCT-NDX)
           MOVE XML-TAG(1:XML-TAG-LEN)
            TO XML-BUFFER(XML-BUFFER-OFFSET + 1:XML-TAG-LEN)
           ADD XML-TAG-LEN TO XML-BUFFER-OFFSET
           .
       END-TAG.
           SET ADDRESS OF XML-TAG-DESCRIPTOR
            TO MBETGPTR(INSTRUCT-NDX)
           MOVE XML-TAG(1:XML-TAG-LEN)
            TO XML-BUFFER(XML-BUFFER-OFFSET + 1:XML-TAG-LEN)
           ADD XML-TAG-LEN TO XML-BUFFER-OFFSET
           .
       CONVERT-ALPHANUMERIC.
           CALL 'XCHRFLTR' USING
            BY VALUE MBDATLEN(INSTRUCT-NDX)
            BY VALUE MBDATPTR(INSTRUCT-NDX)
            BY REFERENCE VALID-TEXT-FLAG
           IF VALID-TEXT-FLAG EQUAL 'Y'
           CALL 'XWSPFLTR' USING
            BY VALUE MBWSPOPT(INSTRUCT-NDX)
            BY VALUE MBDATPTR(INSTRUCT-NDX)
            BY REFERENCE MBDATLEN(INSTRUCT-NDX)
            BY VALUE MBDATYPE(INSTRUCT-NDX)
            SET ADDRESS OF CONTENT-TXT
             TO MBDATPTR(INSTRUCT-NDX)
            PERFORM VARYING CMP-TMPA FROM 1 BY 1
             UNTIL CMP-TMPA > MBDATLEN(INSTRUCT-NDX)
             MOVE CONTENT-TXT(CMP-TMPA:1) TO NEXT-CHAR
             EVALUATE NEXT-CHAR
              WHEN '&'
               MOVE '&amp;'
                TO XML-BUFFER(XML-BUFFER-OFFSET + 1:5)
               ADD 5 TO XML-BUFFER-OFFSET
              WHEN '<'
               MOVE '&lt;'
                TO XML-BUFFER(XML-BUFFER-OFFSET + 1:4)
               ADD 4 TO XML-BUFFER-OFFSET
              WHEN '>'
               MOVE '&gt;'
                TO XML-BUFFER(XML-BUFFER-OFFSET + 1:4)
               ADD 4 TO XML-BUFFER-OFFSET
              WHEN ''''
               MOVE '&apos;'
                TO XML-BUFFER(XML-BUFFER-OFFSET + 1:6)
               ADD 6 TO XML-BUFFER-OFFSET
              WHEN '"'
               MOVE '&quot;'
                TO XML-BUFFER(XML-BUFFER-OFFSET + 1:6)
               ADD 6 TO XML-BUFFER-OFFSET
             WHEN OTHER
               MOVE NEXT-CHAR
                TO XML-BUFFER(XML-BUFFER-OFFSET + 1:1)
               ADD 1 TO XML-BUFFER-OFFSET
             END-EVALUATE
            END-PERFORM
           END-IF
           .
       CONVERT-NUMERIC.
           SET ADDRESS OF CONTENT-TXT
            TO MBDATPTR(INSTRUCT-NDX)
           CALL 'XWSPFLTR' USING
            BY VALUE MBWSPOPT(INSTRUCT-NDX)
            BY VALUE MBDATPTR(INSTRUCT-NDX)
            BY REFERENCE MBDATLEN(INSTRUCT-NDX)
            BY VALUE MBDATYPE(INSTRUCT-NDX)
           MOVE MBDATLEN(INSTRUCT-NDX)
             TO CMP-TMPA
           IF CMP-TMPA > 0
            MOVE CONTENT-TXT(1:CMP-TMPA)
              TO XML-BUFFER(XML-BUFFER-OFFSET + 1:CMP-TMPA)
            ADD CMP-TMPA TO XML-BUFFER-OFFSET
           ELSE
            MOVE '0'
              TO XML-BUFFER(XML-BUFFER-OFFSET + 1:1)
            ADD 1 TO XML-BUFFER-OFFSET
           END-IF
           .
       CONVERT-FLOAT.
           SET ADDRESS OF CONTENT-TXT
            TO MBDATPTR(INSTRUCT-NDX)
           CALL 'XWSPFLTR' USING
            BY VALUE MBWSPOPT(INSTRUCT-NDX)
            BY VALUE MBDATPTR(INSTRUCT-NDX)
            BY REFERENCE MBDATLEN(INSTRUCT-NDX)
            BY VALUE MBDATYPE(INSTRUCT-NDX)
           MOVE MBDATLEN(INSTRUCT-NDX)
             TO CMP-TMPA
           MOVE CONTENT-TXT(1:CMP-TMPA)
             TO XML-BUFFER(XML-BUFFER-OFFSET + 1:CMP-TMPA)
           ADD CMP-TMPA TO XML-BUFFER-OFFSET
           .
      * -------------------------------------------------------------
      *          Language Structure to XML Character Filter
      * -------------------------------------------------------------
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'XCHRFLTR'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 X0000003E.
       2 PIC X(24) USAGE DISPLAY
           VALUE X'0001020304060708090A0B0C0E0F1011121314161718191A'.
       2 PIC X(24) USAGE DISPLAY
           VALUE X'1B1C1D1E1F2021222324262728292A2B2C2D2E2F30313233'.
       2 PIC X(13) USAGE DISPLAY
           VALUE X'3435363738393A3B3C3D3E3FFF'.
       1 ILLEGAL-XML-CHARS REDEFINES X0000003E
           PIC X(61).
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       1 CONTENT-LEN PIC 9(9) COMP.
       1 CONTENT-PTR POINTER.
       1 CONTENT-TXT PIC X(128).
       1 VALID-TEXT-FLAG PIC X.
       PROCEDURE DIVISION USING BY VALUE CONTENT-LEN CONTENT-PTR
           BY REFERENCE VALID-TEXT-FLAG.
       MAINLINE SECTION.
      * -------------------------------------------------------------
      *              Please Do Not Modify This Program
      * -------------------------------------------------------------
           SET ADDRESS OF CONTENT-TXT TO CONTENT-PTR
           INSPECT CONTENT-TXT(1:CONTENT-LEN) CONVERTING
            ILLEGAL-XML-CHARS TO SPACES
           MOVE 'Y' TO VALID-TEXT-FLAG
           GOBACK
           .
       END PROGRAM 'XCHRFLTR'.
      * -------------------------------------------------------------
      *         Language Structure to XML WhiteSpace Filter
      * -------------------------------------------------------------
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'XWSPFLTR'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       1 CONTENT-BUF PIC X(128).
       1 CONTENT-BUF-NDX PIC 9(9) COMP.
       1 CONTENT-TXT-NDX PIC 9(9) COMP.
       1 CMP-TMPA PIC 9(9) COMP.
       1 CMP-TMPB PIC 9(9) COMP.
       LINKAGE SECTION.
       1 CONTENT-WSP PIC X.
       1 CONTENT-PTR POINTER.
       1 CONTENT-LEN PIC 9(9) COMP.
       1 CONTENT-TYPE PIC X.
       1 CONTENT-TXT PIC X(128).
       PROCEDURE DIVISION USING
           BY VALUE CONTENT-WSP
           BY VALUE CONTENT-PTR
           BY REFERENCE CONTENT-LEN
           BY VALUE CONTENT-TYPE
           .
       MAINLINE SECTION.
      * -------------------------------------------------------------
      *              Please Do Not Modify This Program
      * -------------------------------------------------------------
           SET ADDRESS OF CONTENT-TXT
            TO CONTENT-PTR
           IF CONTENT-TYPE = 'X' OR
              CONTENT-TYPE = 'D' OR
              CONTENT-TYPE = 'U'
            EVALUATE CONTENT-WSP
             WHEN X'C3'
              PERFORM REPLACE-CTRL-CHARS
              PERFORM TRIM-LEADING-SPACES
              PERFORM TRIM-TRAILING-SPACES
              PERFORM COLLAPSE-SPACES
             WHEN X'C1'
              PERFORM REPLACE-CTRL-CHARS
             WHEN X'C2'
              PERFORM TRIM-TRAILING-SPACES-COMPAT
            END-EVALUATE
           ELSE
            IF CONTENT-TYPE = 'N' OR
               CONTENT-TYPE = 'F' OR
               CONTENT-TYPE = 'B'
              PERFORM TRIM-LEADING-SPACES
              PERFORM TRIM-TRAILING-SPACES
            END-IF
            IF CONTENT-TYPE = 'N'
             PERFORM TRIM-LEADING-ZEROS
            END-IF
           END-IF
           GOBACK
           .
       TRIM-LEADING-SPACES.
           MOVE 1 TO CONTENT-TXT-NDX
           PERFORM TEST BEFORE
            UNTIL CONTENT-TXT-NDX >= CONTENT-LEN OR
            CONTENT-TXT(CONTENT-TXT-NDX:1) NOT = SPACE
            ADD 1 TO CONTENT-TXT-NDX
           END-PERFORM
           IF CONTENT-TXT-NDX > 1
            COMPUTE CONTENT-LEN
             = CONTENT-LEN - (CONTENT-TXT-NDX - 1)
            MOVE CONTENT-TXT(CONTENT-TXT-NDX:CONTENT-LEN)
              TO CONTENT-BUF(1:CONTENT-LEN)
            MOVE CONTENT-BUF(1:CONTENT-LEN)
              TO CONTENT-TXT(1:CONTENT-LEN)
           END-IF
           .
       TRIM-TRAILING-SPACES.
           PERFORM TEST BEFORE
            VARYING CONTENT-LEN FROM CONTENT-LEN BY -1
            UNTIL CONTENT-LEN = 0
            OR CONTENT-TXT(CONTENT-LEN:1) NOT = SPACE
           END-PERFORM
           .
       TRIM-TRAILING-SPACES-COMPAT.
           PERFORM TEST BEFORE
            VARYING CONTENT-LEN FROM CONTENT-LEN BY -1
            UNTIL CONTENT-LEN = 1
            OR CONTENT-TXT(CONTENT-LEN:1) NOT = SPACE
           END-PERFORM
           .
       REPLACE-CTRL-CHARS.
           INSPECT CONTENT-TXT(1:CONTENT-LEN) REPLACING ALL
            X'05' BY SPACE X'0B' BY SPACE
            X'0D' BY SPACE X'25' BY SPACE
           .
       COLLAPSE-SPACES.
           MOVE 1 TO CONTENT-TXT-NDX
           MOVE 1 TO CONTENT-BUF-NDX
           PERFORM TEST BEFORE
            UNTIL CONTENT-TXT-NDX > CONTENT-LEN
            IF CONTENT-TXT(CONTENT-TXT-NDX:1) = SPACE
             MOVE CONTENT-TXT(CONTENT-TXT-NDX:1)
               TO CONTENT-BUF(CONTENT-BUF-NDX:1)
             ADD 1 TO CONTENT-TXT-NDX
             ADD 1 TO CONTENT-BUF-NDX
             PERFORM TEST BEFORE
              UNTIL CONTENT-TXT-NDX > CONTENT-LEN OR
               CONTENT-TXT(CONTENT-TXT-NDX:1) NOT = SPACE
              ADD 1 TO CONTENT-TXT-NDX
             END-PERFORM
            ELSE
             MOVE CONTENT-TXT(CONTENT-TXT-NDX:1)
               TO CONTENT-BUF(CONTENT-BUF-NDX:1)
             ADD 1 TO CONTENT-TXT-NDX
             ADD 1 TO CONTENT-BUF-NDX
            END-IF
           END-PERFORM
           COMPUTE CONTENT-LEN = CONTENT-BUF-NDX - 1
           MOVE CONTENT-BUF(1:CONTENT-LEN)
             TO CONTENT-TXT(1:CONTENT-LEN)
           .
       TRIM-LEADING-ZEROS.
           MOVE 1 TO CONTENT-TXT-NDX
           MOVE 1 TO CONTENT-BUF-NDX
           IF CONTENT-LEN > 0
              AND CONTENT-TXT(1:1) = '-'
            MOVE CONTENT-TXT(1:1)
              TO CONTENT-BUF(1:1)
            ADD 1 TO CONTENT-TXT-NDX
            ADD 1 TO CONTENT-BUF-NDX
           END-IF
           COMPUTE CMP-TMPA
            = CONTENT-LEN - (CONTENT-TXT-NDX - 1)
           IF CMP-TMPA > 0 AND
              CONTENT-TXT(CONTENT-TXT-NDX:1) = '0'
            INITIALIZE CMP-TMPB
            INSPECT CONTENT-TXT(CONTENT-TXT-NDX:CMP-TMPA)
             TALLYING CMP-TMPB FOR LEADING '0'
            IF CMP-TMPB > 0
             COMPUTE CMP-TMPA
              = CONTENT-TXT-NDX + CMP-TMPB
             IF CONTENT-TXT(CMP-TMPA:1) = '.'
              SUBTRACT 1 FROM CMP-TMPB
             END-IF
             ADD CMP-TMPB TO CONTENT-TXT-NDX
            END-IF
           END-IF
           COMPUTE CMP-TMPA
            = CONTENT-LEN - (CONTENT-TXT-NDX - 1)
           IF CMP-TMPA > 0
            MOVE CONTENT-TXT(CONTENT-TXT-NDX:CMP-TMPA)
              TO CONTENT-BUF(CONTENT-BUF-NDX:CMP-TMPA)
            ADD  CMP-TMPA TO CONTENT-BUF-NDX
           END-IF
           COMPUTE CONTENT-LEN = CONTENT-BUF-NDX - 1
           IF CONTENT-LEN > 0
            MOVE CONTENT-BUF(1:CONTENT-LEN)
              TO CONTENT-TXT(1:CONTENT-LEN)
           END-IF
           .
       END PROGRAM 'XWSPFLTR'.
       END PROGRAM 'EPSCSMRC'.
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *  Required Length in Bytes of the Language Structure to XML Ou
      *  tput Buffer
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRK'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       1 LS2XML-XML-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-PROPERTIES PIC X.
       PROCEDURE DIVISION USING
           LS2XML-XML-BUFFER-LENGTH
           LS2XML-PROPERTIES
           .
       MAINLINE SECTION.
           IF ADDRESS OF LS2XML-XML-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 758
              TO LS2XML-XML-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO LS2XML-PROPERTIES
           END-IF
           GOBACK
           .
       END PROGRAM 'EPSCSMRK'.
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *  Maximum Length in Bytes of the Language Structure to XML Inp
      *  ut Buffer
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRL'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       1 LS2XML-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-PROPERTIES PIC X.
       PROCEDURE DIVISION USING
           LS2XML-LANG-BUFFER-LENGTH
           LS2XML-PROPERTIES
           .
       MAINLINE SECTION.
           IF ADDRESS OF LS2XML-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 106
              TO LS2XML-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO LS2XML-PROPERTIES
           END-IF
           GOBACK
           .
       END PROGRAM 'EPSCSMRL'.
      *          *********************************************
      *    ************************RD/z**7.5************************
      *  *************************************************************
      *          Language Structure to XML Exception Handler
      *  *************************************************************
      *    ************************RD/z**7.5************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'EPSCSMRE'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.0.0.V200809191411.
        DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CONVERTER-ERROR-6.
       2 PIC X(39) USAGE DISPLAY
           VALUE 'Failed To Resume Execution Of Converter'.
       LOCAL-STORAGE SECTION.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       LINKAGE SECTION.
       1 CEESRP-DATA.
       2 RECOVERY-POINT PIC S9(9) COMP.
       2 NUMVAL-ERROR PIC X.
       2 UNICODE-ERROR PIC X.
       2 OTHER-ERROR PIC X.
       2 SAVED-CONDITION PIC X(12).
       1 TOKEN  POINTER.
       1 RESULT PIC S9(9) COMP.
       88 RESUME VALUE 10.
       1 CURRENT-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION PIC X(12).
       PROCEDURE DIVISION USING CURRENT-CONDITION TOKEN
           RESULT NEW-CONDITION.
       MAINLINE SECTION.
      * -------------------------------------------------------------
      *              Please Do Not Modify This Program
      * -------------------------------------------------------------
           SET ADDRESS OF CEESRP-DATA TO TOKEN
           SET RESUME TO TRUE
           CALL 'CEEMRCE' USING RECOVERY-POINT FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-6
           END-IF
           IF FACILITY OF CURRENT-CONDITION = 'IGZ'
            EVALUATE MSG-NO OF CURRENT-CONDITION
             WHEN 272
              MOVE 'Y' TO UNICODE-ERROR
             WHEN OTHER
              MOVE 'Y' TO OTHER-ERROR
            END-EVALUATE
           ELSE
            MOVE 'Y' TO OTHER-ERROR
           END-IF
           MOVE CURRENT-CONDITION TO SAVED-CONDITION
           GOBACK
           .
       END PROGRAM 'EPSCSMRE'.
