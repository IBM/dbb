       ID DIVISION.
       PROGRAM-ID. EPSNBRVL
      *    ORIGINAL THIS PROGRAM VALIDATES THE INPUT AND
      *    CONVERTS IT TO VARIOUS FORMATS.
      *    modif
      *    IT DOES NOT CALC THE MORTGAGE. xx
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. FLEX-ES.
       OBJECT-COMPUTER. FLEX-ES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-STATIC-DATA.
           03 STATIC-ERRORS.
              05 FILLER                  PIC 99 VALUE 1.
              05 FILLER                  PIC X(80)
              VALUE 'NO NUMBER PRESENT'.
              05 FILLER                  PIC 99 VALUE 2.
              05 FILLER                  PIC X(80)
              VALUE 'INVALID CHARACTERS IN NUMBER'.
              05 FILLER                  PIC 99 VALUE 3.
              05 FILLER                  PIC X(80)
              VALUE 'TOO MANY DECIMAL POINTS'.
              05 FILLER                  PIC 99 VALUE 4.
              05 FILLER                  PIC X(80)
              VALUE 'YEARS INDICATED, BUT YEARS ZERO OR LESS'.
              05 FILLER                  PIC 99 VALUE 5.
              05 FILLER                  PIC X(80)
              VALUE 'ZERO OR LESS MONTHS'.
              05 FILLER                  PIC 99 VALUE 6.
              05 FILLER                  PIC X(80)
              VALUE 'LOAN TERM MUST BE BETWEEN 1 AND 40 YEARS'.
              05 FILLER                  PIC 99 VALUE 7.
              05 FILLER                  PIC X(80)
              VALUE 'LOAN AMOUNT MUST BE BETWEEN $500 AND $500,000'.
              05 FILLER                  PIC 99 VALUE 8.
              05 FILLER                  PIC X(80)
              VALUE ' '.
              05 FILLER                  PIC 99 VALUE 9.
              05 FILLER                  PIC X(80)
              VALUE ' '.
              05 FILLER                  PIC 99 VALUE 10.
              05 FILLER                  PIC X(80)
              VALUE ' '.
           03 STATIC-ERROR-TBL REDEFINES STATIC-ERRORS.
              05 STATIC-ERROR-TABLE OCCURS 10 TIMES.
                07 ERROR-INDICATOR         PIC 99.
                07 ERROR-TEXT              PIC X(80).
       01  WS-WORK-AMOUNTS.
           03 WS-LEADING-SPACES      PIC 9(4) COMP VALUE 1.
           03 WS-TRAILING-SPACES     PIC 9(4) COMP VALUE 0.
           03 WS-END-SPACE           PIC 9(4) COMP VALUE 0.
           03 WS-DECIMAL-SPACE       PIC 99        VALUE 0.
           03 WS-IDX                 PIC 9(2) COMP.
           03 WS-DEC-IDX             PIC 9(2) COMP.
           03 WS-NUM-IDX             PIC 9(2) COMP.

           03 WS-MAX-NUMBER-LGTH     PIC 9(2) COMP.
           03 WS-MAX-FIELD           PIC 9(2) COMP.
           03 WS-DEC-ADJUST          PIC 9.


       LINKAGE SECTION.
      *
       COPY EPSNBRPM.

       PROCEDURE DIVISION USING EPS-NUMBER-VALIDATION.
      *
       A000-MAINLINE SECTION.
       A000-10.
           MOVE EPSPARM-MAX-LENGTH              TO WS-IDX.
           MOVE LENGTH OF EPSPARM-VALIDATE-DATA TO WS-MAX-FIELD
           IF WS-IDX > WS-MAX-FIELD
              MOVE WS-MAX-FIELD TO WS-IDX
           ELSE
              MOVE WS-IDX       TO WS-MAX-FIELD
           END-IF.

           MOVE ZERO   TO WS-END-SPACE.
           MOVE ZERO   TO EPSPARM-RETURN-ERROR-RC.
           MOVE SPACES TO EPSPARM-RETURN-ERROR-TEXT.
           MOVE ZERO   TO EPSPARM-BINARY-NUMBER
                          EPSPARM-NUMBER
                          EPSPARM-DECIMAL.

      * FIND TRAILING SPACES
           PERFORM UNTIL WS-IDX = 0
              IF EPSPARM-VALIDATE-DATA(WS-IDX:1) = SPACES
                ADD 1      TO WS-TRAILING-SPACES
                SUBTRACT 1 FROM WS-IDX
              ELSE
                MOVE WS-IDX TO WS-END-SPACE
                MOVE 0 TO WS-IDX
              END-IF
           END-PERFORM.

      * FIND LEADING SPACES
           MOVE 1 TO WS-LEADING-SPACES.

           IF WS-END-SPACE NOT = 0
              MOVE 1 TO WS-IDX
              PERFORM UNTIL WS-IDX >= WS-END-SPACE
                IF EPSPARM-VALIDATE-DATA(WS-IDX:1) = SPACES
                   ADD 1 TO WS-LEADING-SPACES
                   ADD 1 TO WS-IDX
                ELSE
                   COMPUTE WS-IDX = WS-END-SPACE + 1
                END-IF
              END-PERFORM
           ELSE
              MOVE STATIC-ERROR-TABLE(1) TO EPSPARM-RETURN-ERROR
              GO TO A999-EXIT
           END-IF.

           MOVE WS-LEADING-SPACES TO WS-IDX.
           MOVE 1                 TO WS-DEC-IDX.
           MOVE 0                 TO WS-DECIMAL-SPACE.

      * FIND DECIMAL POINT
           PERFORM A002-COMPUTE-DECIMAL
                   UNTIL WS-IDX > WS-END-SPACE
                      OR EPSPARM-RETURN-ERROR-RC > 0
           .

           IF  EPSPARM-RETURN-ERROR-RC > 0
               GO TO A999-EXIT
           END-IF.

           IF WS-DECIMAL-SPACE > 0
              COMPUTE WS-END-SPACE = WS-DECIMAL-SPACE - 1
           END-IF.

      * VALIDATE NO INTERNAL BLANKS
           MOVE WS-END-SPACE             TO WS-IDX.
           MOVE LENGTH OF EPSPARM-NUMBER TO WS-NUM-IDX.
      *     SUBTRACT 1 FROM WS-NUM-IDX.

           PERFORM A001-COMPUTE-INTEGER
                   UNTIL WS-IDX < WS-LEADING-SPACES
                      OR EPSPARM-RETURN-ERROR-RC > 0
           .

           IF  EPSPARM-RETURN-ERROR-RC > 0
               GO TO A999-EXIT
           END-IF.


      *** RC Must be 0 at this point to calc value.
           COMPUTE EPSPARM-BINARY-NUMBER = EPSPARM-NUMBER
                                         + EPSPARM-DECIMAL.

      * Apply requested rule to the value:
      * Loan term must be between 1 and 40 years

           IF EPSPARM-RULE-FLAG-YEARS
              IF EPSPARM-BINARY-NUMBER < 1 OR
                 EPSPARM-BINARY-NUMBER > 40
                    MOVE STATIC-ERROR-TABLE(6) TO
                         EPSPARM-RETURN-ERROR
              END-IF
           END-IF.

           IF EPSPARM-RULE-FLAG-AMOUNT
              IF EPSPARM-BINARY-NUMBER < 500 OR
                 EPSPARM-BINARY-NUMBER > 500000
                    MOVE STATIC-ERROR-TABLE(7) TO
                         EPSPARM-RETURN-ERROR
              END-IF
           END-IF.

       A999-EXIT.
           GOBACK.

       A001-COMPUTE-INTEGER SECTION.
       A001-10.
           IF EPSPARM-VALIDATE-DATA(WS-IDX:1) = ','
              SUBTRACT 1 FROM WS-IDX
           ELSE
              IF EPSPARM-VALIDATE-DATA(WS-IDX:1) = SPACE
              OR EPSPARM-VALIDATE-DATA(WS-IDX:1) IS NOT NUMERIC
                 MOVE STATIC-ERROR-TABLE(2) TO EPSPARM-RETURN-ERROR
                 MOVE 0 TO WS-IDX
              ELSE
                 MOVE EPSPARM-VALIDATE-DATA(WS-IDX:1) TO
                      EPSPARM-NUMBER(WS-NUM-IDX:1)
                 SUBTRACT 1 FROM WS-IDX
                                 WS-NUM-IDX
              END-IF
           END-IF
           .
       A001-99.
           EXIT.

       A002-COMPUTE-DECIMAL SECTION.
       A002-10.
           IF WS-DECIMAL-SPACE = 0
              IF EPSPARM-VALIDATE-DATA(WS-IDX:1) = '.'
                 MOVE WS-IDX TO WS-DECIMAL-SPACE
              END-IF
           ELSE
              IF EPSPARM-VALIDATE-DATA(WS-IDX:1) = '.'
                 MOVE STATIC-ERROR-TABLE(3) TO EPSPARM-RETURN-ERROR
                 MOVE WS-END-SPACE TO WS-IDX
                 MOVE 1            TO WS-DEC-IDX
              ELSE
                 MOVE EPSPARM-VALIDATE-DATA(WS-IDX:1) TO
                      EPSPARM-DECIMAL(WS-DEC-IDX:1)
                 ADD 1 TO WS-DEC-IDX
              END-IF
           END-IF
           ADD 1 TO WS-IDX
           .
       A002-99.
           EXIT.
