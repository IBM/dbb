       ID DIVISION.
       PROGRAM-ID. EPSNBRVL
      *    THIS IS A CALLED PROGRAM EXAMPLE FOR DEMONSTRATION
      *
      *    THIS PROGRAM WILL BE CALLED BY ANOTHER, RECEIVE
      *    THE FOLLOWING INFOMATION AND RETURN A MONTLY PAYMENT AMOUNT
      *    INPUT:
      *       ORIGINAL PRINCIPLE AMOUNT
      *       YEARS OR MONTH INDICATOR
      *       NUMBER OF YEARS
      *       NUMBER OF MONTHS
      *       INTEREST RATE
      *    OUTPUT:
      *       MONTHLY PAYMENT
      *
      *    (C) 2008 IBM - Jim Hildner
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
              VALUE 'SPACES IN NUMBER'.
              05 FILLER                  PIC 99 VALUE 3.
              05 FILLER                  PIC X(80)
              VALUE 'TOO MANY DEICMAL POINTS'.
              05 FILLER                  PIC 99 VALUE 4.
              05 FILLER                  PIC X(80)
              VALUE 'YEARS INDICATED, BUT YEARS ZERO OR LESS'.
              05 FILLER                  PIC 99 VALUE 5.
              05 FILLER                  PIC X(80)
              VALUE 'ZERO OR LESS MONTHS'.
              05 FILLER                  PIC 99 VALUE 6.
              05 FILLER                  PIC X(80)
              VALUE ' '.
              05 FILLER                  PIC 99 VALUE 7.
              05 FILLER                  PIC X(80)
              VALUE ' '.
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
       A000-MAINLINE.
           MOVE EPSPARM-MAX-LENGTH              TO WS-IDX.
           MOVE LENGTH OF EPSPARM-VALIDATE-DATA TO WS-MAX-FIELD
           IF WS-IDX > WS-MAX-FIELD
              MOVE WS-MAX-FIELD TO WS-IDX
           ELSE
              MOVE WS-IDX       TO WS-MAX-FIELD
           END-IF.

           MOVE ZERO   TO WS-END-SPACE.
           MOVE SPACES TO EPSPARM-RETURN-ERROR.
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
           END-IF.

           MOVE WS-LEADING-SPACES TO WS-IDX.
           MOVE 1                 TO WS-DEC-IDX.
           MOVE 0                 TO WS-DECIMAL-SPACE.

      * FIND DECIMAL POINT
           PERFORM A002-COMPUTE-DECIMAL
                   UNTIL WS-IDX > WS-END-SPACE
           .

           IF WS-DECIMAL-SPACE > 0
              COMPUTE WS-END-SPACE = WS-DECIMAL-SPACE - 1
           END-IF.

      * VALIDATE NO INTERNAL BLANKS
           MOVE WS-END-SPACE             TO WS-IDX.
           MOVE LENGTH OF EPSPARM-NUMBER TO WS-NUM-IDX.
      *     SUBTRACT 1 FROM WS-NUM-IDX.

           PERFORM A001-COMPUTE-INTEGER
                   UNTIL WS-IDX < WS-LEADING-SPACES
           .

           IF EPSPARM-RETURN-ERROR = SPACES
              COMPUTE EPSPARM-BINARY-NUMBER = EPSPARM-NUMBER
                                            + EPSPARM-DECIMAL
           END-IF.
           GOBACK
           .

       A001-COMPUTE-INTEGER.
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

       A002-COMPUTE-DECIMAL.
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
