   CBL NUMPROC(MIG),FLAG(I,W),RENT
       ID DIVISION.
       PROGRAM-ID. EPSMPMV.
      *   Clone of EPSMPMT
      *
      *  comment
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. FLEX-ES.
       OBJECT-COMPUTER. FLEX-ES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-STATIC-DATA.
           03 STATIC-MAXIMUM-PRINCIPLE    PIC 9(9)V99
                                VALUE 100000000.01.
           03 STATIC-ERRORS.
              05 FILLER                  PIC 99 VALUE 1.
              05 FILLER                  PIC X(80)
              VALUE 'PRINCIPLE AMOUNT IS NEGATIVE'.
              05 FILLER                  PIC 99 VALUE 2.
              05 FILLER                  PIC X(80)
              VALUE 'PRINCIPLE EXCEEDED MAXIMUM AMOUNT'.
              05 FILLER                  PIC 99 VALUE 3.
              05 FILLER                  PIC X(80)
              VALUE 'NEGATIVE INTEREST RATE'.
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
              VALUE 'LOAN TERM MUST BE BETWEEN 10,20,30 or 40 YEARS'.
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
       01  WS-INDICATORS-AND-FLAGS.
           03 VALIDATION-INDICATOR   PIC 9.
       01  WS-WORK-AMOUNTS.
           03 WS-NUMBER-OF-MONTHS    PIC 9(9)V99   COMP.
           03 WS-CALC-INTEREST       COMP-1.
      *     03 L                      COMP-1.
      *     03 C                      COMP-1.
      *     03 N                      PIC S9(5) COMP.
      *     03 P                      COMP-1.
       01  Loan             Pic 9(9)V99.
       01  Payment          Pic 9(9)V99.
       01  Interest         Pic 9(9)V9999.
       01  Number-Periods   Pic 999.
      *

       LINKAGE SECTION.
      *
       COPY EPSPDATA.

       PROCEDURE DIVISION USING EPSPDATA.
      *
       A000-MAINLINE.
      *    DISPLAY 'ENTERING EPSMPMT'
           MOVE 0 TO VALIDATION-INDICATOR.
           MOVE 0 TO WS-NUMBER-OF-MONTHS.
           PERFORM A100-VALIDATE-INPUT.

           IF VALIDATION-INDICATOR = 0
              PERFORM A200-CALCULATE-MONTH-PAYMENT
              PERFORM A300-TRY2
           ELSE
              PERFORM A999-RETURN-ERROR-TEXT
              DISPLAY EPSPDATA-RETURN-ERROR
           END-IF.
           GOBACK
           .
      *
       A100-VALIDATE-INPUT.
           MOVE SPACES TO EPSPDATA-RETURN-ERROR.
           DISPLAY 'EPSPDATA-PRINCIPLE-DATA:' EPSPDATA-PRINCIPLE-DATA
           IF EPSPDATA-PRINCIPLE-DATA > 0
              IF EPSPDATA-PRINCIPLE-DATA > STATIC-MAXIMUM-PRINCIPLE
                 MOVE 2 TO VALIDATION-INDICATOR
              END-IF
           ELSE
              MOVE 1 TO VALIDATION-INDICATOR
           END-IF
           .
           DISPLAY 'EPSPDATA-QUOTED-INTEREST-RATE:'
                  EPSPDATA-QUOTED-INTEREST-RATE
           IF VALIDATION-INDICATOR = 0
              IF EPSPDATA-QUOTED-INTEREST-RATE <= 0
                 MOVE 3 TO VALIDATION-INDICATOR
              ELSE
                 IF EPSPDATA-YEAR-MONTH-IND = 'Y'
                    COMPUTE WS-NUMBER-OF-MONTHS =
                               EPSPDATA-NUMBER-OF-YEARS * 12
                    DISPLAY 'MONTHS:' WS-NUMBER-OF-MONTHS
                 ELSE
                    MOVE EPSPDATA-NUMBER-OF-MONTHS TO
                            WS-NUMBER-OF-MONTHS
                 END-IF
              END-IF
           END-IF

           .
           IF VALIDATION-INDICATOR = 0
              PERFORM B001-LOAN-AMT-LIMITS
           END-IF
           .


           IF VALIDATION-INDICATOR = 0
              PERFORM B002-LOAN-YEARS-REGULATIONS
           END-IF
           .
           MOVE '000'                 TO EPSPDATA-CREDIT-SCORE

      * Credit Score calc

      *    DISPLAY 'QUOTED INTEREST BEFORE:'
      *              EPSPDATA-QUOTED-INTEREST-RATE
      *    DISPLAY 'PRINCIPLE:' EPSPDATA-QUOTED-INTEREST-RATE
      *    DISPLAY 'MONTHS:' WS-NUMBER-OF-MONTHS
      *    DISPLAY 'PAN:' EPSPDATA-PAN-NUMBER
      * new change

      *    /* Credit score logic start */
           PERFORM A110-GET-CREDIT-SCORE
           .

           EVALUATE TRUE
              WHEN EPSPDATA-CREDIT-SCORE >= 700
                MOVE 6 TO EPSPDATA-QUOTED-INTEREST-RATE
              WHEN EPSPDATA-CREDIT-SCORE >= 600
                MOVE 7 TO EPSPDATA-QUOTED-INTEREST-RATE
              WHEN EPSPDATA-CREDIT-SCORE >= 500
                MOVE 8 TO EPSPDATA-QUOTED-INTEREST-RATE
              WHEN EPSPDATA-CREDIT-SCORE >= 400
                MOVE 9 TO EPSPDATA-QUOTED-INTEREST-RATE
              WHEN OTHER
                MOVE 9 TO EPSPDATA-QUOTED-INTEREST-RATE
           END-EVALUATE
           .
      *    /* Credit score logic end */
      *    DISPLAY 'INTEREST CHANGED:' EPSPDATA-QUOTED-INTEREST-RATE
           COMPUTE WS-CALC-INTEREST =
                      (EPSPDATA-QUOTED-INTEREST-RATE / 100) / 12
      *    DISPLAY 'DUMMY LINES'
      *    DISPLAY 'MORE'
           .

       A110-GET-CREDIT-SCORE.
           EVALUATE EPSPDATA-PAN-NUMBER
               WHEN 'A111111111'
                   MOVE 650    TO EPSPDATA-CREDIT-SCORE
               WHEN 'A222222222'
                   MOVE 720    TO EPSPDATA-CREDIT-SCORE
               WHEN OTHER
                   MOVE 550    TO EPSPDATA-CREDIT-SCORE
           END-EVALUATE.


       A200-CALCULATE-MONTH-PAYMENT.
           DISPLAY 'INSIDE MONTH PAYMENT'
           COMPUTE EPSPDATA-RETURN-MONTH-PAYMENT
                   = EPSPDATA-PRINCIPLE-DATA *
                     (WS-CALC-INTEREST *
                     (1 + WS-CALC-INTEREST) ** WS-NUMBER-OF-MONTHS) /
                     (((1 + WS-CALC-INTEREST )
                                            ** WS-NUMBER-OF-MONTHS) - 1)
           .
      *     DISPLAY 'RETURN PAYMENT = ' EPSPDATA-RETURN-MONTH-PAYMENT.
      *     COMPUTE C = WS-CALC-INTEREST.
      *     COMPUTE N = WS-NUMBER-OF-MONTHS.
      *     COMPUTE L = EPSPDATA-PRINCIPLE-DATA.chg
      *     COMPUTE P = L * (C * (1 + C ) ** N)/(((1 + C) ** N) - 1).


      * DEAD CODE USED FOR TESTING
       A300-TRY2.
           MOVE EPSPDATA-PRINCIPLE-DATA TO Loan.
           COMPUTE Interest = EPSPDATA-QUOTED-INTEREST-RATE / 100.
           MOVE WS-NUMBER-OF-MONTHS TO Number-Periods.
           Compute Payment =
           Loan * Function Annuity((Interest / 12) Number-Periods)
           DISPLAY 'Verify Payment = ' Payment.

       A999-RETURN-ERROR-TEXT.
           MOVE ERROR-TEXT(VALIDATION-INDICATOR) TO
                                                 EPSPDATA-RETURN-ERROR
           GOBACK
           .
       B001-LOAN-AMT-LIMITS.
      **** Loan must be > 500 and < 500,000
           IF EPSPDATA-PRINCIPLE-DATA < 500 OR
              EPSPDATA-PRINCIPLE-DATA > 500000
              MOVE 7 TO VALIDATION-INDICATOR.

       B002-LOAN-YEARS-REGULATIONS.
      ***** Loan number of years must be between 1 and 40.
      *    IF EPSPDATA-NUMBER-OF-YEARS < 1 OR
      *       EPSPDATA-NUMBER-OF-YEARS > 40
      *       MOVE 6 TO VALIDATION-INDICATOR.
      ***** Loan number of years must be 10, 20, 30 or 40.
           IF EPSPDATA-NUMBER-OF-YEARS = 10  OR
              EPSPDATA-NUMBER-OF-YEARS = 20  OR
              EPSPDATA-NUMBER-OF-YEARS = 30  OR
              EPSPDATA-NUMBER-OF-YEARS = 40
           NEXT SENTENCE
           ELSE
              MOVE 8 TO VALIDATION-INDICATOR.
