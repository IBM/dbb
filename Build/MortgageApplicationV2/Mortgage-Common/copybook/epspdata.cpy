       01  EPSPDATA.
      * INPUT
           03 EPSPDATA-PRINCIPLE-DATA   PIC S9(9)V99 COMP.
           03 EPSPDATA-NUMBER-OF-YEARS  PIC S9(4)    COMP.
           03 EPSPDATA-NUMBER-OF-MONTHS PIC S9(4)    COMP.
           03 EPSPDATA-QUOTED-INTEREST-RATE
                                        PIC S9(2)v9(3) COMP.
           03 EPSPDATA-YEAR-MONTH-IND   PIC X.
      * OUTPUT
           03 EPSPDATA-RETURN-MONTH-PAYMENT
                                        PIC S9(7)V99 COMP.
           03 EPSPDATA-RETURN-ERROR     PIC X(80).

