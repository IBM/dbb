       01  EPS-NUMBER-VALIDATION.
      * INPUT - change 7
           03 EPSPARM-VALIDATE-DATA     PIC X(13).
           03 EPSPARM-MAX-LENGTH        PIC 99.
      * OUTPUT
           03 EPSPARM-NUMBER            PIC 9(13).
           03 EPSPARM-DECIMAL           PIC V9(13).
           03 EPSPARM-BINARY-NUMBER     PIC 9(9)V99 COMP.
           03 EPSPARM-RETURN-ERROR      PIC X(80).

