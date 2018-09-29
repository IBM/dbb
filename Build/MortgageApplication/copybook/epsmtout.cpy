      * OUTPUT
          10 EPSPCOM-RETURN-MONTH-PAYMENT
                                      PIC S9(7)V99 COMP.
          10 EPSPCOM-ERRMSG           PIC X(80).
          10 EPSPCOM-PROGRAM-RETCODE  PIC 9(4).
             88 EPS02-REQUEST-SUCCESS VALUE 0.
          10 EPSPCOM-PROGRAM-RETCODE-RDF
                  REDEFINES EPSPCOM-PROGRAM-RETCODE
                                      PIC X(4).
