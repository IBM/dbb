000100 01  MORTGAGE-COMPANY-INFO.
000200     03 MORT-FILE-COMPANY               PIC X(24).
000300     03 MORT-FILE-PHONE-NUM             PIC X(13).
000400     03 MORT-FILE-RATE                  PIC 9(3)V99.
000401     03 MORT-FILE-RATE-RDF    REDEFINES MORT-FILE-RATE
000403                                        PIC X(5).
000500     03 MORT-FILE-LOAN                  PIC 9(10)V99.
000501     03 MORT-FILE-LOAN-RDF    REDEFINES MORT-FILE-LOAN
000503                                        PIC X(12).
000600     03 MORT-FILE-YEARS                 PIC 9(2).
      * Comment 10
