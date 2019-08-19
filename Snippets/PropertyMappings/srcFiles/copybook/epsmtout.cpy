      *****************************************************************
      *                                                               *
      * Licensed Materials - Property of IBM                          *
      *                                                               *
      * EPSMTOUT.cpy   bla                                            *
      *                        modif                                 *
      * Â© Copyright IBM Corporation 2018                             *
      * U.S. Government Users Restricted Rights:                      *
      *      Use, duplication or disclosure                           *
      *      restricted by GSA ADP Schedule Corp.                     *
      *                                                               *
      *****************************************************************


      * OUTPUT for Getting Companies and Monthly Payments
          03  MORTGAGE-COMPANY-TABLE.
              05 ROW-COUNT                     PIC 9(3) COMP.
              05 MORT-TABLE-REC OCCURS 8 TIMES.
                 10 MORT-COMPANY               PIC X(24).
                 10 MORT-PHONE-NUM             PIC X(13).
                 10 MORT-RATE                  PIC 9(3)V99.
                 10 MORT-LOAN                  PIC 9(10)V99.
                 10 MORT-YEARS                 PIC 9(2).
          03 EPSPCOM-RETURN-MONTH-PAYMENT
                                      PIC S9(7)V99 COMP.
          03 EPSPCOM-ERRMSG           PIC X(80).
          03 EPSPCOM-PROGRAM-RETCODE  PIC 9(4).
             88 EPS02-REQUEST-SUCCESS VALUE 0.
          03 EPSPCOM-PROGRAM-RETCODE-RDF
                  REDEFINES EPSPCOM-PROGRAM-RETCODE
                                      PIC X(4).
          03 EPSPCOM-CREDIT-SCORE     PIC X(3).
