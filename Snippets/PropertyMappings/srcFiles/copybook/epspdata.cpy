      *****************************************************************
      *                                                               *
      * Licensed Materials - Property of IBM                          *
      *                                                               *
      * EPSPDATA.cpy                                                  *
      *                                                               *
      * Â© Copyright IBM Corporation 2012                              *
      * U.S. Government Users Restricted Rights:                      *
      *      Use, duplication or disclosure                           *
      *      restricted by GSA ADP Schedule Corp.                     *
      *                                                               *
      *****************************************************************

       01  EPSPDATA.
      * INPUT for calculating mortgage
           03 EPSPDATA-PRINCIPLE-DATA   PIC S9(9)V99 COMP.
           03 EPSPDATA-NUMBER-OF-YEARS  PIC S9(4)    COMP.
           03 EPSPDATA-NUMBER-OF-MONTHS PIC S9(4)    COMP.
           03 EPSPDATA-QUOTED-INTEREST-RATE
                                        PIC S9(2)v9(3) COMP.
           03 EPSPDATA-YEAR-MONTH-IND   PIC X.
           03 EPSPDATA-PAN-NUMBER       PIC X(10).
           03 EPSPDATA-CREDIT-SCORE     PIC X(03).
      * OUTPUT of mortgage calculation
           03 EPSPDATA-RETURN-MONTH-PAYMENT
                                        PIC S9(7)V99 COMP.
           03 EPSPDATA-RETURN-ERROR     PIC X(80).
