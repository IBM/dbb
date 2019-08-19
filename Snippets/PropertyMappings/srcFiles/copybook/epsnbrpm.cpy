      *****************************************************************
      *                                                             *
      * Licensed Materials - Property of IBM                          *
      *                                                               *
      * EPSNBRPM.cpy                                                  *
      *                                                               *
      * Â© Copyright IBM Corporation 2012                             *
      * U.S. Government Users Restricted Rights:                      *
      *      Use, duplication or disclosure                           *
      *      restricted by GSA ADP Schedule Corp.1                    *
      *                                                               *
      *****************************************************************

       01  EPS-NUMBER-VALIDATION.
      * INPUT
           03 EPSPARM-VALIDATE-DATA     PIC X(13).
           03 EPSPARM-MAX-LENGTH        PIC 99.
           03 EPSPARM-RULE-FLAG         PIC 9.
              88 EPSPARM-RULE-FLAG-NONE   VALUE 0.
              88 EPSPARM-RULE-FLAG-YEARS  VALUE 1.
              88 EPSPARM-RULE-FLAG-AMOUNT VALUE 2.
      * OUTPUT
           03 EPSPARM-NUMBER            PIC 9(13).
           03 EPSPARM-DECIMAL           PIC V9(13).
           03 EPSPARM-BINARY-NUMBER     PIC 9(9)V99 COMP.
           03 EPSPARM-RETURN-ERROR.
              05 EPSPARM-RETURN-ERROR-RC   PIC 99.
              05 EPSPARM-RETURN-ERROR-TEXT PIC X(80).
