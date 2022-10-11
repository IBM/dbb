
ASAM1    AMODE 31
ASAM1    CSECT
         USING ASAM1,R15
* *************************************************
*  SAMPLE PROGRAM ASAM1
*     AUTHOR: DOUG STOUT
*             IBM CORPORATION
*
*  A SIMPLE PROGRAM THAT:
*   - READS A QSAM FILE
*   - WRITES THE FIRST 80 BYTES OF EACH RECORD TO AN OUTPUT FILE
*     IN CHARACTER AND HEX
*
*  PARAMETERS PASSED FROM CALLING PROGRAM:
*    NONE
*
*  FILES:
*    1. INPUT FILE IS QSAM AND HAS DD NAME = FILEIN
*         LRECL = 80
*    2. OUTPUT FILE IS QSAM OR SYSPRINT AND HAS DD NAME = FILEOUT
*         LRECL = 80
*
************************************************************
*                                  AT ENTRY, R13 = REGISTER SAVE AREA
*                                            R14 = RETURN ADDR
*                                            R15 = ADDR OF THIS PROGRAM
START    STM   R14,R12,12(R13)     SAVE REGISTERS IN PASSED SAVE AREA
         DROP  R15                 NO LONGER NEEDED AS BASE REGISTER
         LR    R12,R15             USE R12 AS THE BASE REGISTER
         USING ASAM1,R12           "
         LA    R14,SAVEAREA        R14 = ADDR OF NEW SAVE AREA
         ST    R13,4(R14)          STORE PREVIOUS SAVE AREA ADDR
         ST    R14,8(R13)          STORE NEW SAVE AREA ADDRESS
         LR    R13,R14             R13 = ADDR OF NEW SAVE AREA
         B     MAINLINE
         DS    0H
         DC    CL32'******** PROGRAM ASAM1 *********'
MAINLINE BAL   R11,OPENFILS
         BAL  R11,MAINLOOP
         BAL   R11,CLOSFILS
*
         MVC  STATUS,=C'RETURNING TO CALLING PROGRAM  '
*
RETURN00 L     R15,RETCODE
         L     R13,4(R13)          R13 = ADDR OF PREVIOUS SAVE AREA
         ST    R15,16(R13)         SAVE RETURN CODE
         LM    R14,R12,12(R13)     RESTORE REGISTERS (EXCEPT 13)
         BR    R14                 RETURN TO CALLER
*******************************************
* PROCEDURE MAINLOOP
*
MAINLOOP ST    R11,MAINLSAV      SAVE RETURN ADDRESS
*     * READ INPUT RECORD
MAINLTOP BAL   R11,READIN
*     * CHECK FOR END-OF-FILE
         CLC   EOFFLAG,=X'FF'      END OF FILE?
         BE    MAINLEX               IF YES - EXIT MAIN LOOP
*     * CALL SUBPROGRAM TO GET HEX CHARACTERS
         MVC   STATUS,=C'CALLING SUBPROGRAM ASAM2      '
         LA    R13,SAVEAREA
****     LINK  EP=ASAM2,(DATALEN,INREC,HEXTOP,HEXBOT),VL=1
*  FOLLOWING LINES COMMENTED OUT WHEN LOAD CHANGED TO STATIC CALL
*****    LOAD  EP=ASAM2
*****    LTR   R15,R15
*****    BNZ   LOADERR
*****    LA    R13,SAVEAREA
*****    LA    R1,PARMLIST     R1 = PARM LIST
*****    LR    R15,R0          R0 = ADD OF LOADED PROGRAM
*****    BALR  R14,R15         BRANCH TO SUBPROGRAM
         CALL  ASAM2,(DATALEN,INREC,HEXTOP,HEXBOT),VL
*     * WRITE LINE: 'RECORD NUMBER NNNNNN'
         UNPK  DIAGRECT,RECCOUNT   FORMAT RECORD COUNT
         OI    DIAGRECT+9,X'F0'
         MVC   DIAGREC,OUTLINE1     REC NUM LINE
         BAL   R11,WRITEOUT
*     * WRITE RULE LINES   ....5...10...15...20...
         MVC   DIAGREC,OUTLINE2
         BAL   R11,WRITEOUT
         MVC   DIAGREC,OUTLINE3
         BAL   R11,WRITEOUT
*     * WRITE DATA LINE 1: (CHARACTER FORMAT)
         MVC   DIAGREC,INREC
         BAL   R11,WRITEOUT
*     * WRITE DATA LINE 2: (TOP HEX LINE)
         MVC   DIAGREC,HEXTOP
         BAL   R11,WRITEOUT
*     * WRITE DATA LINE 3: (BOTTOM HEX LINE)
         MVC   DIAGREC,HEXBOT
         BAL   R11,WRITEOUT
*     * WRITE BLANK LINE
         MVC   DIAGREC,BLANKS
         BAL   R11,WRITEOUT
*     * GO BACK TO TOP OF LOOP
         B     MAINLTOP
*
LOADERR  WTO   '* ASAM1: ERROR LOADING PROGRAM ASAM2 '
         MVI   EOFFLAG,X'FF'
MAINLEX  L     R11,MAINLSAV
         BR    R11                    RETURN TO MAINLINE LOGIC
*
*******************************************
* PROCEDURE OPENFILS
*
OPENFILS ST    R11,OPENFSAV
         MVC  STATUS,=C'IN OPENFILS SUBROUTINE        '
         SLR   R15,R15                SET DEFAULT RETURN CODE TO ZERO
         OPEN  (FILEOUT,OUTPUT)       OPEN DDNAME
         LTR   R15,R15                OPEN RC = 0 ?
         BNZ   BADOPENI                 IF NO - THEN ERROR
         OPEN  (FILEIN,INPUT)         OPEN DDNAME
         LTR   R15,R15                OPEN RC = 0 ?
         BNZ   BADOPENI                 IF NO - THEN ERROR
         L     R11,OPENFSAV
         BR    R11                    RETURN TO MAINLINE LOGIC
BADOPENI WTO   '* ASAM1: ERROR OPENING INPUT FILE    '
         ST    R15,RETCODE
         B     RETURN00
BADOPENO WTO   '* ASAM1: ERROR OPENING OUTPUT FILE   '
         ST    R15,RETCODE
         B     RETURN00
*******************************************
* PROCEDURE CLOSFILS
*
CLOSFILS ST    R11,CLOSFSAV      SAVE RETURN ADDRESS
         MVC   STATUS,=C'IN CLOSFILS PROCEDURE         '
         CLOSE FILEOUT
         CLOSE FILEIN
         L     R11,CLOSFSAV
         BR    R11               RETURN
*******************************************
* PROCEDURE READIN
*
READIN   ST    R11,READISAV
         MVC   STATUS,=C'IN READIN PROCEDURE           '
         SLR   R15,R15                SET DEFAULT RETURN CODE TO ZERO
         GET   FILEIN,INREC           READ INPUT RECORD
         AP    RECCOUNT,=PL1'1'       INCREMENT RECORD COUNTER
         B     READIEX
READIEOF MVI   EOFFLAG,X'FF'
READIEX  L     R11,READISAV
         BR    R11
*
*******************************************
* PROCEDURE WRITEOUT
*
WRITEOUT ST    R11,WRITOSAV
         MVC   STATUS,=C'IN WRITEOUT PROCEDURE         '
         PUT   FILEOUT,DIAGREC         WRITE OUTPUT RECORD
         L     R11,WRITOSAV
         BR    R11
********************************************************
* STORAGE AREAS
*
EYECATCH DC    CL32'*** PROGRAM ASAM1 DATA AREAS ***'
FILECC   DC    H'0'                  MAX RETURN CODE FROM FILE OPENS
RECCOUNT DC    PL4'0'                INPUT RECORD COUNT
PROCCNT  DC    PL4'0'                PROCEDURE COUNT
STATUS   DC    CL30' '               CURRENT PROGRAM STATUS
EOFFLAG  DC    XL1'00'               END OF INPUT FILE FLAG
DATALEN  DC    F'80'                 LENGTH OF DATA PASSED TO SUBPGM
INREC    DC    CL80' '               INPUT RECORD
DIAGREC   DC    CL80' '               OUTPUT RECORD
HEXTOP   DC    CL80' '               TOP ROW OF HEX DATA
HEXBOT   DC    CL80' '               BOTTOM ROW OF HEX DATA
RETCODE  DC    F'0'                  DEFAULT RETURN CODE IS ZERO
MAINLSAV DC    F'0'
OPENFSAV DC    F'0'
CLOSFSAV DC    F'0'
READISAV DC    F'0'
WRITOSAV DC    F'0'
*
*  PARAMETER LIST FOR SUBPGM ASAM2
PARMLIST EQU   *
         DC    AL4(DATALEN)
         DC    AL4(INREC)
         DC    AL4(HEXTOP)
         DC    AL4(HEXBOT)                  SET HIGH ORDER BIT ON
*                                           TO FLAG END OF PARM LIST
OUTLINE1 DS    0CL80
         DC    CL14'RECORD NUMBER '
DIAGRECT DC    CL10'          '
         DC    56C' '
*
OUTLINE2 DS    0CL80
         DC    CL40'....0....1....1....2....2....3....3....4'
         DC    CL40'....4....5....5....6....6....7....7....8'
*
OUTLINE3 DS    0CL80
         DC    CL40'....5....0....5....0....5....0....5....0'
         DC    CL40'....5....0....5....0....5....0....5....0'
*
BLANKS   DC    80C' '
*
*        LTORG
SAVEAREA DC    18F'0'
********************************************************
* FILE DEFINITIONS
*
FILEOUT  DCB   DSORG=PS,RECFM=FB,MACRF=(PM),LRECL=80,                  X
               DDNAME=FILEOUT
FILEIN   DCB   DSORG=PS,RECFM=FB,MACRF=(GM),LRECL=80,                  X
               DDNAME=FILEIN,EODAD=READIEOF
*
* REGISTER EQUATES
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         END