/* REXX */
/* DEFAULT IT TO NO MAINFRAME CD WILL PULL IN COMPILED LOAD */
COMPLNK =  'N'
LIFE = 'N'
 
CALL GET_INPUT
CALL GET_LIST_LVL
 
EXIT
 
GET_INPUT:
 
 "EXECIO * DISKR RQSTPRM (STEM INPARM. FINIS)"
 IF RC \= 0 THEN
    DO
      SAY 'FILE ALLOCATION ERROR'
    END
 IF INPARM.0 \= 0 THEN
    DO
      USRID =  STRIP(INPARM.1,T)
      TSTMP =  STRIP(INPARM.2,T)
    END
 
RETURN
 
GET_LIST_LVL:
 
    PDSPFX = 'C03058.MFCD.ISS.UPLD.'USRID'.'TSTMP
    DROP INTEXT.   
    X=OUTTRAP("A.")   
    ADDRESS TSO   
    "LISTC LVL('"PDSPFX"')"   
    X=OUTTRAP("OFF")   
    N=A.0   
    DO I = 1 TO N   
       PARSE VALUE A.I WITH 'NONVSAM ------- 'PDSNAME         
       PARSE VALUE PDSNAME WITH PDS1'.'PDS2'.'PDS3'.'PDS4'.'ISSTYPE
       CALL GET_ISS_TYPE
       CALL GEN_JCL_2_SUBMIT
    END I   
 
RETURN 
 
GET_ISS_TYPE:
 
    SELECT
          WHEN ISSTYPE = 'PLI' THEN
               DO
                  PDS = 'Y'
                  LG600DS = 'PIC.NONRMS.LG600'
               END
          WHEN ISSTYPE = 'CBL' THEN
               DO
                  PDS = 'Y'
                  PDSDS = PDSNAME
                  LG600DS = 'PIC.NONRMS.LG600'
               END
          WHEN ISSTYPE = 'PSB' THEN
               DO
                  PSB = 'Y'
                  PSBDS = PDSNAME
                  LG600DS = 'PIC.NONRMS.PSB.LG600'
               END
          WHEN ISSTYPE = 'DBD' THEN
               DO
                  DBD = 'Y'
                  DBDDS = PDSNAME
                  LG600DS = 'PIC.NONRMS.DBD.LG600'
               END
          WHEN ISSTYPE = 'DMC' THEN
               DO
                  DMC = 'Y'
                  DMCDS = PDSNAME
               END
          OTHERWISE
               DO
                  UPLDDS = PDSNAME
                  LG600DS = 'PIC.NONRMS.LG600'
               END
     END
 
RETURN
GEN_JCL_2_SUBMIT:
 
 TEMP_PS = 'C03058.MFCD.ISS.RQST.'SUBSTR(TIME('L'),10,6)
 ADDRESS TSO "ALLOC F(ACCESS) DA('"TEMP_PS"') NEW",
             "SPACE(1,1) CYL",
             "RETPD(1) LRECL(80) RECFM(F B)",
             "BLKSIZE(0) DSORG(PS) NEW"
 ADDRESS ISPEXEC "LIBDEF ISPFILE DATASET ",
                 " ID('"TEMP_PS"')"
  ADDRESS ISPEXEC "FTINCL KBB0O"
  ADDRESS ISPEXEC "FTCLOSE"
  ADDRESS TSO "SUBMIT  '"TEMP_PS"'"
 
  ADDRESS TSO "FREE F(ACCESS)"
  ADDRESS ISPEXEC "LIBDEF ISPFILE"
 
RETURN