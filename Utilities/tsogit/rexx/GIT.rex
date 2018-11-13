/********************************REXX********************************/
/*                                                                  */
/* NAME:C0CZB    TITLE: TSO GIT                                     */
/*                                                                  */
/**********************************************************************/
/*                    LOG OF CHANGES                                  */
/*                                                                    */
/* NAME   VER# CCYY/MM/DD ANALYST         REASON                      */
/* -------  -  ---------- -------- ---------------------------------  */
/* A00         2018/09/24 BALAJI   NEW CLIST FOR GIT COMMANDS         */
/**********************************************************************/
TRACE I
 "ISPEXEC CONTROL ERRORS RETURN"
CLIB = 'C03058.ISPCLIB'
"ALTLIB ACTIVATE APPLICATION(CLIST) DATASET('"CLIB"')"
     "ISPEXEC SELECT CMD(C0CZB)"
 EXIT