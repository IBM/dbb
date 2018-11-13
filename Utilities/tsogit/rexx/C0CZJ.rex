/*REXX*/
/**************************REXX************************************\
* NAME:  C0CU3                                                     *
*    DESCRIPTION: FEEDBACK ON THE TASK                             *
********************************************************************
*                    LOG OF CHANGES                                *
* VER  MM/DD/YYYY  ANALYST   DESCRIPTION                           *
* ---  ----------  --------  -------   -----------                 *
* A00  10/06/2016  LAKSHMI   INITIAL SETUP                         *
\******************************************************************/
PARSE UPPER ARG PARM 'USRID('USRID')'
PARSE UPPER ARG PARM 'GITFNC('GITFNC')'

CALL A000_INSERT_LOG

EXIT;

A000_INSERT_LOG:

     QUERY = " INSERT INTO DDFHA0D.MNFRMCD.GIT_TRK_TBL   ",
              " VALUES (                                 ",
              " '"USRID"',                               ",
              " '"GITFNC"',                              ",
              "  CURRENT TIMESTAMP )                     "

     CALL RUN_QUERY QUERY
     IF SQLCA.SQLCODE ¬= 0 THEN
        SAY "INSERT SQLCODE =>" SQLCA.SQLCODE

RETURN
/********************************************************************/
/* SETRXSQL                                                         */
/********************************************************************/
SETRXSQL:

  CC = 0
  CALL RXSUBCOM 'ADD', 'SQL2', 'RXTASQL2'

  IF RC > 4 THEN
    CC = RC

RETURN CC

/********************************************************************/
/* RUN QUERY                                                        */
/********************************************************************/

RUN_QUERY:
ARG QUERY
CALL SETRXSQL
X = DB2SET('T','O')

ADDRESS SQL2 ""QUERY

IF ((SQLCA.SQLCODE ¬= 0) & (SQLCA.SQLCODE ¬= 100)) THEN
DO
  SAY "PLEASE SEND A NOTE TO OR TO DL-TEAM-INFINITYSTONE"
  SAY "SAYING THE LOG TABLE UPDATE FAILED               "
  SAY QUERY
END
X = DB2SET('T','C')
RETURN

