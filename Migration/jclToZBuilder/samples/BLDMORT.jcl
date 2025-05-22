//BLDMORT JOB REGION=0M
//*********************************************************************
//* Licensed materials - Property of IBM                              *
//* Copyright IBM Corp. 2025                                          *
//* All rights reserved                                               *
//* US Government users restricted rights  -  Use, duplication or     *
//* disclosure restricted by GSA ADP schedule contract with IBM Corp. *
//*                                                                   *
//* IBM Dependency Based Build                                        *
//*                                                                   *
//* FUNCTION:                                                         *
//* This JCL compiles and link-edits the MortgageApplication's        *
//* cobol/epsmpmt.cbl program. The MortgageApplication is shipped     *
//* with DBB and can be found in the $DBB_HOME/samples directory.     *
//*                                                                   *
//* This JCL is provided for use in a tutorial on the DBB doc center. *
//*                                                                   *
//*********************************************************************
//**********************************************************************
//* Compile Mortage Application cobol/epsmpmt.cbl
//**********************************************************************
//COMP     EXEC PGM=IGYCRCTL
//* INPUT/OUTPUT
//SYSIN    DD DISP=SHR,DSN=BUILDER.MORT.COBOL(EPSMPMT)
//SYSLIN   DD DISP=SHR,DSN=BUILDER.MORT.OBJ(EPSMPMT)
//SYSPRINT DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=133,BLKSIZE=133),DISP=NEW
//* DEPENDENCIES
//STEPLIB  DD DISP=SHR,DSN=IGY.V6R1M0.SIGYCOMP
//SYSLIB   DD DISP=SHR,DSN=BUILDER.MORT.COPY
//* TEMP DATASETS
//SYSMDECK DD SPACE=(CYL,(5,5)),UNIT=VIO,                             
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT1   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT2   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT3   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT4   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT5   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT6   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT7   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT8   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT9   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT10  DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT11  DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT12  DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT13  DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT14  DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT15  DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT16  DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSUT17  DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//**********************************************************************
//* LinkEdit Mortage Application cobol/epsmpmt.cbl
//**********************************************************************
//LINK     EXEC PGM=IEWBLINK,PARM='MAP,RENT'
//* INPUT/OUTPUT
//SYSLIN   DD DISP=SHR,DSN=BUILDER.MORT.OBJ(EPSMPMT)
//SYSLMOD  DD DISP=SHR,DSN=BUILDER.MORT.LOAD(EPSMPMT)
//SYSPRINT DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=133,BLKSIZE=133),DISP=NEW
//SYSUT1   DD SPACE=(CYL,(5,5)),UNIT=VIO,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),DISP=NEW
//SYSLIB   DD DISP=SHR,DSN=BUILDER.MORT.OBJ
//         DD DISP=SHR,DSN=CEE.SCEELKED