//BGZOPUT  JOB <JOB PARAMETERS>
//*********************************************************************
//* JOB NAME: BGZOPUT                                                 *
//*                                                                   *
//*    Licensed materials - Property of IBM                           *
//*    5725-F21                                                       *
//*    COPYRIGHT IBM CORP 2021        ALL RIGHTS RESERVED.            *
//*                                                                   *
//*    US GOVERNMENT USERS RESTRICTED RIGHTS -                        *
//*    USE, DUPLICATION OR DISCLOSURE RESTRICTED                      *
//*    BY GSA ADP SCHEDULE CONTRACT WITH IBM CORP.                    *
//*                                                                   *
//* DESCRIPTION: THIS JCL WILL CAN BE USED TO PUT DATA SET VERSIONS   *
//*              OF FILES BACK INTO THE CLONED GIT REPO IF THEY HAVE  *
//*              BEEN CHANGED.                                        *
//*              SOME PANELS NEED TO BE STORED AS BINARY TO ROUND     *
//*              TRIP CORRECTLY WHICH IS WHY IT IS REQUIRED.          *
//*                                                                   *
//* CAUTION:     THIS IS NEITHER A JCL PROCEDURE NOR                  *
//*              A COMPLETE JOB.                                      *
//*              YOU WILL HAVE TO MAKE MODIFICATIONS BEFORE           *
//*              SUBMITTING THIS JOB.                                 *
//*                                                                   *
//* NOTES:                                                            *
//*                                                                   *
//* 1) Add the job parameters to meet your system requirements.       *
//*                                                                   *
//* 2) Change #hlqual to the appropriate high-level qualifier         *
//*    that complies with your site's naming standards.               *
//*                                                                   *
//* 3) Change #clonePath to the path containing the clone of the      *
//*    ispf git repository.                                           *
//*                                                                   *
//* 4) Change the ISP.SISP* and SYS1.SBPX* data sets to your sites    *
//*    standard for the ISPF and z/OS Unix data sets.                 *
//*                                                                   *
//* GENERAL NOTES:                                                    *
//*                                                                   *
//* 1) This job WILL complete with a return code 0.                   *
//*    You must check allocation messages to verify that the          *
//*    data sets are allocated and cataloged as expected.             *
//*                                                                   *
//*********************************************************************
//*
//*
//GITCOPY  EXEC PGM=IKJEFT01,DYNAMNBR=400,COND=(0,LT)
//*
//STEPLIB   DD DISP=SHR,DSN=ISP.SISPLOAD
//          DD DISP=SHR,DSN=ISP.SISPLPA
//ISPMLIB   DD DISP=SHR,DSN=ISP.SISPMENU
//          DD DISP=SHR,DSN=SYS1.SBPXMENU
//ISPSLIB   DD DISP=SHR,DSN=ISP.SISPSENU
//          DD DISP=SHR,DSN=ISP.SISPSLIB
//ISPPLIB   DD DISP=SHR,DSN=ISP.SISPPENU
//          DD DISP=SHR,DSN=SYS1.SBPXPENU
//ISPTLIB   DD UNIT=VIO,DISP=(NEW,PASS),SPACE=(CYL,(1,1,5)),
//             DCB=(LRECL=80,BLKSIZE=19040,DSORG=PO,RECFM=FB)
//          DD DSN=ISP.SISPTENU,DISP=SHR
//          DD DISP=SHR,DSN=SYS1.SBPXTENU
//ISPTABL   DD UNIT=VIO,DISP=(NEW,PASS),SPACE=(CYL,(1,1,5)),
//             DCB=(LRECL=80,BLKSIZE=19040,DSORG=PO,RECFM=FB)
//ISPPROF   DD UNIT=VIO,DISP=(NEW,PASS),SPACE=(CYL,(1,1,5)),
//             DCB=(LRECL=80,BLKSIZE=19040,DSORG=PO,RECFM=FB)
//ISPLOG    DD SYSOUT=*,
//             DCB=(LRECL=120,BLKSIZE=2400,DSORG=PS,RECFM=FB)
//SYSPROC   DD DSN=ISP.SISPCLIB,DISP=SHR
//          DD DSN=ISP.SISPEXEC,DISP=SHR
//SYSEXEC   DD DSN=SYS1.SBPXEXEC,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 OPUTX '#hlqual.SBGZGML' +
       '#clonePath/IDE/GitISPFClient/sbgzgml' +
       LC SUFFIX(gml) ASIS
 OPUTX '#hlqual.SBGZGMLI' +
       '#clonePath/IDE/GitISPFClient/sbgzgmli' +
       LC SUFFIX(gmli) ASIS
 OPUTX '#hlqual.SBGZEXEC' +
       '#clonePath/IDE/GitISPFClient/sbgzexec' +
       LC SUFFIX(rexx) ASIS
 OPUTX '#hlqual.SBGZMENU' +
       '#clonePath/IDE/GitISPFClient/sbgzmenu' +
       LC SUFFIX(menu) ASIS
 OPUTX '#hlqual.SBGZPENU' +
       '#clonePath/IDE/GitISPFClient/sbgzpenu' +
       LC SUFFIX(penu) ASIS
 OPUTX '#hlqual.SBGZPENU(BGZMAIN)' +
       '#clonePath/IDE/GitISPFClient/sbgzpenu/+
bgzmain.penu' +
       BIN LC
 OPUTX '#hlqual.SBGZPENU(BGZSLBRA)' +
       '#clonePath/IDE/GitISPFClient/sbgzpenu/+
bgzslbra.penu' +
       BIN LC
 OPUTX '#hlqual.SBGZPENU(BGZSLREP)' +
       '#clonePath/IDE/GitISPFClient/sbgzpenu/+
bgzslrep.penu' +
       BIN LC
 OPUTX '#hlqual.SBGZPENU(BGZSLUDL)' +
       '#clonePath/IDE/GitISPFClient/sbgzpenu/+
bgzsludl.penu' +
       BIN LC
 OPUTX '#hlqual.SBGZPENU(BGZCFCLO)' +
       '#clonePath/IDE/GitISPFClient/sbgzpenu/+
bgzcfclo.penu' +
       BIN LC
 OPUTX 'DOHERTL.D210803.SBGZTLIB(BGZKEYS)' +
       '#clonePath/IDE/GitISPFClient/sbgztlib/+
bgzkeys.gmlkey' +
       BIN LC
/*
//*
