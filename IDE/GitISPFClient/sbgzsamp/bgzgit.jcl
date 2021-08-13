//BGZGIT   JOB <JOB PARAMETERS>
//*********************************************************************
//* JOB NAME: BGZGIT                                                  *
//*                                                                   *
//*    Licensed materials - Property of IBM                           *
//*    5725-F21                                                       *
//*    COPYRIGHT IBM CORP 1993, 2019  ALL RIGHTS RESERVED.            *
//*                                                                   *
//*    US GOVERNMENT USERS RESTRICTED RIGHTS -                        *
//*    USE, DUPLICATION OR DISCLOSURE RESTRICTED                      *
//*    BY GSA ADP SCHEDULE CONTRACT WITH IBM CORP.                    *
//*                                                                   *
//* DESCRIPTION: THIS JCL WILL ALLOCATE TARGET AND DISTRIBUTION       *
//*              LIBRARIES FOR                                        *
//*              IBM DBB Git ISPF Client                              *
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
//* 3) Change DSP=CATLG on the EXEC statements to the appropriate     *
//*    final disposition of the data sets if you choose not to use    *
//*    the default.                                                   *
//*                                                                   *
//* 4) Change #volsert to the volser for the target libraries.        *
//*                                                                   *
//*    If you want SMS to decide where to place the data sets, you    *
//*    should comment out all the lines that reference TARGVOL.       *
//*                                                                   *
//* 5) Change #clonePath to the path containing the clone of the      *
//*    ispf git repository.                                           *
//*                                                                   *
//* 6) Change the ISP.SISP* and SYS1.SBPX* data sets to your sites    *
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
//GITALLOC PROC UNIT=SYSALLDA,
//            TARGPRE=#hlqual,
//            TARGVOL=#volsert,
//            DSP=CATLG
//*
//ALLOC    EXEC PGM=IEFBR14
//*
//*********************************************************************
//* ALLOCATE TARGET LIBRARIES                                         *
//*********************************************************************
//*
//SBGZGML  DD DSN=&TARGPRE..SBGZGML,
//            UNIT=&UNIT,
//            VOL=SER=&TARGVOL,
//            DISP=(NEW,&DSP),
//            RECFM=FB,
//            LRECL=80,
//            BLKSIZE=0,
//            DSNTYPE=LIBRARY,
//            SPACE=(TRK,(10,1,5))
//*
//SBGZGMLI DD DSN=&TARGPRE..SBGZGMLI,
//            UNIT=&UNIT,
//            VOL=SER=&TARGVOL,
//            DISP=(NEW,&DSP),
//            RECFM=FB,
//            LRECL=80,
//            BLKSIZE=0,
//            DSNTYPE=LIBRARY,
//            SPACE=(TRK,(10,5,5))
//*
//SBGZEXEC DD DSN=&TARGPRE..SBGZEXEC,
//            UNIT=&UNIT,
//            VOL=SER=&TARGVOL,
//            DISP=(NEW,&DSP),
//            RECFM=FB,
//            LRECL=80,
//            BLKSIZE=0,
//            DSNTYPE=LIBRARY,
//            SPACE=(TRK,(2,5,2))
//*
//SBGZMENU DD DSN=&TARGPRE..SBGZMENU,
//            UNIT=&UNIT,
//            VOL=SER=&TARGVOL,
//            DISP=(NEW,&DSP),
//            RECFM=FB,
//            LRECL=80,
//            BLKSIZE=0,
//            DSNTYPE=LIBRARY,
//            SPACE=(TRK,(10,2,10))
//*
//SBGZPENU DD DSN=&TARGPRE..SBGZPENU,
//            UNIT=&UNIT,
//            VOL=SER=&TARGVOL,
//            DISP=(NEW,&DSP),
//            RECFM=FB,
//            LRECL=80,
//            BLKSIZE=0,
//            DSNTYPE=LIBRARY,
//            SPACE=(TRK,(50,5,30))
//*
//SBGZTLIB DD DSN=&TARGPRE..SBGZTLIB,
//            UNIT=&UNIT,
//            VOL=SER=&TARGVOL,
//            DISP=(NEW,&DSP),
//            RECFM=FB,
//            LRECL=80,
//            BLKSIZE=0,
//            DSNTYPE=LIBRARY,
//            SPACE=(TRK,(5,2,2))
//*
//COPY     EXEC PGM=IEFBR14
//*
//*********************************************************************
//* ALLOCATE TARGET LIBRARIES                                         *
//*********************************************************************
//*
//GITALLOC PEND
//*
//ALLOCATE EXEC GITALLOC
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
 OGETX '#clonePath/IDE/GitISPFClient/sbgzgml' +
       '#hlqual.SBGZGML'  LC SUFFIX ASIS
 OGETX '#clonePath/IDE/GitISPFClient/sbgzgmli' +
       '#hlqual.SBGZGMLI' LC SUFFIX ASIS
 OGETX '#clonePath/IDE/GitISPFClient/sbgzexec' +
       '#hlqual.SBGZEXEC' LC SUFFIX ASIS
 OGETX '#clonePath/IDE/GitISPFClient/sbgzmenu' +
       '#hlqual.SBGZMENU' LC SUFFIX ASIS
 OGETX '#clonePath/IDE/GitISPFClient/sbgzpenu' +
       '#hlqual.SBGZPENU' LC SUFFIX ASIS
 OGETX '#clonePath/IDE/GitISPFClient/sbgzpenu/bgzmain.penu' +
       '#hlqual.SBGZPENU(BGZMAIN)' BIN LC SUFFIX
 OGETX '#clonePath/IDE/GitISPFClient/sbgzpenu/bgzslbra.penu' +
       '#hlqual.SBGZPENU(BGZSLBRA)' BIN LC SUFFIX
 OGETX '#clonePath/IDE/GitISPFClient/sbgzpenu/bgzslrep.penu' +
       '#hlqual.SBGZPENU(BGZSLREP)' BIN LC SUFFIX
 OGETX '#clonePath/IDE/GitISPFClient/sbgzpenu/bgzsludl.penu' +
       '#hlqual.SBGZPENU(BGZSLUDL)' BIN LC SUFFIX
 OGETX '#clonePath/IDE/GitISPFClient/sbgzpenu/bgzcfclo.penu' +
       '#hlqual.SBGZPENU(BGZCFCLO)' BIN LC SUFFIX
 OGETX '#clonePath/IDE/GitISPFClient/sbgztlib/bgzkeys.gmlkey' +
       '#hlqual.SBGZTLIB(BGZKEYS)' BIN LC SUFFIX
/*
//*
