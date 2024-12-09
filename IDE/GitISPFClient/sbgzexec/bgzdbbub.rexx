/* REXX */
/*%STUB CALLCMD*/
/*********************************************************************/
/*                                                                   */
/* IBM ISPF Git Interface                                            */
/*                                                                   */
/*********************************************************************/
/*                                                                   */
/* NAME := BGZDBBUB                                                  */
/*                                                                   */
/* DESCRIPTIVE NAME := GIT ISPF Client DBB user build request        */
/*                                                                   */
/* FUNCTION :=                                                       */
/*                                                                   */
/*                                                                   */
/* CALLED BY : BGZUSLST                                              */
/*                                                                   */
/* PARAMETERS : BGZUSREP BGZUSLOC                                    */
/*                                                                   */
/* OUTPUT := None                                                    */
/*                                                                   */
/* Change History                                                    */
/*                                                                   */
/* Who   When     What                                               */
/* ----- -------- -------------------------------------------------- */
/* XH    03/09/19 Initial version                                    */
/* MDalb 04/17/20 Enhancements to support zAppBuild                  */
/* LD    11/09/24 Enhance user build support                         */
/*                                                                   */
/*********************************************************************/

   Parse Arg BGZUSREP BGZUSLOC BGZUSSDR BGZUSFIL
   /* Need to create a permanent ISPF table to store dbb user build options */
   UPPERCASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   lowercase = 'abcdefghijklmnopqrstuvwxyz'

   Address ISPEXEC
   'TBOPEN BGZDBBUB'
   /* If it didn't exist yet, create it. */
   If RC = 8 Then
   Do
     'TBCREATE BGZDBBUB',
     'KEYS(BGZUSREP,BGZUSLOC)',
     'NAMES(BGZBLSCR,BGZBLSAN,BGZBLWRK,BGZBLHLQ,',
           'BGZBLLOG,BGZBLDDP,BGZDODMN) WRITE'
   End

   'VGET (BGZBRANC) PROFILE'
   'VGET (BGZJAVAH,BGZDBBH,BGZDBBC) SHARED'
   'VGET (BGZDMNHO,BGZDMNPO) SHARED'
   x = lastPos('/',BGZUSLOC)
   sandbox = Substr(BGZUSLOC,1,x-1)
   appname = Substr(BGZUSLOC,x+1)
   fullPath = BGZUSSDR'/'BGZUSFIL
   relativePathIndex = Pos(appname'/', fullpath)
   relativePath = Substr(fullpath, relativePathIndex)

   BGZBLSCR = ''
   BGZBLSAN = sandbox
   BGZBLWRK = ''
   BGZBLHLQ = ''
   BGZBLLOG = '/'
   BGZBLDDP = ' '
   BGZDODMN = '/'

   'TBGET BGZDBBUB'
   GetDBB_RC = RC
   DoDBBub = 0
   Do Until DoDBBub <> 0
     DoDBBub = 1
     'ADDPOP'
     'DISPLAY PANEL(BGZDBBUB)'
     TB_RC = RC
     'VGET (ZVERB)'
     If TB_RC = 8 | ZVERB = 'CANCEL' Then
     Do
       DoDBBub = -1
       'REMPOP'
     End
     If DoDBBub <> -1 Then
     Do
       If BGZDODMN = '/' Then
       Do
       /*Call startDaemon*/
         groovyz = BGZDBBH'/bin/groovyz' ||,
                          ' -DBB_DAEMON_HOST 'BGZDMNHO ||,
                          ' -DBB_DAEMON_PORT 'BGZDMNPO
       End
       Else
         groovyz = BGZDBBH'/bin/groovyz'

       dbb     = BGZDBBH'/bin/dbb'

       /* DBB user build options */
       script  = BGZBLSCR
       sandbox = BGZBLSAN
       workdir = BGZBLWRK

       /* Create the workdir directory if not already existing */
       Address SYSCALL 'readdir 'BGZBLWRK' ls. lsst.'
       If ls.0 = 0 Then
       Do
         /* Address SYSCALL 'mkdir (BGZNDIR)' 750 */
         shellcmd = 'mkdir -m 750 -p 'BGZBLWRK
         sh_rc = bpxwunix(shellcmd,,stdout.,stderr.)
         If sh_rc > 0 Then
         Do
           Do e = 1 to stderr.0
             Say stderr.e
           End
           Do e = 1 to stdout.0
             Say stdout.e
           End
           Iterate
         End
       End
       hlq     = BGZBLHLQ
       BGZFILE = relativePath
       JavaOpts = '-Xquickstart '
       shellcmd  = ''
       shellcmd  = shellcmd || '. 'BGZDBBC'/dbbenv.sh;cd' sandbox';'
       shellcmd  = shellcmd || 'export JAVA_HOME='BGZJAVAH';'
       shellcmd  = shellcmd || 'export JAVA_OPTS="'JavaOpts'";'
       shellcmd  = shellcmd ||  groovyz script

       /* Add the source file we are building first */
       bldFile.1 = BGZFILE
       bldFile.0 = 1

       If BGZBLDDP /= '' Then
       Do
         Call getDeps(BGZFILE)
         If rc = 0 Then
           userBuildCmd = ' --userBuild 'BGZBLWRK'/buildFiles.txt'
         Else
           userBuildCmd = ' --userBuild' BGZFILE
       End
       Else
         userBuildCmd = ' --userBuild' BGZFILE

       /* Enter pressed without command S - navigate to Script Parameters */
       If BGZCMD = '' Then
       Do
         /* BGZPROPS table to be displayed on Script Parameters panel */
         'TBOPEN BGZPROPS'
         /* If it didn't exist yet, create it. */
         If RC = 8 Then
         Do
           'TBCREATE BGZPROPS',
           'KEYS(BGZUBREP,BGZUBLOC,BGZPROW)',
           'NAMES(BGZPNAME,BGZPVAL,BGZPMCMD) WRITE'
         End

         BGZUBREP = BGZUSREP
         BGZUBLOC = BGZUSLOC
         'TBSORT BGZPROPS FIELDS(BGZPROW)'
         'TBTOP  BGZPROPS'
         /* Need to delete all the -- properties and rebuild from BGZDBBUB */
         'TBSKIP BGZPROPS'

         TB_RC = rc
         Do While (TB_RC = 0)
           If BGZPNAME = '--sourceDir' |,
              BGZPNAME = '--workDir'   |,
              BGZPNAME = '--hlq'       |,
              BGZPNAME = '--application' Then
           Do
             'TBDELETE BGZPROPS'
           End
           'TBSKIP BGZPROPS'
           TB_RC = rc
         End
         BGZUBREP = BGZUSREP
         BGZUBLOC = BGZUSLOC
         /* Now add them back in */
         BGZPROW  = '000001'
         BGZPNAME = '--sourceDir'
         BGZPVAL  = sandbox
         BGZPMCMD = ''
         'TBADD BGZPROPS ORDER'
         BGZPROW  = '000002'
         BGZPNAME = '--workDir'
         BGZPVAL  = workdir
         BGZPMCMD = ''
         'TBADD BGZPROPS ORDER'
         BGZPROW  = '000003'
         BGZPNAME = '--hlq'
         BGZPVAL  = hlq
         BGZPMCMD = ''
         'TBADD BGZPROPS ORDER'
         BGZPROW  = '000004'
         BGZPNAME = '--application'
         BGZPVAL  = appname
         BGZPMCMD = ''
         'TBADD BGZPROPS ORDER'
         'TBBOTTOM BGZPROPS'
         nbparm = BGZPROW
         /* Need to load a temp table with the props for this repo */
         'CONTROL ERRORS RETURN'
         'TBEND    TMPPROPS'
         'CONTROL ERRORS CANCEL'
         'TBCREATE TMPPROPS',
           'KEYS(BGZUBREP,BGZUBLOC,BGZPROW)',
           'NAMES(BGZPNAME,BGZPVAL,BGZPMCMD) WRITE'
         'TBTOP  BGZPROPS'
         'TBSKIP BGZPROPS'
         TB_RC = rc
         Do While (TB_RC = 0)
           If BGZUBREP = BGZUSREP Then
             'TBADD TMPPROPS ORDER'

           'TBSKIP BGZPROPS'
           TB_RC = rc
         End
         DoReq = 0
         PRPROW = '000001'
         Do Until DoReq > 0
           PRPROW = '000001'
           'TBTOP TMPPROPS'
           'TBSKIP TMPPROPS NUMBER('PRPROW')'
           'TBDISPL TMPPROPS PANEL(BGZDBBPM)'
           TB_RC = RC
           'VGET (ZVERB)'
           If TB_RC = 8 | ZVERB = 'CANCEL' Then
           Do
             DoReq = -1
             Leave
           End
/*         If BGZNPNAM <> '' & BGZNPVAL <> '' Then */
           If BGZNPNAM <> '' Then
           Do
             nbparm = nbparm + 1
             BGZPROW = nbparm
             BGZPNAME = BGZNPNAM
             BGZPVAL  = BGZNPVAL
             BGZNPNAM = ''
             BGZNPVAL = ''
             'TBADD BGZPROPS ORDER'
             'TBADD TMPPROPS ORDER'
             'TBSORT TMPPROPS FIELDS(BGZPROW)'
           End
           If ZTDSELS = 0 Then
             PRPROW = ZTDTOP
           Do While ZTDSELS > 0
             If BGZPMCMD = '/' Then
             Do
               PRPROW = TEMPROW
               BGZPMCMD = GetPMCMD(BGZBLSCR)
             End
             Select
               When BGZPMCMD = 'D' Then
               Do
                 PRPROW = TEMPROW
                 'TBDELETE TMPPROPS'
               End
               When BGZPMCMD = 'E' Then
               Do
                 PRPROW = TEMPROW
                 'CONTROL DISPLAY SAVE'
                 oldprnme = BGZPNAME
                 'ADDPOP'
                 BGZPRNME = BGZPNAME
                 BGZPRVAL = BGZPVAL
                 'DISPLAY PANEL(BGZDBBSC)'
                 TB_RC = RC
                 'VGET (ZVERB)'
                 If TB_RC <> 8 & ZVERB <> 'CANCEL' Then
                 Do
                   If COMPARE(BGZPRNME,BGZPNAME) = 0 Then
                   Do
                     BGZPVAL = BGZPRVAL
                     'TBMOD TMPPROPS'
                   End
                   Else
                   Do
                     'TBDELETE TMPPROPS'
                     BGZPNAME = BGZPRNME
                     BGZPVAL  = BGZPRVAL
                     'TBADD TMPPROPS'
                   End
                 End
                 'REMPOP'
                 'CONTROL DISPLAY RESTORE'
               End
               Otherwise NOP
             End /* Select; */

             If BGZPMCMD <> 'D' Then
             Do
               PRPROW = TEMPROW
               BGZPMCMD = ''
               'TBMOD TMPPROPS'
             End
             If ZTDSELS = 1 Then
               ZTDSELS = 0
             Else
               'TBDISPL BGZPROPS'
           End
           If ZVERB <> ' ' Then
             Iterate
           /* S command on BGZDBBPM panel  */
           If BGZCMD = 'S' Then
           Do
             DoReq = 1
             'TBTOP TMPPROPS'
             'TBSKIP TMPPROPS'
             Do While(RC = 0)
               shellcmd=shellcmd || '' BGZPNAME BGZPVAL
               'TBSKIP TMPPROPS'
             End

             shellcmd=shellcmd || userBuildCmd

             'TBMOD BGZDBBUB'
             BGZFLOG = BGZBLWRK'/dbbub.log'
             'VPUT (BGZFLOG) SHARED'
             DBB_rc = BGZCMD('dbbub' shellcmd)
             BGZCMD = ''
             Iterate
           End
         End
         /* Now load temporary props back to main props */
         'TBTOP  TMPPROPS'
         'TBSKIP TMPPROPS'
         TB_RC = rc
         Do While (TB_RC = 0)
           'CONTROL ERRORS RETURN'
           'TBADD BGZPROPS ORDER'
           If rc = 8 Then
             'TBMOD BGZPROPS ORDER'
           'CONTROL ERRORS CANCEL'
           'TBSKIP TMPPROPS'
           TB_RC = rc
         End
         'TBEND   TMPPROPS'
         'TBCLOSE BGZPROPS'
       End
       /* S command on BGZDBBUB panel  */
       If BGZCMD = 'S' Then
       Do
         DoReq = 1

         /* Need to load a temp table with the props for this repo */
         'CONTROL ERRORS RETURN'
         'TBEND    TMPPROPS'
         'CONTROL ERRORS CANCEL'
         'TBCREATE TMPPROPS',
           'KEYS(BGZUBREP,BGZUBLOC,BGZPROW)',
           'NAMES(BGZPNAME,BGZPVAL,BGZPMCMD) WRITE'
         'TBOPEN BGZPROPS'
         /* If it didn't exist yet, create it. */
         If RC = 8 Then
         Do
           'TBCREATE BGZPROPS',
           'KEYS(BGZUBREP,BGZUBLOC,BGZPROW)',
           'NAMES(BGZPNAME,BGZPVAL,BGZPMCMD) WRITE'
         End
         'TBSORT BGZPROPS FIELDS(BGZPROW)'
         'TBTOP  BGZPROPS'
         /* Need to delete all the -- properties and rebuild from BGZDBBUB */
         'TBSKIP BGZPROPS'
         TB_RC = rc
         Do While (TB_RC = 0)
           If BGZPNAME = '--sourceDir' |,
              BGZPNAME = '--workDir'   |,
              BGZPNAME = '--hlq'       |,
              BGZPNAME = '--application' Then
           Do
             'TBDELETE BGZPROPS'
           End
           'TBSKIP BGZPROPS'
           TB_RC = rc
         End

         BGZUBREP = BGZUSREP
         BGZUBLOC = BGZUSLOC
         /* Now add them back in */
         BGZPROW = '000001'
         BGZPNAME = '--sourceDir'
         BGZPVAL  = sandbox
         BGZPMCMD = ''
         'TBADD BGZPROPS ORDER'
         BGZPROW = '000002'
         BGZPNAME = '--workDir'
         BGZPVAL  = workdir
         BGZPMCMD = ''
         'TBADD BGZPROPS ORDER'
         BGZPROW = '000003'
         BGZPNAME = '--hlq'
         BGZPVAL  = hlq
         BGZPMCMD = ''
         'TBADD BGZPROPS ORDER'
         BGZPROW = '000004'
         BGZPNAME = '--application'
         BGZPVAL  = appname
         BGZPMCMD = ''
         'TBADD BGZPROPS ORDER'

         'TBTOP  BGZPROPS'
         'TBSKIP BGZPROPS'
         TB_RC = rc
         Do While (TB_RC = 0)
           If BGZUBREP = BGZUSREP Then
             'TBADD TMPPROPS ORDER'

           'TBSKIP BGZPROPS'
           TB_RC = rc
         End
         'TBSORT TMPPROPS FIELDS(BGZPROW)'
         'TBTOP  TMPPROPS'
         'TBSKIP TMPPROPS'
         Do While(RC = 0)
           shellcmd=shellcmd || '' BGZPNAME BGZPVAL
           'TBSKIP TMPPROPS'
         End
         'TBCLOSE BGZPROPS'
         'TBEND   TMPPROPS'

         shellcmd=shellcmd || userBuildCmd

         'TBMOD BGZDBBUB'
         BGZFLOG = BGZBLWRK'/dbbub.log'
         'VPUT (BGZFLOG) SHARED'
         DBB_rc = BGZCMD('dbbub' shellcmd)
         BGZCMD = ''
       End
       'REMPOP'
       /* View build output on completion  */
       If BGZBLLOG = '/' & DoReq = 1 Then
       Do
         x = Lastpos('.',BGZUSFIL)
         filename = Substr(BGZUSFIL,1,x-1)
         ext      = Substr(BGZUSFIL,x+1)
         input= filename
         input_upper = translate(input, uppercase, lowercase)
         filename = input_upper
         BGZUSLOG = filename'.log'
         BGZFLOG = BGZBLWRK'/'BGZUSLOG

         /* The DTL Groovy writes the log in UTF8 - Need to fix */
         UTF8 = ''
         If Substr(ext,1,3) = 'dtl' then
           UTF8 = 'UTF8'

         BGZEMIX = 'NO'
         'VGET (ZDBCS) SHARED'
         If ZDBCS = 'YES' THEN BGZEMIX = 'YES'
         "CONTROL ERRORS RETURN"
         "VIEW File(BGZFLOG) MIXED("BGZEMIX")"
         BR_RC = RC
         "CONTROL ERRORS CANCEL"
         If BR_RC = 20 Then
           'SETMSG MSG(BGZC039)'
         /* Now we need to VIEW the link-edit logs */
         Do i = 1 to bldFile.0
           y = lastpos('/',bldFile.i)
           depFile = Substr(bldFile.i,y+1)

           If depFile /= BGZUSFIL Then
           Do
             x = Lastpos('.',depFile)
             filename = Substr(depFile,1,x-1)
             ext      = Substr(depFile,x+1)
             input= filename
             input_upper = translate(input, uppercase, lowercase)
             filename = input_upper
             BGZUSLOG = filename'.log'
             BGZFLOG = BGZBLWRK'/'BGZUSLOG
             BGZEMIX = 'NO'
             'VGET (ZDBCS) SHARED'
             If ZDBCS = 'YES' THEN BGZEMIX = 'YES'
             "CONTROL ERRORS RETURN"
             "VIEW File(BGZFLOG) MIXED("BGZEMIX")"
             BR_RC = RC
             "CONTROL ERRORS CANCEL"
             If BR_RC = 20 Then
               'SETMSG MSG(BGZC039)'

           End
         End
       End
     End
   End
   'TBCLOSE BGZDBBUB'

Return DBB_rc

GetPMCMD: PROCEDURE
  Parse Arg BGZBLSCR
  'ADDPOP'
  'DISPLAY PANEL(BGZSLSCR)'
  Select;
    When BGZSEL = '1' Then
      Cmd = 'E'
    When BGZSEL = '2' Then
      Cmd = 'D'
    Otherwise
      Cmd = ''
  End
  'REMPOP'
Return Cmd

/*********************************************************************/
/*                                                                   */
/* getDeps : Get static link dependencies                            */
/*                                                                   */
/*********************************************************************/
/*                                                                   */
/* This procedure will use the file system metadata store to find    */
/* static link dependencies for a source module.                     */
/* This means that when a userbuild is performed on a source program */
/* then a link will also be performed.                               */
/*                                                                   */
/* It uses the DBB CLI commands to find the dependencies.            */
/*********************************************************************/

getDeps: PROCEDURE expose BGZJAVAH BGZBLWRK dbb UPPERCASE lowercase bldFile. ,
                          BGZBRANC BGZDBBC;

  Parse Arg BGZFILE
  /* Need to use the DBB CLI to get the logical files */
  x = pos('/',bgzfile)
  repo = Substr(BGZFILE,1,x-1)
  y = lastpos('/',bgzfile)
  depFile = Substr(BGZFILE,y+1)
  parse var depfile depFile'.'ext
  depFile = translate(depFile, UPPERCASE, lowercase)
  ext     = translate(ext, lowercase, UPPERCASE)

  repos  = 'repo1 repo2 repo3'
  users  = 'user1 user2 user3'
  x = wordpos(repo,repos)
  If x = 0 Then
  Do
    Say "Can't get metadata, repository not in repos list."
    Say "Just building the requested source file."
    Return 8
  End

  Call getMetaData
  j = 1

  /* Now add the dependencies                  */
  If DBB_rc = 0 Then
  Do
    logfile  = BGZFLOG
    Address syscall "readfile (logfile) dbbline."
    Do i = 1 to dbbline.0
      line = dbbline.i
      Parse var line lname file
/*    If lname = depFile then*/
      If Pos(repo,file) /= 0 Then
      Do
        j = j + 1
        bldFile.j = strip(file)
      End
    End
  End
  Else
  Do
    logfile  = BGZFLOG
    Address syscall "readfile (logfile) dbbline."
    Do i = 1 to dbbline.0
      line = dbbline.i
      If Pos('does not exist',line) /= 0 Then
      Do
        /* New Branch, no metadata, try main instead */
        BGZBRANC = 'main'
        Call getMetaData
        /* Now add the dependencies                  */
        If DBB_rc = 0 Then
        Do
          logfile  = BGZFLOG
          Address syscall "readfile (logfile) dbbline."
          Do i = 1 to dbbline.0
            line = dbbline.i
            Parse var line lname file
       /*   If lname = depFile then */
            If Pos(repo,file) /= 0 Then
            Do
              j = j + 1
              bldFile.j = strip(file)
            End
          End
        End
      End
      Else
        say line
    End
  End

  bldFile.0 = j
  depFile  = BGZBLWRK'/buildFiles.txt'
  Address syscall "writefile (depFile) 755 bldFile."

Return 0

getMetaData:

  script = 'logical-file find --collection 'repo'-'BGZBRANC'-outputs ' ||,
           '--type file --location /u/'word(users,x) ||,
           ' --dependency :LINK:'depFile

  shellcmd  = ''
  shellcmd  = shellcmd || '. 'BGZDBBC'/dbbenv.sh;'
  shellcmd  = shellcmd || 'export JAVA_HOME='BGZJAVAH';'
  shellcmd  = shellcmd ||  dbb script

  BGZFLOG = BGZBLWRK'/dbbub.log'
  'VPUT (BGZFLOG) SHARED'

  DBB_rc = BGZCMD('getdep' shellcmd)

Return

/*********************************************************************/
/*                                                                   */
/* startDeamon : Procedure to start the DBB daemon if not running    */
/*                                                                   */
/*********************************************************************/
/*                                                                   */
/* This procedure will use SDSF REXX to see if the daemon is running */
/* and if not, use SDSF REXX to start the daemon.                    */
/*                                                                   */
/* We cannot assume that the customer has SDSF, or if the user has   */
/* the authority to use it, or of the user has the authority to      */
/* issue start commends.                                             */
/*                                                                   */
/* As such, the procedure is call is commented out and this code has */
/* been left in as a sample call.                                    */
/*********************************************************************/

startDaemon: PROCEDURE;

  /* Check and see if the user has a daemon running */
  rc=isfcalls('ON')
  /* Access the ST display */
  isfprefix = 'DBB'
  Address SDSF "ISFEXEC ST"
  /* Loop for all running BLZJMON jobs */
  dmnRun = 0
  Do ix=1 to JNAME.0
    if JNAME.ix = "DBB" & QUEUE.ix = "EXECUTION" & ACTSYS.ix <> "" then
    Do
      dmnRun = 1
      leave
    End
  End
  If dmnRun = 0 Then
  Do
    Say 'daemon is not running. Issuing /s DBB command'
    Say 'First build will be slower as Java classes being loaded'

    mycmd.0=1
    mycmd.1="s DBB"
    Address SDSF ISFSLASH "("mycmd.") (WAIT)"
  End
  rc=isfcalls('OFF')


Return
