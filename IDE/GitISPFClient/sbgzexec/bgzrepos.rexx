/* REXX */
/*%STUB CALLCMD*/
/*********************************************************************/
/*                                                                   */
/* IBM ISPF Git Interface                                            */
/*                                                                   */
/*********************************************************************/
/*                                                                   */
/* NAME := BGZrepos                                                  */
/*                                                                   */
/* DESCRIPTIVE NAME := GIT ISPF Client repository                    */
/*                                                                   */
/* FUNCTION := The ISPF Client Git repository allow userid to work   */
/*             with a cloned Git repository                          */
/*                                                                   */
/*                                                                   */
/* CALLED BY : BGZMAIN                                               */
/*                                                                   */
/* PARAMETERS :                                                      */
/*                                                                   */
/* OUTPUT := None                                                    */
/*                                                                   */
/* Change History                                                    */
/*                                                                   */
/* Who   When     What                                               */
/* ----- -------- -------------------------------------------------- */
/* XH    11/01/19 Initial version                                    */
/*                                                                   */
/*********************************************************************/

   Address ISPEXEC

   'TBOPEN BGZCLONE'
   /* If it didn't exist yet, create it. */
   If RC = 8 Then
   Do
     'TBCREATE BGZCLONE',
     'KEYS(BGZREPOS,BGZUSDIR) NAMES(BGZRPCMD,BGZSTATU,BGZBRANC) WRITE'
   End

   'TBSORT BGZCLONE FIELDS(BGZREPOS)'

   ReturnCode = 0
   ROWID = 1
   Do Until (ReturnCode <> 0)
     'TBTOP BGZCLONE'
     'TBSKIP BGZCLONE ROW('ROWID')'
     'TBDISPL BGZCLONE PANEL(BGZREPOS) ROWID(ROWID)'
     TB_RC = RC
     'VGET (ZVERB)'
     If TB_RC = 8 | Zverb = 'CANCEL' Then
     Do
       ReturnCode = -1
       'TBCLOSE BGZCLONE'
       Iterate
     End
     /* New repository to clone entered on top line */
     If BGZNREPO <> '' Then
     Do
       If BGZNDIR = '' Then
       Do
         'SETMSG MSG(BGZC014)'
         Iterate
       End

       If Verify(BGZNDIR,'/') = 1 Then
       Do
         'SETMSG MSG(BGZC015)'
         Iterate
       End

       /* Create the USS directory if not already existing */
       Address SYSCALL 'readdir 'BGZNDIR' ls. lsst.'
       If ls.0 = 0 Then
       Do
         /* Address SYSCALL 'mkdir (BGZNDIR)' 750 */
         shellcmd = 'mkdir -m 750 -p 'BGZNDIR
         sh_rc = bpxwunix(shellcmd,,stdout.,stderr.)
         If sh_rc > 0 Then
         Do
           Do e = 1 to stderr.0
             Say stderr.e
           End
           Do e = 1 to stdout.0
             Say stdout.e
           End
           Return
         End
       End

       /* Need to check if BGZUSDIR/reponame already exists */
       ToClone = 1
       /* Capture master folder of cloned repository toto of toto.git */
       x = lastPos('/',BGZNREPO)
       repoName = Substr(BGZNREPO,x+1)
       y = lastPos('.git',repoName)
       repoName = Substr(repoName,1,y-1)
       BGZUSDIR = BGZNDIR'/'repoName
       Address SYSCALL 'readdir 'BGZUSDIR' ls. lsst.'
       /* working directory already exist  */
       If ls.0 /= 0 Then
       Do
         'ADDPOP'
         'DISPLAY PANEL(BGZCFCLO)'
           TB_RC = RC
           'VGET (ZVERB)'
           /* Reconnect or Raplace clone */
           If TB_RC <> 8 & ZVERB <> 'CANCEL' Then
           Do
             /* Only reconnect the git repo in BGZCLONE table */
             If BGZRECON = 1 Then
               ToClone = 0

             If BGZRECON = 2 Then
             Do
               /* Remove Working Directory           */
               Call bpxwunix 'rm -R' '"'BGZUSDIR'"',,list.,stderr.
               /* Check USS directory no more exist */
               Address SYSCALL 'readdir 'BGZUSDIR' ls. lsst.'
               If ls.0 = 0 Then
               Do
                 ToClone = 1
                 Git_rc = 0
               End
               Else
               Do
                 /* Error message on romove directory */
                 'SETMSG MSG(BGZC032)'
               End
             End
           End
           /* PF3 : do nothing           */
           Else
           Do
             ToClone = 0
             Git_rc = 1
           End

           'REMPOP'

       End

       Git_rc = 0
       If ToClone = 1 Then
       Do
         /* Git Clone operation  */
         'VGET BGZENVIR SHARED'
         shellcmd = ''
         shellcmd = shellcmd || BGZENVIR

         shellcmd=shellcmd || 'cd' BGZNDIR';'||,
                'git clone' BGZNREPO

         Git_rc = BGZCMD('clone' shellcmd)
       End
       If Git_rc = 0 Then
       Do
         /* Create BGZCLONE row for the cloned repository  */
         BGZREPOS = BGZNREPO
         /* Capture master folder of cloned repository toto of toto.git */
         x = lastPos('/',BGZREPOS)
         repoName = Substr(BGZREPOS,x+1)
         y = lastPos('.git',repoName)
         repoName = Substr(repoName,1,y-1)
         BGZUSDIR = BGZNDIR'/'repoName
         BGZSTATU = ''
         /* Need to get the current branch */
         Call BGZBRANC (BGZREPOS BGZUSDIR 'noDisplay')
         'VGET (BGZBRANC) PROFILE'
         'TBADD BGZCLONE ORDER'
         BGZNREPO = ''
         BGZNDIR = ''
       End
     End

     Do While ZTDSELS > 0
       'TBMOD BGZCLONE ORDER'
       If ZTDSELS = 1 Then
         ZTDSELS = 0
       Else
         'TBDISPL BGZCLONE'
     End

     If ZVERB <> ' ' Then
     Do
       Iterate
     End

     'TBTOP BGZCLONE'
     'TBSKIP BGZCLONE'

     Do While RC = 0

       If BGZRPCMD = '/' Then
         BGZRPCMD = GetCMD(BGZREPOS BGZUSDIR)

       Select

         When BGZRPCMD = 'JU' Then
         Do
           /* Jump to USS directory - call BGZUDLST  */
           BGZUSSDR = BGZUSDIR
           'VPUT (BGZUSSDR) PROFILE'
           BGZUSREP = BGZREPOS
           'VPUT (BGZUSREP) PROFILE'
           'VPUT (BGZBRANC) PROFILE'
           BGZRPCMD = ''
           'TBMOD BGZCLONE'

           Call BGZUSLST
         End

         When BGZRPCMD = 'ST' Then
         Do
           BGZRPCMD = ''
           'TBMOD BGZCLONE'
           /* Git Status command  */
           Git_rc = 0
           'VGET BGZENVIR SHARED'
           shellcmd  = ''
             shellcmd = shellcmd || BGZENVIR

           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                  'git status'
           Git_rc = BGZCMD('status' shellcmd)

         End

         When BGZRPCMD = 'AD' Then
         Do
           BGZRPCMD = ''
           'TBMOD BGZCLONE'
           /* Call BGZADD rexx for Git add Command */
           Call BGZADD (BGZREPOS BGZUSDIR)
         End

         When BGZRPCMD = 'PL' Then
         Do
           BGZRPCMD = ''
           'TBMOD BGZCLONE'
           /* Git Pull command  */
           Git_rc = 0
           'VGET BGZENVIR SHARED'
           shellcmd  = ''
             shellcmd = shellcmd || BGZENVIR

           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                  'git pull'
           Git_rc = BGZCMD('pull' shellcmd)

         End

         When BGZRPCMD = 'CO' | BGZRPCMD = 'CP' Then
         Do
           /* Call BGZCOMIT rexx for Git commit Command */
           Git_rc = BGZCOMIT(BGZREPOS BGZUSDIR)

           If BGZRPCMD = 'CP' & Git_rc = 0 Then
           Do
             /* Git Push command  */
             Git_rc = 0
             'VGET BGZENVIR SHARED'
             shellcmd  = ''
               shellcmd = shellcmd || BGZENVIR

             shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                    'git push'
             Git_rc = BGZCMD('push' shellcmd)
           End
           BGZRPCMD = ''
           'TBMOD BGZCLONE'

         End

         When BGZRPCMD = 'CM' Then
         Do
           BGZRPCMD = ''
           'TBMOD BGZCLONE'
           /* Call BGZCOMMP rexx for Git Command */
           Call BGZCOMMP (BGZREPOS BGZUSDIR)
         End

         When BGZRPCMD = 'PS' Then
         Do
           BGZRPCMD = ''
           'TBMOD BGZCLONE'
           /* Git Push command  */
           Git_rc = 0
           'VGET BGZENVIR SHARED'
           shellcmd  = ''
             shellcmd = shellcmd || BGZENVIR

           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                  'git push'
           Git_rc = BGZCMD('push' shellcmd)

         End

         When BGZRPCMD = 'BR' Then
         Do
           /* Call BGZBRANC rexx for Git add Command */
           Call BGZBRANC (BGZREPOS BGZUSDIR 'Display')
           BGZRPCMD = ''
           'VGET (BGZBRANC) PROFILE'
           'TBMOD BGZCLONE'
         End

         When BGZRPCMD = 'RM' Then
         Do
           'ADDPOP'
           'DISPLAY PANEL(BGZCFREM)'
           TB_RC = RC
           'VGET (ZVERB)'
           If TB_RC <> 8 & ZVERB <> 'CANCEL' Then
           Do
             /* Remove Working Directory           */
             Call bpxwunix 'rm -R' '"'BGZUSDIR'"',,list.,stderr.
             /* Check USS directory no more exist */
             Address SYSCALL 'readdir 'BGZUSDIR' ls. lsst.'
             If ls.0 = 0 Then
             Do
               /* Delete BGZCLONE corresponding row  */
               'TBDELETE BGZCLONE'
               'TBCLOSE BGZCLONE'
               'TBOPEN BGZCLONE'
               'REMPOP'
               BGZRPCMD = ''
               RC = -1
               Iterate
             End
             Else
             Do
               /* Error message on romove directory */
               'SETMSG MSG(BGZC032)'
             End
           End
           BGZRPCMD = ''
           'TBMOD BGZCLONE'
           'REMPOP'
         End

         Otherwise NOP
       End /* Select; */

       If ReturnCode = 0 Then
       Do
         /* Capture Status of cloned repository ('*' for push to do) */
         /* Git Status command  */
         Git_rc = 0
         'VGET BGZENVIR SHARED'
         /* Need to craeet a temporary ISPF table to hold info */
         'TBCREATE BGZTEMP KEYS(BGZROW),
                           NAMES(BGZSTCMD,BGZLINE,BGZFILE) NOWRITE'
         shellcmd  = ''
           shellcmd = shellcmd || BGZENVIR

         shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                'git status'
         Git_rc = BGZCMD('stage' shellcmd)

         /* Now read the Status list */
         /* up-to-date : nothing to do  -  ahead of : Push to do   */
         ToPush = ''
         'TBTOP BGZTEMP'
         'TBSKIP BGZTEMP'
         Do While RC = 0
           onBranch = Substr(BGZLINE,1,9)
           If onBranch = 'On branch' Then
           Do
             BGZBRANC = Substr(BGZLINE,11)
             'VPUT (BGZBRANC) PROFILE'
           End
           yourBranch = Substr(BGZLINE,1,14)
           ToPush = Substr(BGZLINE,16,8)
           If yourBranch = 'Your branch is' Then
           Do
             If ToPush = 'ahead of' Then
               BGZSTATU = '*'
             Else
               BGZSTATU = ' '
           End

           'TBSKIP BGZTEMP'
         End
         'TBCLOSE BGZTEMP'

         BGZRPCMD = ''
         'TBMOD  BGZCLONE ORDER'

       End
       'TBSKIP BGZCLONE'
     End

   End

 /* 'TBCLOSE BGZCLONE' */
Return 0
/* End Git Repository     */

GetCMD: PROCEDURE
  Parse Arg BGZREPOS BGZUSDIR
  'ADDPOP'
  'DISPLAY PANEL(BGZSLREP)'
  'VGET (ZVERB)'
  if ZVERB = 'CANCEL' | ZVERB = 'EXIT' Then
    BGZSEL = ''

  Select;
    When BGZSEL = '1' Then
      Cmd = 'JU'
    When BGZSEL = '2' Then
      Cmd = 'ST'
    When BGZSEL = '3' Then
      Cmd = 'AD'
    When BGZSEL = '4' Then
      Cmd = 'CO'
    When BGZSEL = '5' Then
      Cmd = 'PS'
    When BGZSEL = '6' Then
      Cmd = 'CP'
    When BGZSEL = '7' Then
      Cmd = 'PL'
    When BGZSEL = '8' Then
      Cmd = 'BR'
    When BGZSEL = '9' Then
      Cmd = 'CM'
    When BGZSEL = '10' Then
      Cmd = 'RM'
    Otherwise
      Cmd = ''
  End
  'REMPOP'
Return Cmd

