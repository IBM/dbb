/* REXX */
/*%STUB CALLCMD*/
/*********************************************************************/
/*                                                                   */
/* IBM ISPF Git Interface                                            */
/*                                                                   */
/*********************************************************************/
/*                                                                   */
/* NAME := BGZBRANC                                                  */
/*                                                                   */
/* DESCRIPTIVE NAME := GIT ISPF Client git branch                    */
/*                                                                   */
/* FUNCTION :=                                                       */
/*                                                                   */
/*                                                                   */
/* CALLED BY : BGZREPOS                                              */
/*                                                                   */
/* PARAMETERS :                                                      */
/*                                                                   */
/* OUTPUT := None                                                    */
/*                                                                   */
/* Change History                                                    */
/*                                                                   */
/* Who   When     What                                               */
/* ----- -------- -------------------------------------------------- */
/* XH    14/02/19 Initial version                                    */
/* TLD   31/05/23 FIX: Added view of git command results to correct  */
/*                CLIST variable value overflow exception.           */
/* LMD   11/09/24 Add multiple commands. Press help on the panel for */
/*                details. For example DF - Diff, ME - Merge, etc    */
/*                                                                   */
/*********************************************************************/
  Parse Source ENVIR CALLTYPE PGM SPECIFICS;
  PGM = SUBSTR(PGM,1,8)

  DEBUG = "N"           /* Enable Debug Messages (Y|N)               */

   Parse Arg BGZREPOS BGZUSDIR NoDisp

   ReturnBranch = -1
   Rebuild = 1

   Address ISPEXEC
   'VGET BGZSHRMT PROFILE'
   If rc = 8 Then
   Do
     BGZSHRMT = '/'
     'VPUT BGZSHRMT PROFILE'
   End

   Do Until (ReturnBranch = 0)
     If Rebuild = 1 Then
     Do
       Rebuild = 0
       /* Git branch command  */
       Git_rc = 0
       'VGET BGZENVIR SHARED'
       /* Need to create a temporary ISPF table to hold info */
       'CONTROL  ERRORS RETURN'
       'TBEND    BGZTEMP'
       'CONTROL  ERRORS CANCEL'
       'TBCREATE BGZTEMP KEYS(BGZLINE),
                         NAMES(BGZBRCMD) NOWRITE'

       shellcmd  = ''
       shellcmd = shellcmd || BGZENVIR

       shellcmd = shellcmd || 'cd' BGZUSDIR';' ||,
              'git branch -a --no-color'
       Git_rc = BGZCMD('branch' shellcmd)
       'VGET BGZSHRMT PROFILE'

       If (BGZSHRMT = " ") Then
       Do
         /* Delete rows that start with remotes/origin/  */
         'TBTOP BGZTEMP'
         'TBSKIP BGZTEMP'
         Do While RC = 0
           branch = Substr(BGZLINE,1,17)
           If Verify(branch,'  remotes/origin/') = 0 Then
           Do
             'TBDELETE BGZTEMP'
           End
           'TBSKIP BGZTEMP'
         End
       End /* Of If (ShowRemotes = "N") Then ... */

       /* Just using this module to get the current branch */
       If NoDisp = 'noDisplay' Then
         Return Git_rc
     End

     store_BGZSHRMT = BGZSHRMT

     /* Code to display branch on panel  */
     'TBTOP BGZTEMP'
     'TBSKIP BGZTEMP'
     'TBDISPL BGZTEMP PANEL(BGZBRANC)'
     TB_RC = RC
     'VGET (ZVERB)'

     If TB_RC = 8 | Zverb = 'CANCEL' Then
     Do
       ReturnBranch = 0
       'TBCLOSE BGZTEMP'
       Iterate
     End

     /* New branch to create entered on top line */
     If BGZNBRAN <> '' Then
     Do

       /* Git checkout operation  */
       Git_rc = 0
       'VGET BGZENVIR SHARED'
       shellcmd  = ''
       shellcmd = shellcmd || BGZENVIR

       shellcmd=shellcmd || 'cd' BGZUSDIR';'||,
         'git checkout -b ' BGZNBRAN

       Git_rc = BGZCMD('crbran' shellcmd)

       If Git_rc = 0 Then
       Do

         BGZBRANC = BGZNBRAN
         BGZNEWBR = 'Y'
         'VPUT (BGZBRANC) PROFILE'
         'VPUT (BGZNEWBR) PROFILE'
         BGZLINE = BGZNBRAN
         'TBADD BGZTEMP ORDER'
         'TBCLOSE BGZTEMP'
         BGZNBRAN = ''
         Rebuild = 1
         Iterate
       End
       Else
       Do
         Iterate
       End
     End
     If BGZSHRMT /= store_BGZSHRMT Then
     Do
       store_BGZSHRMT = BGZSHRMT
       'VPUT BGZSHRMT PROFILE'
       Rebuild = 1
     End

     Do While ZTDSELS > 0
       'TBMOD BGZTEMP'
       If ZTDSELS = 1 Then
         ZTDSELS = 0
       Else
         'TBDISPL BGZTEMP'
     End

     If ZVERB <> ' ' Then
     Do
       Rebuild = 0
       Iterate
     End

     'TBTOP BGZTEMP'
     'TBSKIP BGZTEMP'

     Do While RC = 0

       If BGZBRCMD = '/' Then
         BGZBRCMD = GetBrCMD(BGZREPOS BGZUSDIR BGZLINE)

       /* Capture branch name from BGZLINE */
       BGZBRAN = Substr(BGZLINE,3)

       Select

         When BGZBRCMD = 'CB' Then
         Do
           /* Change working branch */
           /* Git Checkout <branch> */
           Git_rc = 0
           'VGET BGZENVIR SHARED'
           shellcmd  = ''
           shellcmd = shellcmd || BGZENVIR

           /* Check to see if working with a Remote Branch. */
           /* If true, isolate the branch name. Otherwise   */
           /* assume local branch.                          */
           If (DEBUG = "Y") Then
             Say PGM": BGZBRAN = "BGZBRAN

           Parse var BGZBRAN 'remotes/origin/'Branch

           If (Strip(Branch) = "") Then
             Branch = BGZBRAN

           If (DEBUG = "Y") Then
             Say PGM": Branch = "Branch

           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                  'git checkout ' ||'"'Branch'"'
           Git_rc = BGZCMD('checkout' shellcmd)

           If Git_rc= 0 Then
           Do
             BGZBRANC = Branch
             BGZNEWBR = 'N'
             'VPUT (BGZBRANC) PROFILE'
             'VPUT (BGZNEWBR) PROFILE'
             Rebuild = 1
             ReturnBranch = -1
           End

         End

         When BGZBRCMD = 'LG' Then
         Do
           /* git log --graph */
           'VGET BGZENVIR SHARED'
           shellcmd = BGZENVIR || 'cd' BGZUSDIR';' ||,
             'echo "Log for Branch:' BGZBRAN '\n";',
             'git -c color.ui=always log --graph --abbrev-commit ',
             '--date=format:"%a %b %d %H:%I %Y" --decorate ',
             '--decorate-refs-exclude="refs/remotes/origin/HEAD" ',
             '--format=format:"' ||,
             'Commit: %C(yellow)%h%C(reset)%C(magenta)%d%C(reset)%n'||,
             'Author: %C(green)%an%C(reset) %C(blue)%ad '||,
                     '%C(cyan)(%C(blue)%ar%C(cyan))%C(reset)%n' ||,
             '        %C(white)%s%n"' BGZBRAN
           Git_rc = BGZCMD('log' shellcmd)
         End

         When BGZBRCMD = 'DF' Then
         Do

           diffopts = '--ignore-space-at-eol'

           /* Run a Git Diff between branches */
           shellcmd=BGZENVIR || 'cd' BGZUSDIR';' ||,
                  'git diff --stat'    diffopts BGZBRAN '--;',
                  'echo "\n";git diff' diffopts BGZBRAN '--;'

           Git_rc = BGZCMD('diff' shellcmd)
         End

         When BGZBRCMD = 'MB' Then
         Do
           /* Push branch to origin */
           Git_rc = 0
           'VGET BGZENVIR SHARED'
           shellcmd  = ''
             shellcmd = shellcmd || BGZENVIR

           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                  'git merge ' || '"'BGZBRAN'"'
           Git_rc = BGZCMD('merge' shellcmd)
           If Git_rc= 0 Then
           Do
             ReturnBranch = -1
             Rebuild = 1
           End

         End

         When BGZBRCMD = 'ST' Then
         Do
           /* Git Status command  */
           Git_rc = 0
           'VGET BGZENVIR SHARED'
           shellcmd  = ''
             shellcmd = shellcmd || BGZENVIR
           shellcmd=shellcmd || 'cd' BGZUSDIR';'
           /* Check if there's an active merge */
           checkMergeCmd = shellcmd || 'git merge HEAD > /dev/null 2>&1'
           Git_rc = BGZCMD('mergecheck' checkMergeCmd)

           shellcmd=shellcmd || 'git status;'
           If (Git_rc \= 0) Then
           Do
             /* If actively merging, list conflicts as well */
             shellcmd = shellcmd || 'echo "Conflicts:\n";' ||,
                                    'git diff --cc'
           End

           Git_rc = BGZCMD('status' shellcmd)
           If Git_rc= 0 Then
           Do
             ReturnBranch = -1
             Rebuild = 1
           End

         End

         When BGZBRCMD = 'PB' Then
         Do
           /* Push branch to origin */
           Git_rc = 0
           'VGET BGZENVIR SHARED'
           shellcmd  = ''
             shellcmd = shellcmd || BGZENVIR

           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                  'git push -u origin ' BGZBRAN
           Git_rc = BGZCMD('pushbra' shellcmd)
           If Git_rc= 0 Then
           Do
             ReturnBranch = -1
             Rebuild = 1
           End

         End

         When BGZBRCMD = 'CM' Then
         Do
           'VGET BGZENVIR SHARED'
           /* Call BGZCOMMP rexx for Git Command */
           Call BGZCOMMP (BGZREPOS BGZUSDIR)
         End

         When BGZBRCMD = 'FE' Then
         Do
           /* Push branch to origin */
           Git_rc = 0
           'VGET BGZENVIR SHARED'
           shellcmd  = ''
             shellcmd = shellcmd || BGZENVIR

           branchName = BGZBRAN
           if pos('remotes/origin/',BGZBRAN) Then
             branchName = Substr(BGZBRAN,16)

           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                  'git fetch origin ' branchName
           Git_rc = BGZCMD('fetchbr' shellcmd)
           If Git_rc= 0 Then
           Do
             ReturnBranch = -1
             Rebuild = 1
           End

         End

         When BGZBRCMD = 'DB' Then
         Do
           /* Delete local branch */
           Git_rc = 0
           'VGET BGZENVIR SHARED'
           shellcmd  = ''
           shellcmd = shellcmd || BGZENVIR

           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,
                  'git branch -d ' BGZBRAN
           Git_rc = BGZCMD('delbran' shellcmd)
           If Git_rc= 0 Then
           Do
             ReturnBranch = -1
             Rebuild = 1
           End

         End

         When BGZBRCMD = 'JU' Then
         Do
           /* Jump to USS directory - call BGZUDLST  */
           BGZUSSDR = BGZUSDIR
           'VPUT (BGZUSSDR) PROFILE'
           BGZUSREP = BGZREPOS
           'VPUT (BGZUSREP) PROFILE'
           BGZBRANC = BGZBRAN
           'VPUT (BGZBRANC) PROFILE'
           Call BGZUSLST
           Rebuild = 0
           ReturnBranch = 0
           RC = -1
           Iterate
         End

         Otherwise NOP
       End

       BGZBRCMD = ''
       'TBMOD  BGZTEMP ORDER'

       'TBSKIP BGZTEMP'

     End

     If Rebuild = 1 Then
       'TBCLOSE BGZTEMP'

   End   /* End Do until ReturnBranch <> 0 */

Return Git_rc

GetBrCMD: PROCEDURE
  Parse Arg BGZREPOS BGZUSDIR BGZLINE
  'ADDPOP'
  'DISPLAY PANEL(BGZSLBRA)'
  'VGET (ZVERB)'
  if ZVERB = 'CANCEL' | ZVERB = 'EXIT' Then
    BGZSEL = ''

  Select;
    When BGZSEL = '1' Then
      Cmd = 'CB'
    When BGZSEL = '2' Then
      Cmd = 'PB'
    When BGZSEL = '3' Then
      Cmd = 'DB'
    When BGZSEL = '4' Then
      Cmd = 'JU'
    When BGZSel = '5' Then
      Cmd = 'FE'
    When BGZSel = '6' Then
      Cmd = 'MB'
    When BGZSel = '7' Then
      Cmd = 'LG'
    When BGZSel = '8' Then
      Cmd = 'DF'
    When BGZSel = '9' Then
      Cmd = 'ST'
    When BGZSel = '10' Then
      Cmd = 'CM'
    Otherwise
      Cmd = ''
  End
  'REMPOP'
Return Cmd
