/* REXX */
/*%STUB CPPLEFPL*/
/*********************************************************************/
/*                                                                   */
/* NAME := EXMTDT                                                    */
/*                                                                   */
/* DESCRIPTIVE NAME := Extract metadata from SCLM project            */
/*                                                                   */
/* FUNCTION := Extract information from SCLM accounting files,       */
/*             project definitions and Architecture definitions to   */
/*             create work files for part 2 of the migration.        */
/*             In addition generate linkedit decks for SCLM LEC      */
/*             ARCHDEFs.                                             */
/*                                                                   */
/*********************************************************************/

  Trace o

  Parse arg propmem
  /*-------------------------------------------------*/
  /* Parsing the property member from input, if none */
  /* is provided then look for a member named        */
  /* MIGCFG in the same source data set              */
  /*-------------------------------------------------*/
  Parse SOURCE . . thisExec . thisDsn . . . .
  If thisDsn = '?' & propmem = '' Then
  Do
    Say 'Exec is running from compiled REXX. Parse SOURCE not supported.'
    Say 'As such the config data set and member must be passed as a parameter.'
    Call ExitRtn(8)
  End

  If propmem = '' Then
  Do
    If sysdsn("'"thisDsn"(MIGCFG)'") = 'MEMBER NOT FOUND' Then
    Do
      Say 'Configuration member MIGCFG not found in 'thisDsn'.'
      Call ExitRtn(8)
    End
    propmem = thisDsn'('MIGCFG')'
  End
  Else
  Do
    Parse var propmem cfgDsn '(' cfgmem ')' .
    If sysdsn("'"cfgDsn"'") = 'DATASET NOT FOUND' Then
    Do
      Say 'Configuration data set 'cfgDsn' not found.'
      Call ExitRtn(8)
    End
    If cfgMem <> '' Then
    Do
      If sysdsn("'"cfgDsn"("cfgMem")'") = 'MEMBER NOT FOUND' Then
      Do
        Say 'Configuration member 'cfgMem' not found in 'cfgDsn'.'
        Call ExitRtn(8)
      End
    End
  End

  Say 'Parsing the SCLM Migration information from 'propmem

  /*-------------------------------------------------*/
  /* Parsing the content in the property member into */
  /* an array                                        */
  /*-------------------------------------------------*/
  Address TSO "ALLOC F(PROPMEM) DSN('"propmem"') shr"
  Address TSO "EXECIO * DISKR PROPMEM (STEM props. FINIS)"
  Address TSO "FREE F(PROPMEM)"

  /*-------------------------------------------------*/
  /* Define all required and optionals parameters    */
  /*-------------------------------------------------*/
  proj = ''
  group = ''
  projmem = ''
  projdefs = ''
  maclibs = ''
  migHlq = ''
  outputDir = ''
  allLangs = ''
  empty    = ''
  exist    = ''

  /*-------------------------------------------------*/
  /* Parsing the parameters from the property file   */
  /*-------------------------------------------------*/
  Do i = 1 to props.0
    Parse Var props.i key '=' value
    key = strip(Translate(key))
    value = strip(value)
    Select
      When (key = 'PROJ')      Then proj = value
      When (key = 'GROUP')     Then group = value
      When (key = 'PROJMEM')   Then projmem = value
      When (key = 'PROJDEFS')  Then projdefs = value
      When (key = 'MACLIBS')   Then maclibs = value
      When (key = 'MIGHLQ')    Then migHlq = value
      When (key = 'OUTPUTDIR') Then outputDir = value
      When (key = 'ALLLANGS')  Then allLangs = value
      When (key = 'EMPTY')     Then empty = value
      When (key = 'EXIST')     Then exist = value
      Otherwise
        Say 'Invalid property - 'key
    End
  End

  If projmem  = '' Then
    projmem  = proj
  If projdefs = '' Then
    projdefs = "'"STRIP(proj)".PROJDEFS.SOURCE'"
  Else
  Do
    If Substr(projdefs,1,1) <> "'" Then
      projdefs = "'"STRIP(projdefs)"'"
  End
  If maclibs  = '' Then
    maclibs  = "'ISP.SISPMACS'"
  Else
  Do
    workmacs = maclibs
    maclibs = ''
    Do while (workmacs <> '')
      Parse var workmacs maclib ',' workmacs
      If Substr(maclib,1,1) <> "'" Then
        maclib = "'"STRIP(maclib)"'"
      maclibs = maclibs||maclib','
    End
    maclibs = Strip(maclibs,'T',',')
  End
  If migHlq   = '' Then
    migHlq   = Userid()
  If allLangs = '' Then
    allLangs = 'true'

  /*-------------------------------------------------*/
  /* get invokers home directory for temporary files */
  /*-------------------------------------------------*/
  If outputDir = '' Then
  Do
    user = USERID()
    address syscall 'getpwnam (user) pw.'
    outputDir = pw.4
  End
  upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  lower = 'abcdefghijklmnopqrstuvwxyz'
  proj = Translate(proj,lower,upper)

  outputDir = outputDir'/sclmMigration/'proj
  shellcmd = 'mkdir -m 755 -p 'outputDir
  sh_rc = bpxwunix(shellcmd,,stdout.,stderr.)
  If sh_rc <> 0 Then
  Do
    Say 'Unable to create 'outputDir
    If stdout.0 > 0 Then
    Do
       Say '---STDOUT---'
       Do o = 1 to stdout.0
          Say stdout.o
       End
    End
    If stderr.0 > 0 Then
    Do
       Say '---STDERR---'
       Do o = 1 to stderr.0
          Say stderr.o
       End
    End
  End

  db = 0

  rc = repLine('*' Copies('*',80))
  rc = repLine('*' ' ')
  rc = repLine('*' 'SCLM MIGRATION TOOL : Part 1 - Started 'Date() Time())
  rc = repLine('*' ' ')
  rc = repLine('*' Copies('*',80))

  /* Going to process the SCLM project and store for later use */

  Address TSO "ALLOC F(ISRLCODE) DA("projdefs","maclibs") SHR"
  Address TSO "ALLOC F(ISRLEXPD) NEW SPACE(2,2) CYL"
  Address TSO "ALLOC F(ISRLMSG)  NEW SPACE(2,2) CYL " ||,
                    "RECFM(F B A) LRECL(133) BLKSIZE(3059)"

  PARMS = 'ASM,'projmem',F,Y,E,4, ,00,ENU,4,7,1,/,SYSALLDA'
  Address LINKMVS 'ISRLEMX PARMS'

  If RC <> 0 Then
  Do
    rc = repLine(' ' ' ')
    rc = repLine('E' 'ISRLEMX failed with RC('RC')')
    Address TSO "EXECIO * DISKR ISRLMSG (STEM isrlmsg. FINIS)"
    rc = repLine(' ' ' ')
    Do i = 1 to isrlmsg.0
      rc = repLine('E' isrlmsg.i)
    End
    Call ExitRtn(8)
  End
  Else
  Do
    "ISPEXEC LMINIT DATAID(DID) DDNAME(ISRLEXPD) ENQ(SHR)"

    Address TSO "EXECIO * DISKR ISRLEXPD (STEM isrlexpd. FINIS)"

    Address TSO "FREE F(ISRLCODE)"
    Address TSO "FREE F(ISRLEXPD)"
    Address TSO "FREE F(ISRLMSG)"

    "ISPEXEC LMFREE DATAID("DID")"

    elapsed = Time('E')
    rc = repLine(' ' ' ')
    rc = repLine('I' 'Triming project definition source')

    Call oneLine

    elapsed = Time('E')
    rc = repLine('I' 'Triming project definition source complete in 'elapsed)

    If projdefs.0 = 0 Then
    Do
      rc = repLine(' ' ' ')
      rc = repLine('E' 'Projdefs is empty. Something went wrong')
      Call ExitRtn(8)
    End

    Call getLang

  End

  /* First we are going to process the ARCHDEFs                 */
  rc = repLine(' ' ' ')
  rc = repLine('I' 'Processing ARCHDEFs')
  rc = DButilArch()
  If rc > 4 Then
    Call ExitRtn(8)

  elapsed = Time('E')
  /* Work out the type of archdef                               */
  Call getArchMem

  elapsed = Time('E')
  rc = repLine('I' 'Processing ARCHDEFs complete in 'elapsed)

  /* Process LEC archdefs to generate link decks                */
  rc = repLine(' ' ' ')
  rc = repLine('I' 'Generating link decks')
  If rc = 0 Then
    Call ProcArch

  elapsed = Time('E')
  rc = repLine('I' 'Generating link decks complete in 'elapsed)

  /* Next we are going to process the member information        */
  rc = DButilMems()
  If rc > 4 Then
    Call ExitRtn(8)

  /* Update member info stem with language definition           */
  rc = repLine(' ' ' ')
  rc = repLine('I' 'Processing the members')
  Call ProcMems

  elapsed = Time('E')
  rc = repLine('I' 'Processing the members complete in 'elapsed)

  /* If we are only going to migrate languages that are         */
  /* assigned to members then get rid of redundant languages    */

  j = 0
  Do i = 1 to ll
    If allLangs = 'false' Then
    Do
      If langs.i.dfltsrc <> '' Then
      Do
        j = j + 1
        dfltsrc.j = langs.i':'langs.i.dfltsrc':'langs.i.ext
      End
    End
    Else
    Do
      j = j + 1
      dfltsrc.j = langs.i':'langs.i.dfltsrc':'langs.i.ext
    End
  End

  dfltsrc.0 = j
  extfile  = outputDir'/langext.txt'
  Address syscall "writefile (extfile) 755 dfltsrc."

  /* Create member XML                                          */
  Call createXml

  rc = repLine(' ' ' ')
  rc = repLine('*' Copies('*',80))
  rc = repLine('*' ' ')
  rc = repLine('*' 'SCLM MIGRATION TOOL : Part 1 - Finished 'Date() Time())
  rc = repLine('*' ' ')
  rc = repLine('*' Copies('*',80))

  Call ExitRtn(0)
Exit

/*---------------------------------------------------------------*/
/* Put each macro on one line and get rid of comments            */
/*---------------------------------------------------------------*/

OneLine :

  /* Need to get rid of comment lines and blank lines             */

  j = 0
  Do i = 1 to isrlexpd.0 While (Pos('FLMAEND',isrlexpd.i) = 0)
    If Substr(isrlexpd.i,1,1) <> '*' |,
       Pos('* Begin copied member - ',isrlexpd.i) <> 0 |,
       Pos('*   End copied member - ',isrlexpd.i) <> 0 Then
    Do
      If isrlexpd.i <> '' Then
      Do
        j = j + 1
        noComment.j = isrlexpd.i
      End
    End
  End
  j = j + 1
  noComment.j = isrlexpd.i
  noComment.0 = j
  rc = repLine('I' 'Count of lines after blanks removed :' j)

  /* Need to put each macro onto a single line for ease of parsing */
  j = 0
  Line = ''
  Do i = 1 to noComment.0
    If Substr(noComment.i,72,1) <> ' ' then
    Do
      /* Find the last comma. Everthing between the last comma */
      /* followed by blank and the continuation is junk        */
      If Substr(noComment.i,71,1) = ',' Then
        line = Line || Strip(Substr(noComment.i,1,xx))
      Else
      Do
        xx = Lastpos(', ',Substr(noComment.i,1,71))
        If xx = 0 Then
          line = Line || Strip(Substr(noComment.i,1,71))
        Else
          line = Line || Strip(Substr(noComment.i,1,xx))
      End
    End
    Else
    Do
      j = j + 1
      delTrans.j = Line || Strip(Substr(noComment.i,1,71))
      Line = ''
    End
  End
  delTrans.0 = j
  noComment. = ''
  rc = repLine('I' 'Count of lines after put on one line :' j)

  /* Need to get rid of redundant translators PROMOTE VERIFY, etc */

  j = 0
  Do i = 1 to delTrans.0
    If Pos('FLMTRNSL',delTrans.i) <> 0 Then
    Do
      Select
        When (Pos('FUNCTN=PARSE',delTrans.i) = 0 &,
              Pos('FUNCTN=BUILD',delTrans.i) = 0) Then
        Do
          i = i + 1
          Do While (Pos('FLMSYSLB',delTrans.i) = 0 &,
                    Pos('FLMLANGL',delTrans.i) = 0 &,
                    Pos('FLMTRNSL',delTrans.i) = 0 &,
                    Pos('FLMAEND',delTrans.i) = 0  &,
                    Pos('*   End copied member - ',delTrans.i) = 0)
            i = i + 1
          End
          i = i - 1
        End
        When (Pos('FUNCTN=PARSE',delTrans.i) <> 0) Then
        Do
          j = j + 1
          /* Make sure there's a blank between first keyword and rest of line */
          projdefs.j = Substr(delTrans.i,1,8)' 'Substr(delTrans.i,9)
          i = i + 1
          /* Get rid of the FLMALLOC and FLMSYSLB after PARSE */
          Do While (Pos('FLMSYSLB',delTrans.i) = 0 &,
                    Pos('FLMLANGL',delTrans.i) = 0 &,
                    Pos('FLMTRNSL',delTrans.i) = 0 &,
                    Pos('FLMAEND',delTrans.i) = 0  &,
                    Pos('*   End copied member - ',delTrans.i) = 0)
            i = i + 1
          End
          i = i - 1
        End
        Otherwise
        Do
          j = j + 1
          /* Make sure there's a blank between first keyword and rest of line */
          projdefs.j = Substr(delTrans.i,1,8)' 'Substr(delTrans.i,9)
        End
      End
    End
    Else
    Do
      j = j + 1
      /* Make sure there is a blank between first keyword and rest of line */
      projdefs.j = Substr(delTrans.i,1,8)' 'Substr(delTrans.i,9)
    End
  End
  projdefs.0 = j
  delTrans.  = ''
  rc = repLine('I' 'Count of lines after redundant translators removed :' j)

  projseq = outputDir'/projseq.txt'
  Address syscall "writefile (projseq) 755 projdefs."

Return

/**********************************************************************/
/* Read the FLMLANGL to work out the language extension               */
/**********************************************************************/
getLang :

  ll = 0
  langs. = ''

  Do langCnt = 1 to projdefs.0
    Select
      When (Pos('FLMLANGL',projdefs.langcnt) <> 0) Then
      Do
        /* Not an ARCHDEF Language definition */
        If Pos('ARCH=Y',projdefs.langcnt) = 0 Then
        Do
          Parse var projdefs.langcnt . 'LANG='language ',' .

          /* get rid of extra rubbish */
          Parse var language language .

          /* going to create them all for now */
          language = Strip(language)
          /* Need to see if we have this lang already */
          Do z = 1 to ll While (language <> langs.z)
          End
          If z > ll Then
          Do
            ll = ll + 1
            langs.ll = language
            langs.ll.dfltsrc = ''
            langs.ll.languageCode = 'OTH'
            langs.ll.ext = 'txt'
          End

          /* Use the parser translator to determine the language */

          langcnt = langcnt + 1

          Call parsers

          Call LangTran(language)

          Select
            When (langs.ll.languageCode = 'ASM') Then
            Do
              If noOfTran > 0 Then
                langs.ll.ext = 'asm'
              Else
                langs.ll.ext = 'mac'
            End
            When (langs.ll.languageCode = 'COB') Then
            Do
              If noOfTran > 0 Then
                langs.ll.ext = 'cbl'
              Else
                langs.ll.ext = 'cpy'
            End
            When (langs.ll.languageCode = 'PLI') Then
            Do
              If noOfTran > 0 Then
                langs.ll.ext = 'pli'
              Else
                langs.ll.ext = 'plincl'
            End
            When (langs.ll.languageCode = 'PLX') Then
            Do
              If noOfTran > 0 Then
                langs.ll.ext = 'plx'
              Else
                langs.ll.ext = 'mac'
            End
            When (langs.ll.languageCode = 'C')   Then
            Do
              If noOfTran > 0 Then
                langs.ll.ext = 'c'
              Else
                langs.ll.ext = 'h'
            End
            When (langs.ll.languageCode = 'LNK') Then langs.ll.ext = 'bnd'
            When (langs.ll.languageCode = 'JOV') Then langs.ll.ext = 'jov'
            When (langs.ll.languageCode = 'PAS') Then langs.ll.ext = 'pas'
            Otherwise
              Nop
          End
        End
      End
      Otherwise
        NOP
    End
  End

Return

/*---------------------------------------------------------------*/
/* Use the parser if one exists to determine language            */
/*---------------------------------------------------------------*/
parsers :

  Do parsecnt = langcnt to projdefs.0,
                While (Pos('FLMLANGL',projdefs.parsecnt) = 0)
    Select
      When (Pos('FLMTRNSL',projdefs.parsecnt) <> 0) Then
      Do
        If Pos('FUNCTN=PARSE',projdefs.parsecnt) <> 0 Then
        Do
          Parse var projdefs.parsecnt . 'COMPILE='Parser ',' .
          Parse var projdefs.parsecnt . 'LANG='ParsLang ',' .

          /* get rid of extra rubbish */
          Parse var Parser Parser .
          ParsLang = Substr(ParsLang,1,1)

          /* going to create them all for now */
          Parser = Strip(Parser)
          Select
            When (Parser = 'FLMLPCBL' |,
                  Parser = 'FLMLRCBL') Then
            Do
              langs.ll.languageCode = 'COB'
            End
            When (Parser = 'FLMLPFRT') Then
            Do
              langs.ll.languageCode = 'OTH'
            End
            When (Parser = 'FLMLPGEN') Then
            Do
              Select
                When (ParsLang = 'A') Then
                  langs.ll.languageCode = 'ASM'
                When (ParsLang = 'T') Then
                  langs.ll.languageCode = 'OTH'
                When (ParsLang = 'R') Then
                  langs.ll.languageCode = 'REX'
                When (ParsLang = 'C') Then
                  langs.ll.languageCode = 'OTH'
                When (ParsLang = 'I') Then
                  langs.ll.languageCode = 'PLI'
                When (ParsLang = '1') Then
                  langs.ll.languageCode = 'PLI'
                When (ParsLang = 'L') Then
                  langs.ll.languageCode = 'PAS'
                Otherwise
                Do
                  rc = repLine(' ' ' ')
                  rc = repLine('W' 'Unknown parser language - 'ParsLang)
                End
              End
            End
            When (Parser = 'FLMLRASM') Then
            Do
              langs.ll.languageCode = 'ASM'
            End
            When (Parser = 'FLMLRCIS' |,
                  Parser = 'FLMLRC37') Then
            Do
              langs.ll.languageCode = 'C'
            End
            When (Parser = 'FLMLRDTL') Then
            Do
              langs.ll.languageCode = 'DTL'
            End
            When (Parser = 'FLMLSS') Then
            Do
              rc = repLine(' ' ' ')
              rc = repLine('W' 'SCLM DB2 CLIST parser - 'Parser ||,
                                  ' is not supported')
            End
            Otherwise
            Do
              rc = repLine(' ' ' ')
              rc = repLine('W' 'Unknown parser - 'Parser)
            End
          End

        End
      End
      Otherwise
        Nop
    End
  End

Return

/*---------------------------------------------------------------*/
/* Get a list of Translators in a language definition            */
/*---------------------------------------------------------------*/
langTran :

  Parse arg Lang

  noOfTran = 0

  Do trancnt = langcnt to projdefs.0,
               While (Pos('FLMLANGL',projdefs.trancnt) = 0)
    Select
      When (Pos('FLMTRNSL',projdefs.trancnt) <> 0) Then
      Do
        If Pos('FUNCTN=BUILD',projdefs.trancnt) <> 0 Then
        Do

          noOfTran = noOfTran + 1

          /* Get translator options                  */

          Call TranOpts

          translator  = ''
          Compiler    = ''
          dsname      = ''
          GoodRc      = ''
          Pord        = ''
          CompOptions = ''
          Tasklib     = ''

          Do st = 1 to trnsStmt.0
            Select
              When trnsStmt.st = 'CALLNAM' Then
                translator  = trnsStmt.st.stmtParm
              When trnsStmt.st = 'COMPILE' Then
                Compiler    = trnsStmt.st.stmtParm
              When trnsStmt.st = 'DSNAME'  Then
                dsname      = trnsStmt.st.stmtParm
              When trnsStmt.st = 'GOODRC'  Then
                GoodRc      = trnsStmt.st.stmtParm
              When trnsStmt.st = 'PORDER'  Then
                Pord        = trnsStmt.st.stmtParm
              When trnsStmt.st = 'OPTIONS' Then
                CompOptions = trnsStmt.st.stmtParm
              When trnsStmt.st = 'TASKLIB' Then
                Tasklib     = trnsStmt.st.stmtParm
              Otherwise
                Nop
            End
          End

          /* Need to make sure right language set for obvious langs */
          If langs.ll.languageCode = 'OTH' Then
          Do
            Select
              When (Compiler = 'ASMA90')   Then langs.ll.languageCode = 'ASM'
              When (Compiler = 'IEV90')    Then langs.ll.languageCode = 'ASM'
              When (Compiler = 'IGYCRCTL') Then langs.ll.languageCode = 'COB'
              When (Compiler = 'IKFCBL00') Then langs.ll.languageCode = 'COB'
              When (Compiler = 'IEL0AA')   Then langs.ll.languageCode = 'PLI'
              When (Compiler = 'IBMZPLI')  Then langs.ll.languageCode = 'PLI'
              When (Compiler = 'AKEEPLX')  Then langs.ll.languageCode = 'PLX'
              When (Compiler = 'AKHPLX')   Then langs.ll.languageCode = 'PLX'
              When (Compiler = 'AKHPLX2')  Then langs.ll.languageCode = 'PLX'
              When (Compiler = 'AKHTSO')   Then langs.ll.languageCode = 'PLX'
              When (Compiler = 'PLX390')   Then langs.ll.languageCode = 'PLX'
              When (Compiler = 'CCNDRVR')  Then langs.ll.languageCode = 'C'
              When (Compiler = 'CCNED230') Then langs.ll.languageCode = 'C'
              When (Compiler = 'EDCCOMP')  Then langs.ll.languageCode = 'C'
              When (Compiler = 'IEWL')     Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'HEWL')     Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'HEWLDRGO') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'HEWLH096') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'HEWLOAD')  Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'HEWLOADR') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'IEWBLDGO') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'IEWBLIMG') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'IEWBLOAD') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'IEWBLODI') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'IEWBODEF') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'IEWLDRGO') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'IEWLOAD')  Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'IEWLOADI') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'IEWLOADR') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'LINKEDIT') Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'LOADER')   Then langs.ll.languageCode = 'LNK'
              When (Compiler = 'JOVIAL')   Then langs.ll.languageCode = 'JOV'
              When (Compiler = 'PASCALI')  Then langs.ll.languageCode = 'PAS'
              Otherwise
                Nop
            End
          End
        End
      End
      Otherwise
        Nop
    End
  End

  langcnt = trancnt - 1

Return

/*---------------------------------------------------------------*/
/* Read and store FLMTRNSL keywords. Could not use REXX parse    */
/* as OPTIONS contains anything I could use as a delimiter       */
/*---------------------------------------------------------------*/

TranOpts :

  trnslopt = 'CALLNAM FUNCTN COMPILE DSNAME GOODRC NOSVEXT OPTFLAG ' ||,
             'OPTIONS PARMKWD PDSDATA PORDER VERSION CALLMETH ' ||,
             'TASKLIB INPLIST MBRRC'

  workStmt  = projdefs.trancnt
  trnsStmt. = ''
  st = 0

  processed = 0

  Do While (processed = 0)
    /* Gives us the position of the next = so therefore next option */
    x = Pos('=',workStmt)
    If x = 0 Then
    Do
      If SCLMlast <> '' Then
      Do
        st = st + 1
        trnsStmt.st = SCLMlast
        trnsStmt.st.stmtParm = lastStmt
      End
      processed = 1
    End
    Else
    Do
      /* Find the previous blank or comma */
      Do i = x by -1 while (i > 0)
        If Substr(workStmt,i,1) = ',' |,
           Substr(workStmt,i,1) = ' ' Then
          Leave
      End
      If i = 0 then
        i = 1
      Else
        i = i + 1
      SCLMstmt = Substr(workStmt,i,x-i)
      /* This is an SCLM keyword so continue */
      If Wordpos(SCLMstmt,trnslopt) > 0 Then
      Do
        /* This might be the last statement so store it */
        SCLMlast = SCLMstmt
        lastStmt = Substr(workStmt,x+1)
        /* Need to find the next SCLM option if there is one */
        workStmt = Substr(workStmt,x+1)
        x = Pos('=',workStmt)
        /* If this was the last statement then just assign rest */
        If x = 0 Then
        Do
          st = st + 1
          trnsStmt.st = SCLMstmt
          trnsStmt.st.stmtParm = workStmt
          processed = 1
        End
        Else
        Do
          /* Find the previous blank or comma */
          Do i = x by -1 while (i > 0)
            If Substr(workStmt,i,1) = ',' |,
               Substr(workStmt,i,1) = ' ' Then
              Leave
          End
          If i = 0 then
            i = 1
          Else
            i = i + 1
          SCLMnext = Substr(workStmt,i,x-i)
          /* This is an SCLM keyword assign parms */
          If Wordpos(SCLMnext,trnslopt) > 0 Then
          Do
            st = st + 1
            trnsStmt.st = SCLMstmt
            trnsStmt.st.stmtParm = Substr(workStmt,1,i-2)
            workStmt = Substr(workStmt,i)
          End
        End
      End
      Else
        workStmt = Substr(workStmt,x+1)
    End
  End
  trnsStmt.0 = st

Return

/**********************************************************************/
/* Run DBUTIL report to list every member that is in the 'PROD' group */
/* that is an ARCHDEF member                                          */
/**********************************************************************/
DButilArch:

  "ALLOC F(DUMMY) DUMMY"
  "ALLOC F(DBMSG) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(5 5)",
          "TRACKS REUSE"
  "ALLOC F(DBRPT) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(5 5)",
          "TRACKS REUSE"

  "FLMCMD DBUTIL,"proj",,"group",,,,,,*,*"||,
                ",*,*,*,*,ARCHDEF,NO,ACCT,*,,,,NORMAL,NO,NO,," ||,
                "DBMSG,DUMMY,DBRPT" ||,
                ",@@FLMMBR @@FLMLAN @@FLMTYP"
  dbutil_rc = rc

  If dbutil_rc > 0  Then
  Do
    rc = repLine(' ' ' ')
    rc = repLine('E' 'DBUTIL error, return code ='rc)
    rc = repLine(' ' ' ')
    "EXECIO * DISKR DBMSG (STEM dbmsgs. FINIS)"
    Do i = 1 to dbmsgs.0
      rc = repLine(' ' dbmsgs.i)
    End
  End
  "EXECIO * DISKR DBRPT (STEM dbutil. FINIS)"
  rc = repLine(' ' ' ')
  rc = repLine('I' 'DBUTIL returned 'dbutil.0' ARCHDEFS')

  "FREE F(DUMMY)"
  "FREE F(DBMSG)"
  "FREE F(DBRPT)"

Return dbutil_rc

/**********************************************************************/
/* Run DBUTIL report to list every member that is in the 'PROD' group */
/* and store the data in a stem variable.                             */
/**********************************************************************/
DButilMems :

  "ALLOC F(DUMMY) DUMMY"
  "ALLOC F(DBRPT) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(5 5)",
          "TRACKS REUSE"

  "FLMCMD DBUTIL,"proj",,"group",,,,,,*,*"||,
                ",*,*,*,*,*,NO,ACCT,*,,,,NORMAL,NO,NO,," ||,
                "DUMMY,DUMMY,DBRPT" ||,
                ",@@FLMMBR @@FLMLAN @@FLMTYP @@FLMSTA"
  dbutil_rc = rc

  If dbutil_rc > 0  Then
  Do
    rc = repLine(' ' ' ')
    rc = repLine('E' 'DBUTIL error, return code ='rc)
    rc = repLine(' ' ' ')
    "EXECIO * DISKR DBMSG (STEM dbmsgs. FINIS)"
    Do i = 1 to dbmsgs.0
      rc = repLine(' ' dbmsgs.i)
    End
  End
  Else
    "EXECIO * DISKR DBRPT (STEM dbutil. FINIS)"
  rc = repLine(' ' ' ')
  rc = repLine('I' 'DBUTIL returned 'dbutil.0' members')

  "FREE F(DUMMY)"
  "FREE F(DBRPT)"

Return dbutil_rc

/**********************************************************************/
/* Process the ARCHDEFs                                               */
/**********************************************************************/

ProcArch :

  /* Need to do a 2 pass process because of the INCL statements */
  /* First pass to work out what type of ARCHDEFs they are      */
  /* Second pass to process the LEC and CC ARCHDEFs             */

  bndDsn = "'"migHlq".SCLMMIG.BND'"
  ListdsiRC = Listdsi(bndDsn)
  Select
    When (ListdsiRC = 0) Then
      Nop
    When (ListdsiRC = 16 & SYSREASON = 5) Then
    Do
      "ALLOC DA("bndDsn") RECFM(F B) LRECL(80) BLKSIZE(27920) " ||,
            "DSNTYPE(LIBRARY) SPACE(1 10) CYLINDERS DSORG(PO)"
      rc = repLine(' ' ' ')
      rc = repLine('I' "Data set "bndDsn" allocated")
    End
    Otherwise
    Do
      rc = repLine(' ' ' ')
      rc = repLine('E' "Problem allocating "bndDsn)
      rc = repLine('E' "LISTDSI Return code "ListdsiRC ||,
                         "Reason Code "SYSREASON)
      Call ExitRtn(8)
    End
  End
  /* Get rid of quotes for member usage */
  bndDsn = Strip(bndDsn,,"'")
  bndCnt = 0

  If pl > 0 Then
  Do
    prebndDsn = "'"migHlq".SCLMMIG.PREBND'"
    ListdsiRC = Listdsi(prebndDsn)
    Select
      When (ListdsiRC = 0) Then
        Nop
      When (ListdsiRC = 16 & SYSREASON = 5) Then
      Do
        "ALLOC DA("prebndDsn") RECFM(F B) LRECL(80) BLKSIZE(27920) " ||,
              "DSNTYPE(LIBRARY) SPACE(1 10) CYLINDERS DSORG(PO)"
        rc = repLine(' ' ' ')
        rc = repLine('I' "Data set "prebndDsn" allocated")
      End
      Otherwise
      Do
        rc = repLine(' ' ' ')
        rc = repLine('E' "Problem allocating "prebndDsn)
        rc = repLine('E' "LISTDSI Return code "ListdsiRC ||,
                           "Reason Code "SYSREASON)
        Call ExitRtn(8)
      End
    End
    /* Get rid of quotes for member usage */
    prebndDsn = Strip(prebndDsn,,"'")
    prebndCnt = 0
  End

  /* Create Link deck library if it does not exist from LEC ARCHDEF */
  Do i = 1 to archmems.0
    parse var archmems.i archMem':'typeName':'archType':'isProcessed
    Select
      When (archType = 'LEC') Then
      Do
        CCCnt  = 0
        LECcnt = LECcnt + 1
        memCnt = memCnt + 1
        memInfo.memCnt.name = Strip(archMem)||'.bnd'
        memInfo.memCnt.type = Strip(typeName)
        memInfo.memCnt.memLang = 'LE370'
        keyCnt = 0

        rc = readArchdef(2 archMem)
        If rc = 0 Then
          call CreateBnd(archMem 'LEC')
        memInfo.memCnt.stmt.0 = keyCnt
        TotCC = TotCC + CCCnt
        memCnt = LECcnt + TotCC
        archMems.i = archMem':'typeName':'archType':Processed'
      End
      When (archType = 'PRELEC') Then
      Do
        CCCnt  = 0
        LECcnt = LECcnt + 1
        memCnt = memCnt + 1
        memInfo.memCnt.name = Strip(archMem)||'.prebnd'
        memInfo.memCnt.type = Strip(typeName)
        memInfo.memCnt.memLang = 'LE370'
        keyCnt = 0

        rc = readArchdef(2 archMem)
        If rc = 0 Then
          call createBnd(archMem 'PRELEC')
        memInfo.memCnt.stmt.0 = keyCnt
        TotCC = TotCC + CCCnt
        memCnt = LECcnt + TotCC
        archMems.i = archMem':'typeName':'archType':Processed'
      End
      Otherwise
        Nop
    End
  End
  /* Process the generic ARCHDEFs seperately                        */
  Do i = 1 to archmems.0
    parse var archmems.i archMem':'typeName':'archType':'isProcessed
    Select
      When (archType = 'GEN') Then
      Do
        If isProcessed = '' Then
        Do
          CCCnt  = 0
          GENcnt = GENcnt + 1
          memCnt = memCnt + 1
          memInfo.memCnt.name = Strip(archMem)||'.txt'
          memInfo.memCnt.type = Strip(typeName)
          memInfo.memCnt.memLang = '?'
          keyCnt = 0

          rc = readArchdef(2 archMem)

          memInfo.memCnt.stmt.0 = keyCnt
          TotCC = TotCC + CCCnt
          memCnt = LECcnt + TotCC + Gencnt
          archMems.i = archMem':'typeName':'archType':Processed'
        End
      End
      Otherwise
        Nop
    End
  End
  rc = repLine(' ' ' ')
  rc = repLine('I' 'Count of link-edit members created : 'bndCnt)
  rc = repLine(' ' ' ')
  rc = repLine('I' 'Count of pre-link members created : 'prebndCnt)
  Call keyRefXml

  archfile  = outputDir'/archtype.txt'
  Address syscall "writefile (archfile) 755 archmems."

  /* Free up memory   */
  archmems. = ''

Return

/**********************************************************************/
/* Process the members account information                            */
/**********************************************************************/

ProcMems :

  Do m = 1 to dbutil.0

    Parse var dbutil.m sclmmem 9 sclmlang 18 sclmtype 27 status .
    sclmmem  = Strip(sclmmem)
    sclmlang = Strip(sclmlang)
    sclmtype = Strip(sclmtype)
    status   = Strip(status)

    /* ARCHDEFS already processed so      */
    /* only care about buildable parts    */
    If sclmlang = '' | sclmlang = 'ARCHDEF' | status = 'NON-EDIT' Then
      Iterate

    /* Need to update the language:defaultType:default extension list */
    Do z = 1 to ll While (sclmlang <> langs.z)
    End
    If z > ll Then
    Do
      rc = repLine(' ' ' ')
      rc = repLine('W' sclmmem 'has type of 'sclmtype ||,
                         ' and language of 'sclmlang'.' ||,
                         ' But language not found in language stem')
    End
    Else
    Do
      Select
        When (langs.z.dfltsrc = '') Then
          langs.z.dfltsrc = sclmtype

        When (langs.z.dfltsrc <> sclmtype) Then
        Do
          rc = repLine(' ' ' ')
          rc = repLine('W' sclmmem 'has type of 'sclmtype ||,
                             ' and language of 'sclmlang'.' ||,
                             ' But default type already set to ' ||,
                             langs.z.dfltsrc' for this language.')
          rc = repLine('W' 'Adding the default type and extension anyway,' ||,
                             ' but default SYSLIBs may be generated ' ||,
                             ' incorrectly if there is no FLMINCLS.')
          ll = ll + 1
          langs.ll = langs.z
          langs.ll.dfltsrc = sclmtype
          langs.ll.languageCode = langs.z.languageCode
          langs.ll.ext = langs.z.ext
        End
        Otherwise
          Nop
      End
      sclmext = langs.z.ext
    End

    /* Need to see if we have this member/type already in member list */
    Do z = 1 to memCnt,
           While (sclmmem'.'sclmext||sclmtype <> memInfo.z.name||meminfo.z.type)
    End
    If z > memCnt Then
    Do
      /* need to get the default file extension for the member */
      memCnt = memCnt + 1
      memInfo.memCnt.name     = sclmmem'.'sclmext
      memInfo.memCnt.type     = sclmtype
      memInfo.memCnt.memLang  = sclmlang
      memInfo.memCnt.stmt.0   = 0
    End
  End

  /* Free up memory   */
  dbutil. = ''

Return

/**********************************************************************/
/* Save members by type and archdef type (HL, LEC, CC, GEN)           */
/**********************************************************************/
getArchMem:

  mm = 0
  archMems. = ''
  memCnt = 0
  LECcnt = 0
  GENcnt = 0
  TotCC  = 0
  TotGen = 0
  memInfo.  = ''
  pl = 0
  preLink.  = ''

  rc = repLine(' ' ' ')
  Do m = 1 to dbutil.0

    Parse var dbutil.m mem 9 lang 18 typeName 27 .
    mem      = Strip(mem)
    lang     = Strip(lang)
    typeName = Strip(typeName)

    If lang = '' Then Iterate

    /* ARCHDEFS not supported in DBB/RTC             */
    /* Going to store ARCHDEF names and their type   */
    Call readArchdef(1 mem)

    mm = mm + 1
    archMems.mm = mem||':'||typeName||':'||archType
  End
  archMems.0 = mm

  Do m = 1 to archMems.0
    parse var archMems.m mem':'typeName':'archType
    Do p = 1 to pl
      If mem||typeName = preLink.p.plname||preLink.p.pltype Then
      Do
        archType = 'PRELEC'
        archMems.m = mem||':'||typeName||':'||archType
        Leave
      End
    End
  End

  archfile  = outputDir'/archtype.txt'
  Address syscall "writefile (archfile) 755 archmems."

Return

/**********************************************************************/
/* Read Architecture member to determine what type of ARCHDEF each    */
/* member is.                                                         */
/**********************************************************************/
readArchdef:

  parse arg pass mem

  "FLMCMD DSALLOC,"proj",,"group",P,1,"typeName",ARCHDEF"
  Address ISPEXEC "QBASELIB ARCHDEF ID(ARCHDEF)"
  "FREE  F(ARCHDEF)"
  ARCHDEF = Strip(ARCHDEF,,"'")

  "ALLOC F(ARCHDEF) DA('"ARCHDEF"("mem")') SHR"
  "EXECIO * DISKR ARCHDEF (STEM archdef. FINIS)"
  "FREE  F(ARCHDEF)"

  If pass = 1 Then
  Do
    Load = 0
    Lmap = 0
    Obj  = 0
    Sinc = 0
    Lked = 0
    Out  = 0
    Alias= 0
    Cmd  = 0
    Kref = 0
    Link = 0
    List = 0
    Parm = 0
    Sref = 0
  End

  archRc = 0
  rr = 0          /* Recursive read counter */
  ii = 0
  cc = 0
  aa = 0
  ld = 0
  includes. = ''
  cmds.     = ''
  aliases.  = ''
  linkdeck  = ''
  storeType = archType

  Do a = 1 to archdef.0
    recursINCL = 0
    archdef.a = Strip(Substr(archdef.a,1,72))

    Call procStmt(archdef.a)
  End
  archType = storeType

  If pass = 1 Then
  Do
    archType = ''

    Select
      When (Load = 1) Then
      Do
        archtype = 'LEC'

        /* Need to check if this archdef contains recursive INCLs */
        Call preLec

      End
      When (Obj = 1 & Sinc = 1) Then
        archtype = 'CC'
      When (Sinc = 1) Then
        archtype = 'GEN'
      When (Cmd = 1 | Kref = 1 | Link = 1 | List = 1 | Lked = 1 |,
            Out = 1 | Parm = 1 | Sref = 1) Then
        archtype = 'COPY'
      Otherwise
        archtype = 'HL'
    End
  End
  Else
  Do
    Select
      When(archType = 'LEC') Then
      Do
        /* Using the populated stems lets build the link deck */
        Do inc = 1 to ii
          ld = ld + 1
          linkdeck.ld = includes.inc
        End
        Do cmd = 1 to cc
          ld = ld + 1
          linkdeck.ld = cmds.cmd
        End
        Do ali = 1 to aa
          ld = ld + 1
          linkdeck.ld = aliases.ali
        End
        ld = ld + 1
        linkdeck.ld = " NAME     "module"(R)"
      End
      When(archType = 'PRELEC') Then
      Do
        /* Using the populated stems lets build the link deck */
        Do inc = 1 to ii
          ld = ld + 1
          linkdeck.ld = includes.inc
        End
        Do cmd = 1 to cc
          ld = ld + 1
          linkdeck.ld = cmds.cmd
        End
        Do ali = 1 to aa
          ld = ld + 1
          linkdeck.ld = aliases.ali
        End
      End
      Otherwise
        Nop
    End
  End

  linkdeck.0 = ld

Return archRc

/**********************************************************************/
/* Check if LEC ARCHDEF has recursive INCLs                           */
/**********************************************************************/
preLec :

  /* Need to work out if this is a real LEC or a   */
  /* pre-link INCL. Need to process these separate */
  /* to the LEC ARCHDEFs.                          */

  Do a = 1 to archdef.0

    archdef.a = Strip(Substr(archdef.a,1,72))

    If Substr(archdef.a,1,5) = 'INCL ' Then
    Do
      Parse var archdef.a 'INCL ' memName copytype .
      "FLMCMD DSALLOC,"proj",,"group",P,1,"copytype",COPY"
      Address ISPEXEC "QBASELIB COPY ID(COPY)"
      "FREE  F(COPY)"
      COPY = Strip(COPY,,"'")

      If sysdsn("'"COPY"("memName")'") = 'MEMBER NOT FOUND' Then
        rc = repLine('W' 'INCL or COPY Member 'memName' from data set 'COPY||,
                         'was not found. It has been ignored')
      Else
      Do
        "ALLOC F(COPY) DA('"COPY"("memName")') SHR"
        "EXECIO * DISKR COPY (STEM inclChk. FINIS)"
        "FREE  F(COPY)"

        preLecFound = 0

        Do ic = 1 to inclChk.0 While (preLecFound = 0)
          If Pos('INCL ',inclChk.ic) <> 0 Then
          Do
            rc = repLine('W' 'INCL Member 'memName' of type 'copyType ||,
                             ' contains a recursive INCL statement. ' ||,
                             'Assuming this is a Pre link ARCHDEF.')

            Do z = 1 to pl While (memName||copyType <>,
                                  prelink.z.plname||prelink.z.pltype)
            End
            If z > pl Then
            Do
              pl = pl + 1
              preLink.pl.plname = memName
              preLink.pl.pltype = copyType
            End
            preLecFound = 1
          End
        End
      End
    End
  End

Return

/**********************************************************************/
/* Process each statement possibly recursively for COPY and INCL      */
/**********************************************************************/
procStmt:

  parse arg statement

  Select
    When (Substr(statement,1,5) = 'LOAD ') Then
    Do
      Load = 1
      If pass = 2 Then
      Do
        keyCnt = keyCnt + 1
        memInfo.memCnt.stmt.keyCnt = "LOAD"

        Parse var statement 'LOAD' module loadLib .

        valCnt = 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "member"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(module)
        valCnt = valCnt + 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "type"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(loadLib)
        memInfo.memCnt.stmt.keyCnt.nameval.0 = 2
      End
    End
    When (Substr(statement,1,5) = 'LMAP ') Then
    Do
      Lmap = 1
      If pass = 2 Then
      Do
        keyCnt = keyCnt + 1
        memInfo.memCnt.stmt.keyCnt = "LMAP"

        Parse var statement 'LMAP' module lmapLib .

        valCnt = 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "member"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(module)
        valCnt = valCnt + 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "type"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(lmapLib)
        memInfo.memCnt.stmt.keyCnt.nameval.0 = 2
      End
    End
    When (Substr(statement,1,4) = 'OBJ ')  Then
    Do
      Obj  = 1
      If pass = 2 Then
      Do
        keyCnt = keyCnt + 1
        memInfo.memCnt.stmt.keyCnt = "OBJ"

        Parse var statement 'OBJ' module objLib .

        ii = ii + 1
        includes.ii = " INCLUDE  SYSLIB("module")"

        valCnt = 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "member"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(module)
        valCnt = valCnt + 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "type"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(objLib)
        memInfo.memCnt.stmt.keyCnt.nameval.0 = 2
      End
    End
    When (Substr(statement,1,4) = 'SINC')  Then
    Do
      Sinc = 1
      If pass = 2 Then
      Do
        Parse var statement 'SINC' SourceMbr SourceLib .
        If archType = 'CC' | archType = 'GEN' Then
        Do
          /* Need to get the Language definition of the source */
          "FLMCMD ACCTINFO,"proj",,"group","SourceLib","SourceMbr

          /* Need to get extension for this language */
          Do z = 1 to ll While (zsalang <> langs.z)
          End
          If z > ll Then
          Do
            rc = repLine(' ' ' ')
            rc = repLine('W' SourceMbr 'has type of 'SourceLib ||,
                               ' and language of 'zsalang'.' ||,
                               ' But language not found in language stem')
          End
          Else
            sclmext = langs.z.ext

          memInfo.memCnt.name = Strip(SourceMbr)'.'sclmext
          memInfo.memCnt.type = Strip(SourceLib)
          memInfo.memCnt.memLang = Strip(zsalang)

          /* Need to see if we have this one already */
          memFound = 0
          Do mc = 1 to memCnt-1 While (memFound = 0)
            If memInfo.memCnt.name = memInfo.mc.name &,
               memInfo.memCnt.type = memInfo.mc.type Then
              memFound = 1
          End

          If archType = 'GEN' Then
          Do
            /* If a GEN ARCHDEF has been used instead of a CC */
            /* There will be no OBJ keyword. So take a punt   */
            /* With the source name                           */
            ii = ii + 1
            includes.ii = " INCLUDE  SYSLIB("SourceMbr")"
          End
        End
      End
    End
    When (Substr(statement,1,4) = 'LKED')  Then
    Do
      Lked = 1
      If pass = 2 Then
      Do
        Parse var statement 'LKED' langDef .
        memInfo.memCnt.memLang = Strip(langDef)
      End
    End
    When (Substr(statement,1,3) = 'OUT')  Then
    Do
      Out = 1
      If pass = 2 Then
      Do
        keyCnt = keyCnt + 1
        memInfo.memCnt.stmt.keyCnt = Substr(statement,1,4)

        Parse var statement outx module outLib .

        valCnt = 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "member"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(module)
        valCnt = valCnt + 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "type"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(outLib)
        memInfo.memCnt.stmt.keyCnt.nameval.0 = 2
      End
    End
    When (Substr(statement,1,6) = 'ALIAS ') Then
    Do
      Alias = 1
      If pass = 2 Then
      Do
        aa = aa + 1
        Parse var statement 'ALIAS' aliases.aa AliasLib .
        aliases.aa = " ALIAS    "Strip(aliases.aa)
      End
    End
    When (Substr(statement,1,4) = 'CMD ')  Then
    Do
      Cmd  = 1
      If pass = 2 Then
      Do
        cc = cc + 1
        Parse var statement 'CMD' cmds.cc
        cmds.cc = " "Strip(cmds.cc)
      End
    End
    When (Substr(statement,1,5) = 'KREF ')  Then
    Do
      Kref = 1
    End
    When (Substr(statement,1,5) = 'LIST ')  Then
    Do
      List = 1
      If pass = 2 Then
      Do
        keyCnt = keyCnt + 1
        memInfo.memCnt.stmt.keyCnt = "LIST"

        Parse var statement 'LIST' module listLib .

        valCnt = 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "member"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(module)
        valCnt = valCnt + 1
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "type"
        memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(listLib)
        memInfo.memCnt.stmt.keyCnt.nameval.0 = 2
      End
    End
    When (Substr(statement,1,5) = 'PARM ')  Then
    Do
      Parm = 1
      /* Translator override parms. Store in member XML */
      /* For example LIST,MAP,AMODE=24                  */
      If pass = 2 Then
      Do
        /* If we already have a PARM for this member    */
        /* Then just append the PARM to it              */
        append = 0
        Do pp = 1 to keyCnt
          If memInfo.memCnt.stmt.pp = 'PARM' Then
          Do
            append = 1

            Parse var statement parmx parameters

            valCnt = 1
            memInfo.memCnt.stmt.pp.nameval.valCnt.val = ,
               memInfo.memCnt.stmt.pp.nameval.valCnt.val || Strip(parameters)
          End
        End
        If append = 0 Then
        Do
          keyCnt = keyCnt + 1
          memInfo.memCnt.stmt.keyCnt = Substr(statement,1,5)

          Parse var statement parmx parameters

          valCnt = 1
          memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "parameters"
          memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(parameters)
          memInfo.memCnt.stmt.keyCnt.nameval.0 = 1
        End
      End
    End
    When (Substr(statement,1,4) = 'PARM')  Then
    Do
      Parm = 1
      /* Translator override parms. Store in member XML */
      /* For example LIST,MAP,AMODE=24                  */
      If pass = 2 Then
      Do

        Parse var statement parmx parameters
        /* If we already have a PARM for this member    */
        /* Then just append the PARM to it              */
        append = 0
        Do pp = 1 to keyCnt
          If memInfo.memCnt.stmt.pp = parmx Then
          Do
            append = 1

            valCnt = 1
            memInfo.memCnt.stmt.pp.nameval.valCnt.val =,
               memInfo.memCnt.stmt.pp.nameval.valCnt.val ||,
               ','Strip(parameters)
          End
        End
        If append = 0 Then
        Do
          keyCnt = keyCnt + 1
          memInfo.memCnt.stmt.keyCnt = parmx

          valCnt = 1
          memInfo.memCnt.stmt.keyCnt.nameval.valCnt.name = "parameters"
          memInfo.memCnt.stmt.keyCnt.nameval.valCnt.val = Strip(parameters)
          memInfo.memCnt.stmt.keyCnt.nameval.0 = 1
        End
      End
    End
    When (Substr(statement,1,4) = 'SREF')  Then
    Do
      Sref = 1
    End
    When (Substr(statement,1,5) = 'INCLD')  Then
    Do
      If pass = 2 Then
      Do
        ii = ii + 1
        Parse var statement 'INCLD' includes.ii SourceLib .
        includes.ii = " INCLUDE  SYSLIB("includes.ii")"
      End
    End
    When (Substr(statement,1,5) = 'INCL ') |,
         (Substr(statement,1,5) = 'LINK ') |,
         (Substr(statement,1,5) = 'COPY ') Then
    Do
      keyword = Substr(statement,1,5)
      If keyword = 'LINK ' Then
        Link = 1
      /* For a LEC ARCHDEF this should be CC ARCHDEF */
      /* So treat it the same way as a COPY          */
      If pass = 2 Then
      Do
        rr = rr + 1
        Select
          When (Keyword = 'INCL ') Then
          Do
            Parse var statement 'INCL' memName copytype .
            /* If this is a CC ARCHDEF then we need to capture info */
            archFound = 0
            notHL     = 1
            Do ccd = 1 to archmems.0 While (archFound = 0 & notHL)
              parse var archmems.ccd ccMem':'incltype':'archType':'isProcessed
              If ccMem = memName & incltype = copyType Then
              Do
                Select
                  /* Don't process the recursive INCL */
                  When (archType = 'PRELEC') Then
                  Do
                    ii = ii + 1
                    includes.ii = " INCLUDE  SYSLIB("memName")"
                    recursINCL = 1
                    rr = rr - 1
                  End
                  /* Don't process the HL in GEN ARCHDEFS */
                  When (archType = 'HL') Then
                  Do
                    notHL     = 0
                  End
                  Otherwise
                  Do
                    archFound = 1
                    archMems.ccd = ccMem':'incltype':'archType':Processed'
                  End
                End
              End
            End

            If archFound = 1 Then
            Do
              CCCnt = CCCnt + 1
              memCnt = memCnt + CCCnt
              storekeyCnt.rr  = keyCnt
              storekeyWord.rr = keyWord
              memInfo.memCnt.name = ccMem
              memInfo.memCnt.type = incltype
              memInfo.memCnt.memLang = '?'
              keyCnt = 0
            End
            Else
              archType = ''
          End
          When (Keyword = 'COPY ') Then
          Do
            Parse var statement 'COPY' memName copytype .
            archFound = 0
            Do ccd = 1 to archmems.0 While (archFound = 0)
              parse var archmems.ccd ccMem':'incltype':'archType':'isProcessed
              If ccMem = memName & incltype = copyType Then
              Do
                archFound = 1
                archMems.ccd = ccMem':'incltype':'archType':Processed'
                storekeyWord.rr = keyWord
              End
            End
          End
          When (Keyword = 'LINK ') Then
            Parse var statement 'LINK' memName copytype .
          Otherwise
            Nop
        End
        If recursINCL = 0 & archFound = 1 Then
        Do
          "FLMCMD DSALLOC,"proj",,"group",P,1,"copytype",COPY"
          Address ISPEXEC "QBASELIB COPY ID(COPY)"
          "FREE  F(COPY)"
          COPY = Strip(COPY,,"'")

          If sysdsn("'"COPY"("memName")'") = 'MEMBER NOT FOUND' Then
          Do
            If keyWord = 'INCL' Then
            Do
              CCCnt = CCCnt - 1
              memCnt = memCnt - 1
            End
          End
          Else
          Do
            stem = "copy"rr"."
            "ALLOC F(COPY) DA('"COPY"("memName")') SHR"
            "EXECIO * DISKR COPY (STEM "stem" FINIS)"
            "FREE  F(COPY)"

            /* Build the recursive call */
            Call interp_stmt("Do a"rr" = 1 to "stem"0;" ||,
                             stem"a"rr" = Strip(Substr("stem"a"rr",1,72));" ||,
                             "Call procStmt("stem"a"rr");" ||,
                             "End")

            memInfo.memCnt.stmt.0 = keyCnt

            If storekeyWord.rr = 'INCL ' &,
              (archType = 'CC' | archType = 'GEN' | archType = 'COPY') Then
            Do
              memCnt = memCnt - CCCnt
              if memFound Then
                CCCnt = CCCnt - 1
              keyCnt = storekeyCnt.rr
              storekeyCnt.rr  = 0
              storekeyWord.rr = ''
            End
            rr = rr - 1
          End
        End
      End
    End
    Otherwise
      /* PROM and CCODE */
      Nop
  End

Return

/*---------------------------------------------------------------*/
/* Rexx interpret to construct a statement and execute it        */
/*---------------------------------------------------------------*/

interp_stmt :

  Parse arg interpStmt

  Interpret interpStmt

Return

/*---------------------------------------------------------------*/
/* Create Binder member                                          */
/*---------------------------------------------------------------*/

createBnd :

  Parse arg Member archType

  Select
    When (archType = 'LEC') Then
    Do
      Address ISPEXEC "LMINIT DATAID(DID) DATASET('"bndDsn"') ENQ(SHRW)"
      Address ISPEXEC "LMOPEN DATAID("DID") OPTION(INPUT)"
      Address ISPEXEC "LMMFIND DATAID("DID") MEMBER("Member")"
      If RC = 0 Then
        Nop
      Else
      Do

        Address ISPEXEC "LMCLOSE DATAID("DID")"
        Address ISPEXEC "LMOPEN DATAID("DID") OPTION(OUTPUT)"
        datastr = ' '
        data_length = Length(datastr)
        Address ISPEXEC "LMPUT DATAID("DID") MODE(invar)" ||,
                              "DATALOC(datastr) DATALEN("data_length")"
        Address ISPEXEC "LMMADD DATAID("DID") MEMBER("Member")"
      End
      Address ISPEXEC "LMCLOSE DATAID("DID")"
      Address ISPEXEC "LMFREE DATAID("DID")"

      "ALLOC F(LINKDECK) DA('"bndDsn"("Member")') SHR"
      "EXECIO "linkdeck.0" DISKW LINKDECK (STEM linkdeck. FINIS)"
      "FREE  F(LINKDECK)"

      bndCnt = bndCnt + 1
    End
    When (archType = 'PRELEC') Then
    Do
      Address ISPEXEC "LMINIT DATAID(DID) DATASET('"prebndDsn"') ENQ(SHRW)"
      Address ISPEXEC "LMOPEN DATAID("DID") OPTION(INPUT)"
      Address ISPEXEC "LMMFIND DATAID("DID") MEMBER("Member")"
      If RC = 0 Then
        Nop
      Else
      Do
        Address ISPEXEC "LMCLOSE DATAID("DID")"
        Address ISPEXEC "LMOPEN DATAID("DID") OPTION(OUTPUT)"
        datastr = ' '
        data_length = Length(datastr)
        Address ISPEXEC "LMPUT DATAID("DID") MODE(invar)" ||,
                              "DATALOC(datastr) DATALEN("data_length")"
        Address ISPEXEC "LMMADD DATAID("DID") MEMBER("Member")"
      End
      Address ISPEXEC "LMCLOSE DATAID("DID")"
      Address ISPEXEC "LMFREE DATAID("DID")"

      "ALLOC F(LINKDECK) DA('"prebndDsn"("Member")') SHR"
      "EXECIO "linkdeck.0" DISKW LINKDECK (STEM linkdeck. FINIS)"
      "FREE  F(LINKDECK)"

      prebndCnt = prebndCnt + 1
    End
    Otherwise
      Nop
  End

Return

/*---------------------------------------------------------------*/
/* Create XML that contains KeyRef info for languages            */
/*---------------------------------------------------------------*/

keyRefXml :

  xml. = ''
  xcnt = 0

  Call xmlHeader
  Call xmlKeyRef
  Call xmlFooter

  xml.0 = xcnt
  memsfile = outputDir'/keyref.xml'
  Address syscall "writefile (memsfile) 755 xml."

Return

/*---------------------------------------------------------------*/
/* XML for KEYREF information for languages                      */
/*---------------------------------------------------------------*/
xmlKeyRef :

  xcnt = xcnt + 1
  xml.xcnt = '  '
  xcnt = xcnt + 1
  xml.xcnt = '  <!-- KEYREF info -->'
  xcnt = xcnt + 1
  xml.xcnt = '  <target name="keyrefinfo" ' ||,
                       'description="SCLM Keyref information">'

  krl = 0
  krk = 0
  krv = 0
  kref. = ''
  langCnt = 0

  Do m = 1 to memCnt
    /* Need to see if we have this one already */
    lang = memInfo.m.memLang

    Do z = 1 to langCnt While (lang <> kref.z)
    End
    If z > langCnt Then
    Do
      langCnt = langCnt + 1
      krl = langCnt
      kref.krl = lang
      kref.krl.stmt.0 = 0
    End
    Else
      krl = z

    Do k = 1 to memInfo.m.stmt.0
      krk = kref.krl.stmt.0

      If memInfo.m.stmt.k = 'LOAD' |,
         memInfo.m.stmt.k = 'OBJ'  |,
         memInfo.m.stmt.k = 'LIST' |,
         memInfo.m.stmt.k = 'LMAP' |,
         Substr(memInfo.m.stmt.k,1,3) = 'OUT' Then
      Do
        statement = memInfo.m.stmt.k
        Do z = 1 to krk While (statement <> kref.krl.stmt.z)
        End
        If z > krk Then
        Do
          krk = krk + 1
          kref.krl.stmt.krk = statement
          kref.krl.stmt.krk.nameval.0 = 0
        End
        Else
          krk = z

        Do v = 1 to memInfo.m.stmt.k.nameval.0
          krv = kref.krl.stmt.krk.nameval.0
          If memInfo.m.stmt.k.nameval.v.name = 'type' Then
          Do
            libType = memInfo.m.stmt.k.nameval.v.val
            Do z = 1 to krv While (libType <> kref.krl.stmt.krk.nameval.z)
            End
            If z > krv Then
            Do
              krv = krv + 1
              kref.krl.stmt.krk.nameval.krv = libType
            End
          End
          If kref.krl.stmt.krk.nameval.0 < krv Then
            kref.krl.stmt.krk.nameval.0 = krv
        End
      End
      If kref.krl.stmt.0 < krk Then
        kref.krl.stmt.0 = krk
    End
  End
  kref.0 = langCnt

  do m = 1 to kref.0
    xcnt = xcnt + 1
    xml.xcnt = '    <language name="'kref.m'">'
    do k = 1 to kref.m.stmt.0
      xcnt = xcnt + 1
      xml.xcnt = '      <keyword name="'Strip(kref.m.stmt.k) ||,
                              '" valueCnt="'kref.m.stmt.k.nameval.0'">'
      do v = 1 to kref.m.stmt.k.nameval.0
        xcnt = xcnt + 1
        xml.xcnt = '        <values type="'kref.m.stmt.k.nameval.v'"/>'
      end
      xcnt = xcnt + 1
      xml.xcnt = '      </keyword>'
    end
    xcnt = xcnt + 1
    xml.xcnt = '    </language>'
  end

  xcnt = xcnt + 1
  xml.xcnt   = '  </target>'

Return

/*---------------------------------------------------------------*/
/* Create XML that contains information about members            */
/*---------------------------------------------------------------*/

createXml :

  xml. = ''
  xcnt = 0

  Call xmlHeader
  Call xmlMember
  Call xmlFooter

  xml.0 = xcnt
  memsfile = outputDir'/members.xml'
  Address syscall "writefile (memsfile) 755 xml."

Return

/*---------------------------------------------------------------*/
/* XML Header information                                        */
/*---------------------------------------------------------------*/
xmlHeader :

  xml.1  = '<?xml version="1.0"?>'
  xml.2  = '<!--'
  xml.3  = '    Licensed Materials - Property of IBM'
  xml.4  = '    (c) Copyright IBM Corporation 2012,2013. All Rights Reserved.'
  xml.5  = '    Note to U.S. Government Users Restricted Rights:'
  xml.6  = '    Use, duplication or disclosure restricted by GSA ADP Schedule'
  xml.7  = '    Contract with IBM Corp.'
  xml.8  = ' -->'
  xml.9  = '<project name="SCLM-Migration" default="all" ' ||,
           'xmlns:ld="antlib:com.ibm.team.enterprise.zos.' ||,
           'systemdefinition.toolkit">'
  xml.10 = '  <description>SCLM Migration file metadata XML</description>'
  xml.11 = ''

  xcnt = 11

Return

/*---------------------------------------------------------------*/
/* XML for Member information                                    */
/*---------------------------------------------------------------*/
xmlMember :

  xcnt = xcnt + 1
  xml.xcnt = '  '
  xcnt = xcnt + 1
  xml.xcnt = '  <!-- File metadata -->'
  xcnt = xcnt + 1
  xml.xcnt = '  <target name="filemetadata" ' ||,
                       'description="Create file metadata">'

  Do m = 1 to memCnt
    xcnt = xcnt + 1
    xml.xcnt = '    <member name="'memInfo.m.name'" ' ||,
                           'type="'memInfo.m.type'" ' ||,
                           'language="'memInfo.m.memLang'">'
    Do k = 1 to memInfo.m.stmt.0
      xcnt = xcnt + 1
      xml.xcnt = '      <keyword name="'Strip(memInfo.m.stmt.k)'">'
      Do v = 1 to memInfo.m.stmt.k.nameval.0
        xcnt = xcnt + 1
        xml.xcnt = '        <values 'memInfo.m.stmt.k.nameval.v.name'=' ||,
                                    '"'memInfo.m.stmt.k.nameval.v.val'"/>'
      End
      xcnt = xcnt + 1
      xml.xcnt = '      </keyword>'
    End
    xcnt = xcnt + 1
    xml.xcnt = '    </member>'
  End

  xcnt = xcnt + 1
  xml.xcnt   = '  </target>'

Return


/*---------------------------------------------------------------*/
/* XML Footer information                                        */
/*---------------------------------------------------------------*/
xmlFooter :

  xcnt = xcnt + 1
  xml.xcnt = '  '
  xcnt = xcnt + 1
  xml.xcnt = '  <target depends="filemetadata"' ||,
                     ' description="File Metadata" name="all"/>'
  xcnt = xcnt + 1
  xml.xcnt = '</project>'

Return

/*---------------------------------------------------------------*/
/* Create report lines                                           */
/*---------------------------------------------------------------*/
repLine :

  parse arg sev line
  db = db + 1
  If sev = '' Then
    report.1 = line
  Else
    report.1 = sev '-' line
  report.0 = 1

  If db = 1 Then
  Do
    repfile = outputDir'/'thisExec'-Report.txt'
    Address syscall "writefile (repfile) 755 report."
  End
  Else
    Address syscall "writefile (repfile) 755 report. 1"

Return 0

/*---------------------------------------------------------------*/
/* Exit routine                                                  */
/*---------------------------------------------------------------*/
exitRtn :

  Parse arg max_rc

  ZISPFRC = max_rc
  Address ISPEXEC 'VPUT (ZISPFRC) SHARED'

Exit max_rc
