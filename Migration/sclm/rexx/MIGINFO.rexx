/* REXX */
/*********************************************************************/
/*                                                                   */
/* NAME := MIGINFO                                                   */
/*                                                                   */
/* DESCRIPTIVE NAME := SCLM Migration analysis                       */
/*                                                                   */
/* FUNCTION := Extract information from SCLM such as.                */
/*               - Hierarchy structure                               */
/*               - Languages                                         */
/*               - Number of parts per type                          */
/*               - Number of parts per language                      */
/*               - Build report per language to see what is called   */
/*                                                                   */
/*                                                                   */
/* USAGE := Call this exec with the name of the SCLM project and if  */
/*          required the alternate. Leaving the alternate blank      */
/*          will default to the project:                             */
/*                                                                   */
/*          e.g. EX 'MYDSN.REXX(MIGINFO)' 'PROJ1'                    */
/*                                                                   */
/*               or                                                  */
/*                                                                   */
/*               EX 'MYDSN.REXX(MIGINFO)' 'PROJ1,ALT1'               */
/*                                                                   */
/*********************************************************************/

  parse arg proj projdef debug

  If projdef = '' Then
    projdef = proj

  parm1 = proj','projdef

  Say 'Starting SCLM information for project : 'proj' Alternate : 'projdef

  Drop groups.
  Drop authcodes.
  Drop types.
  Drop langs.
  Drop patterns.
  gr = 0
  au = 0
  ty = 0
  ll = 0
  dp = 0

  user = USERID()
  address syscall 'getpwnam (user) pw.'
  homeDir = pw.4


  /* Call SCLMINFO service */
  Address TSO "FLMCMD SCLMINFO,"proj","projdef
  If rc > 0 then
    Say 'rc : 'rc' from SCLMINFO 'proj projdef

  Call DoHTML

  Say 'SCLM information complete. See 'htmlFile' for generated HTML'

Exit

/**********************************************************************/
/* Run DBUTIL report to list every member that is in the 'PROD' group */
/* that is an ARCHDEF member                                          */
/**********************************************************************/
DButilLang:

  "ALLOC F(DUMMY) DUMMY"
  "ALLOC F(DBMSG) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(10 50)",
          "CYLINDERS REUSE"
  "ALLOC F(DBRPT) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(10 50)",
          "CYLINDERS REUSE"
  "ALLOC F(FLMMSGS) DUMMY"

  "FLMCMD DBUTIL,"proj","projdef","topGroup",,,,,,*,*"||,
                ",*,*,*,*,"language",NO,ACCT,*,,,,NORMAL,NO,NO,," ||,
                "DBMSG,DUMMY,DBRPT," ||,
                "@@FLMTYP @@FLMMBR @@FLMSTA"
  dbutil_rc = rc

  If dbutil_rc > 4  Then
  Do
    Say
    Say 'E' 'DBUTIL error, return code ='rc
    Say
    "EXECIO * DISKR DBMSG (STEM dbmsgs. FINIS)"
    Do db = 1 to dbmsgs.0
      Say ' ' dbmsgs.db
    End
  End
  "EXECIO * DISKR DBRPT (STEM dbutil. FINIS)"

  "FREE F(DUMMY)"
  "FREE F(DBMSG)"
  "FREE F(DBRPT)"
  "FREE F(FLMMSGS)"

Return dbutil_rc

/**********************************************************************/
/* Run Build information to show compilers and SYSLIBs                */
/**********************************************************************/
BuildInfo:

  "ALLOC F(DUMMY) DUMMY"
  "ALLOC F(DBMSG) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(10 50)",
          "CYLINDERS REUSE"
  "ALLOC F(DBRPT) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(10 50)",
          "CYLINDERS REUSE"
  "ALLOC F(FLMMSGS) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(1 1)",
          "CYLINDERS REUSE"

  "FLMCMD BUILD,"strip(proj)","strip(projdef)","strip(topGroup)"," ||,
                 strip(type)","strip(member)"," ||,
                "DOHERTL,N,I,Y,," ||,
                "DOHERTL,DBMSG,DBRPT,DUMMY,DUMMY"
  build_rc = rc

  If build_rc > 4  Then
  Do
    Say
    Say 'E' 'BUILD error, return code ='rc
    Say
    "EXECIO * DISKR DBMSG (STEM dbmsgs. FINIS)"
    Do db = 1 to dbmsgs.0
      Say ' ' dbmsgs.db
    End
    "EXECIO * DISKR FLMMSGS (STEM dbmsgs. FINIS)"
    Do db = 1 to dbmsgs.0
      Say ' ' dbmsgs.db
    End
  End
  "EXECIO * DISKR DBRPT (STEM buildrpt. FINIS)"

  "FREE F(DUMMY)"
  "FREE F(DBMSG)"
  "FREE F(DBRPT)"
  "FREE F(FLMMSGS)"

  ht = ht + 1
  html.ht  = '    <h4>Language : 'lang'</h4>'

  ht = ht + 1
  html.ht  = '    <p><textarea id="buildRep" name="buildRep" ' ||,
             'rows="'buildrpt.0+2'" cols="90">'
  Do b = 1 to buildrpt.0
    ht = ht + 1
    html.ht  = buildrpt.b
  End
  ht = ht + 1
  html.ht  = '    </textarea></p>'

Return dbutil_rc

parseIt:

  arg id

  j = index(var,id)

  Do While j > 0
    i = j
    j = index(var,id,i+3)
    If j = 0 Then
      wrd = substr(var,i)
    Else
      wrd = substr(var,i,j-i-1)

    Select
      When (id = 'GR=') Then
      Do

        If Pos('NP=',wrd) /= 0 Then
        Do
          dp = dp + 1
          patterns.dp = wrd
        End
        Else
        Do
          au = au + 1
          authcodes.au = wrd
        End
      End
      When (id = 'LV=') Then
      Do
        Parse var wrd 'LV='level 'GR='group
        If Strip(level) = '001' Then
        Do
          topGroup = group
          Do k = 1 to dp
            Parse var patterns.k 'GR='group 'NP='pattern
            If topGroup = group then
            Do
              /* Need to convert SCLM metavariables */
              Do while (Pos('@@',pattern) /= 0)

                xx = Pos('@@',pattern)
                If xx /= 0 Then
                Do
                  Select
                    When (Substr(pattern,xx,8) = '@@FLMPRJ') Then
                    Do
                      pattern = Substr(pattern,1,xx-1) ||,
                                    proj ||,
                                    Substr(pattern,xx+8)
                    End

                    When (Substr(pattern,xx,8) = '@@FLMGRP') Then
                    Do
                      pattern = Substr(pattern,1,xx-1) ||,
                                    Strip(TopGroup) ||,
                                    Substr(pattern,xx+8)
                    End
                    When (Substr(pattern,xx,8) = '@@FLMTYP') Then
                    Do
                      pattern = Substr(pattern,1,xx-1) ||,
                                    '<type>' ||,
                                    Substr(pattern,xx+8)
                    End
                    Otherwise
                      Nop
                  End
                End
              End
              Leave
            End
          End
        End
        gr = gr + 1
        groups.gr = wrd
      End
      When (id = 'LA=') Then
      Do
        firstMember = 1
        memberCount = 0
        storeType   = ''
        storeMember = ''

        Parse var wrd 'LA='language 'LD='Description
        if Description = '' Then
          wrd = 'LA='language 'LD='

        Say 'Processing language : 'language
        rc = DBUtilLang()

        If rc = 0 Then
        Do
          /* Need to get Counts per language      */
          Do l = 1 to dbutil.0
            parse var dbutil.l Type Member Status
            If Status = 'EDITABLE' Then
            Do
              memberCount = memberCount + 1
              If firstMember Then
              Do
                storeType   = Type
                storeMember = Member
                firstMember = 0
              End
            End
          End
        End

        ll = ll + 1
        langs.ll = wrd 'TY='storeType ||,
                   ' members='memberCount 'firstmember='storeMember
      End
      When (id = 'TY=') Then
      Do
        Parse var wrd 'TY='type 'XT='extend .
        if extend = '' Then
          wrd = 'TY='type 'XT='
        xx = Pos('<type>',pattern)
        dspatt  = Substr(pattern,1,xx-1) ||,
                  type ||,
                  Substr(pattern,xx+6)

        Address ISPEXEC "DSINFO DATASET('"Strip(dspatt)"')"
        nummems = Space(ZDS#MEM,0)

        ty = ty + 1
        types.ty = wrd 'members='nummems
      End
      Otherwise
        Nop
    End
    If debug = 'dbg' Then
      Say wrd
  End

Return

DoHTML:

  Drop html.
  ht = 38
  htmlFile = homeDir"/"proj"-sclminfo.html"

  acct_fs  = ZSCIACTF
  acct_len = length(acct_fs)
  acct_n   = acct_len / 44

  html.1   = '<!DOCTYPE html>'
  html.2   = '<html>'
  html.3   = '  <head>'
  html.4   = '  <style>'
  html.5   = '  table {'
  html.6   = '    font-family: arial, sans-serif;'
  html.7   = '    border-collapse: collapse;'
  html.8   = '    width: 100%;'
  html.9   = '  }'
  html.10  = '  td, th {'
  html.11  = '    border: 1px solid #dddddd;'
  html.12  = '    text-align: left;'
  html.13  = '    padding: 8px;'
  html.14  = '  }'
  html.15  = '  tr:nth-child(even) {'
  html.16  = '    background-color: #dddddd;'
  html.17  = '  }'
  html.18  = '  </style>'
  html.19  = '  </head>'
  html.20  = '  <body>'
  html.21  = '    <h1>SCLM Information for project : ' ||,
                  '<font color="blue">'Strip(ZSCIPROJ)'</font></h1>'
  html.22  = '    <p></p>'
  html.23  = '    <h2>Alternate : ' ||,
                  '<font color="blue">'Strip(ZSCIPDEF)'</font></h2>'
  html.24  = '    <table style="width:50%">'
  html.25  = '      <tr>'
  html.26  = '        <td>SCLM version id for the project</td>'
  html.27  = '        <td><b>'Strip(ZSCISVER)'</td>'
  html.28  = '      </tr>'
  html.29  = '      <tr>'
  html.30  = '        <td>Project generation timestamp</td>'
  html.31  = '        <td><b>'Strip(ZSCITMST)'</td>'
  html.32  = '      </tr>'
  html.33  = '      <tr>'
  html.34  = '        <td>Account file(s) for the project</td>'
  html.35  = '        <td>'acct_n'</td>'
  Do I = 1 to acct_n
    acct_f   = left(acct_fs,44)
    acct_fs  = substr(acct_fs,45)
    If i = 1 Then
    Do
      html.36  = '        <td><b>'Strip(acct_f)'</td>'
      html.37  = '      </tr>'
      html.38  = '      <tr>'
    End
    Else
    Do
      ht = ht + 1
      html.ht  = '        <td></td><td></td><td><b>'Strip(acct_f)'</td>'
    End
  End
  ht = ht + 1
  html.ht  = '      </tr>'
  ht = ht + 1
  html.ht  = '    </table>'

  html.0 = ht
  Address syscall "writefile (htmlFile) 755 html."
  If rc > 0 Then
    Say 'RC from htmlFile write = 'rc

  ht = 0

  ht = ht + 1
  html.ht  = '    <h3>Data set name pattern information for the project:</h3>'
  ht = ht + 1
  html.ht  = '    <table style="width:40%">'
  ht = ht + 1
  html.ht  = '      <tr>'
  ht = ht + 1
  html.ht  = '        <td><b>Group</td>'
  ht = ht + 1
  html.ht  = '        <td><b>Pattern</td>'
  ht = ht + 1
  html.ht  = '      </tr>'

  var = zscinpat; call parseIt 'GR='

  Do I = 1 to dp
    Parse var patterns.i 'GR='group 'NP='pattern
    ht = ht + 1
    html.ht  = '      <tr>'
    ht = ht + 1
    html.ht  = '        <td>'group'</td><td>'Strip(pattern)'</td>'
    ht = ht + 1
    html.ht  = '      </tr>'
  End
  ht = ht + 1
  html.ht  = '    </table>'

  html.0 = ht
  Address syscall "writefile (htmlFile) 755 html. 1"
  If rc > 0 Then
    Say 'RC from htmlFile write = 'rc

  ht = 0

  ht = ht + 1
  html.ht  = '    <h3>Group information for the project:</h3>'
  ht = ht + 1
  html.ht  = '    <table style="width:25%">'
  ht = ht + 1
  html.ht  = '      <tr>'
  ht = ht + 1
  html.ht  = '        <td><b>Level</td>'
  ht = ht + 1
  html.ht  = '        <td><b>Group</td>'
  ht = ht + 1
  html.ht  = '      </tr>'

  var = zscigrp;  call parseIt 'LV='

  Do I = 1 to gr
    Parse var groups.i 'LV='level 'GR='group
    ht = ht + 1
    html.ht  = '      <tr>'
    ht = ht + 1
    html.ht  = '        <td>'level'</td><td>'Strip(group)'</td>'
    ht = ht + 1
    html.ht  = '      </tr>'
  End
  ht = ht + 1
  html.ht  = '    </table>'

  html.0 = ht
  Address syscall "writefile (htmlFile) 755 html. 1"
  If rc > 0 Then
    Say 'RC from htmlFile write = 'rc

  ht = 0

  ht = ht + 1
  html.ht  = '    <h3>Authcode information for the project:</h3>'
  ht = ht + 1
  html.ht  = '    <table style="width:25%">'
  ht = ht + 1
  html.ht  = '      <tr>'
  ht = ht + 1
  html.ht  = '        <td><b>Group</td>'
  ht = ht + 1
  html.ht  = '        <td><b>Authcodes</td>'
  ht = ht + 1
  html.ht  = '      </tr>'

  var = zsciaut; call parseIt 'GR='

  Do I = 1 to au
    Parse var authcodes.i 'GR='group 'AC='authcode
    ht = ht + 1
    html.ht  = '      <tr>'
    ht = ht + 1
    html.ht  = '        <td>'group'</td><td>'Strip(authcode)'</td>'
    ht = ht + 1
    html.ht  = '      </tr>'
  End
  ht = ht + 1
  html.ht  = '    </table>'

  html.0 = ht
  Address syscall "writefile (htmlFile) 755 html. 1"
  If rc > 0 Then
    Say 'RC from htmlFile write = 'rc

  ht = 0

  ht = ht + 1
  html.ht  = '    <h3>Type information for the project:</h3>'
  ht = ht + 1
  html.ht  = '    <table style="width:50%">'
  ht = ht + 1
  html.ht  = '      <tr>'
  ht = ht + 1
  html.ht  = '        <td><b>Type</td>' ||,
             '        <td><b>Extend Type</td>' ||,
             '        <td><b>No. of Members</td>'
  ht = ht + 1
  html.ht  = '      </tr>'

  var = zscitype; call parseIt 'TY='

  Do I = 1 to ty
    Parse var types.i 'TY='type 'XT='extend 'members='members
    ht = ht + 1
    html.ht  = '      <tr>'
    ht = ht + 1
    html.ht  = '        <td>'type'</td><td>'extend'</td><td>'members'</td>'
    ht = ht + 1
    html.ht  = '      </tr>'
  End
  ht = ht + 1
  html.ht  = '    </table>'

  html.0 = ht
  Address syscall "writefile (htmlFile) 755 html. 1"
  If rc > 0 Then
    Say 'RC from htmlFile write = 'rc

  ht = 0

  ht = ht + 1
  html.ht  = '    <h3>Language information for the project:</h3>'
  ht = ht + 1
  html.ht  = '    <table style="width:50%">'
  ht = ht + 1
  html.ht  = '      <tr>'
  ht = ht + 1
  html.ht  = '        <td><b>Language</td>' ||,
             '        <td><b>Description</td>' ||,
             '        <td><b>No. of Members</td>'
  ht = ht + 1
  html.ht  = '      </tr>'

  var = zscilang; call parseIt 'LA='

  Do I = 1 to ll
    Parse var langs.i 'LA='lang' LD='desc' TY='type  ,
                      'members='members' firstmember='member
    ht = ht + 1
    html.ht  = '      <tr>'
    ht = ht + 1
    html.ht  = '        <td>'lang'</td><td>'desc'</td><td>'members'</td>'
    ht = ht + 1
    html.ht  = '      </tr>'
  End
  ht = ht + 1
  html.ht  = '    </table>'

  html.0 = ht
  Address syscall "writefile (htmlFile) 755 html. 1"
  If rc > 0 Then
    Say 'RC from htmlFile write = 'rc

  ht = 0

  ht = ht + 1
  html.ht  = '    <h3>Build information for each buildable language</h3>'
  /* Now get the build info */
  Do I = 1 to ll
    Parse var langs.i 'LA='lang' LD='desc' TY='type  ,
                      'members='members' firstmember='member
    If lang = 'ARCHDEF' Then iterate

    If members > 0 Then
      Call BuildInfo
  End

  ht = ht + 1
  html.ht  = '  </body>'
  ht = ht + 1
  html.ht  = '</html>'

  html.0 = ht
  Address syscall "writefile (htmlFile) 755 html. 1"
  If rc > 0 Then
    Say 'RC from htmlFile write = 'rc

Return
