/* REXX */

/*********************************************************************/
/*                                                                   */
/* NAME := EXTSRC                                                    */
/*                                                                   */
/* DESCRIPTIVE NAME := Extract editable source from SCLM             */
/*                                                                   */
/* FUNCTION := Extract editable source from a SCLM project to        */
/*             a temporary data sets prefixed with the given HLQ     */
/*             and also generate a text file in HFS to list all      */
/*             extracted members along its language definition.      */
/*                                                                   */
/*********************************************************************/

  Trace o                                    /* trace - a, o, r   */

  Call SYSCALLS 'ON'

  rsize = 0  /* reports size */
  reports. = ''

  /*-------------------------------------------------*/
  /* Parsing the property member from input, if none */
  /* is provided then look for a member named        */
  /* MIGCFG in the same source data set              */
  /*-------------------------------------------------*/
  Parse SOURCE . . thisExec . thisDsn . . . .
  Parse ARG propmem
  If propmem = '' Then
  Do
    propmem = thisDsn'('MIGCFG')'
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
  migHlq = ''
  outputDir = ''
  vermax = ''

  /*-------------------------------------------------*/
  /* Parsing the parameters from the property file   */
  /*-------------------------------------------------*/
  Do i = 1 to props.0
    next = Strip(props.i)
    If next = '' | Pos('#',next) = 1 Then
       Iterate
    Parse Var next key '=' value
    key = strip(Translate(key))
    value = strip(value)
    Select
      When (key = 'PROJ')      Then proj = value
      When (key = 'GROUP')     Then group = value
      When (key = 'MIGHLQ')    Then migHlq = value
      When (key = 'OUTPUTDIR') Then outputDir = value
      When (key = 'VERMAX')    Then vermax = value
      Otherwise
    End
  End

  If vermax = '' Then
     vermax = -1
  If migHlq   = '' Then
    migHlq   = Userid()
  If outputDir = '' Then
  Do
    user = USERID()
    Address SYSCALL 'getpwnam (user) pw.'
    outputDir = pw.4
  End

  upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  lower = 'abcdefghijklmnopqrstuvwxyz'
  projLower = TRANSLATE(proj, lower, upper)

  /* DEBUG */
  Say 'proj='proj
  Say 'group='group
  Say 'migHlq='migHlq
  Say 'outputDir='outputDir
  Say 'vermax='vermax

  tempHlq = migHlq'.SCLMMIG'

  Call retrvVersions

Exit


/*---------------------------------------------------------------*/
/* Retrieve versions from SCLM                                   */
/*---------------------------------------------------------------*/
retrvVersions:

  outputs. = ''
  osize = 0   /* outputs size */
  type   = ' '
  member = ' '
  datev  = ' '
  timev   = ' '
  vernum = 0
  verinfo_rc = 0

  currentMember = ''
  verCount = 0
  versions. = ''
  verIndexStart = 1

  Do Until verinfo_rc <> 0

     "FLMCMD VERINFO,"proj","proj","group","type","member",,"timev||,
                    ",,,,,FORWARD,,"datev

     verinfo_rc = rc

     If verinfo_rc = 0 Then
     Do
         type    = STRIP(zsvtype)
         member  = STRIP(zsvambr)
         datev   = STRIP(zsvdat4)
         timev   = STRIP(zsvtime)
         formatv = STRIP(zsvcfmt)
         resultv = STRIP(zsvreslt)
         vernum = STRIP(ZSAVER)
         lang = STRIP(ZSALANG)

         If formatv <> 'AUDIT' Then
            rc = report('I' 'Skip 'proj'.'group'.'type'('member||,
                            ') because it is an Audit record')
         If resultv <> 'COMPLETE' Then
            rc = report('W' 'Skip 'proj'.'group'.'type'('member||,
                            ') because failed to parse the record')
         If formatv <> 'AUDIT' | resultv <> 'COMPLETE' Then
         Do
           If currentMember <> member Then
           Do
             If verCount > 0 Then
             Do
                 If vermax <> -1 & verCount > vermax Then
                     verIndexStart = verCount - vermax + 1
                 Else
                     verIndexStart = 1

                 Do i = verIndexStart to verCount
                    Parse var versions.i t' 'm' 'vn' 'dv' 'tv' 'l
                    dsn = restore(t' 'm' 'vn' 'dv' 'tv)
                    osize = osize + 1
                    outputs.osize = dsn' 'l
                 End
             End
             verCount = 0
             currentMember = member
           End

           verCount = verCount + 1
           versions.verCount = type' 'member' 'vernum' 'datev' 'timev' 'lang
         End
         timev = incrementTime(timev)
     End
  End

  If verCount > 0 Then
  Do
     If vermax <> -1 & verCount > vermax Then
        verIndexStart = verCount - vermax + 1
     Else
        verIndexStart = 1

     Do i = verIndexStart to verCount
         Parse var versions.i t' 'm' 'vn' 'dv' 'tv' 'l
         dsn = restore(t' 'm' 'vn' 'dv' 'tv)
         osize = osize + 1
         outputs.osize = dsn' 'l
     End
  End

  outputs.0 = osize
  membersfile = outputDir'/sclmMigration/'projLower'/members.txt'
  Address SYSCALL "writefile (membersfile) 755 outputs."

    
  bndPds = tempHlq'.BND'
  bndSize = 0
  bndMembers. = ''                 
  x = Outtrap('bnds.')                              
  ADDRESS TSO "LISTDS ('"bndPds"') MEMBERS"                       
  x = Outtrap('OFF')                              
  Do n = 7 TO bnds.0
     member = STRIP(bnds.n)
     bndSize = bndSize + 1
     bndMembers.bndSize = bndPds'('member')'     
  End
  
  bndMembers.0 = bndSize
  Address SYSCALL "writefile (membersfile) 755 bndMembers. 1"
  

  /*-------------------------------------------------*/
  /* Output the report                               */
  /*-------------------------------------------------*/
  reports.0 = rsize
  reportFile = outputDir'/sclmMigration/'projLower'/'thisExec'_Report.txt'
  Address SYSCALL "writefile (reportFile) 755 reports."

Return

/*---------------------------------------------------------------*/
/* Function: incrementTime                                       */
/* Desc    : Increment the micro-second fragment                 */
/*---------------------------------------------------------------*/
incrementTime :

    Parse ARG time
    increment = RIGHT((RIGHT(time,5) + 0.01),5,0)
    newtime   = LEFT(time,6)||increment

Return newtime

/*---------------------------------------------------------------*/
/* Function: reportLine                                          */
/* Desc    : Add a message to the report Stem                    */
/*---------------------------------------------------------------*/
report :

  Parse ARG sev msg
  rsize = rsize + 1
  If sev = '' Then
     reports.rsize = msg
  Else
     reports.rsize = sev' - 'msg

Return 0

restore :
   /*Parse ARG version*/
   Parse ARG t' 'm' 'vn' 'dv' 'tv
   verdsn = tempHlq".V"vn"."proj"."group"."t
   modeldsn = proj'.'group'.'t

   If sysdsn("'"verdsn"'") <> 'OK' Then
   Do
      Address TSO "ALLOC DATASET('"verdsn"')"||,
                  " LIKE('"modeldsn||,
                  "') NEW CATALOG"
      Address TSO "FREE DATASET('"verdsn"')"
   End

   "FLMCMD VERRECOV,"proj","proj","group","||,
                    t","m",,"||,
                    tv","verdsn",,,,,"dv
   rc = report('I' 'Restore 'modeldsn'('m') to '||,
                   verdsn'('m')')

Return verdsn'('m')'


