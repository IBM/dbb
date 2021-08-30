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
  Trace o                                          /* trace - o, r   */         
                                                                                
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
  scopeflg = ''                                                                 
  migrdate = ''                                                                 
  copytype = ''                                                                 
  vermax = ''                                                                   
  selectionCriteria = ''                                                        
  repo = ''                                                                     
  lectypes = ''                                                                 
  dependtypes = ''                                                              
  defComponent = ''                                                             
  defzProject = ''                                                              
                                                                                
  upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                          
  lower = 'abcdefghijklmnopqrstuvwxyz'                                          
  /*-------------------------------------------------*/                         
  /* Parsing the parameters from the property file   */                         
  /*-------------------------------------------------*/                         
  Do i = 1 to props.0                                                           
    next = Strip(props.i)                                                       
    If next = '' | Pos('#',next) = 1 Then                                       
       Iterate                                                                  
    Parse Var props.i key '=' value                                             
    key = Translate(key,lower,upper)                                            
    value = strip(value)                                                        
    Select                                                                      
      When (key = 'proj')              Then proj = value                        
      When (key = 'group')             Then group = value                       
      When (key = 'projmem')           Then projmem = value                     
      When (key = 'projdefs')          Then projdefs = value                    
      When (key = 'maclibs')           Then maclibs = value                     
      When (key = 'mighlq')            Then mighlq = value                      
      When (key = 'outputdir')         Then outputdir = value                   
      When (key = 'alllangs')          Then alllangs = value                    
      When (key = 'empty')             Then empty = value                       
      When (key = 'exist')             Then exist = value                       
      When (key = 'scopeflg')          Then scopeflg = value                    
      When (key = 'migrdate')          Then migrdate = value                    
      When (key = 'vermax')            Then vermax = value                      
      When (key = 'selectioncriteria') Then selectioncriteria = value           
      When (key = 'repo')              Then repo = value                        
      When (key = 'copytype')          Then copytype = value                    
      When (key = 'lectypes')          Then lectypes = value                    
      When (key = 'dependtypes')       Then dependtypes = value                 
      When (key = 'defcomponent')      Then defcomponent = value                
      When (key = 'defzproject')       Then defzproject = value                 
      Otherwise                                                                 
        Say 'Invalid property - 'key                                            
    End                                                                         
  End                                                                           
                                                                                
  If proj = '' Then                                                             
  Do                                                                            
    Say 'E SCLM Project must be specified'                                      
    migRc = 8                                                                   
  End                                                                           
                                                                                
  If group = '' Then                                                            
  Do                                                                            
    Say 'E SCLM Group to be migrated must be specified'                         
    migRc = 8                                                                   
  End                                                                           
                                                                                
  If ThisExec = 'BLZMIG4' Then                                                  
  Do                                                                            
    If defComponent = '' Then                                                   
    Do                                                                          
      Say 'E Default RTC Component must be specified'                           
      migRc = 8                                                                 
    End                                                                         
  End                                                                           
                                                                                
  If migRc > 0 Then                                                             
    Exit migRc                                                                  
                                                                                
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
  If migHlq = '' Then                                                           
    migHlq  = Userid()                                                          
                                                                                
  proj = Translate(proj,lower,upper)                                            
                                                                                
  If outputDir = '' Then                                                        
  Do                                                                            
    user = USERID()                                                             
    address syscall 'getpwnam (user) pw.'                                       
    outputDir = pw.4                                                            
  End                                                                           
  outputDir = outputDir'/sclmMigration/'proj                                    
                                                                                
  If allLangs = '' Then                                                         
    allLangs = 'true'                                                           
                                                                                
  If scopeflg = '' Then                                                         
    scopeflg = 'ALL'                                                            
                                                                                
  If ThisExec = 'BLZMIG4' Then                                                  
    edMac = 'BLZSRTVE'                                                          
  Else                                                                          
  Do                                                                            
    edMac = 'SORTVER'                                                           
    Address TSO "ALTLIB ACTIVATE APPLICATION(EXEC),                             
                               DATASET('"migHlq".SCLMMIG.REXX')"                
    Address ISPEXEC "LIBDEF ISPSLIB DATASET ID('"migHlq".SCLMMIG.SKELS')"       
  End                                                                           
                                                                                
  /* Read in the stored languages */                                            
                                                                                
  langfile = outputDir'/langext.txt'                                            
  Address syscall "readfile (langfile) langexts."                               
  If rc <> 0 Then                                                               
  Do                                                                            
    Say 'E Languages file 'langfile ||,                                         
          ' does not exist. BLZMIG1A must be run first'                         
    migRc = 8                                                                   
    Exit migRc                                                                  
  End                                                                           
  Do i = 1 to langexts.0                                                        
    parse var langexts.i langdefs.i ':' langdefs.i.dfltsrc ':',                 
                         langdefs.i.ext ':' langdefs.i.cspdb                    
  End                                                                           
                                                                                
  zqual    = 'VCUR'                                                             
  ver_list = 'VCUR'                                                             
  zimpstmt.0 = 2                                                                
  zimpstmt.1 = "# zimport statements for "proj"."group" - Current version"      
  zimpstmt.2 = "# zimport data sets created on "Date()" at "Time()              
  comp_list = ''                                                                
  zcomp_list = ''                                                               
                                                                                
                                                                                
  /* Create shell script content to run zimport */                              
  zimpshell.0  = 14                                                             
  zimpshell.1  = '#!/bin/sh'                                                    
  zimpshell.2  = '#'                                                            
  zimpshell.3  = ''                                                             
  zimpshell.4  = ''                                                             
  zimpshell.5  = 'projectArea="<your project area>"'                            
  zimpshell.6  = 'stream="<your stream>"'                                       
  zimpshell.7  = 'rws="<your repository workspace>"'                            
  zimpshell.8  = 'repository="https://<rtc-server-url>:<port>/ccm/"'            
  zimpshell.9  = 'userid="<rtc-userid>"'                                        
  zimpshell.10 = 'password="<rtc-password>"'                                    
  zimpshell.11 = '#'                                                            
  zimpshell.12 = 'export SCM_WORK="<your SCM WORK directory>"'                  
  zimpshell.13 = 'cd <rtc-install-dir>/usr/lpp/jazz/v6.0.6/scmtools/eclipse'    
  zimpshell.14 = 'scm zimport --hlq $HLQ --mapfile "$mapFile" ' ||,             
                 '--projectarea "$projectArea" -s "$stream" ' ||,               
                 '-r $repository -u $userid -P $password -w "$rws" -v -c s'     
                                                                                
  counts_dsn = "'"mighlq"."proj"."group".MEMBER.COUNTS'" /* Counts */           
  Address TSO "ALLOC F(CNTRPT) DSN("counts_dsn") shr"                           
  "EXECIO * DISKR CNTRPT (STEM cntrept. FINIS)" /*Read MEMBER.COUNTS dsn*/      
  Address TSO "FREE F(CNTRPT)"                                                  
                                                                                
  dbmsg_dsn = "'"migHlq"."proj"."group".VERSION.DBMSGS'"                        
  x = msg('off')                                                                
  Address TSO "DELETE "dbmsg_dsn                                                
  x = msg('on')                                                                 
  Address TSO "ALLOC F(DBMSG) DA("dbmsg_dsn") RECFM(F B) LRECL(80)              
          BLKSIZE(3120) SPACE(1 5) CYLINDERS REUSE"                             
                                                                                
  version_rpt = "'"migHlq"."proj"."group".VERSION.REPORT'"                      
  x = msg('off')                                                                
  "DELETE "version_rpt                                                          
  x = msg('on')                                                                 
  Address TSO "ALLOC F(VERSRPT) DA("version_rpt") RECFM(F B) LRECL(80) ",       
                    "BLKSIZE(3120) SPACE(1 1) CYLINDERS REUSE"                  
                                                                                
  zimpCur = outputDir"/zImport-"group"-"zqual".mapfile"                         
  If ThisExec = 'BLZMIG4' Then                                                  
    Address syscall "writefile (zimpCur) 755 zimpstmt."                         
  zimpstmt.0 = 1                                                                
                                                                                
  zimpshCur = outputDir"/zImport-"group"-"zqual".sh"                            
                                                                                
  Address syscall "stat (zimpsh"zqual") st."                                    
  Say rc                                                                        
  If st.0 = 0 Then                                                              
  Do                                                                            
    /* Create shell script to run zimport */                                    
    zimpshell.3  = 'HLQ="'migHlq'.'Translate(proj)'.'group'.'zqual'"'           
    zimpshell.4  = 'mapFile="'outputDir'/zImport-'group'-'zqual'.mapfile"'      
    If ThisExec = 'BLZMIG4' Then                                                
      Address syscall "writefile (zimpshCur) 755 zimpshell. "                   
  End                                                                           
                                                                                
  Do s = 1 to cntrept.0                    /* Loop through COUNT report */      
    Parse VAR cntrept.s sclmtyp ',' sclmlang ',' cnt ',' rtclang ',' ,          
              ext ',' rtccomp ',' zcomp ',' zfolder                             
    sclmtyp  = STRIP(sclmtyp)                                                   
    sclmlang = STRIP(sclmlang)                                                  
    cnt      = STRIP(cnt)                                                       
    rtclang  = STRIP(rtclang)                                                   
    ext      = STRIP(ext)                                                       
    rtccomp  = STRIP(rtccomp)                                                   
    zcomp    = STRIP(zcomp)                                                     
    zfolder  = STRIP(zfolder)                                                   
                                                                                
    Select                                                                      
      When sclmtyp = ''          Then Iterate     /* Skip Blank lines */        
      When sclmtyp = 'SCLM TYPE' Then Iterate     /* Skip header lines*/        
      When DATATYPE(sclmtyp,'N') Then Iterate     /* Skip numeric line*/        
      When rtclang = 'SKIP'      Then Iterate     /* Skip these lines */        
      When sclmlang = 'ARCHDEF'  Then Iterate     /* Skip these lines */        
      When rtclang = '' | ext = '' | rtccomp = '' | zcomp = '' | ,              
           zfolder = '' Then                                                    
      Do                                                                        
        Say '****************************************************'              
        Say '* One or more required RTC parameters is missing.  *'              
        Say '* Correct the COUNTS file, and rerun exec 'Left(ThisExec,8)' *'    
        Say '* Line in error is :'                                              
        Say ' '                                                                 
        Say    cntrept.s                                                        
        Say '****************************************************'              
        zispfrc = 8                                                             
        Address 'ISPEXEC' "VPUT (ZISPFRC) SHARED"                               
        Exit zispfrc                                                            
      End                                                                       
      Otherwise                                                                 
    End                                                                         
                                                                                
    /* For each type/lang PDS create zimport cards                     */       
                                             /* Don't dup Comp lines   */       
    If WORDPOS(zcomp'='rtccomp,comp_list) = 0 Then                              
    Do                                                                          
      zimpstmt.1 = "C:"zcomp"="rtccomp                                          
      If ThisExec = 'BLZMIG4' Then                                              
        Address syscall "writefile (zimpCur) 755 zimpstmt. 1"                   
      comp_list = comp_list zcomp'='rtccomp  /* Add comp to list       */       
    End                                                                         
                                                                                
    Call Create_Cards                                                           
                                                                                
  End                                                                           
                                                                                
  If ThisExec = 'BLZMIG4' Then                                                  
    Do i = 1 to Words(ver_list)                                                 
      zqual = Word(ver_list,i)                                                  
      mapFile = outputDir"/zImport-"group"-"zqual".mapfile"                     
      Address 'ISPEXEC' "EDIT FILE(mapfile) MACRO(BLZSRTZI)"                    
    End                                                                         
                                                                                
  Address TSO "FREE F(DBMSG)"                                                   
  Address TSO "FREE F(VERSRPT)"                                                 
                                                                                
Exit                                                                            
                                                                                
/**********************************************************************/        
/* Create P: and L: cards and copy member to zimport dataset.         */        
/**********************************************************************/        
Create_Cards:                                                                   
                                                                                
  Address 'ISPEXEC'                                                             
                                                                                
  /* Create P: card - one per zcomponent:zfolder combination            */      
                                                                                
  zf = WORDPOS(zcomp':'zfolder,zcomp_list)                                      
                                                                                
  If zf = 0 Then                                    /* Don't dup zcomp  */      
  Do                                                                            
    zimppds = "'"migHlq"."proj"."group"."zqual"."zfolder"'"                     
    zimpcrd = zfolder                                                           
    zimpstmt.1 = "P:"zimpcrd".*="zcomp":"zfolder                                
    If ThisExec = 'BLZMIG4' Then                                                
      Address syscall "writefile (zimpCur) 755 zimpstmt. 1"                     
                                                                                
    zcomp_list = zcomp_list zcomp':'zfolder zimppds /* Add to zcomp */          
  End                                                                           
  Else                                                                          
  Do                                                                            
    zw = zf + 1                                                                 
    zimppds = WORD(zcomp_list,zw)                                               
    Parse VAR zimppds "'" (migHlq) "." (proj) "." zimpcrd "'"                   
  End                                                                           
                                                                                
  /* Copy each member from SCLM type/lang PDS into common zFolder PDS   */      
  /* and create the L: language card.                                   */      
  lmrc    = 0                                                                   
  If sclmtyp = 'BND' Then                                                       
    sclmpds = "'"migHlq"."proj"."group".VCUR.BND'"                              
  Else                                                                          
    sclmpds = "'"proj"."group"."sclmtyp"'"                                      
  migrdsn = "'"migHlq"."proj".MIGR."sclmtyp"."sclmlang"'"                       
                                                                                
  "LMINIT DATAID(SCLMID) DATASET("sclmpds") ENQ(SHR)" /* Copy mems from */      
  lmrc = MAX(rc,lmrc)                                                           
                                                                                
  "LMINIT DATAID(MIGR) DATASET("migrdsn") ENQ(SHR)"   /* Get mem names  */      
  lmrc = MAX(rc,lmrc)                                                           
                                                                                
  "LMOPEN DATAID("MIGR") OPTION(INPUT)"               /* Open for LMGET */      
  lmrc = MAX(rc,lmrc)                                                           
                                                                                
  If SYSDSN(zimppds) <> 'OK' Then                                               
  Do                                                                            
    Address 'TSO' ,                                                             
    "ALLOC DATASET("zimppds") NEW CATALOG LIKE("sclmpds")"                      
    Address 'TSO' "FREE DATASET("zimppds")"                                     
  End                                                                           
                                                                                
  "LMINIT DATAID(zimpid) DATASET("zimppds")"            /* Copy mems to */      
  lmrc = MAX(rc,lmrc)                                                           
                                                                                
  If lmrc = 0 Then                                                              
  Do                                                                            
    "LMGET DATAID("MIGR") MODE(INVAR) DATALOC(PDSMEM) " ||,                     
           "DATALEN(RECLEN) MAXLEN(80)"                                         
    lmgetrc = rc                                                                
    pdsmem = STRIP(pdsmem)                                                      
                                                                                
    Do While lmgetrc = 0                                                        
      "LMCOPY FROMID("sclmid") FROMMEM("pdsmem") TODATAID("zimpid") REPLACE"    
      If rc = 0 |,                                                              
        (rc = 8 & sclmtyp = 'BND') Then                                         
      Do                                                                        
        /* Don't create L: cards for FLMCSPDB members */                        
        /* Lets see if the LANG exist in the languages file */                  
        Do ds = 1 to langexts.0 While (sclmlang <> langdefs.ds)                 
        End                                                                     
        /* If FLMCSPDB langusge don't create L: cards */                        
        If sclmlang = langdefs.ds & langdefs.ds.cspdb = 'FLMCSPDB' Then         
          Nop                                                                   
        Else                                                                    
        Do                                                                      
          zimpcrd = zfolder                                                     
          zimpstmt.1 = "L:"zimpcrd"."pdsmem"="rtclang":"ext                     
          If ThisExec = 'BLZMIG4' Then                                          
            Address syscall "writefile (zimpCur) 755 zimpstmt. 1"               
        End                                                                     
        If vermax <> '' & vermax > 0 Then                                       
          Call doVersions                                                       
      End                                                                       
      "LMGET DATAID("MIGR") MODE(INVAR) DATALOC(PDSMEM) " ||,                   
             "DATALEN(RECLEN) MAXLEN(80)"                                       
      lmgetrc = rc                                                              
      pdsmem = STRIP(pdsmem)                                                    
    End                                                                         
  End                                                                           
                                                                                
  "LMCLOSE DATAID("MIGR")"                                                      
  "LMFREE DATAID("MIGR")"                                                       
                                                                                
  "LMFREE DATAID("zimpid")"                                                     
  "LMFREE DATAID("sclmid")"                                                     
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Rexx interpret to construct a statement and execute it        */             
/*---------------------------------------------------------------*/             
                                                                                
interp_stmt :                                                                   
                                                                                
  Parse arg interpStmt                                                          
                                                                                
  Interpret interpStmt                                                          
                                                                                
Return 0                                                                        
                                                                                
/*---------------------------------------------------------------*/             
/* No do the versions for a member                               */             
/*---------------------------------------------------------------*/             
                                                                                
doVersions :                                                                    
                                                                                
  type    = sclmtyp                                                             
  vertype = ' '                                                                 
  member  = pdsmem                                                              
  vermem  = ' '                                                                 
  datev   = ' '; timev   = ' '                                                  
  vernum  = 0                                                                   
  verinfo_rc = 0                                                                
  reptout. = ''                                                                 
  r        = 0                                                                  
                                                                                
  tsoid = USERID()                                                              
  Do Until Verinfo_rc <> 0                                                      
    Address TSO "FLMCMD VERINFO,"proj","projmem","group","type","member","||,   
            ","timev",,,,,FORWARD,"DBMSG","datev                                
    verinfo_rc = rc                                                             
                                                                                
    If verinfo_rc = 0 &,                                                        
       type   = STRIP(zsvtype) &,                                               
       member = STRIP(zsvambr) Then                                             
    Do                                                                          
      type    = STRIP(zsvtype)                                                  
      member  = STRIP(zsvambr)                                                  
      datev   = STRIP(zsvdat4)                                                  
      timev   = STRIP(zsvtime)                                                  
      formatv = STRIP(zsvcfmt)                                                  
      resultv = STRIP(zsvreslt)                                                 
      Say "Member: "member" "type                                               
      If formatv = 'AUDIT' Then              /* Skip AUDIT records */           
      Do                                                                        
        newtime = RIGHT((RIGHT(timev,5) + 0.01),5,0)                            
        timev   = LEFT(timev,6)||newtime                                        
        Iterate                                                                 
      End                                                                       
      If resultv <> 'COMPLETE' Then    /* Skip FAILED vers records */           
      Do                                                                        
        Say 'Version 'type member datev timev' skipped. '||,                    
            'Versioning action result is 'resultv'.'                            
        newtime = RIGHT((RIGHT(timev,5) + 0.01),5,0)                            
        timev   = LEFT(timev,6)||newtime                                        
        Iterate                                                                 
      End                                                                       
      If vertype = ' ' Then                 /* Set first rec equal */           
      Do                                                                        
        vertype = type                                                          
        Say 'Processing type 'type                                              
      End                                                                       
      If vermem = ' ' Then vermem = member  /* Set first rec equal */           
                                                                                
      If type <> vertype Then               /* Break on type       */           
      Do                                                                        
        vertype = type                  /* Reset type          */               
        vermem  = member                /* Reset member        */               
        Say 'Processing type 'type                                              
      End                                                                       
      Else                                                                      
        If member <> vermem Then                                                
          vermem = member /*Break on member*/                                   
                                                                                
      r = r + 1                             /* Bump stem var index */           
                                               /* Create report stem  */        
      reptout.r = LEFT(group,10)||LEFT(type,10)||LEFT(member,10)||,             
                  RIGHT(vernum,7)||'  'LEFT(datev,12)||timev                    
                                                                                
      Say "reptout."r"="reptout.r                                               
      newtime = RIGHT((RIGHT(timev,5) + 0.01),5,0) /* Bump time    */           
      timev   = LEFT(timev,6)||newtime             /* to read next */           
    End                                                                         
    Else                                                                        
      verinfo_rc = 4                                                            
  End                                                                           
                                                                                
  If r > 0 Then                                                                 
  Do                                                                            
    Address TSO "EXECIO "r" DISKW VERSRPT (STEM reptout. FINIS)"                
                                                                                
    Say "SORTing dataset "version_rpt                                           
    Address 'ISPEXEC' "EDIT DATASET("version_rpt") MACRO("edMac")"              
                                                                                
    /* Read report, overlay version numbers, add header, and rewrite rpt. */    
    Call Edit_Report                                                            
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/**********************************************************************/        
/* Read report, overlay version numbers, add header, retrieve version */        
/* and rewrite rpt.                                                   */        
/**********************************************************************/        
Edit_Report:                                                                    
                                                                                
  Address TSO "EXECIO * DISKR VERSRPT (STEM versrpt. FINIS)"                    
                                                                                
  versout.1 = LEFT('GROUP',10)||LEFT('TYPE',10)||LEFT('MEMBER',10)||,           
              LEFT('VER NUM',9)||LEFT('VER DATE',12)||'VER TIME'                
  versout.2 = ' '                                                               
  vo = 2                                                                        
                                                                                
  vernum  = 1                                                                   
  chktype = ' '                                                                 
  chkmem  = ' '                                                                 
                                                                                
  Do v = 1 to versrpt.0                                                         
    group  = STRIP(WORD(versrpt.v,1))                                           
    type   = STRIP(WORD(versrpt.v,2))                                           
    member = STRIP(WORD(versrpt.v,3))                                           
    datev  = STRIP(WORD(versrpt.v,5))                                           
    timev  = STRIP(WORD(versrpt.v,6))                                           
    If v = 1 Then                        /* Sync vars for first record */       
    Do                                                                          
      chktype = type                                                            
      chkmem  = member                                                          
    End                                                                         
    Select                                                                      
      When type = chktype & member = chkmem Then /* Same type and mem */        
           vernum = vernum - 1                                                  
      When type <> chktype & member <> chkmem Then /*Diff type and mem*/        
      Do                                                                        
        vernum  = 0                                                             
        chktype = type                                                          
        chkmem  = member                                                        
      End                                        /* Different type    */        
      When type <> chktype Then                                                 
      Do                                                                        
        vernum  = 0                                                             
        chktype = type                                                          
      End                                                                       
      When member <> chkmem Then                 /* Different member  */        
      Do                                                                        
        vernum  = 0                                                             
        chkmem  = member                                                        
      End                                                                       
      Otherwise                                                                 
    End                                                                         
                                                                                
    vo = vo + 1                                                                 
    versout.vo = OVERLAY(RIGHT(vernum,7),versrpt.v,31,9)                        
                                                                                
    Say 'Proj:  'proj                                                           
    Say 'group: 'group                                                          
    Say 'type:  'type                                                           
    Say 'member:'member                                                         
    Say 'datev: 'datev                                                          
    Say 'timev: 'timev                                                          
    Say 'vernum:'vernum                                                         
                                                                                
    /* We already got all the current versions */                               
    If vernum <> 0 Then                                                         
      Call Retrieve_Version                                                     
                                                                                
  End                                                                           
                                                                                
  "EXECIO "vo" DISKW VERSRPT (STEM versout. FINIS)"                             
                                                                                
Return                                                                          
                                                                                
/**********************************************************************/        
/* Submit job to retrieve member version.                             */        
/**********************************************************************/        
Retrieve_Version:                                                               
                                                                                
  Address ISPEXEC                                                               
                                                                                
  If vermax < ABS(vernum) Then                                                  
    Return                                                                      
                                                                                
  vernumdsn = 'V'ABS(vernum)                                                    
                                                                                
  If Wordpos(vernumdsn,ver_list) = 0 Then                                       
  Do                                                                            
    ver_list = ver_list' 'vernumdsn                                             
    zimpstmt.0 = 2                                                              
    zimpstmt.1 = "# zimport statements for "proj"."group" - Version "vernumdsn  
    zimpstmt.2 = "# zimport data sets created on "Date()" at "Time()            
    interp_stmt("zimp"vernumdsn" = '" ||,                                       
                outputDir"/zImport-"group"-"vernumdsn".mapfile'")               
    If ThisExec = 'BLZMIG4' Then                                                
      Address syscall "writefile (zimp"vernumdsn") 755 zimpstmt. "              
    zimpstmt.0 = 1                                                              
                                                                                
    interp_stmt("zimpsh"vernumdsn" = '" ||,                                     
                outputDir"/zImport-"group"-"vernumdsn".sh'")                    
                                                                                
    Address syscall "stat (zimpsh"vernumdsn") st."                              
    If st.0 = 0 Then                                                            
    Do                                                                          
      /* Create shell script to run zimport */                                  
      zimpshell.3  = 'HLQ="'migHlq'.'Translate(proj)'.'group'.'vernumdsn'"'     
      zimpshell.4  = 'mapFile="'outputDir'/zImport-'group'-'vernumdsn'.mapfile"'
      If ThisExec = 'BLZMIG4' Then                                              
        Address syscall "writefile (zimpsh"vernumdsn") 755 zimpshell. "         
    End                                                                         
  End                                                                           
                                                                                
                                                                                
  /* Create C: card - one per zcomponent:component                      */      
                                             /* Don't dup Comp lines   */       
  If WORDPOS(zcomp'='rtccomp,comp_list.vernumdsn) = 0 Then                      
  Do                                                                            
    zimpstmt.1 = "C:"zcomp"="rtccomp                                            
    If ThisExec = 'BLZMIG4' Then                                                
      Address syscall "writefile (zimp"vernumdsn") 755 zimpstmt. 1"             
    comp_list.vernumdsn = comp_list.vernumdsn zcomp'='rtccomp                   
  End                                                                           
                                                                                
  /* Create P: card - one per zcomponent:zfolder combination            */      
                                                                                
  zf = WORDPOS(zcomp':'zfolder,zcomp_list.vernumdsn)                            
                                                                                
  If zf = 0 Then                                    /* Don't dup zcomp  */      
  Do                                                                            
    zimppds = "'"migHlq"."proj"."group"."zfolder"."vernumdsn"'"                 
    zimpcrd = zfolder                                                           
    zimpstmt.1 = "P:"zimpcrd".*="zcomp":"zfolder                                
    If ThisExec = 'BLZMIG4' Then                                                
      Address syscall "writefile (zimp"vernumdsn") 755 zimpstmt. 1"             
    zcomp_list.vernumdsn = zcomp_list.vernumdsn zcomp':'zfolder zimppds         
  End                                                                           
  Else                                                                          
  Do                                                                            
    zw = zf + 1                                                                 
    zimppds = WORD(zcomp_list.vernumdsn,zw)                                     
    Parse VAR zimppds "'" (migHlq) "." (proj) "." zimpcrd "'"                   
  End                                                                           
                                                                                
  /* Don't create L: cards for FLMCSPDB members */                              
  /* Lets see if the LANG exist in the languages file */                        
  Do ds = 1 to langexts.0 While (sclmlang <> langdefs.ds)                       
  End                                                                           
  /* If FLMCSPDB langusge don't create L: cards */                              
  If sclmlang = langdefs.ds & langdefs.ds.cspdb = 'FLMCSPDB' Then               
    Nop                                                                         
  Else                                                                          
  Do                                                                            
    zimpcrd = zfolder                                                           
    zimpstmt.1 = "L:"zimpcrd"."pdsmem"="rtclang":"ext                           
    If ThisExec = 'BLZMIG4' Then                                                
      Address syscall "writefile (zimp"vernumdsn") 755 zimpstmt. 1"             
                                                                                
    migrFile = outputDir"/members.txt"                                          
    migrStmt.0 = 1                                                              
    migrStmt.1 = mighlq"."Translate(proj)"."group"."vernumdsn"." ||,            
                 type"("pdsmem") "sclmlang                                      
    Address syscall "writefile (migrFile) 755 migrStmt. 1"                      
  End                                                                           
                                                                                
  versdsn = migHlq"."proj"."group"."vernumdsn"."type                            
  jobname = tsoid'R'                                                            
                                                                                
  "FTOPEN TEMP"                         /* Open temp for ISPF tailoring */      
  If rc = 0 Then                                                                
  Do                                                                            
    If ThisExec = 'BLZMIG4' Then                                                
      "FTINCL BLZVERJC"             /* Skel for Promote job card    */          
    Else                                                                        
      "FTINCL VERJOBC"              /* Skel for Promote job card    */          
                                                                                
    If SYSDSN("'"versdsn"'") <> 'OK' Then                                       
    Do                                                                          
      modeldsn = proj'.'group'.'type                                            
      Address TSO "ALLOC DATASET('"versdsn"') "||,                              
                    "LIKE('"modeldsn"') NEW CATALOG"                            
      Address TSO "FREE DATASET('"versdsn"')"                                   
    End                                                                         
    If ThisExec = 'BLZMIG4' Then                                                
      "FTINCL BLZVERRT"             /* SCLM Version Retrieve SKEL   */          
    Else                                                                        
      "FTINCL VERRETR"                                                          
    "FTCLOSE"                       /* Done with file tailoring     */          
    "VGET ZTEMPF"                   /* Get temp jcl variable        */          
    Dummy = Outtrap("cmd_output_line.","*") /* Trap output of SUB   */          
                                                                                
    Address TSO "SUBMIT '"ztempf"'"                                             
    Say "Submit job: "rc                                                        
    If rc = 0 Then                                                              
    Do                                                                          
      Parse Var cmd_output_line.1 .  jobname  "(" jobnum ")" .                  
      Say LEFT(jobname jobnum' has been submitted.',77)                         
    End                                                                         
  End                                                                           
                                                                                
Return                                                                          
