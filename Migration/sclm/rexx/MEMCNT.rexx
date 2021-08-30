/* REXX */                                                                      
/*********************************************************************/         
/*                                                                   */         
/* NAME := EXTARCH                                                   */         
/*                                                                   */         
/* DESCRIPTIVE NAME := SCLM Migration Member count exec              */         
/*                                                                   */         
/* FUNCTION := This rexx exec will check the "PROD" group and count  */         
/*             the members by type and language.  It will also copy  */         
/*             all members out to MIGR datasets to be used by the    */         
/*             ZIMPORT process.                                      */         
/*                                                                   */         
/*********************************************************************/         
  Trace o                                        /* trace - o, r     */         
                                                                                
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
  proj = Translate(proj,lower,upper)                                            
                                                                                
  If group = '' Then                                                            
  Do                                                                            
    Say 'E SCLM Group to be migrated must be specified'                         
    migRc = 8                                                                   
  End                                                                           
                                                                                
  If migHlq = '' Then                                                           
    migHlq  = Userid()                                                          
                                                                                
  If ThisExec = 'BLZMIG3' Then                                                  
  Do                                                                            
    If defComponent = '' Then                                                   
    Do                                                                          
      Say 'E Default RTC Component must be specified'                           
      migRc = 8                                                                 
    End                                                                         
    If defzProject = '' Then                                                    
    Do                                                                          
      defzProject = proj'-Migration'                                            
      Say 'I Default project defaulted to 'defzProject                          
    End                                                                         
    edMac = 'BLZSRTDB'                                                          
  End                                                                           
  Else                                                                          
  Do                                                                            
    defComponent = 'git'                                                        
    defzProject = 'git'                                                         
    edMac = 'SORTDB'                                                            
    Address TSO "ALTLIB ACTIVATE APPLICATION(EXEC),                             
                               DATASET('"migHlq".SCLMMIG.REXX')"                
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
  srce_types = ''                                                               
  languages  = ''                                                               
  Do i = 1 to langexts.0                                                        
    parse var langexts.i langs.i ':' langs.i.dfltsrc ':' langs.i.ext ':' .      
    If (Wordpos(langs.i.dfltsrc,dependtypes) = 0 &,                             
        Wordpos(langs.i.dfltsrc,srce_types) = 0) |,                             
       (langs.i.ext = 'bnd')Then                                                
      srce_types = srce_types ||' '||Strip(langs.i.dfltsrc)                     
      languages  = languages  ||' '||Strip(langs.i)                             
  End                                                                           
  ll = langexts.0                                                               
  migrtypes = srce_types ||' '||dependtypes                                     
                                                                                
  migrStmt.0 = 0                                                                
  migrFile = outputDir"/members.txt"                                            
  Address syscall "writefile (migrFile) 755 migrStmt."                          
                                                                                
  total_count = 0                                                               
                                                                                
  Call Dbutil_Report                                                            
                                                                                
  Select                                                                        
    When result > 4 Then                                                        
    Do                                                                          
      Say ' '                                                                   
      Say 'E Major issue with DBUTIL *'                                         
      Say 'result > 4'                                                          
      Say result                                                                
    End                                                                         
    When result = 4 Then                                                        
    Do                                                                          
      Say ' '                                                                   
      Say COPIES('*',79)                                                        
      Say LEFT('* No results from DBUTIL.  No members '||,                      
               'in 'fromgrp' group for 'proj,77) '*'                            
      Say COPIES('*',79)                                                        
      Say ' '                                                                   
    End                                                                         
    Otherwise                          /* Report on members in PROD */          
    Do                                                                          
      Call Delete_MIGR_dsn             /* Clean up MIGR dsns        */          
                                                                                
      Say "SORTing dataset "dbutil_dsn                                          
      "ALLOC F(DBRPT) DA("dbutil_dsn") SHR REUSE"                               
      Address 'ISPEXEC' "EDIT DATASET("dbutil_dsn") MACRO("edMac")"             
                                                                                
      Call Process_DBUTIL_Report       /* Get counts and copy to PDS*/          
                                                                                
      If copytype <> '' Then           /* Copy copytype mems using  */          
      Do                               /* generated list.           */          
        type = copytype                                                         
        lang = 'TEXT'                                                           
        copytype = ''                                                           
        Do m = 1 to WORDS(copylibmems)                                          
          mem = WORD(copylibmems,m)                                             
          If scopeflg = 'INCLUDE' Then                                          
          Do                                                                    
            Call Process_Include                                                
            If result Then                                                      
              Call Write_Member                                                 
          End                                                                   
          Else                                                                  
            Call Write_Member                                                   
        End                                                                     
      End                                                                       
                                                                                
      Call Count_Report                       /* Create count report*/          
                                                                                
      If ce > 3 Then                          /* Copy errors report */          
      Do                                                                        
        copyerr_dsn = "'"mighlq"."proj"."group".MEMBER.COPYERRS'"               
        x = msg('off')                                                          
        "DELETE "copyerr_dsn                                                    
        x = msg('on')                                                           
        "ALLOC F(COPYRPT) DA("copyerr_dsn") RECFM(F B) LRECL(80) ",             
                         "BLKSIZE(3120) SPACE(1 1) CYLINDERS REUSE"             
        "EXECIO "ce" DISKW COPYRPT (STEM copyerr. FINIS)"                       
      End                                                                       
      If sk > 6 Then                          /* Skipped mem report */          
      Do                                                                        
        skiperr_dsn = "'"mighlq"."proj"."group".MEMBER.SKIPERRS'"               
        x = msg('off')                                                          
        "DELETE "skiperr_dsn                                                    
        x = msg('on')                                                           
        "ALLOC F(SKIPRPT) DA("skiperr_dsn") RECFM(F B) LRECL(80) ",             
                         "BLKSIZE(3120) SPACE(1 1) CYLINDERS REUSE"             
        skipmem.3 = OVERLAY(sk-6,skipmem.3,35)                                  
        "EXECIO "sk" DISKW SKIPRPT (STEM skipmem. FINIS)"                       
      End                                                                       
    End                                                                         
  End                                                                           
                                                                                
Exit                                                                            
                                                                                
/**********************************************************************/        
/* Run DBUTIL report to list every member that is in the 'PROD' group */        
/* and store the data in a stem variable.                             */        
/**********************************************************************/        
Dbutil_Report:                                                                  
                                                                                
  dbutilrc   = 0                                                                
  dbutil_dsn = "'"mighlq"."proj"."group".DBUTLRPT'" /* DBUITL report dsn */     
  x = msg('off')                                                                
  "DELETE "dbutil_dsn                                                           
  x = msg('on')                                                                 
  "ALLOC F(DBRPT) DA("dbutil_dsn") RECFM(V B) LRECL(133) BLKSIZE(15000) ",      
          "SPACE(10 50) CYLINDERS REUSE"                                        
  "ALLOC F(DUMMY) DUMMY"                                                        
  "ALLOC F(DBMSG) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(1 50) ",           
          "CYLINDERS REUSE"                                                     
                                                                                
  If copytype <> '' Then                                                        
    "FLMCMD DBUTIL,"proj","projmem","group",,,,,,*,*"||,                        
                   ",*,*,*,*,*,NO,ACCT,*,,,,NORMAL,NO,NO,," ||,                 
                   "DUMMY,DUMMY,DBRPT" ||,                                      
                   ",@@FLMMBR @@FLMTYP @@FLMLAN " ||,                           
                   "@@FLMCD4 @@FLMMVR @@FLMSTA @@FLM$IN"                        
  Else                                                                          
  Do                                                                            
    Say 'DBUTIL exec'                                                           
    "FLMCMD DBUTIL,"proj","projmem","group",,,,,,*,*"||,                        
                   ",*,*,*,*,*,NO,ACCT,*,,,,NORMAL,NO,NO,," ||,                 
                   "DBMSG,DUMMY,DBRPT" ||,                                      
                   ",@@FLMMBR @@FLMTYP @@FLMLAN " ||,                           
                   "@@FLMCD4 @@FLMMVR @@FLMSTA"                                 
  End                                                                           
                                                                                
                                                                                
  dbutilrc = MAX(rc,dbutilrc)                                                   
  If dbutilrc > 0  Then                                                         
  Do                                                                            
    Say ' '                                                                     
    Say 'E' 'DBUTIL error, return code ='rc                                     
    Say ' '                                                                     
    "EXECIO * DISKR DBMSG (STEM dbmsgs. FINIS)"                                 
    Do i = 1 to dbmsgs.0                                                        
      Say ' ' dbmsgs.i                                                          
    End                                                                         
  End                                                                           
  "EXECIO * DISKR DBRPT (STEM dbutil. FINIS)"                                   
  Say ' '                                                                       
  Say 'I' 'DBUTIL returned 'dbutil.0 'Members'                                  
                                                                                
  "FREE F(DUMMY)"                                                               
  "FREE F(DBMSG)"                                                               
  "FREE F(DBRPT)"                                                               
                                                                                
Return dbutilrc                                                                 
                                                                                
/**********************************************************************/        
/* Count members by type and language.                                */        
/**********************************************************************/        
Process_DBUTIL_Report:                                                          
                                                                                
  "EXECIO * DISKR DBRPT (STEM DBUTline. FINIS)"   /* Read DBUTIL output */      
                                                                                
  copylibmems = ''                                                              
  /* Header information for Copy error report dataset                   */      
  copyerr.1 = 'The following members were NOT copied to the migration PDS'      
  copyerr.2 = LEFT('MEMBER',10)||LEFT('SCLM PDS',27)||'MIGRATION PDS'           
  copyerr.3 = ' '                                                               
  ce = 3                                                                        
                                                                                
  /* Header information for skipped member report dataset               */      
  skipmem.1 = 'The following members were SKIPPED in the migration '||,         
              'counts report.'                                                  
  skipmem.2 = 'They have not been changed since 'migrdate'.'                    
  skipmem.3 = LEFT('Total number of members skipped =',80)                      
  skipmem.4 = ' '                                                               
  skipmem.5 = LEFT('SCLM MEM',10)||LEFT('SCLM TYPE',10)||,                      
              LEFT('SCLM LANG',10)||'CHANGE DATE'                               
  skipmem.6 = ' '                                                               
  sk = 6                                                                        
                                                                                
  If scopeflg = 'INCLUDE' Then                                                  
    Call Read_Types                 /* Get list of types to copy into   */      
                                    /* MIGR datasets.                   */      
                                                                                
  Do d = 1 to DBUTline.0            /* Loop through DBUTIL report output*/      
                                                                                
    Parse UPPER VAR DBUTline.d mem 9 type 18 lang 27 chkdate 38 ,               
                               chkvers 47 chkstat 56 chkcopy                    
    mem     = STRIP(mem)                                                        
    type    = STRIP(type)                                                       
    lang    = STRIP(lang)                                                       
    chkdate = STRIP(chkdate)                                                    
    chkvers = STRIP(chkvers)                                                    
    status  = STRIP(chkstat)                                                    
    chkcopy = STRIP(chkcopy)                                                    
                                                                                
    If lang = '' | status = 'NON-EDIT' Then Iterate                             
                                                                                
    /* SCLM is using a language that is no longer in the project definition */  
    If Wordpos(lang,languages) = 0 & Pos('ARCHDEF',lang) = 0 Then               
    Do                                                                          
      sk = sk + 1                              /* Bump report stem */           
      skipmem.sk = LEFT(mem,10)||LEFT(type,10)||LEFT(lang,10)||,                
                   ' - Language not in project definition'                      
      Iterate                                                                   
    End                                                                         
                                                                                
    If chkdate < migrdate Then                                                  
    Do                                                                          
      sk = sk + 1                              /* Bump report stem */           
      skipmem.sk = LEFT(mem,10)||LEFT(type,10)||LEFT(lang,10)||chkdate          
      Iterate                                                                   
    End                                                                         
                                  /* Process multi copylib mems that  */        
    If copytype <> '' & WORDS(DBUTline.d) = 1 Then  /* sort to the top */       
    Do                                                                          
      If WORDPOS(mem,copylibmems) = 0 Then                                      
        copylibmems = copylibmems mem                                           
      Iterate                                                                   
    End                                                                         
                                                                                
    If scopeflg = 'INCLUDE' Then                                                
    Do                                                                          
      Call Process_Include                                                      
      If result Then                                                            
        Call Write_Member                                                       
    End                                                                         
    Else                                                                        
      Call Write_Member                                                         
                                                                                
    If chkcopy <> '' & WORDPOS(type,dependtypes) > 0 Then                       
    Do                                                                          
      If WORDPOS(chkcopy,copylibmems) = 0 Then                                  
        copylibmems = copylibmems chkcopy                                       
    End                                                                         
    If type = copytype Then                                                     
    Do                                                                          
      If WORDPOS(mem,copylibmems) = 0 Then                                      
        copylibmems = copylibmems mem                                           
    End                                                                         
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/**********************************************************************/        
/* Read INCLUDE dataset to see if member is to be included in migr    */        
/**********************************************************************/        
Process_INCLUDE:                                                                
                                                                                
  inclrc = 0                                                                    
  dsndata. = ''                                                                 
                                                                                
  dsname = mighlq"."proj"."group"."type"."scopeflg                              
                                                                                
  If SYSDSN("'"dsname"'") <> 'OK' Then Return inclrc                            
                                                                                
  "ALLOC F(DSNX) DA('"dsname"') SHR"                                            
  "EXECIO * DISKR DSNX (STEM dsndata. FINIS)"                                   
                                                                                
  Do i = 1 to dsndata.0                                                         
    memi = STRIP(dsndata.i)                                                     
    If memi = mem Then inclrc = 1                                               
  End                                                                           
                                                                                
  "FREE F(DSNX)"                                                                
                                                                                
Return inclrc                                                                   
                                                                                
/**********************************************************************/        
/* Read dataset hlq.project.prdgrp.TYPES to detemine which types will */        
/* be copied into the MIGR datasets for use by ZIMPORT.               */        
/**********************************************************************/        
Read_Types:                                                                     
                                                                                
  typesmem = "'"mighlq"."proj"."group".TYPES'"/* Set TYPES member     */        
                                                                                
  If SYSDSN(typesmem) = 'DATA SET NOT FOUND' Then                               
    Return                                                                      
                                                                                
  "ALLOC FILE(TYPES) DATASET("typesmem") SHR"  /* Alloc TYPES member  */        
  "EXECIO * DISKR TYPES (STEM types. FINIS)"   /* Read TYPES member   */        
  "FREE FILE(TYPES)"                           /* Free TYPES member   */        
                                                                                
  migrtypes = ''                                                                
  Do t = 1 to types.0                                                           
    If SUBSTR(types.t,1,1) = '*' Then Iterate  /* Skip comment        */        
    migrtyp = WORD(types.t,1)                  /* Set specific type   */        
    If POS(migrtyp,excludetypes) = 0 Then   /* Check if type excluded?*/        
      migrtypes = migrtypes migrtyp         /* No, add type to list   */        
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/**********************************************************************/        
/* Create a list of members to copy split by type and language        */        
/**********************************************************************/        
Write_Member:                                                                   
                                                                                
  Address 'ISPEXEC'                                                             
                                                                                
  copyrc = 0                                                                    
                               /* If TYPES specified, only copy those */        
  If WORDPOS(type,lectypes) <> 0 Then                                           
  Do                                                                            
    lang = 'LE370'             /* Default SCLM language for binder    */        
                                                                                
    /* Need to read the LEC ARCDEF and see if a different lang is used*/        
    archdmem = "'"proj"."group"."type"("mem")'"    /* Set LEC member  */        
                                                                                
    If SYSDSN(archdmem) = 'OK' Then                                             
    Do                                                                          
      Address TSO "ALLOC FILE(ARCHD) DATASET("archdmem") SHR"                   
      Address TSO "EXECIO * DISKR ARCHD (STEM archd. FINIS)"                    
      Address TSO "FREE FILE(ARCHD)"                                            
                                                                                
      Do a = 1 to archd.0                                                       
        If POS('LKED ',archd.a) <> 0 Then                                       
        Do                                                                      
          parse var archd.a 'LKED 'lang .                                       
          lang = Strip(lang)                                                    
          leave                                                                 
        End                                                                     
      End                                                                       
    End                                                                         
    type = 'BND'                                                                
  End                                                                           
                                                                                
  If WORDPOS(type,migrtypes) = 0 & migrtypes <> 'ALL' Then Return               
                               /* If COPYLIB specified, don't copy use*/        
                               /* generated list.                     */        
  If type = copytype Then Return                                                
                                                                                
  migrdsn = "'"mighlq"."proj".MIGR."type"."lang"'"    /* Output dataset */      
  If Length(migrdsn) > 44 Then                                                  
  Do                                                                            
    migmsg = 'Migrate data set name > 44 bytes - 'migrdsn                       
    ce = ce + 1                                                                 
    copyerr.ce = LEFT(' ',10)||LEFT(' ',27)||migmsg                             
  End                                                                           
                                                                                
  If SYSDSN(migrdsn) <> 'OK' Then                                               
  Do                                                                            
    Address 'TSO' ,                                                             
    "ALLOC DATASET("migrdsn") NEW CATALOG "||,                                  
           "SPACE(15,10) TRACKS UNIT(SYSDA) LRECL(80) "||,                      
           "BLKSIZE(3120) RECFM(F B) DSORG(PS)"                                 
    Address 'TSO' "FREE DATASET("migrdsn")"                                     
  End                                                                           
                                                                                
  "LMINIT DATAID(migrid) DATASET("migrdsn") ENQ(MOD)"                           
  copyrc = MAX(rc,copyrc)                                                       
                                                                                
  If copyrc > 0 Then      /* Update stem variable for copy errors */            
  Do                                                                            
    migmsg = 'Error on LMINIT : rc='rc                                          
    ce = ce + 1                                                                 
    copyerr.ce = LEFT(mem,10)||LEFT(migmsg,27)||migrdsn                         
    Return copyrc                                                               
  End                                                                           
                                                                                
  "LMOPEN DATAID("migrid") OPTION(OUTPUT)"                                      
  copyrc = MAX(rc,copyrc)                                                       
                                                                                
  If copyrc > 0 Then      /* Update stem variable for copy errors */            
  Do                                                                            
    migmsg = 'Error on LMOPEN : rc='rc                                          
    ce = ce + 1                                                                 
    copyerr.ce = LEFT(mem,10)||LEFT(migmsg,27)||migrdsn                         
    "LMFREE DATAID("migrid")"                                                   
    Return copyrc                                                               
  End                                                                           
                                                                                
  "LMPUT DATAID("migrid") MODE(INVAR) DATALOC(MEM) DATALEN(80)"                 
  copyrc = MAX(rc,copyrc)                                                       
                                                                                
  If copyrc > 0 Then            /* Update stem variable for copy errors */      
  Do                                                                            
    migmsg = 'Error on LMPUT  : rc='rc                                          
    ce = ce + 1                                                                 
    copyerr.ce = LEFT(mem,10)||LEFT(migmsg,27)||migrdsn                         
  End                                                                           
                                                                                
  /* Only write members.txt line if language is not ARCHDEF */                  
  If lang <> 'ARCHDEF' Then                                                     
  Do                                                                            
    migrStmt.0 = 1                                                              
    migrStmt.1 = mighlq"."Translate(proj)"."group".VCUR."type"("mem") "lang     
    Address syscall "writefile (migrFile) 755 migrStmt. 1"                      
  End                                                                           
                                                                                
  "LMCLOSE DATAID("migrid")"                                                    
  "LMFREE DATAID("migrid")"                                                     
                                                                                
Return                                                                          
                                                                                
/**********************************************************************/        
/* Create count report                                                */        
/**********************************************************************/        
Count_Report:                                                                   
                                                                                
  cntrpt_dsn = "'"mighlq"."proj"."group".MEMBER.COUNTS'" /* Counts */           
  x = msg('off')                                                                
  Address TSO "DELETE "cntrpt_dsn                                               
  x = msg('on')                                                                 
  Address TSO "ALLOC F(CNTRPT) DA("cntrpt_dsn") RECFM(F B) LRECL(160) ",        
          "BLKSIZE(3200) SPACE(1 1) CYLINDERS REUSE"                            
  Address ISPEXEC                                                               
                                                                                
  /* Header information for COUNT report dataset                        */      
  reptout.1 = 'SCLM TYPE,SCLM LANG,COUNT,RTC LANG,EXT,RTC COMP,ZCOMP,ZFOLDER'   
  reptout.2 = ' '                                                               
  r = 2                                                                         
                                                                                
  migr_dsn = ''                                                                 
  migr_dsn_level = mighlq"."Translate(proj)".MIGR"                              
                                                                                
  "LMDINIT LISTID(MIGRDSNA) LEVEL("migr_dsn_level")"                            
  lmrc = rc                                                                     
                                                                                
  "LMDLIST LISTID("migrdsna") OPTION(LIST) DATASET(migr_dsn)"                   
  lmrc = MAX(rc,lmrc)                                                           
                                                                                
  Do While lmrc = 0                                                             
    If SYSDSN("'"migr_dsn"'") = "OK" Then                                       
    Do                                                                          
      Address 'TSO' "ALLOC FILE(MIGRDSN) DATASET('"migr_dsn"') SHR"             
      Address 'TSO' "EXECIO * DISKR MIGRDSN (STEM migrdsn. FINIS)"              
      Address 'TSO' "FREE FILE(MIGRDSN)"                                        
      count = migrdsn.0                                                         
      Parse Var migr_dsn (migr_dsn_level) '.' typer '.' langr                   
      rlang = proj'-'langr                                                      
                                                                                
      /* Find extension */                                                      
      notFound = 1                                                              
      rext = ''                                                                 
      Do i = 1 to ll While (notFound)                                           
        If typer||langr = langs.i.dfltsrc||langs.i Then                         
        Do                                                                      
          rext = langs.i.ext                                                    
          notFound = 0                                                          
        End                                                                     
      End                                                                       
                                                                                
      r = r + 1                                                                 
      reptout.r = typer','langr','count',' ||,                                  
                  rlang','rext','defComponent','defzProject','typer             
      total_count = total_count + count                                         
    End                                                                         
    "LMDLIST LISTID("migrdsna") OPTION(LIST) DATASET(migr_dsn)"                 
    lmrc = MAX(rc,lmrc)                                                         
  End                                                                           
                                                                                
  "LMDLIST LISTID("migrdsna") OPTION(FREE)"                                     
  "LMDFREE LISTID("migrdsna")"                                                  
                                                                                
  /* Add total count to end of stem var*/                                       
  Say total_count' members have been processed'                                 
                                                                                
  r = r + 1                                                                     
  reptout.r = RIGHT(total_count,25)                                             
                                                                                
  Address TSO "EXECIO "r" DISKW CNTRPT (STEM reptout. FINIS)"                   
  Address TSO "FREE F(CNTRPT)"                                                  
                                                                                
Return                                                                          
                                                                                
/**********************************************************************/        
/* Delete Migration datasets                                          */        
/**********************************************************************/        
Delete_MIGR_dsn:                                                                
                                                                                
  Address 'ISPEXEC'                                                             
                                                                                
  migr_dsn = ''                                                                 
  migr_dsn_level = mighlq"."proj".MIGR"                                         
                                                                                
  "LMDINIT LISTID(MIGRDSND) LEVEL("migr_dsn_level")"                            
  lmrc = rc                                                                     
                                                                                
  "LMDLIST LISTID("migrdsnd") OPTION(LIST) DATASET(migr_dsn)"                   
  lmrc = MAX(rc,lmrc)                                                           
                                                                                
  Do While lmrc = 0                                                             
    If SYSDSN("'"migr_dsn"'") = "OK" Then "LMERASE DATASET('"migr_dsn"')"       
    "LMDLIST LISTID("migrdsnd") OPTION(LIST) DATASET(migr_dsn)"                 
    lmrc = MAX(rc,lmrc)                                                         
  End                                                                           
                                                                                
  "LMDLIST LISTID("migrdsnd") OPTION(FREE)"                                     
  "LMDFREE LISTID("migrdsnd")"                                                  
                                                                                
Return                                                                          
