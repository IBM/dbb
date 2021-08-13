/* REXX */                                                                      
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
                                                                                
  Select                                                                        
    When (thisExec = 'BLZMIG1B') Then prevExec = 'Job BLZMIG1'                  
    When (thisExec = 'EXTMTDT1') Then prevExec = 'extrmetadata.sh'              
    Otherwise                                                                   
      Nop                                                                       
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
  rc = repLine('*' 'SCLM MIGRATION TOOL : Part 1B - Started 'Date() Time())     
  rc = repLine('*' ' ')                                                         
  rc = repLine('*' Copies('*',80))                                              
                                                                                
  /* Read in the stored languages */                                            
                                                                                
  langfile = outputDir'/langext.txt'                                            
  Address syscall "readfile (langfile) langexts."                               
  If rc <> 0 Then                                                               
  Do                                                                            
    rc = repLine(' ' ' ')                                                       
    rc = repLine('E' 'Languages file 'langfile' does not exist.' ||,            
                     prevExec' must be run first')                              
    migRc = 8                                                                   
  End                                                                           
  Do i = 1 to langexts.0                                                        
    parse var langexts.i langs.i ':' langs.i.dfltsrc ':' langs.i.ext            
    If langs.i.ext = 'bnd' & langs.i.dfltsrc = '' Then                          
     langs.i.dfltsrc = 'BND'                                                    
  End                                                                           
  ll = langexts.0                                                               
                                                                                
  /* Read in the stored members   */                                            
                                                                                
  memfile = outputDir'/members.xml'                                             
  Address syscall "readfile (memfile) memxml."                                  
  If rc <> 0 Then                                                               
  Do                                                                            
    rc = repLine(' ' ' ')                                                       
    rc = repLine('E' 'Members file 'memfile' does not exist.' ||,               
                     prevExec' must be run first')                              
    migRc = 8                                                                   
  End                                                                           
  j = 0                                                                         
  memInfoX. = ''                                                                
  Do i = 1 to memxml.0                                                          
    If Pos('<member name',memxml.i) <> 0 Then                                   
    Do                                                                          
      j = j + 1                                                                 
      Parse var memxml.i '<member name="'memInfoX.j.name,                       
                         '" type="'memInfoX.j.type'"' .                         
    End                                                                         
  End                                                                           
  memcntX = j                                                                   
                                                                                
  /* Next we are going to process the member information        */              
  rc = DButilMems()                                                             
  If rc > 4 Then                                                                
    Call ExitRtn(8)                                                             
                                                                                
  elapsed = Time('E')                                                           
                                                                                
  /* Update member info stem with language definition           */              
  rc = repLine(' ' ' ')                                                         
  rc = repLine('I' 'Processing the members')                                    
  Call ProcMems                                                                 
                                                                                
  elapsed = Time('E')                                                           
  rc = repLine(' ' ' ')                                                         
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
  rc = repLine('*' 'SCLM MIGRATION TOOL : Part 1B - Finished 'Date() Time())    
  rc = repLine('*' ' ')                                                         
  rc = repLine('*' Copies('*',80))                                              
                                                                                
  Call ExitRtn(0)                                                               
Exit                                                                            
                                                                                
/**********************************************************************/        
/* Run DBUTIL report to list every member that is in the 'PROD' group */        
/* and store the data in a stem variable.                             */        
/**********************************************************************/        
DButilMems :                                                                    
                                                                                
  "ALLOC F(DUMMY) DUMMY"                                                        
  "ALLOC F(DBRPT) RECFM(V B) LRECL(133) BLKSIZE(15000) SPACE(10 50)",           
          "CYLINDERS REUSE"                                                     
                                                                                
  "FLMCMD DBUTIL,"proj","projmem","group",,,,,,*,*"||,                          
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
/* Process the members account information                            */        
/**********************************************************************/        
                                                                                
ProcMems :                                                                      
                                                                                
  memcnt = 0                                                                    
                                                                                
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
          /* Only add it if it doesn't exist already */                         
          Do w = 1 to ll While (sclmlang||sclmtype <> langs.w||langs.w.dfltsrc) 
          End                                                                   
          If w > ll Then                                                        
          Do                                                                    
            ll = ll + 1                                                         
            langs.ll = langs.z                                                  
            langs.ll.dfltsrc = sclmtype                                         
            langs.ll.ext = langs.z.ext                                          
          End                                                                   
        End                                                                     
        Otherwise                                                               
          Nop                                                                   
      End                                                                       
      sclmext = langs.z.ext                                                     
    End                                                                         
                                                                                
    /* Need to see if we have this member/type already in member list */        
    Do z = 1 to memCntX,                                                        
           While (sclmmem'.'sclmext||sclmtype <>,                               
                  memInfoX.z.name||meminfoX.z.type)                             
    End                                                                         
    If z > memCntX Then                                                         
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
  Drop dbutil.                                                                  
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Create XML that contains information about members            */             
/*---------------------------------------------------------------*/             
                                                                                
createXml :                                                                     
                                                                                
  Drop xml.                                                                     
  xcnt = 0                                                                      
                                                                                
  /* We will append additional member information to members.xml */             
  /* created in part 1A                                          */             
                                                                                
  Call xmlMember                                                                
  Call xmlFooter ('member')                                                     
                                                                                
  xml.0 = xcnt                                                                  
  memsfile = outputDir'/members.xml'                                            
  Address syscall "writefile (memsfile) 755 xml. 1"                             
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML for Member information                                    */             
/*---------------------------------------------------------------*/             
xmlMember :                                                                     
                                                                                
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
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML Footer information                                        */             
/*---------------------------------------------------------------*/             
xmlFooter :                                                                     
                                                                                
  parse arg xmlType                                                             
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt   = '  </target>'                                                    
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  '                                                               
  xcnt = xcnt + 1                                                               
  If xmlType = 'keyref' Then                                                    
    xml.xcnt = '  <target depends="keyrefinfo"' ||,                             
                     ' description="SCLM Keyref information" name="all"/>'      
  Else                                                                          
    xml.xcnt = '  <target depends="memberinfo"' ||,                             
                     ' description="SCLM Member information" name="all"/>'      
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
