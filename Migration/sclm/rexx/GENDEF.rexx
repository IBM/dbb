/* REXX */                                                                      
/*********************************************************************/         
/*                                                                   */         
/* NAME := GENDEF                                                    */         
/*                                                                   */         
/* DESCRIPTIVE NAME := Generate systemDefinition.xml                 */         
/*                                                                   */         
/* FUNCTION := Using information in files created from SCLM          */         
/*             migration part 1 generate system definition XML for   */         
/*             data sets, translators and language definitions.      */         
/*             Also gemerate file level metadata overrides.          */         
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
    call ExitRtn(8)                                                             
  End                                                                           
                                                                                
  Select                                                                        
    When (thisExec = 'BLZMIG2') Then prevExec = 'Job BLZMIG1'                   
    When (thisExec = 'GENDEF')  Then prevExec = 'extrmetadata.sh'               
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
                                                                                
  db = 0                                                                        
  report. = ''                                                                  
  migRc = 0                                                                     
                                                                                
  If outputDir = '' Then                                                        
  Do                                                                            
    user = USERID()                                                             
    address syscall 'getpwnam (user) pw.'                                       
    outputDir = pw.4                                                            
  End                                                                           
  proj = Translate(proj,lower,upper)                                            
                                                                                
  If projmem  = '' Then                                                         
    projmem  = proj                                                             
                                                                                
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
                                                                                
  If migHlq   = '' Then                                                         
    migHlq   = Userid()                                                         
                                                                                
  rc = repLine('*' Copies('*',80))                                              
  rc = repLine('*' ' ')                                                         
  rc = repLine('*' 'SCLM MIGRATION TOOL : Part 2 - Started' date() Time())      
  rc = repLine('*' ' ')                                                         
  rc = repLine('*' Copies('*',80))                                              
                                                                                
  If defzProject = '' Then                                                      
  Do                                                                            
    defzProject = proj'-Migration'                                              
    rc = repLine(' ' ' ')                                                       
    rc = repLine('I' 'Default project defaulted to 'defzProject)                
  End                                                                           
                                                                                
  projfile = outputDir'/projseq.txt'                                            
  Address syscall "readfile (projfile) projdefs."                               
  If rc <> 0 | projdefs.0 = 0 Then                                              
  Do                                                                            
    rc = repLine(' ' ' ')                                                       
    rc = repLine('E' 'Project definitions file 'projfile' does not ' ||,        
                     'exist or is empty. 'prevExec' must be run first')         
    migRc = 8                                                                   
  End                                                                           
                                                                                
  langfile = outputDir'/langext.txt'                                            
  Address syscall "readfile (langfile) langexts."                               
  If rc <> 0 Then                                                               
  Do                                                                            
    rc = repLine(' ' ' ')                                                       
    rc = repLine('E' 'Languages file 'langfile' does not ' ||,                  
                     'exist. 'prevExec' must be run first')                     
    migRc = 8                                                                   
  End                                                                           
                                                                                
  archfile = outputDir'/archtype.txt'                                           
  Address syscall "readfile (archfile) archtype."                               
  If rc <> 0 | projdefs.0 = 0 Then                                              
  Do                                                                            
    rc = repLine(' ' ' ')                                                       
    rc = repLine('E' 'ARCHDEF type file 'archfile' does not ' ||,               
                     'exist or is empty. 'prevExec' must be run first')         
    migRc = 8                                                                   
  End                                                                           
                                                                                
  kreffile = outputDir'/keyref.xml'                                             
  Address syscall "readfile (kreffile) keyrefs."                                
  If rc <> 0 Then                                                               
  Do                                                                            
    rc = repLine(' ' ' ')                                                       
    rc = repLine('E' 'Keyref file 'kreffile' does not exist. ' ||,              
                     prevExec' must be run first')                              
    Return 8                                                                    
  End                                                                           
                                                                                
  If migRc = 0 Then                                                             
  Do                                                                            
    Call Controls                                                               
                                                                                
    Call DsDefs                                                                 
                                                                                
    /* Validate the default source is in the SCLM types table */                
    langdefs.  = ''                                                             
    langdefs.0 = langexts.0                                                     
    Do i = 1 to langexts.0                                                      
      parse var langexts.i langdefs.i ':' langdefs.i.dfltsrc ':' langdefs.i.ext 
      langdefs.i.cspdb = ''                                                     
      Do j = 1 to types.0 while (langdefs.i.dfltsrc <> types.j)                 
      End                                                                       
      If j > types.0 & langdefs.i.dfltsrc <> '' &,                              
         langdefs.i.dfltsrc <> 'BND' Then                                       
      Do                                                                        
        rc = repLine(' ' ' ')                                                   
        rc = repLine('E' 'Invalid source type 'langdefs.i.dfltsrc)              
      End                                                                       
    End                                                                         
                                                                                
    /* Process the language definitions              */                         
    Call LangDefs                                                               
                                                                                
    If rc = 0 Then                                                              
    Do                                                                          
      Drop projdefs.                                                            
      Drop langexts.                                                            
      Drop archtype.                                                            
                                                                                
      /* Process the member metadata                   */                       
      Call fileMetaData                                                         
                                                                                
      /* Process the system definition XML             */                       
      Call genXML                                                               
    End                                                                         
  End                                                                           
                                                                                
  /* Lets write out the langext in case we changed FLMCSPDB     */              
  Do i = 1 to langdefs.0                                                        
    dfltsrc.i = langdefs.i':'langdefs.i.dfltsrc':'langdefs.i.ext':' ||,         
                langdefs.i.cspdb                                                
  End                                                                           
                                                                                
  dfltsrc.0 = langdefs.0                                                        
  extfile  = outputDir'/langext.txt'                                            
  Address syscall "writefile (extfile) 755 dfltsrc."                            
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine('*' Copies('*',80))                                              
  rc = repLine('*' ' ')                                                         
  rc = repLine('*' 'SCLM MIGRATION TOOL : Part 2 - Finished' date() Time())     
  rc = repLine('*' ' ')                                                         
  rc = repLine('*' Copies('*',80))                                              
                                                                                
  Call exitRtn(0)                                                               
Exit                                                                            
                                                                                
/*---------------------------------------------------------------*/             
/* Get a project definition control information                  */             
/*---------------------------------------------------------------*/             
                                                                                
Controls :                                                                      
                                                                                
  Drop types.                                                                   
  ty = 0                                                                        
                                                                                
  /* Need to determine some project control information           */            
  /* For example project name and MLQ or alternate data set name  */            
                                                                                
  Do i = 1 to projdefs.0                                                        
    Select                                                                      
      /* Project name determined from the FLMABEG macro           */            
      When (Pos('FLMABEG',projdefs.i) <> 0) Then                                
      Do                                                                        
        Parse var projdefs.i Project 'FLMABEG' .                                
        Project = Strip(Project)                                                
      End                                                                       
      /* Project name determined from the FLMABEG macro           */            
      When (Pos('FLMCNTRL',projdefs.i) <> 0) Then                               
      Do                                                                        
        Parse var projdefs.i . 'VIOUNIT='viounit ','  .                         
        /* get rid of extra rubbish */                                          
        Parse var vioUnit vioUnit .                                             
        If vioUnit = '' Then                                                    
          vioUnit = 'VIO'                                                       
      End                                                                       
      /* Need to work out the top group. Only need to create data */            
      /* set definitions for the type not the whole hierarchy     */            
      When (Pos('FLMGROUP',projdefs.i) <> 0) Then                               
      Do                                                                        
        If Pos('PROMOTE',projdefs.i) = 0 Then                                   
        Do                                                                      
          Parse var projdefs.i TopGroup 'FLMGROUP' . 'ALTC='AltCntl             
                                                                                
          /* get rid of extra rubbish */                                        
          Parse var TopGroup TopGroup .                                         
          Parse var AltCntl AltCntl .                                           
                                                                                
          altDsname = ''                                                        
          If AltCntl <> '' Then                                                 
          Do                                                                    
            /* We need to read the FLMALTC to get a data set      */            
            /* naming pattern if it exists                        */            
            Do x = 1 to projdefs.0                                              
              If Pos('FLMALTC',projdefs.x) <> 0 &,                              
                 Pos(AltCntl,projdefs.x) <> 0 Then                              
              Do                                                                
                Parse var projdefs.x . 'DSNAME='altDsname .                     
                /* Need to change @@FLMPRJ and @@FLMGRP to names  */            
                y = Pos('@@FLMPRJ',altDsname)                                   
                If y <> 0 Then                                                  
                  altDsname = Substr(altDsname,1,y-1)||,                        
                              Project||Substr(altDsname,y+8)                    
                y = Pos('@@FLMGRP',altDsname)                                   
                If y <> 0 Then                                                  
                  altDsname = Substr(altDsname,1,y-1)||,                        
                           TopGroup||Substr(altDsname,y+8)                      
                Leave                                                           
              End                                                               
            End                                                                 
          End                                                                   
        End                                                                     
      End                                                                       
                                                                                
      /* Need get a list of types to store for later              */            
      When (Pos('FLMTYPE',projdefs.i) <> 0) Then                                
      Do                                                                        
        Parse var projdefs.i Type 'FLMTYPE' . 'EXTEND='Extend_Type              
                                                                                
        /* get rid of extra rubbish */                                          
        Parse var Extend_Type Extend_Type .                                     
                                                                                
        ty = ty + 1                                                             
        types.ty = Type                                                         
                                                                                
        If Extend_Type <> '' then                                               
          types.ty.Extend = Extend_Type                                         
        Else                                                                    
          types.ty.Extend = ''                                                  
      End                                                                       
                                                                                
      Otherwise                                                                 
        NOP                                                                     
    End                                                                         
  End                                                                           
  types.0 = ty                                                                  
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Get a data set definitions                                    */             
/*---------------------------------------------------------------*/             
                                                                                
DsDefs :                                                                        
                                                                                
  t1 = 0                                                                        
  t2 = 0                                                                        
  t3 = 0                                                                        
  t4 = 0                                                                        
  fs = 0                                                                        
  Typet1. = ''                                                                  
  Typet2. = ''                                                                  
  Typet3. = ''                                                                  
  Typet4. = ''                                                                  
  flmsyslb. = ''                                                                
  firstArch = 1                                                                 
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine(' ' Copies('*',80))                                              
  rc = repLine(' ' ' ')                                                         
  rc = repLine(' ' 'Processing Data Set Definitions')                           
  rc = repLine(' ' ' ')                                                         
  rc = repLine(' ' Copies('*',80))                                              
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine('I' 'Please check the generated list of source ' ||,             
                      'data sets as unused data set definitions '  ||,          
                      'may be generated from the FLMTYPE macros.')              
                                                                                
  Do i = 1 to projdefs.0                                                        
    Select                                                                      
                                                                                
      /* Need get a list of types 1 & 2 and store for later       */            
      When (Pos('FLMTYPE',projdefs.i) <> 0) Then                                
      Do                                                                        
        Parse var projdefs.i Type 'FLMTYPE' . 'EXTEND='Extend_Type .            
        Type = Strip(Type)                                                      
                                                                                
        /* We don't want to create types for ARCHDEF types        */            
        /* So use the archtype.txt file to skip those             */            
                                                                                
        archdef = 0                                                             
        Do at = 1 to archtype.0 While (archdef = 0)                             
          parse var archtype.at archmem':'typename':'archtype                   
          If Type = typename Then                                               
            archdef = 1                                                         
        End                                                                     
                                                                                
        If (archdef = 0) |,                                                     
           (archdef = 1 & firstArch = 1) Then                                   
        Do                                                                      
          /* Now lets go get the data set attributes                */          
          /* If data set does no exist then don't save this one     */          
                                                                                
          /* Need to create a data set definition for the BND cards */          
          /* we generated in the first migration script             */          
          If (archdef = 1 & firstArch = 1) Then                                 
          Do                                                                    
            dsname = migHlq"."proj"."topGroup".VCUR.BND"                        
            firstArch = 0                                                       
            Type = 'BND'                                                        
          End                                                                   
          Else                                                                  
          Do                                                                    
            If altDsname <> '' then                                             
            Do                                                                  
              y = Pos('@@FLMTYP',altDsname)                                     
              If y <> 0 Then                                                    
                dsname = Substr(altDsname,1,y-1)||,                             
                            Type||Substr(altDsname,y+8)                         
              Else                                                              
                dsname = altDsname'.'Type                                       
            End                                                                 
            Else                                                                
              dsname = project'.'topGroup'.'Type                                
          End                                                                   
          dsname = Translate(dsname,upper,lower)                                
                                                                                
          Address ISPEXEC "DSINFO DATASET('"Strip(dsname)"')"                   
          dsinfoRC = rc                                                         
          If dsinfoRC <> 0 Then                                                 
          Do                                                                    
            rc = repLine(' ' ' ')                                               
            rc = repLine('E' 'DSINFO Failed for 'Strip(dsname))                 
            rc = repLine('E' 'There was a FLMTYPE macro of type 'Type||,        
                             ' but the data set for this type does ' ||,        
                             'not exist.')                                      
            If exist = 'true' Then                                              
            Do                                                                  
              rc = repLine('W' 'Flag to not create data set definitions ' ||,   
                               'for non-exising data sets set to true. ' ||,    
                               'Data set definition not created for ' ||,       
                               Strip(dsname)'.')                                
              Iterate                                                           
            End                                                                 
            Else                                                                
            Do                                                                  
              rc = repLine('W' 'Defaulting data set information to ' ||,        
                               'FB 80 32720 for ' ||,                           
                               Strip(dsname)'.')                                
              ZDSORG  = 'PO'           /* dsorg     */                          
              ZDSRF   = 'FB'           /* recfm     */                          
              ZDSLREC = '80'           /* lrecl     */                          
              ZDSBLK  = '32720'        /* blksize   */                          
              ZDSDSNT = 'LIBRARY'      /* dsnType   */                          
              ZDSSPC  = 'CYLINDER'     /* priSpaceU */                          
              ZDS1EX  = '10'           /* priSpaceA */                          
              ZDS2EX  = '10'           /* secSpaceA */                          
              ZDSDIRA = '0'            /* dirBlocks */                          
              ZDSDC   = ''             /* dataClass */                          
              ZDSMC   = ''             /* mgmtClass */                          
              ZDSSC   = ''             /* stgClass  */                          
            End                                                                 
          End                                                                   
                                                                                
          /* If the Empty option is selected then a dsdef is not */             
          /* created if the data set is empty.                   */             
          If empty = 'true' & ZDS#MEM = 0 Then                                  
          Do                                                                    
            rc = repLine('W' 'Flag to not create data set definitions ' ||,     
                             'for data sets with no members is set to ' ||,     
                             'true. Data set definition not created for ' ||,   
                             Strip(dsname)'.')                                  
            Iterate                                                             
          End                                                                   
                                                                                
          /* Determine if Type 1 or Type 2 */                                   
          If Extend_Type <> '' then                                             
            stem = "t1"                                                         
          Else                                                                  
            stem = Type12(Type)                                                 
                                                                                
          stmt = stem" = "stem" + 1"                                            
          interpret stmt                                                        
                                                                                
          stmt = "Type"stem"."stem" = Strip(Type)"                              
          interpret stmt                                                        
                                                                                
          If Extend_Type <> '' then                                             
            stmt = "Type"stem"."stem".Extend = Extend_Type"                     
          Else                                                                  
            stmt = "Type"stem"."stem".Extend = ''"                              
          interpret stmt                                                        
                                                                                
          Call GetDsinfo(dsname)                                                
        End                                                                     
      End                                                                       
                                                                                
      /* Need get a list of type 3 from FLMTRNSL for compilers    */            
      When (Pos('FLMTRNSL',projdefs.i) <> 0) Then                               
      Do                                                                        
        If Pos('FUNCTN=BUILD',projdefs.i) <> 0 Then                             
        Do                                                                      
          If Pos('CALLMETH=TSOLNK',projdefs.i) <> 0 |,                          
             Pos('CALLMETH=ISPLNK',projdefs.i) <> 0 Then                        
            Iterate                                                             
                                                                                
          parse var projdefs.i . 'DSNAME='dsname ',' .                          
          parse var projdefs.i . 'COMPILE='compiler ',' .                       
                                                                                
          /* Get rid of extra rubbish */                                        
          Parse var dsname dsname .                                             
          Parse var compiler compiler .                                         
                                                                                
          If compiler = 'FLMCSPDB' Then                                         
          Do                                                                    
            rc = repLine(' ' ' ')                                               
            rc = repLine('E' 'Migration does not process the ' ||,              
                               'SCLM Bind processor - FLMCSPDB.')               
            rc = repLine('E'    'You will need to rework your DB2 Bind process')
            Iterate                                                             
          End                                                                   
          Else                                                                  
            If Substr(compiler,1,3) = 'FLM' Then                                
            Do                                                                  
              rc = repLine(' ' ' ')                                             
              rc = repLine('W' 'SCLM Specific program 'compiler ||,             
                             ' being used. Check translator usage')             
            End                                                                 
                                                                                
          If dsname <> '' Then                                                  
          Do                                                                    
            parse var dsname checkdsn '(' .                                     
            Address ISPEXEC "DSINFO DATASET('"Strip(checkdsn)"')"               
            dsinfoRC = rc                                                       
            If dsinfoRC <> 0 Then                                               
            Do                                                                  
              rc = repLine(' ' ' ')                                             
              rc = repLine('E' 'DSINFO Failed for 'Strip(checkdsn))             
              rc = repLine('E'    'You will need to modify existing data '||,   
                                  'sets in the generated XML, or through ' ||,  
                                  'prior to rerunning the script.')             
            End                                                                 
          End                                                                   
                                                                                
          /* Need to see if we have this one already */                         
          dsname = Strip(dsname)"("Strip(compiler)")"                           
          Do z = 1 to t3 While (dsname <> Typet3.z)                             
          End                                                                   
          If z > t3 Then                                                        
          Do                                                                    
            t3 = t3 + 1                                                         
            Typet3.t3 = Strip(dsname)                                           
            Typet3.t3.Volume = ''                                               
          End                                                                   
        End                                                                     
                                                                                
      End                                                                       
                                                                                
      /* Need get a list of type 3 from FLMSYSLB, store for later */            
      When (Pos('FLMSYSLB',projdefs.i) <> 0) Then                               
      Do                                                                        
        Do While (Pos('FLMSYSLB',projdefs.i) <> 0)                              
          Parse var projdefs.i lang 'FLMSYSLB' dsname ',' Rest                  
          parse var rest . 'INCLS='Include_Set ',' .                            
          parse var rest . 'VOL='Volser ',' .                                   
                                                                                
          /* Get rid of extra rubbish */                                        
          Parse var dsname dsname .                                             
          Parse var Include_Set Include_Set .                                   
          Parse var Volser Volser .                                             
                                                                                
          parse var dsname checkdsn '(' .                                       
          If checkdsn <> 'NULLFILE' Then                                        
          Do                                                                    
            Address ISPEXEC "DSINFO DATASET('"Strip(checkdsn)"')"               
            dsinfoRC = rc                                                       
            If dsinfoRC <> 0 Then                                               
            Do                                                                  
              rc = repLine(' ' ' ')                                             
              rc = repLine('E' 'DSINFO Failed for 'Strip(checkdsn))             
              rc = repLine('E'    'You will need to modify type 3 data sets '||,
                           'in the generated XML, or through prior to ' ||,     
                           'rerunning the script.')                             
            End                                                                 
          End                                                                   
          /* Need to see if we have this one already */                         
          dsname = Strip(dsname)                                                
          Do z = 1 to t3 While (dsname <> Typet3.z)                             
          End                                                                   
          If z > t3 Then                                                        
          Do                                                                    
            t3 = t3 + 1                                                         
            Typet3.t3 = Strip(dsname)                                           
            If Volser <> '' then                                                
              Typet3.t3.Volume = Strip(Volser)                                  
            Else                                                                
              Typet3.t3.Volume = ''                                             
          End                                                                   
                                                                                
          If lang <> '' Then Store_lang = lang                                  
          fs = fs + 1                                                           
          Flmsyslb.fs = Strip(Store_lang)                                       
          Flmsyslb.fs.dsn  = Strip(dsname)                                      
          If Include_Set <> '' then                                             
            Flmsyslb.fs.InclSet = Strip(Include_Set)                            
          Else                                                                  
             Flmsyslb.fs.inclset = 'RTCDeflt'                                   
          i = i + 1                                                             
        End                                                                     
        i = i - 1                                                               
      End                                                                       
                                                                                
      /* Need get a list of type 3 from FLMCPYLB, store for later */            
      /* Just for existing. Some FLMCPYLD reference project data  */            
      /* sets through the use of @@FLMDSN and other @@ variables  */            
      When (Pos('FLMCPYLB',projdefs.i) <> 0) Then                               
      Do                                                                        
        Parse var projdefs.i . 'FLMCPYLB' dsname ',' Rest                       
                                                                                
        dsname = Strip(dsname)                                                  
                                                                                
        /* Don't create dsdefs for Unix files */                                
        If Substr(dsname,1,1) = '/' Then                                        
          Iterate                                                               
                                                                                
        /* Get rid of extra rubbish */                                          
        parse var rest 'VOL='Volser .                                           
        Parse var dsname dsname .                                               
                                                                                
        dsname = proc@@('dsdef' dsname)                                         
                                                                                
        /* Don't create dsdefs for @@FLMDSN files */                            
        If Pos('@{source.dataset}',dsname) <> 0 Then                            
          Iterate                                                               
                                                                                
        If dsname <> 'NULLFILE' Then                                            
        Do                                                                      
          parse var dsname checkdsn '(' .                                       
          Address ISPEXEC "DSINFO DATASET('"Strip(checkdsn)"')"                 
          dsinfoRC = rc                                                         
          If dsinfoRC <> 0 Then                                                 
          Do                                                                    
            rc = repLine(' ' ' ')                                               
            rc = repLine('E' 'DSINFO Failed for 'Strip(checkdsn))               
            rc = repLine('E'    'You will need to modify type 3 data sets '||,  
                                'in the generated XML, or through the ' ||,     
                                'dialogs.')                                     
          End                                                                   
        End                                                                     
                                                                                
        /* Need to see if we have this one already */                           
        dsname = Strip(dsname)                                                  
        Do z = 1 to t3 While (dsname <> Typet3.z)                               
        End                                                                     
        If z > t3 Then                                                          
        Do                                                                      
          t3 = t3 + 1                                                           
          Typet3.t3 = Strip(dsname)                                             
          Typet3.t3.inclSet = ''                                                
          If Volser <> '' then                                                  
            Typet3.t3.Volume = Volser                                           
          Else                                                                  
            Typet3.t3.Volume = ''                                               
        End                                                                     
      End                                                                       
                                                                                
      /* Need get a list of type 4 from FLMALLOC, store for later */            
      When (Pos('FLMALLOC',projdefs.i) <> 0) Then                               
      Do                                                                        
        stem = 't4'                                                             
        Parse var projdefs.i . 'IOTYPE='IOType ',' .                            
        Parse var projdefs.i . 'DFLTTYP='dflttyp ',' .                          
        Parse var projdefs.i . 'KEYREF='keyref ',' .                            
        /* Temporaries are W but if an IOTYPE is O or P and there is */         
        /* no keyref or dflttyp then we treat it like IOTYPE of W    */         
        If IOType = 'W' |,                                                      
           ((IOType = 'O' | IOType = 'P') &,                                    
            (dflttyp = '' & keyref = '')) Then                                  
        Do                                                                      
          parse var projdefs.i . 'RECNUM='recnum ',' .                          
          parse var projdefs.i . 'RECFM='ZDSRF ',' .                            
          parse var projdefs.i . 'LRECL='ZDSLREC ',' .                          
          parse var projdefs.i . 'BLKSIZE='ZDSBLK ',' .                         
          parse var projdefs.i . 'DISP='ZDISP ',' .                             
          parse var projdefs.i . 'DDNAME='ddn ',' .                             
                                                                                
          If ZDSBLK = 0 Then ZDSBLK = ''                                        
                                                                                
          /* Get rid of extra rubbish */                                        
          Parse var recnum recnum .                                             
          Parse var ZDSRF ZDSRF .                                               
          Parse var ZDSLREC ZDSLREC .                                           
          Parse var ZDSBLK ZDSBLK .                                             
          Parse var ZDISP ZDISP .                                               
          Parse var ddn ddn .                                                   
                                                                                
          /* need to check forward in this translator if there is */            
          /* a DD and it is part of an IOTYPE=U in a later        */            
          /* translator.                                          */            
          dsname = ''                                                           
          Do lu = i+1 to projdefs.0,                                            
                    While (Pos('FLMLANGL',projdefs.lu) = 0)                     
            If Pos('IOTYPE=U',projdefs.lu) <> 0 &,                              
               Pos(ddn,projdefs.lu) <> 0 Then                                   
            Do                                                                  
              /* found the dd in this language */                               
              tempDD = '&&'ddn                                                  
              dsname = 'TemporaryFile-'tempDD                                   
            End                                                                 
          End                                                                   
                                                                                
          If dsname = '' Then                                                   
            If ZDSLREC = '' Then                                                
              dsname = 'TemporaryFile'                                          
            Else                                                                
              dsname = 'TemporaryFile-'ZDSRF||ZDSLREC                           
                                                                                
          Call Type4                                                            
        End                                                                     
      End                                                                       
                                                                                
      Otherwise                                                                 
        Nop                                                                     
    End                                                                         
  End                                                                           
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine(' ' Copies('*',80))                                              
  rc = repLine(' ' ' ')                                                         
  rc = repLine(' ' 'Finished Processing data set definitions')                  
  rc = repLine(' ' ' ')                                                         
  rc = repLine(' ' Copies('*',80))                                              
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Get a data set information                                    */             
/*---------------------------------------------------------------*/             
                                                                                
GetDsinfo :                                                                     
                                                                                
  Call interp_stmt("dsorg" ZDSORG)                                              
  Call interp_stmt("recfm" ZDSRF)                                               
  Call interp_stmt("lrecl" ZDSLREC)                                             
  Call interp_stmt("blksize" ZDSBLK)                                            
  Call interp_stmt("dsnType" ZDSDSNT)                                           
  Select                                                                        
    When (ZDSSPC = 'BLOCK') Then Call interp_stmt("priSpaceU" "blks")           
    When (ZDSSPC = 'TRACK') Then Call interp_stmt("priSpaceU" "trks")           
    When (ZDSSPC = 'CYLINDER') Then Call interp_stmt("priSpaceU" "cyls")        
    Otherwise                                                                   
    Do                                                                          
      rc = repLine(' ' ' ')                                                     
      rc = repLine('E' 'Unhandled Space unit of 'ZDSSPC' for ' ||,              
                     'data set : 'dsname)                                       
      Call exitRtn(8)                                                           
    End                                                                         
  End                                                                           
  Call interp_stmt("priSpaceA" ZDS1EX)                                          
  Call interp_stmt("secSpaceA" ZDS2EX)                                          
  Call interp_stmt("dirBlocks" ZDSDIRA)                                         
  Call interp_stmt("dataClass" ZDSDC)                                           
  Call interp_stmt("mgmtClass" ZDSMC)                                           
  Call interp_stmt("stgClass" ZDSSC)                                            
                                                                                
  /* If load library (RECFM=U) and PDS then put out warning   */                
  If ZDSRF = 'U' & ZDSDSNT <> 'LIBRARY' Then                                    
  Do                                                                            
    rc = repLine(' ' ' ')                                                       
    rc = repLine('W' 'Load library 'dsname' is a PDS. The linkedit parser ' ||, 
                     'will only capture static dependencies with PDSE')         
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Rexx interpret to construct a statement and execute it        */             
/*---------------------------------------------------------------*/             
                                                                                
interp_stmt :                                                                   
                                                                                
  Parse arg var zvar                                                            
                                                                                
  If zvar <> '' then                                                            
    stmt = "Type"stem"."stem"."var" = '"zvar"'"                                 
  Else                                                                          
    stmt = "Type"stem"."stem"."var" = ''"                                       
                                                                                
  Interpret stmt                                                                
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Determine if a Type 1 or Type 2 data set                      */             
/*---------------------------------------------------------------*/             
Type12 :                                                                        
                                                                                
  Parse arg Type                                                                
                                                                                
  stem = 't1'                                                                   
                                                                                
  /* First lets check if this is used in an EXTEND. If it is  */                
  /* then it will be a type 1. It is possible that a type can */                
  /* contain ARCHDEFs or a type 1 might be used as an output  */                
  /* data set in a FLMALLOC.                                  */                
                                                                                
  Do i12 = 1 to projdefs.0                                                      
    Select                                                                      
      When (Pos('FLMTYPE',projdefs.i12) <> 0) Then                              
      Do                                                                        
        Parse var projdefs.i12 testType 'FLMTYPE' . 'EXTEND='testExtend .       
        testType = Strip(testType)                                              
        testExtend = Strip(testExtend)                                          
        If testExtend = Type Then                                               
          Return stem                                                           
      End                                                                       
      Otherwise                                                                 
        Nop                                                                     
    End                                                                         
  End                                                                           
                                                                                
  /* Check if it is used as an output type. Then it is most   */                
  /* likely a type 2.                                         */                
                                                                                
  Do i12 = 1 to projdefs.0                                                      
    Select                                                                      
      When (Pos('FLMALLOC',projdefs.i12) <> 0) Then                             
      Do                                                                        
        Parse var projdefs.i12 . 'DFLTTYP='dflttyp ',' .                        
        Parse var projdefs.i12 . 'KEYREF='keyref ',' .                          
        Parse var projdefs.i12 . 'IOTYPE='IOType ',' .                          
                                                                                
        /* Get rid of extra rubbish */                                          
        Parse var dflttyp dflttyp .                                             
        Parse var keyref  keyref .                                              
        Parse var IOType  IOType .                                              
                                                                                
        If IOType = 'O' | IOType = 'P' Then                                     
        Do                                                                      
          If (dflttyp = Type) |,                                                
             (dflttyp = '' & keyref = Type) Then                                
              stem = 't2'                                                       
        End                                                                     
      End                                                                       
      Otherwise                                                                 
        Nop                                                                     
    End                                                                         
  End                                                                           
                                                                                
  /* Lets go see if the type is in the keyref file. A type 2  */                
  /* could just be mentioned in an ARCHDEF without actually   */                
  /* being in an IOTYPE O/P in a Language Definition.         */                
                                                                                
  typeNotFound = 1                                                              
  Do kr = 1 to keyrefs.0 While (typeNotFound)                                   
    If Pos('<values type=',keyrefs.kr) <> 0 Then                                
    Do                                                                          
      Parse var keyrefs.kr '<values type="'keyType'"/>'                         
      If type = keyType Then                                                    
      Do                                                                        
        typeNotFound = 1                                                        
        stem = 't2'                                                             
      End                                                                       
    End                                                                         
  End                                                                           
                                                                                
Return stem                                                                     
                                                                                
/*---------------------------------------------------------------*/             
/* Determine if dsdef for a type 4 has already been created      */             
/*---------------------------------------------------------------*/             
Type4 :                                                                         
                                                                                
  /* Need to see if we have this one already */                                 
  Do z = 1 to t4 While (dsname <> Typet4.z)                                     
  End                                                                           
  If z > t4 Then                                                                
  Do                                                                            
    t4 = t4 + 1                                                                 
    Typet4.t4 = Strip(dsname)                                                   
    Typet4.t4.vio = vioUnit                                                     
    If ZDSLREC = '' | ZDSLREC = 0 Then                                          
      recLen = 80                                                               
    Else                                                                        
      recLen = ZDSLREC                                                          
    If recnum = '' Then                                                         
      recnum = 500                                                              
    blocks = (((recnum / ((3120 / reclen) + 1)) + 1) / 16) + 1                  
    blocks = Trunc(blocks + 1)                                                  
    tracks = Trunc(blocks/15 + 1)                                               
                                                                                
    If ZDSBLK = '' & ZDSLREC = '' Then                                          
      ZDSBLK = ''                                                               
    If ZDSBLK <> '' & ZDSLREC <> '' &,                                          
      Pos('V',ZDSRF) = 0 Then                                                   
    Do                                                                          
      /* Validate blockzize is a multiple of record length */                   
      If Trunc(ZDSBLK/ZDSLREC) <> ZDSBLK/ZDSLREC Then                           
      Do                                                                        
        rc = repLine(' ' ' ')                                                   
        rc = repLine('W'    'Temporary data set 'dsname' has '      ||,         
                            'BLKSIZE 'ZDSBLK' and LRECL 'ZDSLREC' ' ||,         
                            'LRECL is not a multiple of BLKSIZE. '  ||,         
                            'Recalculating blocksize')                          
        ZDSBLK = ''                                                             
      End                                                                       
    End                                                                         
    If ZDSBLK = '' & ZDSLREC <> '' Then                                         
    Do                                                                          
      ZDSBLK = recLen                                                           
      If Pos('B',ZDSRF) <> 0 Then                                               
      Do                                                                        
        /* Make it as big as possible */                                        
        Do While (ZDSBLK < 32760)                                               
          ZDSBLK = ZDSBLK + reclen                                              
        End                                                                     
        ZDSBLK = ZDSBLK - reclen                                                
      End                                                                       
      Else                                                                      
      Do                                                                        
        If Pos('V',ZDSRF) <> 0 Then                                             
          ZDSBLK = ZDSBLK + 4                                                   
      End                                                                       
    End                                                                         
    If ZDSRF = 'U' & ZDSLREC <> '0' Then                                        
    Do                                                                          
      rc = repLine(' ' ' ')                                                     
      rc = repLine('W'    'Temporary data set 'dsname' has '        ||,         
                          'RECFM 'ZDSRF' and LRECL 'ZDSLREC'.')                 
      rc = repLine('W'    'Due to a defect in RTC (472084) it is '  ||,         
                          'not possible to create this data set '   ||,         
                          'definition.')                                        
      rc = repLine('W'    'It will be created with LRECL or 0 and ' ||,         
                          'blocksize of 6144 and will need to be '  ||,         
                          'changed in the user interface.')                     
      ZDSLREC = '0'                                                             
      ZDSBLK = '6144'                                                           
    End                                                                         
    If ZDSRF = 'U' & ZDSBLK = '' Then                                           
    Do                                                                          
      If ZDSLREC = '0' Then                                                     
        ZDSBLK = '6144'                                                         
      Else                                                                      
        ZDSBLK = ZDSLREC                                                        
    End                                                                         
                                                                                
    Call interp_stmt("disp" ZDISP)                                              
    Call interp_stmt("recfm" ZDSRF)                                             
    Call interp_stmt("lrecl" ZDSLREC)                                           
    Call interp_stmt("blksize" ZDSBLK)                                          
    Call interp_stmt("priSpaceU" "trks")                                        
    Call interp_stmt("priSpaceA" tracks)                                        
    Call interp_stmt("secSpaceA" tracks)                                        
    Call interp_stmt("secSpaceA" tracks)                                        
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Get a list of language definitions                            */             
/*---------------------------------------------------------------*/             
Langdefs:                                                                       
                                                                                
  ld = 0                                                                        
  langs. = ''                                                                   
                                                                                
  tr = 0                                                                        
  trans. = ''                                                                   
                                                                                
  ns = 0                                                                        
  noSyslib. = ''                                                                
                                                                                
  lecCnt  = 0                                                                   
  lecSys. = ''                                                                  
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine(' ' Copies('*',80))                                              
  rc = repLine(' ' ' ')                                                         
  rc = repLine(' ' 'Processing Language Definitions')                           
  rc = repLine(' ' ' ')                                                         
  rc = repLine(' ' Copies('*',80))                                              
                                                                                
  Do langCnt = 1 to projdefs.0                                                  
    Select                                                                      
      When (Pos('FLMLANGL',projdefs.langcnt) <> 0) Then                         
      Do                                                                        
        /* Not an ARCHDEF Language definition */                                
        If Pos('ARCH=Y',projdefs.langcnt) = 0 Then                              
        Do                                                                      
          Parse var projdefs.langcnt . 'LANG='language ',' .                    
          Parse var projdefs.langcnt . 'LANGDESC='LanguageDesc ',' .            
          Parse var projdefs.langcnt . 'ALCSYSLB='AllocSyslib ',' .             
          Parse var projdefs.langcnt . 'ENCODE='Encoding ',' .                  
          Parse var projdefs.langcnt . 'CANEDIT='CanEdit ',' .                  
                                                                                
          /* get rid of extra rubbish */                                        
          Parse var language language .                                         
          Parse var LanguageDesc LanguageDesc .                                 
          Parse var AllocSyslib AllocSyslib .                                   
          Parse var Encoding Encoding .                                         
          Parse var CanEdit CanEdit .                                           
                                                                                
          /* going to create them all for now */                                
          language = Strip(language)                                            
                                                                                
          /* Lets see if the LANG exist in the languages file */                
          Do ds = 1 to langdefs.0                                               
            If language = langdefs.ds Then leave                                
          End                                                                   
          If ds > langdefs.0 & CanEdit <> 'N' Then                              
          Do                                                                    
            /* Not in the file so we don't migrate this one */                  
            /* and not an LE370 language                    */                  
            langExist = 0                                                       
            rc = repLine('-' ' ')                                               
            rc = repLine('-' 'Language 'language' is not in the ' ||,           
                               'language extention file - 'langfile'. ' ||,     
                                'Language not migrated')                        
            Iterate                                                             
          End                                                                   
          Else                                                                  
            langExist = 1                                                       
                                                                                
          rc = repLine(' ' ' ')                                                 
          rc = repLine('-' Copies('-',80))                                      
          rc = repLine('-' ' ')                                                 
          rc = repLine('-' 'Processing SCLM Language 'language)                 
          rc = repLine('-' ' ')                                                 
          rc = repLine('-' Copies('-',80))                                      
                                                                                
          ld = ld + 1                                                           
          Langs.ld = Strip(language,,"'")                                       
                                                                                
          If lanuageDesc <> '' then                                             
            langs.ld.langDesc = languageDesc                                    
          Else                                                                  
            langs.ld.langDesc = ''                                              
          If AllocSyslib <> '' then                                             
            langs.ld.alcsyslib = AllocSyslib                                    
          Else                                                                  
            langs.ld.alcsyslib = 'N'                                            
          If Encoding <> '' then                                                
            langs.ld.encode = Encoding                                          
          Else                                                                  
            langs.ld.encode = 'N'                                               
                                                                                
          langs.ld.languageCode = 'OTH'                                         
                                                                                
          langcnt = langcnt + 1                                                 
                                                                                
          /* Work out the include sets for this language         */             
          Call inclSet(language)                                                
                                                                                
          /* Use the parser translator to determine the language */             
          Call parsers                                                          
                                                                                
          /* Position the keyref file at the right language      */             
          langFound = 0                                                         
          Do kr = 1 to keyrefs.0 While (langFound = 0)                          
            If Pos('<language name=',keyrefs.kr) <> 0 Then                      
            Do                                                                  
              Parse var keyrefs.kr '<language name="'keyLang'">'                
              If lang = keyLang Then                                            
              Do                                                                
                firstkr = kr + 1                                                
                langFound = 1                                                   
              End                                                               
            End                                                                 
          End                                                                   
                                                                                
          langs.ld.translators = ''                                             
          flmcspdb = 0                                                          
                                                                                
          Call LangTran(language)                                               
                                                                                
          If flmcspdb Then                                                      
          Do                                                                    
            rc = repLine(' ' ' ')                                               
            rc = repLine('E' 'SCLM DB2 bind language definition ' ||,           
                                language 'not created')                         
            ld = ld - 1                                                         
            /* Need to change the language in the langext file to */            
            /* FLMCSPDB for later processing by zImport           */            
                                                                                
            langdefs.ds.cspdb = 'FLMCSPDB'                                      
                                                                                
          End                                                                   
          Else                                                                  
          Do                                                                    
            langs.ld.translators = Strip(langs.ld.translators,T,',')            
                                                                                
            rc = repLine('I' 'Number of translators in language 'language    ||,
                  ' : 'Words(Translate(langs.ld.translators,' ',',')))          
          End                                                                   
                                                                                
          rc = repLine(' ' ' ')                                                 
          rc = repLine('-' Copies('-',80))                                      
          rc = repLine('-' ' ')                                                 
          rc = repLine('-' 'Finished Processing SCLM Language 'language)        
          rc = repLine('-' ' ')                                                 
          rc = repLine('-' Copies('-',80))                                      
        End                                                                     
      End                                                                       
      Otherwise                                                                 
        NOP                                                                     
    End                                                                         
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Work out the include sets                                     */             
/*---------------------------------------------------------------*/             
inclSet :                                                                       
                                                                                
  Parse arg Lang                                                                
                                                                                
  fi = 0                                                                        
  sa = 0                                                                        
  flmincls. = ''                                                                
  sameas.   = ''                                                                
  inclsetFound = 0                                                              
                                                                                
  Do inclcnt = langcnt to projdefs.0,                                           
                While (Pos('FLMLANGL',projdefs.inclcnt) = 0)                    
    Select                                                                      
      When (Pos('FLMINCLS',projdefs.inclcnt) <> 0) Then                         
      Do                                                                        
        inclsetFound = 1                                                        
        Do While (Pos('FLMINCLS',projdefs.inclcnt) <> 0)                        
          Parse var projdefs.inclcnt include_Set 'FLMINCLS' Rest                
          If Pos('SAMEAS=',Rest) <> 0 Then                                      
          Do                                                                    
            Parse var Rest 'SAMEAS=' SA_inclset ','                             
                                                                                
            sa = sa + 1                                                         
            sameas.sa = lang                                                    
            If SA_inclset <> '' then                                            
              sameas.sa.saincls = Strip(SA_inclset)                             
            Else                                                                
              sameas.sa.saincls = 'RTCDeflt'                                    
            If Include_Set <> '' then                                           
              sameas.sa.InclSet = Strip(Include_Set)                            
            Else                                                                
              sameas.sa.inclset = 'RTCDeflt'                                    
          End                                                                   
          Else                                                                  
          Do                                                                    
            Parse var Rest 'TYPES=(' inclTypes ')' ','                          
                                                                                
            fi = fi + 1                                                         
            Flmincls.fi = lang                                                  
            inclTypes = Translate(inclTypes,' ',',')                            
            /* Need to see if types are set to @@FLMTYP or @@FLMETP) */         
            newTypes = ''                                                       
            Do ty = 1 to Words(inclTypes)                                       
              Select                                                            
                When (Word(inclTypes,ty) = '@@FLMTYP') |,                       
                     (Word(inclTypes,ty) = '@@FLMCRF') Then                     
                Do                                                              
                  /* Read default source to get default source for language */  
                  Do dt = 1 to langdefs.0                                       
                    If lang = langdefs.dt then leave                            
                  End                                                           
                                                                                
                  /* Then read types table to see if there is an extend     */  
                  Do tp = 1 to types.0 While (langdefs.dt.dfltsrc <> types.tp)  
                  End                                                           
                  If tp > types.0 Then                                          
                    Nop                                                         
                  Else                                                          
                    newTypes = newTypes||' '||Strip(types.tp)                   
                  rc = repLine(' ' ' ')                                         
                  rc = repLine('W' 'FLMINCLS specifies @@FLMTYP, ' ||,          
                                '@@FLMETP, @@FLMCRF or @@FLMSRF. ')             
                  rc = repLine('W' 'It is not possible to migrate this. '    ||,
                                'Default source for type used.')                
                End                                                             
                When (Word(inclTypes,ty) = '@@FLMETP') Then                     
                Do                                                              
                  /* Read default source to get default source for language */  
                  Do dt = 1 to langdefs.0                                       
                    If lang = langdefs.dt then leave                            
                  End                                                           
                                                                                
                  /* Then read types table to see if there is an extend     */  
                  Do tp = 1 to types.0 While (langdefs.dt.dfltsrc <> types.tp)  
                  End                                                           
                  If tp > types.0 Then                                          
                    Nop                                                         
                  Else                                                          
                    newTypes = newTypes||' '||Strip(types.tp.Extend)            
                End                                                             
                Otherwise                                                       
                  newTypes = newTypes||' '||Word(inclTypes,ty)                  
              End                                                               
            End                                                                 
            If newTypes <> '' Then                                              
              inclTypes = Strip(newTypes)                                       
                                                                                
            Flmincls.fi.typeset = Strip(inclTypes)                              
            If Include_Set <> '' then                                           
              Flmincls.fi.InclSet = Strip(Include_Set)                          
            Else                                                                
              Flmincls.fi.inclset = 'RTCDeflt'                                  
          End                                                                   
          inclcnt = inclcnt + 1                                                 
        End                                                                     
        inclcnt = inclcnt - 1                                                   
      End                                                                       
      Otherwise                                                                 
        Nop                                                                     
    End                                                                         
  End                                                                           
                                                                                
  If inclsetFound = 0 Then                                                      
  Do                                                                            
    rc = repLine(' ' ' ')                                                       
    rc = repLine('I' 'No include set for 'lang' need to generate default')      
                                                                                
    /* Read default source to get default source for language */                
    Do i = 1 to langdefs.0                                                      
      If lang = langdefs.i then leave                                           
    End                                                                         
                                                                                
    /* Then read types table to see if there is an extend     */                
    Do tp = 1 to types.0 While (langdefs.i.dfltsrc <> types.tp)                 
    End                                                                         
    If tp > types.0 Then                                                        
    Do                                                                          
      rc = repLine(' ' ' ')                                                     
      rc = repLine('W' 'No default include set for 'lang' generated ' ||,       
                         'as default source is blank')                          
    End                                                                         
    Else                                                                        
    Do                                                                          
      fi = fi + 1                                                               
      Flmincls.fi = lang                                                        
      inclTypes = Strip(types.tp) Strip(types.tp.Extend)                        
      Flmincls.fi.typeset = Strip(inclTypes)                                    
      Flmincls.fi.inclset = 'RTCDeflt'                                          
    End                                                                         
  End                                                                           
                                                                                
  /* Need to post process the SAMEAS if it exists */                            
  Do i = 1 to sa                                                                
    Do j = 1 to fi While (sameas.i.saincls <> Flmincls.j.InclSet)               
    End                                                                         
    fi = fi + 1                                                                 
    Flmincls.fi = sameas.i                                                      
    Flmincls.fi.InclSet = sameas.i.InclSet                                      
    Flmincls.fi.typeset = Flmincls.j.typeset                                    
  End                                                                           
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine('I' '-- FLMINCLSs for lang - 'lang' --')                         
  Do i = 1 to fi                                                                
    rc = repLine('I' Flmincls.i Flmincls.i.InclSet Flmincls.i.typeset)          
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
              langs.ld.languageCode = 'COB'                                     
            End                                                                 
            When (Parser = 'FLMLPFRT') Then                                     
            Do                                                                  
              langs.ld.languageCode = 'OTH'                                     
            End                                                                 
            When (Parser = 'FLMLPGEN') Then                                     
            Do                                                                  
              Select                                                            
                When (ParsLang = 'A') Then                                      
                  langs.ld.languageCode = 'ASM'                                 
                When (ParsLang = 'T') Then                                      
                  langs.ld.languageCode = 'OTH'                                 
                When (ParsLang = 'R') Then                                      
                  langs.ld.languageCode = 'REX'                                 
                When (ParsLang = 'C') Then                                      
                  langs.ld.languageCode = 'OTH'                                 
                When (ParsLang = 'I') Then                                      
                  langs.ld.languageCode = 'PLI'                                 
                When (ParsLang = '1') Then                                      
                  langs.ld.languageCode = 'PLI'                                 
                When (ParsLang = 'L') Then                                      
                  langs.ld.languageCode = 'PAS'                                 
                Otherwise                                                       
                Do                                                              
                  rc = repLine(' ' ' ')                                         
                  rc = repLine('W' 'Unknown parser language - 'ParsLang)        
                End                                                             
              End                                                               
            End                                                                 
            When (Parser = 'FLMLRASM') Then                                     
            Do                                                                  
              langs.ld.languageCode = 'ASM'                                     
            End                                                                 
            When (Parser = 'FLMLRCIS' |,                                        
                  Parser = 'FLMLRC37') Then                                     
            Do                                                                  
              langs.ld.languageCode = 'C'                                       
            End                                                                 
            When (Parser = 'FLMLRDTL') Then                                     
            Do                                                                  
              langs.ld.languageCode = 'DTL'                                     
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
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine('-' 'Processing Translators in language 'Lang)                   
                                                                                
  firstTran = 0                                                                 
                                                                                
  Do trancnt = langcnt to projdefs.0,                                           
               While (Pos('FLMLANGL',projdefs.trancnt) = 0)                     
    Select                                                                      
      When (Pos('FLMTRNSL',projdefs.trancnt) <> 0) Then                         
      Do                                                                        
        If Pos('FUNCTN=BUILD',projdefs.trancnt) <> 0 Then                       
        Do                                                                      
                                                                                
          /* Get translator options                  */                         
                                                                                
          Call TranOpts                                                         
                                                                                
          translator  = ''                                                      
          Compiler    = ''                                                      
          dsname      = ''                                                      
          GoodRc      = ''                                                      
          Pord        = ''                                                      
          CompOptions = ''                                                      
          parmkwd     = ''                                                      
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
              When trnsStmt.st = 'PARMKWD' Then                                 
                parmkwd     = trnsStmt.st.stmtParm                              
              When trnsStmt.st = 'TASKLIB' Then                                 
                Tasklib     = trnsStmt.st.stmtParm                              
              Otherwise                                                         
                Nop                                                             
            End                                                                 
          End                                                                   
                                                                                
          Select                                                                
            /* Options specified in quote and parenthisis or   */               
            /* Options specified in parenthisis and quote      */               
            When Substr(CompOptions,1,2) = "('" |,                              
                 Substr(CompOptions,1,2) = "'(" Then                            
            Do                                                                  
              CompOptions = Substr(CompOptions,3)                               
              RevOptions  = Substr(Reverse(CompOptions),3)                      
              CompOptions = Reverse(RevOptions)                                 
            End                                                                 
            /* Options specified in quote or                   */               
            /* Options specified in parenthisis                */               
            When Substr(CompOptions,1,1) = "(" |,                               
                 Substr(CompOptions,1,1) = "'" Then                             
            Do                                                                  
              CompOptions = Substr(CompOptions,2)                               
              RevOptions  = Substr(Reverse(CompOptions),2)                      
              CompOptions = Reverse(RevOptions)                                 
            End                                                                 
            /* Options specified without parenthisis and quote */               
            Otherwise                                                           
              Nop                                                               
          End                                                                   
                                                                                
          CompOptions = proc@@('comp' CompOptions)                              
                                                                                
          /* SCLM uses double quotes. Need to translate to single */            
          tempOptions = ''                                                      
                                                                                
          Do while (Pos("''",CompOptions) <> 0)                                 
            xx = Pos("''",CompOptions)                                          
            tempOptions = tempOptions || Substr(CompOptions,1,xx)               
            CompOptions = Substr(CompOptions,xx+2)                              
          End                                                                   
          compOptions = tempOptions || compOptions                              
                                                                                
          /* Need to make sure right language set for obvious langs */          
          If langs.ld.languageCode = 'OTH' Then                                 
          Do                                                                    
            Select                                                              
              When (Compiler = 'ASMA90')   Then langs.ld.languageCode = 'ASM'   
              When (Compiler = 'IEV90')    Then langs.ld.languageCode = 'ASM'   
              When (Compiler = 'IGYCRCTL') Then langs.ld.languageCode = 'COB'   
              When (Compiler = 'IKFCBL00') Then langs.ld.languageCode = 'COB'   
              When (Compiler = 'IEL0AA')   Then langs.ld.languageCode = 'PLI'   
              When (Compiler = 'IBMZPLI')  Then langs.ld.languageCode = 'PLI'   
              When (Compiler = 'AKEEPLX')  Then langs.ld.languageCode = 'PLX'   
              When (Compiler = 'AKHPLX')   Then langs.ld.languageCode = 'PLX'   
              When (Compiler = 'AKHPLX2')  Then langs.ld.languageCode = 'PLX'   
              When (Compiler = 'AKHTSO')   Then langs.ld.languageCode = 'PLX'   
              When (Compiler = 'PLX390')   Then langs.ld.languageCode = 'PLX'   
              When (Compiler = 'CCNDRVR')  Then langs.ld.languageCode = 'C'     
              When (Compiler = 'CCNED230') Then langs.ld.languageCode = 'C'     
              When (Compiler = 'EDCCOMP')  Then langs.ld.languageCode = 'C'     
              When (Compiler = 'IEWL')     Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'HEWL')     Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'HEWLDRGO') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'HEWLH096') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'HEWLOAD')  Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'HEWLOADR') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'IEWBLDGO') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'IEWBLIMG') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'IEWBLOAD') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'IEWBLODI') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'IEWBODEF') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'IEWLDRGO') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'IEWLOAD')  Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'IEWLOADI') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'IEWLOADR') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'LINKEDIT') Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'LOADER')   Then langs.ld.languageCode = 'LNK'   
              When (Compiler = 'JOVIAL')   Then langs.ld.languageCode = 'JOV'   
              When (Compiler = 'PASCALI')  Then langs.ld.languageCode = 'PAS'   
              Otherwise                                                         
                Nop                                                             
            End                                                                 
          End                                                                   
                                                                                
          Select                                                                
            When (Pos('CALLMETH=TSOLNK',projdefs.trancnt) <> 0) Then            
            Do                                                                  
              rc = repLine(' ' ' ')                                             
              rc = repLine('W' 'TSOLNK Translator - ' ||,                       
                                 'Please verify validity of TSO invocation.')   
              rc = repLine('W' 'See - 'projdefs.trancnt)                        
              Select                                                            
                When (dsname <> '' & compiler <> '') Then                       
                Do                                                              
                  command = "EXEC '"dsname"("compiler")' '"CompOptions"'"       
                End                                                             
                When (dsname <> '' & compiler = '') Then                        
                Do                                                              
                  command = "EXEC '"dsname"' '"CompOptions"'"                   
                End                                                             
                When (dsname =  '' & compiler <> '') Then                       
                Do                                                              
                  command = compiler" '"CompOptions"'"                          
                End                                                             
                Otherwise                                                       
                Do                                                              
                  command = "EXEC '"CompOptions"'"                              
                End                                                             
              End                                                               
              dsname  = ''                                                      
              CompOptions = ''                                                  
                                                                                
              callMeth = '2'                                                    
            End                                                                 
                                                                                
            When (Pos('CALLMETH=ISPLNK',projdefs.trancnt) <> 0) Then            
            Do                                                                  
              rc = repLine(' ' ' ')                                             
              rc = repLine('W' 'ISPLNK Translator - ' ||,                       
                                 'Please verify validity of ISPF invocation.')  
              rc = repLine('W' 'See - 'projdefs.trancnt)                        
              /* SCLM assumes ISPF commands are in SYSPROC or ISPLLIB */        
              Select                                                            
                When (Pos('CMD(',CompOptions) <> 0) Then                        
                Do                                                              
                  Parse var CompOptions 'CMD(' CompOptions                      
                  RevOptions  = Substr(Reverse(CompOptions),2)                  
                  command     = Reverse(RevOptions)                             
                End                                                             
                When (Pos('PGM(',CompOptions) <> 0) Then                        
                Do                                                              
                  Parse var CompOptions 'PGM('pgm')' CompOptions                
                  RevOptions  = Substr(Reverse(CompOptions),2)                  
                  command     = pgm' 'Reverse(RevOptions)                       
                End                                                             
                Otherwise                                                       
                Do                                                              
                  rc = repLine(' ' ' ')                                         
                  rc = repLine('E' 'For ISPLNK only CMD and PGM ' ||,           
                                     'supported. Rework required.')             
                End                                                             
              End                                                               
              dsname  = ''                                                      
              CompOptions = ''                                                  
                                                                                
              callMeth = '1'                                                    
            End                                                                 
            When (Compiler = 'FLMCSPDB') <> 0 Then                              
            Do                                                                  
              rc = repLine(' ' ' ')                                             
              rc = repLine('E' 'Migration does not process the ' ||,            
                                 'SCLM Bind processor')                         
              rc = repLine('E' 'You will need to rework your ' ||,              
                                 'DB2 Bind process')                            
              rc = repLine('E' 'See - 'projdefs.trancnt)                        
              flmcspdb = 1                                                      
              Iterate                                                           
            End                                                                 
            Otherwise                                                           
            Do                                                                  
              dsname = Strip(dsname)"("Strip(compiler)")"                       
              callMeth = '0'                                                    
            End                                                                 
          End                                                                   
                                                                                
          /* Going to create them all for now */                                
          translator = Strip(translator,,"'")                                   
          translator = Strip(translator)                                        
                                                                                
          tr = tr + 1                                                           
          trans.tr = translator                                                 
                                                                                
          trans.tr.callMethod = callMeth                                        
                                                                                
          If callMeth = '1' | callMeth = '2' Then                               
          Do                                                                    
            /* increment no syslib list for scanner */                          
            ns = ns + 1                                                         
            noSyslib.ns = tr                                                    
          End                                                                   
                                                                                
          If firstTran = 0 Then                                                 
            firstTran = tr                                                      
                                                                                
          If dsname    <> '' then                                               
            trans.tr.dsdef    = dsname                                          
          Else                                                                  
            trans.tr.dsdef    = ''                                              
                                                                                
          If command   <> '' then                                               
            trans.tr.commandMember = command                                    
          Else                                                                  
            trans.tr.commandMember = ''                                         
                                                                                
          If GoodRc    <> '' then                                               
            trans.tr.maxRc    = GoodRc                                          
          Else                                                                  
            trans.tr.maxRc    = '0'                                             
                                                                                
          If Pord      <> '' then                                               
            trans.tr.porder   = Pord                                            
          Else                                                                  
            trans.tr.porder   = ''                                              
                                                                                
          If CompOptions   <> '' then                                           
            trans.tr.options  = CompOptions                                     
          Else                                                                  
            trans.tr.options  = ''                                              
                                                                                
          If parmkwd       <> '' then                                           
            trans.tr.parmx    = parmkwd                                         
          Else                                                                  
            trans.tr.parmx    = ''                                              
                                                                                
          trans.tr.parm    = ''                                                 
                                                                                
          trans.tr.binder = ''                                                  
                                                                                
          trancnt = trancnt + 1                                                 
                                                                                
          ddList = 0                                                            
          syslinDD = 0                                                          
                                                                                
          /* Initialize the DD list based on the compiler being called */       
          /* Have had to add all the aliases as well.                  */       
          Select                                                                
            When (Compiler = 'ASMA90')   Then call InitAsmDD                    
            When (Compiler = 'IEV90')    Then call InitAsmDD                    
            When (Compiler = 'IGYCRCTL') Then call InitCobDD                    
            When (Compiler = 'IKFCBL00') Then call InitCobDD                    
            When (Compiler = 'IEL0AA')   Then call InitPliDD                    
            When (Compiler = 'IBMZPLI')  Then call InitEPliDD                   
            When (Compiler = 'DSNHPC')   Then call InitDB2DD                    
            When (Compiler = 'DSNHPSM')  Then call InitDB2DD                    
            When (Compiler = 'SEMEXPD')  Then call InitDB2DD                    
            When (Compiler = 'SEMNEWS')  Then call InitDB2DD                    
            When (Compiler = 'SQLPEMSG') Then call InitDB2DD                    
            When (Compiler = 'DFHEAP1$') Then call InitCICSDD                   
            When (Compiler = 'DFHECP1$') Then call InitCICSDD                   
            When (Compiler = 'DFHEDP1$') Then call InitCICSDD                   
            When (Compiler = 'DFHEPP1$') Then call InitCICSDD                   
            When (Compiler = 'CCNDRVR')  Then call InitCDD                      
            When (Compiler = 'CCNED230') Then call InitCDD                      
            When (Compiler = 'EDCCOMP')  Then call InitCDD                      
            When (Compiler = 'IEWL')     Then call InitLNKDD                    
            When (Compiler = 'HEWL')     Then call InitLNKDD                    
            When (Compiler = 'HEWLDRGO') Then call InitLNKDD                    
            When (Compiler = 'HEWLH096') Then call InitLNKDD                    
            When (Compiler = 'HEWLOAD')  Then call InitLNKDD                    
            When (Compiler = 'HEWLOADR') Then call InitLNKDD                    
            When (Compiler = 'IEWBLDGO') Then call InitLNKDD                    
            When (Compiler = 'IEWBLIMG') Then call InitLNKDD                    
            When (Compiler = 'IEWBLOAD') Then call InitLNKDD                    
            When (Compiler = 'IEWBLODI') Then call InitLNKDD                    
            When (Compiler = 'IEWBODEF') Then call InitLNKDD                    
            When (Compiler = 'IEWLDRGO') Then call InitLNKDD                    
            When (Compiler = 'IEWLOAD')  Then call InitLNKDD                    
            When (Compiler = 'IEWLOADI') Then call InitLNKDD                    
            When (Compiler = 'IEWLOADR') Then call InitLNKDD                    
            When (Compiler = 'LINKEDIT') Then call InitLNKDD                    
            When (Compiler = 'LOADER')   Then call InitLNKDD                    
            When (Compiler = 'IEBGENER') Then call InitDFSDD                    
            When (Compiler = 'ICEGENER') Then call InitDFSDD                    
            When (Compiler = 'IEBCOPY')  Then call InitDFSDD                    
            When (Compiler = 'ICBCOPY')  Then call InitDFSDD                    
            When (Compiler = 'JOVIAL')   Then call InitJovDD                    
            When (Compiler = 'FLMTMSI')  Then call InitBookDD                   
            When (Compiler = 'AKEEPLX')  Then call InitPlxDD                    
            When (Compiler = 'AKHPLX')   Then call InitPlxDD                    
            When (Compiler = 'AKHPLX2')  Then call InitPlxDD                    
            When (Compiler = 'AKHTSO')   Then call InitPlxDD                    
            When (Compiler = 'PLX390')   Then call InitPlxDD                    
            Otherwise                                                           
              Nop                                                               
          End                                                                   
                                                                                
          /* Need to get the allocations. Different depending on */             
          /* PORDER. DDNAME Substitution is PORDER 2 and 3       */             
          Call getAllocs(lang translator)                                       
                                                                                
          /* Just in case the translator does not have all DDs allocated */     
          Do k = 1 to trans.tr.DDcnt                                            
              If trans.tr.k.DSNcnt = '' Then                                    
                 trans.tr.k.DSNcnt = 1                                          
          End                                                                   
                                                                                
          /* If a translator has no FLMALLOCs then move the      */             
          /* loop back one so as not to miss the next translator */             
          If trans.tr.DDcnt = 0 Then                                            
            trancnt = trancnt - 1                                               
                                                                                
          If ddNames <> '' Then                                                 
            trans.tr.ddNameList = Strip(ddNames,T,',')                          
          Else                                                                  
            trans.tr.ddNameList = ''                                            
                                                                                
          Call checkDup                                                         
          If trans.tr.duptran <> '' Then                                        
            langs.ld.translators = langs.ld.translators||trans.tr.duptran||','  
          Else                                                                  
            langs.ld.translators = langs.ld.translators||tr||','                
        End                                                                     
      End                                                                       
      Otherwise                                                                 
        Nop                                                                     
    End                                                                         
  End                                                                           
                                                                                
  /* reset langcnt to one before the next FLMLANGL */                           
  langcnt = trancnt - 1                                                         
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine('-' 'Finished processing Translators in language 'Lang)          
                                                                                
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
  Drop trnsStmt.                                                                
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
                                                                                
/*---------------------------------------------------------------*/             
/* Process Allocations                                           */             
/*---------------------------------------------------------------*/             
getAllocs :                                                                     
                                                                                
  Parse arg Lang Tran                                                           
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine('-' '-- Allocations in translator - 'Tran' --')                  
                                                                                
  DDnum = 0                                                                     
  ddNames = ''                                                                  
                                                                                
  Do alloccnt = trancnt to projdefs.0,                                          
               While (Pos('FLMALLOC',projdefs.alloccnt) <> 0 |,                 
                      Pos('FLMCPYLB',projdefs.alloccnt) <> 0 |,                 
                      Pos('FLMTOPTS',projdefs.alloccnt) <> 0)                   
                                                                                
    ddt = 'DD'                                                                  
    Select                                                                      
      When (Pos('FLMALLOC',projdefs.alloccnt) <> 0) Then                        
      Do                                                                        
        Parse var projdefs.alloccnt . 'IOTYPE='iotype ',' .                     
        Parse var projdefs.alloccnt . 'DDNAME='ddn ',' .                        
        Parse var projdefs.alloccnt . 'KEYREF='keyref ',' .                     
        Parse var projdefs.alloccnt . 'DFLTTYP='dflttyp ',' .                   
        Parse var projdefs.alloccnt . 'DFLTMEM='dfltmem ',' .                   
        Parse var projdefs.alloccnt . 'MEMBER='outmem ',' .                     
        Parse var projdefs.alloccnt . 'INCLS='incls ',' .                       
                                                                                
        /* Get rid of extra rubbish */                                          
        Parse var iotype iotype .                                               
        Parse var ddn ddn .                                                     
        Parse var keyref keyref .                                               
        Parse var dflttyp dflttyp .                                             
        Parse var dfltmem dfltmem .                                             
        Parse var outmem outmem .                                               
        Parse var incls incls .                                                 
                                                                                
        If ddn = tasklib &,                                                     
           ddn <> '' then                                                       
        Do                                                                      
          ddn = 'TASKLIB'                                                       
          ddt = 'CC'                                                            
        End                                                                     
                                                                                
        /* If IOTYPE = O or P but no KEYREF or DFLTTYP is specified */          
        /* then we cannot check the KEYREF table or default the     */          
        /* output type. So treat as a temporary with a message.     */          
                                                                                
        If (iotype = 'O' | iotype = 'P') Then                                   
        Do                                                                      
          Select                                                                
            When (keyref = '' & dflttyp = '') Then                              
            Do                                                                  
              iotype = 'W'                                                      
              rc = repLine(' ' ' ')                                             
              rc = repLine('W' 'Translator - 'trans.tr,                         
                                 'DD - 'trans.tr.ddnum.ddname,                  
                                 ' has no KEYREF or DFLTTYP specified. ' ||,    
                                 ' Cannot determine data set definition.')      
              rc = repLine('W' 'Changing to IOTYPE=W to assume ' ||,            
                                 'temporary data set.')                         
            End                                                                 
            When (keyref <> '' & dflttyp = '') Then                             
            Do                                                                  
              dflttyp = keyref                                                  
            End                                                                 
            Otherwise                                                           
              Nop                                                               
          End                                                                   
        End                                                                     
                                                                                
        Select                                                                  
          When (iotype = 'A') Then                                              
          Do                                                                    
            /* Permanent data set                       */                      
            /* One of more FLMCPYLB follow an IOTYPE=A  */                      
            If ddList = 0 Then                                                  
              ddnum = ddnum + 1                                                 
            Else                                                                
              CAll FindDDnum                                                    
                                                                                
            dsnum = 0                                                           
            /* Need to check if there is more than one FLMCPYLB */              
            /* If so it is a concatenation                      */              
            checkCPY = Alloccnt + 2                                             
            If Pos('FLMCPYLB',projdefs.checkCPY) <> 0 Then                      
              ddt = 'CC'                                                        
                                                                                
            Do cpylbcnt = Alloccnt + 1 to projdefs.0,                           
                    While (Pos('FLMCPYLB',projdefs.cpylbcnt) <> 0)              
              Parse var projdefs.cpylbcnt . 'FLMCPYLB' dsname ',' .             
              /* get rid of extra rubbish */                                    
              Parse var dsname dsname .                                         
                                                                                
              /* Convert SCLM @@ variables   */                                 
              dsname = proc@@('tran' dsname)                                    
                                                                                
              dsnum = dsnum + 1                                                 
              trans.tr.ddnum.ddtype = ddt                                       
              trans.tr.ddnum.ddname = ddn                                       
              trans.tr.ddnum.dsnum.member = ''                                  
              trans.tr.ddnum.dsnum.output = ''                                  
                                                                                
              /* Looks like they are using IOTYPE=A instead of S  */            
              /* So take a punt and either use DFLTTYP or <INPUT> */            
              If dsname = '@{source.dataset}(@{source.member.name})' Then       
              Do                                                                
                If DFLTTYP <> '' Then                                           
                  dsname = dflttyp                                              
                Else                                                            
                  dsname = '<INPUT>'                                            
                trans.tr.ddnum.dsnum.member = 'true'                            
                rc = repLine(' ' ' ')                                           
                rc = repLine('W' 'Translator - 'trans.tr,                       
                                   'DD - 'trans.tr.ddnum.ddname,                
                                   ' is using @@FLMDSN(@@FLMMBR) to specify' ||,
                                   ' input. This cannot be migrated.')          
                rc = repLine('W' 'Assuming this is translator input. ' ||,      
                                   'Check generated XML')                       
              End                                                               
              Else                                                              
              Do                                                                
                /* If the dsdef is a type 2 but is allocated IOTYPE=A */        
                /* we can still assume that it is output=true unless  */        
                /* @@FLMDSN(@@FLMMBR) was used                        */        
                Do ty = 1 to t2                                                 
                  If trans.tr.ddnum.dsnum.dsdef = typet2.ty Then                
                  Do                                                            
                    trans.tr.ddnum.dsnum.output = 'true'                        
                    trans.tr.ddnum.dsnum.member = 'true'                        
                    Leave                                                       
                  End                                                           
                End                                                             
              End                                                               
              trans.tr.ddnum.dsnum.dsdef = Strip(dsname)                        
                                                                                
              trans.tr.ddnum.dsnum.keep = ''                                    
              trans.tr.ddnum.dsnum.publish = ''                                 
              trans.tr.ddnum.dsnum.instream = ''                                
              trans.tr.ddnum.dsnum.pattern = ''                                 
              trans.tr.ddnum.dsnum.outputName = ''                              
              trans.tr.ddnum.dsnum.hfsopts = ''                                 
            End                                                                 
            trans.tr.ddnum.DSNcnt = dsnum                                       
          End                                                                   
          When (iotype = 'O' |,                                                 
                iotype = 'P') Then                                              
          Do                                                                    
            /* Permanent SCLM Controlled data set    */                         
            If ddList = 0 Then                                                  
              ddnum = ddnum + 1                                                 
            Else                                                                
              CAll FindDDnum                                                    
                                                                                
            dsnum = 1                                                           
            trans.tr.ddnum.ddtype = ddt                                         
            trans.tr.ddnum.ddname = ddn                                         
                                                                                
            /* Output type is made up of input type + value after the * */      
            If Pos('*',dflttyp) <> 0 Then                                       
            Do                                                                  
              /* In SCLM the source comes from the ARCHDEF      */              
              /* We need to use the default source for the lang */              
              Do dt = 1 to langdefs.0                                           
                If lang = langdefs.dt then leave                                
              End                                                               
              p = Pos('*',dflttyp)                                              
              dflttyp = Strip(langdefs.dt.dfltsrc) || Substr(dflttyp,p+1)       
            End                                                                 
                                                                                
            Drop condDD.                                                        
                                                                                
            condDD.dsnum.dsn = ''                                               
                                                                                
            /* KEYREF in an archdef overrides DFLTTYP. So check table */        
            If keyref <> '' Then                                                
            Do                                                                  
              /* Need to check for KEYREF from the ARCHDEFs as they */          
              /* may specify different DDs                          */          
              /* So for the language we are processing need to      */          
              /* Check what the KEYREFs are. If they are different  */          
              /* To the DFLTTYP then need to use them instead       */          
              If langFound = 1 Then                                             
              Do                                                                
                noDefKey  = 1                                                   
                condDDset = 0                                                   
                Do kr = firstkr to keyrefs.0,                                   
                         While (Pos('<language name=',keyrefs.kr) = 0 &,        
                                Pos('</language>',keyrefs.kr) = 0)              
                  If Pos('<keyword name=',keyrefs.kr) <> 0 Then                 
                  Do                                                            
                    Parse var keyrefs.kr '<keyword name="'keyWord,              
                                                 '" valueCnt="'valueCnt'">'     
                    If keyref = keyWord Then                                    
                    Do                                                          
                      kr = kr + 1                                               
                      /* Found a KEYREF statement                   */          
                      /* Lets see how many data set names there are */          
                      dsnum = 0                                                 
                      Do kr = kr to keyrefs.0,                                  
                               While (Pos('<keyword name=',keyrefs.kr) = 0 &,   
                                      Pos('<language name=',keyrefs.kr) = 0 &,  
                                      Pos('</keyword>',keyrefs.kr) = 0 &,       
                                      Pos('</language>',keyrefs.kr) = 0)        
                        If Pos('<values type=',keyrefs.kr) <> 0 Then            
                        Do                                                      
                          Parse var keyrefs.kr '<values type="'keyType'"/>'     
                          rc = repLine(' ' ' ')                                 
                          rc = repLine('I' 'For language 'lang ||,              
                             ' found a KEYREF of 'keyref' and type of 'keytype) 
                          dsnum = dsnum + 1                                     
                          condDD.dsnum.dsn = keyType                            
                          If valueCnt > 1 Then                                  
                          Do                                                    
                            condDD.dsnum.cond = 'name='keyWord'TYPE ' ||,       
                                                'value='keyType                 
                            condDDset = 1                                       
                          End                                                   
                        End                                                     
                      End                                                       
                      If dsnum = 0 Then                                         
                        dsnum = 1                                               
                    End                                                         
                  End                                                           
                End                                                             
              End                                                               
            End                                                                 
            Do condCnt = 1 to dsnum                                             
              If condDD.condCnt.dsn <> '' Then                                  
              Do                                                                
                trans.tr.ddnum.condCnt.dsdef = Strip(condDD.condCnt.dsn)        
                If dsnum > 1 Then                                               
                Do                                                              
                  trans.tr.ddnum.condCnt.condition = Strip(condDD.condCnt.cond) 
                  trans.tr.ddnum.condCnt.conddefVal = Strip(dflttyp)            
                End                                                             
                Else                                                            
                Do                                                              
                  trans.tr.ddnum.condCnt.condition = ''                         
                  trans.tr.ddnum.condCnt.conddefVal = ''                        
                End                                                             
              End                                                               
              Else                                                              
              Do                                                                
                trans.tr.ddnum.condCnt.dsdef = Strip(dflttyp)                   
                trans.tr.ddnum.condCnt.condition = ''                           
                trans.tr.ddnum.condCnt.conddefVal = ''                          
              End                                                               
                                                                                
              /* Validate the default type is in the SCLM types table */        
              Do j = 1 to types.0 while(trans.tr.ddnum.condCnt.dsdef <> types.j)
              End                                                               
              If j > types.0 Then                                               
              Do                                                                
                rc = repLine(' ' ' ')                                           
                rc = repLine('E' 'Default type 'trans.tr.ddnum.condCnt.dsdef ||,
                                 ' not found in FLMTYPEs for IOTYPE O or P')    
                                                                                
                /* We probably have a KEYREF of OUTx but no DFLTTYP is      */  
                /* specified but no KEYREFs were found in the KEYREF table. */  
                /* So we need to treat as a temporary with a message.       */  
                                                                                
                rc = repLine('W' 'Changing to IOTYPE=W to assume ' ||,          
                                 'temporary data set.')                         
                                                                                
                Parse var projdefs.alloccnt . 'RECFM='ZDSRF ',' .               
                Parse var projdefs.alloccnt . 'LRECL='ZDSLREC ',' .             
                parse var projdefs.alloccnt . 'RECNUM='recnum ',' .             
                parse var projdefs.alloccnt . 'BLKSIZE='ZDSBLK ',' .            
                                                                                
                If ZDSBLK = 0 Then ZDSBLK = ''                                  
                                                                                
                                                                                
                /* get rid of extra rubbish */                                  
                Parse var ZDSRF ZDSRF .                                         
                Parse var ZDSLREC ZDSLREC .                                     
                Parse var recnum recnum .                                       
                Parse var ZDSBLK ZDSBLK .                                       
                                                                                
                If ZDSLREC = '' Then                                            
                  dsname = 'TemporaryFile'                                      
                Else                                                            
                  dsname = 'TemporaryFile-'ZDSRF||ZDSLREC                       
                                                                                
                Call Type4                                                      
                                                                                
                trans.tr.ddnum.dsnum.dsdef = Strip(dsname)                      
                trans.tr.ddnum.dsnum.output = ''                                
                trans.tr.ddnum.dsnum.outputName = ''                            
                trans.tr.ddnum.dsnum.keep = ''                                  
                trans.tr.ddnum.dsnum.publish = ''                               
                trans.tr.ddnum.dsnum.member = ''                                
                trans.tr.ddnum.dsnum.pattern = ''                               
                trans.tr.ddnum.dsnum.instream = ''                              
                trans.tr.ddnum.dsnum.hfsopts = ''                               
                trans.tr.ddnum.DSNcnt = dsnum                                   
              End                                                               
              Else                                                              
              Do                                                                
                                                                                
                /* Need to check the DCB SCLM is using for the output  */       
                /* If it is different to the DCB of the DSDEF (type 2) */       
                /* Then we need to create a new Type 2 with this DCB   */       
                Do ty = 1 to t2 While(trans.tr.ddnum.condCnt.dsdef <> typet2.ty)
                End                                                             
                If trans.tr.ddnum.condCnt.dsdef = typet2.ty Then                
                Do                                                              
                  Parse var projdefs.alloccnt . 'RECFM='ZDSRF ',' .             
                  Parse var projdefs.alloccnt . 'LRECL='ZDSLREC ',' .           
                                                                                
                  /* Get rid of extra rubbish */                                
                  Parse var ZDSRF ZDSRF .                                       
                  Parse var ZDSLREC ZDSLREC .                                   
                  If ZDSRF   = '' Then ZDSRF   = Strip(Typet2.ty.recfm)         
                  If ZDSLREC = '' Then ZDSLREC = Strip(Typet2.ty.lrecl)         
                                                                                
                  If (Strip(Typet2.ty.recfm) <> ZDSRF |,                        
                      Strip(Typet2.ty.lrecl) <> ZDSLREC) Then                   
                  Do                                                            
                    newType = Typet2.ty'.'ZDSRF||ZDSLREC                        
                    Do tz = 1 to t2 While(newType <> typet2.tz)                 
                    End                                                         
                    If tz > t2 Then                                             
                    Do                                                          
                      t2 = t2 + 1                                               
                      Typet2.t2           = newType                             
                      Typet2.t2.dsnType   = Typet2.ty.dsnType                   
                      Typet2.t2.dirBlocks = Typet2.ty.dirBlocks                 
                      Typet2.t2.dataClass = Typet2.ty.dataClass                 
                      Typet2.t2.mgmtClass = Typet2.ty.mgmtClass                 
                      Typet2.t2.stgClass  = Typet2.ty.stgClass                  
                      Typet2.t2.priSpaceU = Typet2.ty.priSpaceU                 
                      Typet2.t2.priSpaceA = Typet2.ty.priSpaceA                 
                      Typet2.t2.secSpaceA = Typet2.ty.secSpaceA                 
                      Typet2.t2.recfm     = ZDSRF                               
                      Typet2.t2.lrecl     = ZDSLREC                             
                      Select                                                    
                        When (ZDSLREC < 320) Then                               
                          Typet2.t2.blksize   = ZDSLREC*100                     
                        When (ZDSLREC < 3200) Then                              
                          Typet2.t2.blksize   = ZDSLREC*10                      
                        Otherwise                                               
                          Typet2.t2.blksize   = ZDSLREC                         
                      End                                                       
                    End                                                         
                    trans.tr.ddnum.condCnt.dsdef = newType                      
                                                                                
                    rc = repLine('W' 'Translator - 'trans.tr,                   
                                       ' : DD - 'trans.tr.ddnum.ddname)         
                    rc = repLine('W' 'SCLM Temporary data set RECFM/'    ||,    
                                       'LRECL is different to physical ' ||,    
                                       'data set. Creating a new data '  ||,    
                                       'set definition called 'newType)         
                  End                                                           
                                                                                
                End                                                             
                                                                                
                If outmem = '@@FLMONM' Then                                     
                Do                                                              
                  /* There may be multiples used based on different keyrefs */  
                  If keyref <> '' & Substr(keyref,1,3) = 'OUT' Then             
                  Do                                                            
                    /* Keyref has to be a OUTx type */                          
                    Parse var keyref 'OUT'knum                                  
                    outname = 'OUTNAME'Strip(knum)                              
                    trans.tr.ddnum.condCnt.outputName = outname                 
                    rc = repLine(' ' ' ')                                       
                    rc = repLine('I' 'Translator - 'trans.tr,                   
                                     'DD - 'trans.tr.ddnum.ddname,              
                                     ' uses SCLM metavariable @@FLMONM for ' ||,
                                     ' Output Member name.' ||,                 
                                     ' Converting to translator variable '  ||, 
                                     ' of "'outname'".')                        
                  End                                                           
                End                                                             
                Else                                                            
                  trans.tr.ddnum.condCnt.outputName = ''                        
                                                                                
                trans.tr.ddnum.condCnt.output = 'true'                          
                trans.tr.ddnum.condCnt.keep = ''                                
                trans.tr.ddnum.condCnt.publish = ''                             
                trans.tr.ddnum.condCnt.member = 'true'                          
                trans.tr.ddnum.condCnt.pattern = Strip(dfltmem)                 
                trans.tr.ddnum.condCnt.instream = ''                            
                trans.tr.ddnum.condCnt.hfsopts = ''                             
                /* Todo - Need to have some DFTLMEM processing here */          
                trans.tr.ddnum.DSNcnt = dsnum                                   
              End                                                               
            End                                                                 
          End                                                                   
          When (iotype = 'S') Then                                              
          Do                                                                    
            /* Input to translator   */                                         
            If ddList = 0 Then                                                  
              ddnum = ddnum + 1                                                 
            Else                                                                
              CAll FindDDnum                                                    
                                                                                
            dsnum = 1                                                           
            trans.tr.ddnum.ddtype = ddt                                         
            trans.tr.ddnum.ddname = ddn                                         
            trans.tr.ddnum.dsnum.dsdef = '<INPUT>'                              
            trans.tr.ddnum.dsnum.output = ''                                    
            trans.tr.ddnum.dsnum.outputName = ''                                
            trans.tr.ddnum.dsnum.keep = ''                                      
            trans.tr.ddnum.dsnum.publish = ''                                   
            trans.tr.ddnum.dsnum.member = 'true'                                
            trans.tr.ddnum.dsnum.pattern = ''                                   
            trans.tr.ddnum.dsnum.instream = ''                                  
            trans.tr.ddnum.dsnum.hfsopts = ''                                   
            trans.tr.ddnum.DSNcnt = dsnum                                       
                                                                                
            /* If this is a binder language we need to add any */               
            /* used object decks to the front of the SYSLIB    */               
          End                                                                   
          When (iotype = 'I') Then                                              
          Do                                                                    
            /* includes - tricky...  */                                         
            /* Need to get FLMINCLS in FLMINCLS set           */                
            /* Need to generate a defaulyt include set if not */                
            /* Add FLMSYSLBs if ALCSYSLB is set               */                
            /* Maybe one or more FLMCPYLB follow an IOTYPE=I  */                
            If ddList = 0 Then                                                  
              ddnum = ddnum + 1                                                 
            Else                                                                
              CAll FindDDnum                                                    
                                                                                
            ddt = 'CC'                                                          
            dsnum = 0                                                           
            If incls <> '' then                                                 
            Do                                                                  
              Do j = 1 to fi While (Lang || incls <>,                           
                                    flmincls.j || Flmincls.j.InclSet)           
              End                                                               
                                                                                
              Do k = 1 to words(Flmincls.j.typeset)                             
                dsname = Word(Flmincls.j.typeset,k)                             
                dsnum = dsnum + 1                                               
                trans.tr.ddnum.ddtype = ddt                                     
                trans.tr.ddnum.ddname = ddn                                     
                trans.tr.ddnum.dsnum.dsdef = Strip(dsname)                      
                trans.tr.ddnum.dsnum.output = ''                                
                trans.tr.ddnum.dsnum.outputName = ''                            
                trans.tr.ddnum.dsnum.keep = ''                                  
                trans.tr.ddnum.dsnum.publish = ''                               
                trans.tr.ddnum.dsnum.member = ''                                
                trans.tr.ddnum.dsnum.pattern = ''                               
                trans.tr.ddnum.dsnum.instream = ''                              
                trans.tr.ddnum.dsnum.hfsopts = ''                               
              End                                                               
            End                                                                 
            Else                                                                
            Do                                                                  
              /* First look for a default include set */                        
              /* If not found we need to generate one */                        
              incls = 'RTCDeflt'                                                
              Do j = 1 to fi While (Lang || incls <>,                           
                                    flmincls.j || Flmincls.j.InclSet)           
              End                                                               
                                                                                
              Do k = 1 to words(Flmincls.j.typeset)                             
                dsname = Word(Flmincls.j.typeset,k)                             
                dsnum = dsnum + 1                                               
                trans.tr.ddnum.ddtype = ddt                                     
                trans.tr.ddnum.ddname = ddn                                     
                trans.tr.ddnum.dsnum.dsdef = Strip(dsname)                      
                trans.tr.ddnum.dsnum.output = ''                                
                trans.tr.ddnum.dsnum.outputName = ''                            
                trans.tr.ddnum.dsnum.keep = ''                                  
                trans.tr.ddnum.dsnum.publish = ''                               
                trans.tr.ddnum.dsnum.member = ''                                
                trans.tr.ddnum.dsnum.pattern = ''                               
                trans.tr.ddnum.dsnum.instream = ''                              
                trans.tr.ddnum.dsnum.hfsopts = ''                               
              End                                                               
            End                                                                 
                                                                                
            If AllocSyslib = 'Y' Then                                           
            Do                                                                  
              Do j = 1 to fs While (Lang || incls <>,                           
                                  flmsyslb.j || Flmsyslb.j.InclSet)             
              End                                                               
              Do j = j to fs While (Lang || incls =,                            
                                  flmsyslb.j || Flmsyslb.j.InclSet)             
                dsname = Flmsyslb.j.dsn                                         
                dsnum = dsnum + 1                                               
                trans.tr.ddnum.ddtype = ddt                                     
                trans.tr.ddnum.ddname = ddn                                     
                trans.tr.ddnum.dsnum.dsdef = Strip(dsname)                      
                trans.tr.ddnum.dsnum.output = ''                                
                trans.tr.ddnum.dsnum.outputName = ''                            
                trans.tr.ddnum.dsnum.keep = ''                                  
                trans.tr.ddnum.dsnum.publish = ''                               
                trans.tr.ddnum.dsnum.member = ''                                
                trans.tr.ddnum.dsnum.pattern = ''                               
                trans.tr.ddnum.dsnum.instream = ''                              
                trans.tr.ddnum.dsnum.hfsopts = ''                               
              End                                                               
            End                                                                 
            Do cpylbcnt = Alloccnt + 1 to projdefs.0,                           
                    While (Pos('FLMCPYLB',projdefs.cpylbcnt) <> 0)              
              Parse var projdefs.cpylbcnt . 'FLMCPYLB' dsname ',' .             
                                                                                
              /* Convert SCLM @@ variables   */                                 
              dsname = proc@@('tran' dsname)                                    
                                                                                
              /* get rid of extra rubbish */                                    
              Parse var dsname dsname .                                         
              dsnum = dsnum + 1                                                 
              trans.tr.ddnum.ddtype = ddt                                       
              trans.tr.ddnum.ddname = ddn                                       
              trans.tr.ddnum.dsnum.dsdef = Strip(dsname)                        
              trans.tr.ddnum.dsnum.output = ''                                  
              trans.tr.ddnum.dsnum.outputName = ''                              
              trans.tr.ddnum.dsnum.keep = ''                                    
              trans.tr.ddnum.dsnum.publish = ''                                 
              trans.tr.ddnum.dsnum.member = ''                                  
              trans.tr.ddnum.dsnum.pattern = ''                                 
              trans.tr.ddnum.dsnum.instream = ''                                
              trans.tr.ddnum.dsnum.hfsopts = ''                                 
            End                                                                 
            trans.tr.ddnum.DSNcnt = dsnum                                       
          End                                                                   
          When (iotype = 'W') Then                                              
          Do                                                                    
            /* Temporary data set    */                                         
            If ddList = 0 Then                                                  
              ddnum = ddnum + 1                                                 
            Else                                                                
              CAll FindDDnum                                                    
                                                                                
            dsnum = 1                                                           
            trans.tr.ddnum.ddtype = ddt                                         
            trans.tr.ddnum.ddname = ddn                                         
                                                                                
            /* need to check forward in this translator if there is */          
            /* a DD and it is part of an IOTYPE=U in a later        */          
            /* translator.                                          */          
            dsname = ''                                                         
            Do lu = alloccnt+1 to projdefs.0,                                   
                      While (Pos('FLMLANGL',projdefs.lu) = 0)                   
              If Pos('IOTYPE=U',projdefs.lu) <> 0 &,                            
                 Pos(ddn,projdefs.lu) <> 0 Then                                 
              Do                                                                
                /* found the dd in this language */                             
                tempDD = '&&'ddn                                                
                dsname = 'TemporaryFile-'tempDD                                 
              End                                                               
            End                                                                 
                                                                                
            If dsname = '' Then                                                 
            Do                                                                  
              Parse var projdefs.alloccnt . 'RECFM='ZDSRF ',' .                 
              Parse var projdefs.alloccnt . 'LRECL='ZDSLREC ',' .               
                                                                                
              /* get rid of extra rubbish */                                    
              Parse var ZDSRF ZDSRF .                                           
              Parse var ZDSLREC ZDSLREC .                                       
                                                                                
              If ZDSLREC = '' Then                                              
                dsname = 'TemporaryFile'                                        
              Else                                                              
                dsname = 'TemporaryFile-'ZDSRF||ZDSLREC                         
            End                                                                 
                                                                                
            trans.tr.ddnum.dsnum.dsdef = Strip(dsname)                          
            trans.tr.ddnum.dsnum.output = ''                                    
            trans.tr.ddnum.dsnum.outputName = ''                                
            trans.tr.ddnum.dsnum.keep = ''                                      
            trans.tr.ddnum.dsnum.publish = ''                                   
            trans.tr.ddnum.dsnum.member = ''                                    
            trans.tr.ddnum.dsnum.pattern = ''                                   
            trans.tr.ddnum.dsnum.instream = ''                                  
            trans.tr.ddnum.dsnum.hfsopts = ''                                   
            trans.tr.ddnum.DSNcnt = dsnum                                       
          End                                                                   
          When (iotype = 'L') Then                                              
          Do                                                                    
            ddn = '@{source.member.name}'                                       
            /* Load module member    */                                         
            If ddList = 0 Then                                                  
              ddnum = ddnum + 1                                                 
            Else                                                                
              CAll FindDDnum                                                    
                                                                                
            dsnum = 1                                                           
            trans.tr.ddnum.ddtype = ddt                                         
            trans.tr.ddnum.ddname = ddn                                         
            trans.tr.ddnum.dsnum.dsdef = ''                                     
            trans.tr.ddnum.dsnum.output = ''                                    
            trans.tr.ddnum.dsnum.outputName = ''                                
            trans.tr.ddnum.dsnum.keep = ''                                      
            trans.tr.ddnum.dsnum.publish = ''                                   
            trans.tr.ddnum.dsnum.member = ''                                    
            trans.tr.ddnum.dsnum.pattern = ''                                   
            trans.tr.ddnum.dsnum.instream = '@{source.member.name}'             
            trans.tr.ddnum.dsnum.hfsopts = ''                                   
            trans.tr.ddnum.DSNcnt = dsnum                                       
          End                                                                   
          When (iotype = 'U') Then                                              
          Do                                                                    
            If ddList = 0 Then                                                  
              ddnum = ddnum + 1                                                 
            Else                                                                
              CAll FindDDnum                                                    
                                                                                
            rc = repLine(' ' ' ')                                               
            rc = repLine('I' 'IOTYPE=U found for translater - ' trans.tr ||,    
                               ' : DD - 'ddn'. Looking for previous DD')        
            /* DD from previous step */                                         
            ddNotFound = 1                                                      
                                                                                
            Do prevTran = tr-1 to firstTran by - 1 While (ddNotFound)           
              Do prevDD = 1 to trans.prevTran.DDcnt                             
                If trans.prevTran.prevDD.ddname = ddn Then                      
                Do                                                              
                  rc = repLine('I' 'Previous DD found in Translator - ' ||,     
                                     trans.prevTran ||,                         
                                     ' DD - 'trans.prevTran.prevDD.ddname)      
                  dsnum = 1                                                     
                  trans.tr.ddnum.ddtype = ddt                                   
                  trans.tr.ddnum.ddname = ddn                                   
                  trans.tr.ddnum.dsnum.keep = ''                                
                  trans.tr.ddnum.dsnum.publish = ''                             
                  trans.tr.ddnum.dsnum.member = ''                              
                  trans.tr.ddnum.dsnum.output = ''                              
                  trans.tr.ddnum.dsnum.outputName = ''                          
                  trans.tr.ddnum.dsnum.pattern = ''                             
                  trans.tr.ddnum.dsnum.instream = ''                            
                  trans.tr.ddnum.dsnum.hfsopts = ''                             
                  If Pos('TemporaryFile',,                                      
                         trans.prevTran.prevDD.1.dsdef) <> 0 Then               
                  Do                                                            
                    trans.prevTran.prevDD.1.keep = 'true'                       
                    /* ToDo : Need to decide if I add a temp to dsdef */        
                    trans.tr.ddnum.dsnum.dsdef = trans.prevTran.prevDD.1.dsdef  
                  End                                                           
                  Else                                                          
                  Do                                                            
                    rc = repLine('W' 'Translator - 'trans.tr,                   
                                       ' : DD - 'trans.tr.ddnum.ddname)         
                    rc = repLine('W' 'Migration  only supports "Keep" ' ||,     
                                       'for temporary DDs. Except not ' ||,     
                                       'for TSOLNK/ISPLNK translators')         
                    rc = repLine('W' 'Copying previous DD allocation')          
                    trans.tr.ddnum.dsnum.dsdef = trans.prevTran.prevDD.1.dsdef  
                    trans.tr.ddnum.dsnum.output = trans.prevTran.prevDD.1.output
                    trans.tr.ddnum.dsnum.instream =,                            
                                         trans.prevTran.prevDD.1.instream       
                  End                                                           
                  trans.tr.ddnum.DSNcnt = dsnum                                 
                  ddNotFound = 0                                                
                End                                                             
              End                                                               
            End                                                                 
          End                                                                   
          When (iotype = 'H') Then                                              
          Do                                                                    
            /* HFS Allocation        */                                         
            If ddList = 0 Then                                                  
              ddnum = ddnum + 1                                                 
            Else                                                                
              CAll FindDDnum                                                    
                                                                                
            normal   = 'KEEP'                                                   
            abnormal = 'KEEP'                                                   
            If Pos('PATHDSP',projdefs.alloccnt) <> 0 Then                       
            Do                                                                  
              parse var projdefs.alloccnt 'PATHDSP=' rest                       
              If Substr(rest,1,6) = 'DELETE' Then                               
                normal   = 'DELETE'                                             
              If Substr(rest,1,7) = '(DELETE' Then                              
                normal   = 'DELETE'                                             
              If Substr(rest,1,7) = ',DELETE' Then                              
                abnormal   = 'DELETE'                                           
              If Substr(rest,1,8) = '(,DELETE' Then                             
                abnormal   = 'DELETE'                                           
              If Substr(rest,1,13) = 'DELETE,DELETE' Then                       
                abnormal   = 'DELETE'                                           
              If Substr(rest,1,14) = '(DELETE,DELETE' Then                      
                abnormal   = 'DELETE'                                           
              If Substr(rest,1,11) = 'KEEP,DELETE' Then                         
                abnormal   = 'DELETE'                                           
              If Substr(rest,1,12) = '(KEEP,DELETE' Then                        
                abnormal   = 'DELETE'                                           
            End                                                                 
            pathdisp = 'disp="'normal'_'abnormal'"'                             
                                                                                
            filedata = 'fileType="BINARY"'                                      
            If Pos('FILEDAT=TEXT',projdefs.alloccnt) <> 0 Then                  
              filedata = 'fileType="TEXT"'                                      
                                                                                
            /* Set HFS Options */                                               
            pathopts  = ''                                                      
            If Pos('ORDWR',projdefs.alloccnt) <> 0 Then                         
              pathopts = pathopts||',ORDWR'                                     
            If Pos('OEXCL',projdefs.alloccnt) <> 0 Then                         
              pathopts = pathopts||',OEXCL'                                     
            If Pos('OSYNC',projdefs.alloccnt) <> 0 Then                         
              pathopts = pathopts||',OSYNC'                                     
            If Pos('OTRUNC',projdefs.alloccnt) <> 0 Then                        
              pathopts = pathopts||',OTRUNC'                                    
            If Pos('OCREAT',projdefs.alloccnt) <> 0 Then                        
              pathopts = pathopts||',OWRONLY'                                   
            If Pos('OWRONLY',projdefs.alloccnt) <> 0 Then                       
              pathopts = pathopts||',OWRONLY'                                   
            If Pos('ORDONLY',projdefs.alloccnt) <> 0 Then                       
              pathopts = pathopts||',ORDONLY'                                   
            If Pos('OAPPEND',projdefs.alloccnt) <> 0 Then                       
              pathopts = pathopts||',OAPPEND'                                   
            If Pos('ONOCTTY',projdefs.alloccnt) <> 0 Then                       
              pathopts = pathopts||',ONOCTTY'                                   
            If Pos('ONONBLOCK',projdefs.alloccnt) <> 0 Then                     
              pathopts = pathopts||',ONONBLOCK'                                 
            If Substr(pathopts,1,1) = ',' Then pathopts = Substr(pathopts,2)    
            If pathopts <> '' Then                                              
              pathopts = 'options="'pathopts'"'                                 
                                                                                
            /* Set HFS mode    */                                               
            permusr  = 0                                                        
            permgrp  = 0                                                        
            permoth  = 0                                                        
            permid   = 0                                                        
            pathmode = ''                                                       
                                                                                
            If Pos('SIRUSR',projdefs.alloccnt) <> 0 Then                        
              permusr = permusr + 4                                             
            If Pos('SIWUSR',projdefs.alloccnt) <> 0 Then                        
              permusr = permusr + 2                                             
            If Pos('SIXUSR',projdefs.alloccnt) <> 0 Then                        
              permusr = permusr + 1                                             
            If Pos('SIRWXU',projdefs.alloccnt) <> 0 Then                        
              permusr = 7                                                       
            If Pos('SIRGRP',projdefs.alloccnt) <> 0 Then                        
              permgrp = permgrp + 4                                             
            If Pos('SIWGRP',projdefs.alloccnt) <> 0 Then                        
              permgrp = permgrp + 2                                             
            If Pos('SIXGRP',projdefs.alloccnt) <> 0 Then                        
              permgrp = permgrp + 1                                             
            If Pos('SIRWXG',projdefs.alloccnt) <> 0 Then                        
              permgrp = 7                                                       
            If Pos('SIROTH',projdefs.alloccnt) <> 0 Then                        
              permoth = permoth + 4                                             
            If Pos('SIWOTH',projdefs.alloccnt) <> 0 Then                        
              permoth = permoth + 2                                             
            If Pos('SIXOTH',projdefs.alloccnt) <> 0 Then                        
              permoth = permoth + 1                                             
            If Pos('SIRWXO',projdefs.alloccnt) <> 0 Then                        
              permoth = 7                                                       
            If Pos('SISUID',projdefs.alloccnt) <> 0 Then                        
              permid = permid + 1                                               
            If Pos('SISGID',projdefs.alloccnt) <> 0 Then                        
              permid = permid + 2                                               
                                                                                
            If permid <> 0 then                                                 
              pathmode = permid                                                 
            pathmode = pathmode||permusr||permgrp||permoth                      
            If pathmode <> '' Then                                              
              pathmode = 'perm="'pathmode'"'                                    
                                                                                
            /* Need to get the path */                                          
            cpylbcnt = Alloccnt + 1                                             
            Parse var projdefs.cpylbcnt . 'FLMCPYLB' pathname ',' .             
                                                                                
            /* Convert SCLM @@ variables   */                                   
            pathname= proc@@('tran' Strip(pathname))                            
            pathname = 'path="'pathname'"'                                      
                                                                                
            dsnum = 1                                                           
            trans.tr.ddnum.ddtype = ddt                                         
            trans.tr.ddnum.ddname = ddn                                         
            trans.tr.ddnum.dsnum.dsdef = ''                                     
            trans.tr.ddnum.dsnum.output = ''                                    
            trans.tr.ddnum.dsnum.outputName = ''                                
            trans.tr.ddnum.dsnum.keep = ''                                      
            trans.tr.ddnum.dsnum.publish = ''                                   
            trans.tr.ddnum.dsnum.member = ''                                    
            trans.tr.ddnum.dsnum.pattern = ''                                   
            trans.tr.ddnum.dsnum.instream = ''                                  
            trans.tr.ddnum.dsnum.hfsopts  = pathdisp' 'filedata' ' ||,          
                                            pathopts' 'pathname' 'pathmode      
            trans.tr.ddnum.DSNcnt = dsnum                                       
          End                                                                   
          When (iotype = 'N') Then                                              
          Do                                                                    
            /* DDname Substitution - Skip */                                    
            If ddList = 0 Then                                                  
              ddnum = ddnum + 1                                                 
            Else                                                                
              CAll FindDDnum                                                    
                                                                                
            dsnum = 1                                                           
            trans.tr.ddnum.ddtype = ddt                                         
            trans.tr.ddnum.ddname = ddn                                         
            /* Output type is made up of input type + value after the * */      
            trans.tr.ddnum.dsnum.dsdef = 'NULLFILE'                             
            trans.tr.ddnum.dsnum.output = ''                                    
            trans.tr.ddnum.dsnum.outputName = ''                                
            trans.tr.ddnum.dsnum.keep = ''                                      
            trans.tr.ddnum.dsnum.publish = ''                                   
            trans.tr.ddnum.dsnum.member = ''                                    
            trans.tr.ddnum.dsnum.pattern = ''                                   
            trans.tr.ddnum.dsnum.instream = ''                                  
            trans.tr.ddnum.dsnum.hfsopts = ''                                   
            trans.tr.ddnum.DSNcnt = dsnum                                       
          End                                                                   
          Otherwise                                                             
            Nop                                                                 
        End                                                                     
                                                                                
        /* Take a stab at what might be published */                            
        If trans.tr.ddnum.ddname = 'SYSPRINT' |,                                
           trans.tr.ddnum.ddname = 'SYSOUT' |,                                  
           trans.tr.ddnum.ddname = 'SYSCPRT' Then                               
          trans.tr.ddnum.dsnum.publish = 'true'                                 
                                                                                
        /* If this is a SYSLIN DD then store the dataset name used to  */       
        /* add to link edit SYSLIB later                               */       
                                                                                
        If ddnum = syslinDD Then                                                
        Do                                                                      
          Do j = 1 to lecCnt While (trans.tr.ddnum.dsnum.dsdef <> lecSys.j)     
          End                                                                   
          If j > lecCnt &,                                                      
             Pos('TemporaryFile',trans.tr.ddnum.dsnum.dsdef) = 0 Then           
          Do                                                                    
            /* Not found so add to list */                                      
            lecCnt = lecCnt + 1                                                 
            lecSys.lecCnt = trans.tr.ddnum.dsnum.dsdef                          
          End                                                                   
        End                                                                     
                                                                                
      End                                                                       
      Otherwise                                                                 
        Nop                                                                     
    End                                                                         
  End                                                                           
                                                                                
  If ddList = 0 Then                                                            
    trans.tr.DDcnt = ddnum                                                      
                                                                                
  rc = repLine(' ' ' ')                                                         
  rc = repLine('-' '-- End Allocations in translator - 'Tran' --')              
                                                                                
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Calculate Binder SYSLIB                                       */             
/*---------------------------------------------------------------*/             
                                                                                
binderSyslib:                                                                   
                                                                                
  /* We should have what we need in the Keyrefs xml */                          
  Do kr = 1 to keyrefs.0                                                        
    If Pos('<keyword name=',keyrefs.kr) <> 0 Then                               
    Do                                                                          
      Parse var keyrefs.kr '<keyword name="'keyWord'" valueCnt' .               
      If keyWord = 'OBJ' Then                                                   
      Do                                                                        
        kr = kr + 1                                                             
        Do While (Pos('<keyword name=',keyrefs.kr) = 0 &,                       
                  Pos('<language name=',keyrefs.kr) = 0 &,                      
                  Pos('</keyword>',keyrefs.kr) = 0 &,                           
                  Pos('</language>',keyrefs.kr) = 0)                            
          If Pos('<values type=',keyrefs.kr) <> 0 Then                          
          Do                                                                    
            Parse var keyrefs.kr '<values type="'keyType'"/>'                   
            Do j = 1 to lecCnt While (keyType <> lecSys.j)                      
            End                                                                 
            If j > lecCnt Then                                                  
            Do                                                                  
              /* Not found so add to list */                                    
              lecCnt = lecCnt + 1                                               
              lecSys.lecCnt = keyType                                           
            End                                                                 
          End                                                                   
          kr = kr + 1                                                           
        End                                                                     
      End                                                                       
    End                                                                         
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for ASM                                       */             
/*---------------------------------------------------------------*/             
                                                                                
InitASMDD:                                                                      
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'SYSLIN'                                                 
  trans.tr.2.ddname  = 'Not Applicable'                                         
  trans.tr.3.ddname  = 'Not Applicable'                                         
  trans.tr.4.ddname  = 'SYSLIB'                                                 
  trans.tr.5.ddname  = 'SYSIN'                                                  
  trans.tr.6.ddname  = 'SYSPRINT'                                               
  trans.tr.7.ddname  = 'SYSPUNCH'                                               
  trans.tr.8.ddname  = 'SYSUT1'                                                 
  trans.tr.9.ddname  = 'Not Applicable'                                         
  trans.tr.10.ddname = 'Not Applicable'                                         
  trans.tr.11.ddname = 'Not Applicable'                                         
  trans.tr.12.ddname = 'SYSTERM'                                                
  trans.tr.13.ddname = 'Not Applicable'                                         
  trans.tr.14.ddname = 'Not Applicable'                                         
  trans.tr.15.ddname = 'Not Applicable'                                         
  trans.tr.16.ddname = 'SYSADATA'                                               
  trans.tr.17.ddname = 'Not Applicable'                                         
  trans.tr.18.ddname = 'Not Applicable'                                         
  trans.tr.19.ddname = 'Not Applicable'                                         
  trans.tr.20.ddname = 'ASMAOPT'                                                
  trans.tr.DDcnt = 20                                                           
  syslinDD = 1                                                                  
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for COBOL                                     */             
/*---------------------------------------------------------------*/             
                                                                                
InitCobDD:                                                                      
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'SYSLIN'                                                 
  trans.tr.2.ddname  = 'Not Applicable'                                         
  trans.tr.3.ddname  = 'Not Applicable'                                         
  trans.tr.4.ddname  = 'SYSLIB'                                                 
  trans.tr.5.ddname  = 'SYSIN'                                                  
  trans.tr.6.ddname  = 'SYSPRINT'                                               
  trans.tr.7.ddname  = 'SYSPUNCH'                                               
  trans.tr.8.ddname  = 'SYSUT1'                                                 
  trans.tr.9.ddname  = 'SYSUT2'                                                 
  trans.tr.10.ddname = 'SYSUT3'                                                 
  trans.tr.11.ddname = 'SYSUT4'                                                 
  trans.tr.12.ddname = 'SYSTERM'                                                
  trans.tr.13.ddname = 'SYSUT5'                                                 
  trans.tr.14.ddname = 'SYSUT6'                                                 
  trans.tr.15.ddname = 'SYSUT7'                                                 
  trans.tr.16.ddname = 'SYSADATA'                                               
  trans.tr.17.ddname = 'SYSJAVA'                                                
  trans.tr.18.ddname = 'Not Applicable'                                         
  trans.tr.19.ddname = 'SYSMDECK'                                               
  trans.tr.20.ddname = 'DBRMLIB'                                                
  trans.tr.21.ddname = 'SYSOPTF'                                                
  trans.tr.22.ddname = 'SYSUT8'                                                 
  trans.tr.23.ddname = 'SYSUT9'                                                 
  trans.tr.24.ddname = 'SYSUT10'                                                
  trans.tr.25.ddname = 'SYSUT11'                                                
  trans.tr.26.ddname = 'SYSUT12'                                                
  trans.tr.27.ddname = 'SYSUT13'                                                
  trans.tr.28.ddname = 'SYSUT14'                                                
  trans.tr.29.ddname = 'SYSUT15'                                                
  trans.tr.DDcnt = 29                                                           
  syslinDD = 1                                                                  
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for C                                         */             
/*---------------------------------------------------------------*/             
                                                                                
InitCDD:                                                                        
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'SYSIN'                                                  
  trans.tr.2.ddname  = 'SYSLIN'                                                 
  trans.tr.3.ddname  = 'SYSMSGS'                                                
  trans.tr.4.ddname  = 'SYSLIB'                                                 
  trans.tr.5.ddname  = 'USERLIB'                                                
  trans.tr.6.ddname  = 'SYSPRINT'                                               
  trans.tr.7.ddname  = 'SYSCPRT'                                                
  trans.tr.8.ddname  = 'SYSPUNCH'                                               
  trans.tr.9.ddname  = 'SYSUT1'                                                 
  trans.tr.10.ddname = 'SYSUT4'                                                 
  trans.tr.11.ddname = 'SYSUT5'                                                 
  trans.tr.12.ddname = 'SYSUT6'                                                 
  trans.tr.13.ddname = 'SYSUT7'                                                 
  trans.tr.14.ddname = 'SYSUT8'                                                 
  trans.tr.15.ddname = 'SYSUT9'                                                 
  trans.tr.16.ddname = 'SYSUT10'                                                
  trans.tr.17.ddname = 'SYSUT14'                                                
  trans.tr.18.ddname = 'SYSUT15'                                                
  trans.tr.19.ddname = 'SYSEVENT'                                               
  trans.tr.20.ddname = 'TEMPINC'                                                
  trans.tr.21.ddname = 'IPACNTL'                                                
  trans.tr.22.ddname = 'SYSUT16'                                                
  trans.tr.23.ddname = 'SYSUT17'                                                
  trans.tr.24.ddname = 'SYSUTIP'                                                
  trans.tr.25.ddname = 'SYSCDBG'                                                
  trans.tr.26.ddname = 'ASMLIB'                                                 
  trans.tr.DDcnt = 26                                                           
  syslinDD = 2                                                                  
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for PLI Optimizing compiler                   */             
/*---------------------------------------------------------------*/             
                                                                                
InitPliDD:                                                                      
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'SYSLIN'                                                 
  trans.tr.2.ddname  = 'Not Applicable'                                         
  trans.tr.3.ddname  = 'Not Applicable'                                         
  trans.tr.4.ddname  = 'SYSLIB'                                                 
  trans.tr.5.ddname  = 'SYSIN'                                                  
  trans.tr.6.ddname  = 'SYSPRINT'                                               
  trans.tr.7.ddname  = 'SYSPUNCH'                                               
  trans.tr.8.ddname  = 'SYSUT1'                                                 
  trans.tr.9.ddname  = 'Not Applicable'                                         
  trans.tr.10.ddname = 'Not Applicable'                                         
  trans.tr.11.ddname = 'Not Applicable'                                         
  trans.tr.12.ddname = 'Not Applicable'                                         
  trans.tr.13.ddname = 'Not Applicable'                                         
  trans.tr.14.ddname = 'SYSCIN'                                                 
  trans.tr.DDcnt = 14                                                           
  syslinDD = 1                                                                  
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for PLX compiler                              */             
/*---------------------------------------------------------------*/             
                                                                                
InitPlxDD:                                                                      
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'Not Applicable'                                         
  trans.tr.2.ddname  = 'Not Applicable'                                         
  trans.tr.3.ddname  = 'Not Applicable'                                         
  trans.tr.4.ddname  = 'SYSLIB'                                                 
  trans.tr.5.ddname  = 'SYSIN'                                                  
  trans.tr.6.ddname  = 'SYSPRINT'                                               
  trans.tr.7.ddname  = 'SYSPUNCH'                                               
  trans.tr.8.ddname  = 'Not Applicable'                                         
  trans.tr.9.ddname  = 'SYSUT2'                                                 
  trans.tr.10.ddname = 'Not Applicable'                                         
  trans.tr.11.ddname = 'Not Applicable'                                         
  trans.tr.12.ddname = 'SYSTERM'                                                
  trans.tr.13.ddname = 'Not Applicable'                                         
  trans.tr.14.ddname = 'Not Applicable'                                         
  trans.tr.15.ddname = 'Not Applicable'                                         
  trans.tr.16.ddname = 'SYSLOGIC'                                               
  trans.tr.17.ddname = 'Not Applicable'                                         
  trans.tr.18.ddname = 'Not Applicable'                                         
  trans.tr.19.ddname = 'Not Applicable'                                         
  trans.tr.20.ddname = 'ASMLIN'                                                 
  trans.tr.21.ddname = 'ASMLIB'                                                 
  trans.tr.22.ddname = 'ASMPRINT'                                               
  trans.tr.23.ddname = 'ASMPUNCH'                                               
  trans.tr.24.ddname = 'ASMUT1'                                                 
  trans.tr.25.ddname = 'ASMTERM'                                                
  trans.tr.26.ddname = 'SYSADATA'                                               
  trans.tr.DDcnt = 26                                                           
  syslinDD = 20                                                                 
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for Enterprise PLI                            */             
/*---------------------------------------------------------------*/             
                                                                                
InitEPliDD:                                                                     
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'SYSPRINT'                                               
  trans.tr.2.ddname  = 'SYSIN'                                                  
  trans.tr.3.ddname  = 'SYSLIB'                                                 
  trans.tr.4.ddname  = 'SYSPUNCH'                                               
  trans.tr.5.ddname  = 'SYSLIN'                                                 
  trans.tr.6.ddname  = 'SYSADATA'                                               
  trans.tr.7.ddname  = 'SYSXMLSD'                                               
  trans.tr.8.ddname  = 'SYSDEBUG'                                               
  trans.tr.DDcnt = 8                                                            
  syslinDD = 5                                                                  
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for DB2 precompiler                           */             
/*---------------------------------------------------------------*/             
                                                                                
InitDB2DD:                                                                      
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'Not Applicable'                                         
  trans.tr.2.ddname  = 'Not Applicable'                                         
  trans.tr.3.ddname  = 'Not Applicable'                                         
  trans.tr.4.ddname  = 'SYSLIB'                                                 
  trans.tr.5.ddname  = 'SYSIN'                                                  
  trans.tr.6.ddname  = 'SYSPRINT'                                               
  trans.tr.7.ddname  = 'Not Applicable'                                         
  trans.tr.8.ddname  = 'SYSUT1'                                                 
  trans.tr.9.ddname  = 'SYSUT2'                                                 
  trans.tr.10.ddname = 'Not Applicable'                                         
  trans.tr.11.ddname = 'Not Applicable'                                         
  trans.tr.12.ddname = 'SYSTERM'                                                
  trans.tr.13.ddname = 'Not Applicable'                                         
  trans.tr.14.ddname = 'SYSCIN'                                                 
  trans.tr.15.ddname = 'Not Applicable'                                         
  trans.tr.16.ddname = 'DBRMLIB'                                                
  trans.tr.DDcnt = 16                                                           
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for CICS precompiler                          */             
/*---------------------------------------------------------------*/             
                                                                                
InitCICSDD:                                                                     
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'Not Applicable'                                         
  trans.tr.2.ddname  = 'Not Applicable'                                         
  trans.tr.3.ddname  = 'Not Applicable'                                         
  trans.tr.4.ddname  = 'Not Applicable'                                         
  trans.tr.5.ddname  = 'SYSIN'                                                  
  trans.tr.6.ddname  = 'SYSPRINT'                                               
  trans.tr.7.ddname  = 'SYSPUNCH'                                               
  trans.tr.DDcnt = 7                                                            
                                                                                
  /* increment no syslib list for scanner */                                    
  ns = ns + 1                                                                   
  noSyslib.ns = tr                                                              
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for DFSMS Utilities                           */             
/*---------------------------------------------------------------*/             
                                                                                
InitDFSDD:                                                                      
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'Not Applicable'                                         
  trans.tr.2.ddname  = 'Not Applicable'                                         
  trans.tr.3.ddname  = 'Not Applicable'                                         
  trans.tr.4.ddname  = 'Not Applicable'                                         
  trans.tr.5.ddname  = 'SYSIN'                                                  
  trans.tr.6.ddname  = 'SYSPRINT'                                               
  trans.tr.7.ddname  = 'Not Applicable'                                         
  trans.tr.8.ddname  = 'SYSUT1'                                                 
  trans.tr.9.ddname  = 'SYSUT2'                                                 
  trans.tr.10.ddname = 'SYSUT3'                                                 
  trans.tr.11.ddname = 'SYSUT4'                                                 
  trans.tr.DDcnt = 11                                                           
                                                                                
  /* increment no syslib list for scanner */                                    
  ns = ns + 1                                                                   
  noSyslib.ns = tr                                                              
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for Binder                                    */             
/*---------------------------------------------------------------*/             
                                                                                
InitLNKDD:                                                                      
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'SYSLIN'                                                 
  trans.tr.2.ddname  = 'MemberName'                                             
  trans.tr.3.ddname  = 'SYSLMOD'                                                
  trans.tr.4.ddname  = 'SYSLIB'                                                 
  trans.tr.5.ddname  = 'Not Applicable'                                         
  trans.tr.6.ddname  = 'SYSPRINT'                                               
  trans.tr.7.ddname  = 'Not Applicable'                                         
  trans.tr.8.ddname  = 'Not Applicable'                                         
  trans.tr.9.ddname  = 'Not Applicable'                                         
  trans.tr.10.ddname = 'Not Applicable'                                         
  trans.tr.11.ddname = 'Not Applicable'                                         
  trans.tr.12.ddname = 'SYSTERM'                                                
  trans.tr.13.ddname = 'SYSDEFSD'                                               
  trans.tr.DDcnt = 13                                                           
  trans.tr.binder = 'true'                                                      
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for Jovial                                    */             
/*---------------------------------------------------------------*/             
                                                                                
InitJovDD:                                                                      
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'SYSIN'                                                  
  trans.tr.2.ddname  = 'SYSPRINT'                                               
  trans.tr.3.ddname  = 'SYSPLIB'                                                
  trans.tr.4.ddname  = 'SYSLIB'                                                 
  trans.tr.5.ddname  = 'SYSPOOL'                                                
  trans.tr.6.ddname  = 'SYSLIN'                                                 
  trans.tr.7.ddname  = 'SYSTERM'                                                
  trans.tr.8.ddname  = 'SYSOUREF'                                               
  trans.tr.9.ddname  = 'SYSOUT'                                                 
  trans.tr.10.ddname = 'SYSUT1'                                                 
  trans.tr.11.ddname = 'SYSUT2'                                                 
  trans.tr.12.ddname = 'SYSUT3'                                                 
  trans.tr.13.ddname = 'SYSUT4OU'                                               
  trans.tr.14.ddname = 'SYSUT4IN'                                               
  trans.tr.15.ddname = 'SYSGOA'                                                 
  trans.tr.16.ddname = 'SYSXREF'                                                
  trans.tr.17.ddname = 'Not Applicable'                                         
  trans.tr.18.ddname = 'Not Applicable'                                         
  trans.tr.19.ddname = 'Not Applicable'                                         
  trans.tr.DDcnt = 19                                                           
  syslinDD = 6                                                                  
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Initial DD List for SCLM Implementation of Script/VS          */             
/*---------------------------------------------------------------*/             
                                                                                
InitBookDD:                                                                     
                                                                                
  ddList = 1                                                                    
  trans.tr.1.ddname  = 'TEXTIN'                                                 
  trans.tr.2.ddname  = 'TEXTOUT'                                                
  trans.tr.3.ddname  = 'TEXTMAC'                                                
  trans.tr.4.ddname  = 'TEXTPROF'                                               
  trans.tr.5.ddname  = 'TEXTMSGS'                                               
  trans.tr.6.ddname  = 'TEXTLIB'                                                
  trans.tr.DDcnt = 6                                                            
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Find DD number                                                */             
/*---------------------------------------------------------------*/             
                                                                                
findDDnum:                                                                      
                                                                                
  If trans.tr.porder = '0' | trans.tr.porder = '1' Then                         
  Do                                                                            
    Do dd = 1 to trans.tr.DDcnt While (trans.tr.dd.ddname <> ddn)               
    End                                                                         
    If trans.tr.dd.ddname = ddn Then                                            
      ddnum = dd                                                                
    Else                                                                        
    Do                                                                          
      rc = repLine(' ' ' ')                                                     
      rc = repLine('I' 'Additional DD - 'ddn' found for translator - ' ||,      
                         trans.tr' language - 'lang)                            
      ddnum = dd                                                                
      trans.tr.DDcnt = trans.tr.DDcnt + 1                                       
    End                                                                         
  End                                                                           
  Else                                                                          
  Do                                                                            
    ddnum = ddnum + 1                                                           
    If ddn = '' Then                                                            
      ddn = trans.tr.ddnum.ddname                                               
    If ddn = 'Not Applicable' Then                                              
    Do                                                                          
      If Compiler = 'IBMZPLI' Then                                              
        ddNames = ddNames || '*,'                                               
      Else                                                                      
        ddNames = ddNames || ','                                                
    End                                                                         
    Else                                                                        
      ddNames = ddNames || ddn || ','                                           
  End                                                                           
                                                                                
  /* If this is a SYSLIB DD for a LE/370 translator then need to */             
  /* add all OBJ types to the front of the SYSLIB. However this  */             
  /* only works for CC types. If you use Generic ARCHDEFs to     */             
  /* control the compiles then we don't know what OUTx statement */             
  /* is used for the binder input. SCLM works it out internally. */             
                                                                                
  If ((trans.tr.porder = '2' | trans.tr.porder = '3') & ddnum = 4) |,           
     ((trans.tr.porder = '0' | trans.tr.porder = '1') & ddn = 'SYSLIB') Then    
  Do                                                                            
    If trans.tr.binder = 'true' Then                                            
      Call binderSyslib                                                         
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Check if translator is same as a previous version             */             
/*---------------------------------------------------------------*/             
                                                                                
checkDup:                                                                       
                                                                                
  trans.tr.duptran = ''                                                         
                                                                                
  noDup = 1                                                                     
  sameName = 0                                                                  
                                                                                
  Do dup = 1 to tr - 1 While (noDup = 1)                                        
    If Translate(trans.dup) = Translate(trans.tr) Then                          
      sameName = 1                                                              
                                                                                
    If trans.dup.dsdef = trans.tr.dsdef Then                                    
    Do                                                                          
      rc = repLine(' ' ' ')                                                     
      rc = repLine('I' trans.tr 'is a potential duplicate of ' ||,              
                         trans.dup 'Checking DDs')                              
      storeDup = dup                                                            
      noDup = 0                                                                 
      If trans.dup.maxrc   = trans.tr.maxrc   &,                                
         trans.dup.options = trans.tr.options &,                                
         trans.dup.parmx   = trans.tr.parmx   &,                                
         trans.dup.commandMember = trans.tr.commandMember Then                  
      Do                                                                        
        Do dd = 1 to trans.tr.DDcnt                                             
          /* Only process DDs that are specified skip the rest */               
          If trans.dup.dd.ddtype <> '' Then                                     
          Do                                                                    
            If trans.dup.dd.ddtype = trans.tr.dd.ddtype &,                      
               trans.dup.dd.ddname = trans.tr.dd.ddname Then                    
            Do                                                                  
              Do k = 1 to trans.tr.dd.DSNcnt                                    
                If trans.dup.dd.k.dsdef   = trans.tr.dd.k.dsdef &,              
                   trans.dup.dd.k.member  = trans.tr.dd.k.member &,             
                   trans.dup.dd.k.output  = trans.tr.dd.k.output &,             
                   trans.dup.dd.k.pattern = trans.tr.dd.k.pattern &,            
                   trans.dup.dd.k.publish = trans.tr.dd.k.publish Then          
                Do                                                              
                End                                                             
                Else                                                            
                  noDup = 1                                                     
              End                                                               
            End                                                                 
            Else                                                                
              noDup = 1                                                         
          End                                                                   
        End                                                                     
      End                                                                       
      Else                                                                      
        noDup = 1                                                               
    End                                                                         
  End                                                                           
  If noDup = 0 Then                                                             
  Do                                                                            
    rc = repLine('I' 'It looks like these are the same, replacing ' ||,         
                       trans.tr 'with 'trans.storeDup '.')                      
    trans.tr.duptran = storeDup                                                 
  End                                                                           
  Else                                                                          
    If sameName Then                                                            
      trans.tr = trans.tr || ' #'tr                                             
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Process SCLM @@ variables                                     */             
/*---------------------------------------------------------------*/             
                                                                                
proc@@ :                                                                        
                                                                                
  Parse arg defType name                                                        
                                                                                
  xx = 1                                                                        
                                                                                
  If Pos('@@',name) <> 0 Then                                                   
    rc = repLine(' ' ' ')                                                       
                                                                                
  Do while (Pos('@@',name) <> 0)                                                
                                                                                
    /* Look for other @@ variables in FLMCPYLB and report them    */            
    xx = Pos('@@',name)                                                         
    If xx <> 0 Then                                                             
    Do                                                                          
      Select                                                                    
        When (Substr(name,xx,8) = '@@FLMDSN') Then                              
        Do                                                                      
          name = Substr(name,1,xx-1) ||,                                        
                        '@{source.dataset}' ||,                                 
                        Substr(name,xx+8)                                       
          rc = repLine('W ' '@@FLMDSN found in FLMCPYLB. '||,                   
                              'Changed to @{source.dataset}.')                  
          If defType = 'tran' Then                                              
            rc = repLine('W' 'Check allocation in translator : 'trans.tr)       
        End                                                                     
                                                                                
        When (Substr(name,xx,8) = '@@FLMUID') Then                              
        Do                                                                      
          name = Substr(name,1,xx-1) ||,                                        
                        '${buildRequesterUserId}' ||,                           
                        Substr(name,xx+8)                                       
          If defType = 'dsdef' Then                                             
            rc = repLine('W' '@@FLMUID found in FLMCPYLB. '||,                  
                               'Changed to ${buildRequesterUserId}')            
          If defType = 'comp' Then                                              
              rc = repLine('W' '@@FLMUID found in Compiler options. '||,        
                                 'Changed to ${buildRequesterUserId}')          
        End                                                                     
                                                                                
        When (Substr(name,xx,8) = '@@FLMPRJ') Then                              
        Do                                                                      
          name = Substr(name,1,xx-1) ||,                                        
                        project ||,                                             
                        Substr(name,xx+8)                                       
          If defType = 'dsdef' Then                                             
            rc = repLine('W' '@@FLMPRJ found in FLMCPYLB. '||,                  
                               'Changed to 'project)                            
        End                                                                     
                                                                                
        When (Substr(name,xx,8) = '@@FLMALT') Then                              
        Do                                                                      
          name = Substr(name,1,xx-1) ||,                                        
                        projmem ||,                                             
                        Substr(name,xx+8)                                       
          If defType = 'dsdef' Then                                             
            rc = repLine('W' '@@FLMALT found in FLMCPYLB. '||,                  
                               'Changed to 'projmem)                            
        End                                                                     
                                                                                
        When (Substr(name,xx,8) = '@@FLMGRP') Then                              
        Do                                                                      
          name = Substr(name,1,xx-1) ||,                                        
                        TopGroup ||,                                            
                        Substr(name,xx+8)                                       
          If defType = 'dsdef' Then                                             
            rc = repLine('W' '@@FLMGRP found in FLMCPYLB. '||,                  
                               'Changed to 'TopGroup)                           
        End                                                                     
                                                                                
        When (Substr(name,xx,8) = '@@FLMGRB') Then                              
        Do                                                                      
          name = Substr(name,1,xx-1) ||,                                        
                        TopGroup ||,                                            
                        Substr(name,xx+8)                                       
          If defType = 'dsdef' Then                                             
            rc = repLine('W' '@@FLMGRB found in FLMCPYLB. '||,                  
                               'Changed to 'TopGroup)                           
        End                                                                     
                                                                                
        When (Substr(name,xx,8) = '@@FLMMBR') Then                              
        Do                                                                      
          name = Substr(name,1,xx-1) ||,                                        
                        '@{source.member.name}' ||,                             
                       Substr(name,xx+8)                                        
          If defType = 'tran' Then                                              
            trans.tr.ddnum.dsnum.member = 'true'                                
        End                                                                     
                                                                                
        When (Substr(name,xx,8) = '@@FLMLCC') Then                              
        Do                                                                      
          name = Substr(name,1,xx-1) ||,                                        
                        '${RTCReturnCodeForTranslator}' ||,                     
                        Substr(name,xx+8)                                       
          If defType = 'comp' Then                                              
            rc = repLine('W' '@@FLMLCC found in Compiler options. '||,          
                               'Changed to ${RTCReturnCodeForTranslator}')      
        End                                                                     
        Otherwise                                                               
        Do                                                                      
          /* Process other @@ variables */                                      
          rc = repLine('E' 'Unable to migrate SCLM variable ' ||,               
                          Substr(name,xx,8)'. Manual intervention required')    
          name = Substr(name,1,xx-1) ||,                                        
                       '##'Substr(name,xx+2,6) ||,                              
                       Substr(name,xx+8)                                        
        End                                                                     
      End                                                                       
    End                                                                         
  End                                                                           
  name = Translate(name,'@@','##')                                              
                                                                                
Return name                                                                     
                                                                                
/*---------------------------------------------------------------*/             
/* Generate System Definition XML                                */             
/*---------------------------------------------------------------*/             
genXML :                                                                        
                                                                                
  Drop xml.                                                                     
  xcnt = 0                                                                      
                                                                                
  Call doiefbr14  /* Create a do nothing translator just in case */             
                                                                                
  Call xmlHeader                                                                
  Call xmlDsdef                                                                 
  Call xmlTrans                                                                 
  Call xmlLangs                                                                 
  Call xmlFooter                                                                
                                                                                
  xml.0 = xcnt                                                                  
  xmlfile = outputDir'/systemDefinition.xml'                                    
  Address syscall "writefile (xmlfile) 755 xml."                                
                                                                                
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
  xml.9  = '<project name="'defzProject'" default="all" ' ||,                   
           'xmlns:ld="antlib:com.ibm.team.enterprise.zos.' ||,                  
           'systemdefinition.toolkit">'                                         
  xml.10 = '  <description>SCLM Migration system definition XML</description>'  
  xml.11 = ''                                                                   
  xml.12 = '  <property name="resource.def.prefix" value="'proj'-" />'          
  xml.13 = '  <property name="resource.def.suffix" value="" />'                 
  xml.14 = ''                                                                   
  xml.15 = '  <ld:init password="${password}" projectArea="${projectArea}" ' ||,
           'repositoryAddress="${repositoryAddress}" userId="${userId}"/>'      
  xml.16 = ''                                                                   
                                                                                
  xcnt = 16                                                                     
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML Data set definitions                                      */             
/*---------------------------------------------------------------*/             
xmlDsdef :                                                                      
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  '                                                               
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  <!-- Create data set definitions -->'                           
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  <target name="dsdefs" ' ||,                                     
                       'description="Create data set definitions">'             
                                                                                
  /* Type 1 */                                                                  
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  '                                                               
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <!-- Source Data sets -->'                                    
  Do i = 1 to t1                                                                
                                                                                
    dBlocks = ''                                                                
    dClass  = ''                                                                
    mClass  = ''                                                                
    sClass  = ''                                                                
                                                                                
    Select                                                                      
      When (Typet1.i.dsnType = 'LIBRARY') Then dsType = '0'                     
      When (Typet1.i.dsnType = 'PDS') Then                                      
      Do                                                                        
        dsType = '3'                                                            
        dBlocks = 'directoryBlocks="'Strip(Typet1.i.dirBlocks)'"'               
      End                                                                       
      Otherwise                                                                 
        Nop                                                                     
    End                                                                         
                                                                                
    If Typet1.i.dataClass <> '**None**' Then                                    
      dClass = 'dataClass="'Strip(Typet1.i.dataClass)'"'                        
    If Typet1.i.mgmtClass <> '**None**' Then                                    
      mClass = 'managementClass="'Strip(Typet1.i.mgmtClass)'"'                  
    If Typet1.i.stgClass <> '**None**' Then                                     
      sClass  = 'storageClass="'Strip(Typet1.i.stgClass)'"'                     
                                                                                
    xcnt = xcnt + 1                                                             
    xml.xcnt = '    <ld:dsdef dsDefUsageType="0" dsName="'Strip(Typet1.i)'" '||,
               'name="${resource.def.prefix}'Strip(Typet1.i) ||,                
                     '${resource.def.suffix}" ' ||,                             
               'dsType="'dsType'" ' ||,                                         
               'spaceUnits="'Strip(Typet1.i.priSpaceU)'" ' ||,                  
               'primaryQuantity="'Strip(Typet1.i.priSpaceA)'" ' ||,             
               'secondaryQuantity="'Strip(Typet1.i.secSpaceA)'" ' ||,           
               dBlocks' ' ||,                                                   
               'recordFormat="'Strip(Typet1.i.recfm)'" ' ||,                    
               'recordLength="'Strip(Typet1.i.lrecl)'" ' ||,                    
               'blockSize="'Strip(Typet1.i.blksize)'" '  ||,                    
               dClass mClass sClass ||,                                         
               '/>'                                                             
  End                                                                           
                                                                                
  /* Type 2 */                                                                  
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  '                                                               
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <!-- New data sets -->'                                       
  Do i = 1 to t2                                                                
    dBlocks = ''                                                                
    dClass  = ''                                                                
    mClass  = ''                                                                
    sClass  = ''                                                                
                                                                                
    Select                                                                      
      When (Typet2.i.dsnType = 'LIBRARY') Then dsType = '0'                     
      When (Typet2.i.dsnType = 'PDS') Then                                      
      Do                                                                        
        dsType = '3'                                                            
        dBlocks = 'directoryBlocks="'Strip(Typet2.i.dirBlocks)'"'               
      End                                                                       
      Otherwise                                                                 
        Nop                                                                     
    End                                                                         
                                                                                
    If Typet2.i.dataClass <> '**None**' Then                                    
      dClass = 'dataClass="'Strip(Typet2.i.dataClass)'"'                        
    If Typet2.i.mgmtClass <> '**None**' Then                                    
      mClass = 'managementClass="'Strip(Typet2.i.mgmtClass)'"'                  
    If Typet2.i.stgClass <> '**None**' Then                                     
      sClass  = 'storageClass="'Strip(Typet2.i.stgClass)'"'                     
                                                                                
    xcnt = xcnt + 1                                                             
    xml.xcnt = '    <ld:dsdef dsDefUsageType="1" dsName="'Strip(Typet2.i)'" '||,
               'name="${resource.def.prefix}'Strip(Typet2.i) ||,                
                     '${resource.def.suffix}" ' ||,                             
               'dsType="'dsType'" ' ||,                                         
               'spaceUnits="'Strip(Typet2.i.priSpaceU)'" ' ||,                  
               'primaryQuantity="'Strip(Typet2.i.priSpaceA)'" ' ||,             
               'secondaryQuantity="'Strip(Typet2.i.secSpaceA)'" ' ||,           
               dBlocks' ' ||,                                                   
               'recordFormat="'Strip(Typet2.i.recfm)'" ' ||,                    
               'recordLength="'Strip(Typet2.i.lrecl)'" ' ||,                    
               'blockSize="'Strip(Typet2.i.blksize)'" '  ||,                    
               dClass mClass sClass ||,                                         
               '/>'                                                             
  End                                                                           
                                                                                
  /* Type 3 */                                                                  
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  '                                                               
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <!-- Existing data sets -->'                                  
  Do i = 1 to t3                                                                
    parse var Typet3.i dsname'('dsmemb')'                                       
    xcnt = xcnt + 1                                                             
    xml.xcnt = '    <ld:dsdef dsDefUsageType="3" dsName="'dsname'" ' ||,        
                   'dsMember="'dsmemb'" ' ||,                                   
                   'name="${resource.def.prefix}'Typet3.i ||,                   
                         '${resource.def.suffix}" ' ||,                         
                   'volumeSerial="'Typet3.i.Volume'" prefixDSN="false"/>'       
  End                                                                           
                                                                                
  /* Type 4 */                                                                  
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  '                                                               
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <!-- Temporary data sets -->'                                 
  Do i = 1 to t4                                                                
    dsType = '9'                                                                
    xcnt = xcnt + 1                                                             
    Parse var Typet4.i firstBit '&&' tempDD                                     
    If tempDD <> '' Then                                                        
      tempDD = '&amp;&amp;'tempDD                                               
    Typet4.i = firstBit||tempDD                                                 
    xml.xcnt = '    <ld:dsdef dsDefUsageType="2" dsName="'tempDD'" '||,         
               'name="${resource.def.prefix}'Strip(Typet4.i) ||,                
                     '${resource.def.suffix}" ' ||,                             
               'prefixDSN="false" ' ||,                                         
               'dsType="'dsType'" ' ||,                                         
               'spaceUnits="'Strip(Typet4.i.priSpaceU)'" ' ||,                  
               'primaryQuantity="'Strip(Typet4.i.priSpaceA)'" ' ||,             
               'secondaryQuantity="'Strip(Typet4.i.secSpaceA)'" ' ||,           
               'recordFormat="'Strip(Typet4.i.recfm)'" ' ||,                    
               'recordLength="'Strip(Typet4.i.lrecl)'" ' ||,                    
               'blockSize="'Strip(Typet4.i.blksize)'" '  ||,                    
               'genericUnit="'Strip(Typet4.i.vio)'" '  ||,                      
               '/>'                                                             
  End                                                                           
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt   = '  </target>'                                                    
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML translators                                               */             
/*---------------------------------------------------------------*/             
xmlTrans :                                                                      
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  '                                                               
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  <!-- Create translators -->'                                    
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  <target name="translators" description="Create translators.">'  
                                                                                
  rc = repLine('I' Copies('-',80))                                              
  rc = repLine(' ' ' ')                                                         
  rc = repLine('I' 'Complete list of translators created')                      
  rc = repLine(' ' ' ')                                                         
  rc = repLine('I' Copies('-',80))                                              
  rc = repLine(' ' ' ')                                                         
                                                                                
  Do i = 1 to tr                                                                
    If trans.i.duptran <> '' Then                                               
    Do                                                                          
      rc = repLine('I' 'Translator 'i' - "'trans.i'" ' ||,                      
                         'Duplicate of - "'trans.i.duptran'"')                  
      Iterate                                                                   
    End                                                                         
    Else                                                                        
      rc = repLine('I' 'Translator 'i' - "'trans.i)                             
                                                                                
    ddNames = ''                                                                
    If trans.i.ddNameList <> '' Then                                            
      ddNames = 'ddnamelist="'trans.i.ddNameList'" '                            
                                                                                
    Parse var trans.i.parmx 'PARM' parmNum                                      
    trans.i.parmx = 'PARMX'parmNum                                              
    /* NOTE : If there is no PARMX then we set the variable to 'PARMX' */       
                                                                                
    linkedit = ''                                                               
    If trans.i.binder <> '' Then                                                
    Do                                                                          
      linkedit = 'linkedit="'trans.i.binder'" '                                 
      defaultOptions = ''                                                       
      Select                                                                    
        When (trans.i.options <> '' & trans.i.parmx <> 'PARMX') Then            
        Do                                                                      
          trans.i.options = chkmem1(trans.i.options)                            
          trans.i.parmx   = chkmem1(trans.i.parmx)                              
          defaultOptions = 'defaultOptions="'trans.i.options',' ||,             
                          'SSI(@{ssi_info}),&amp;'trans.i.parmx'" '             
        End                                                                     
        When (trans.i.options <> '' & trans.i.parmx = 'PARMX') Then             
        Do                                                                      
          trans.i.options = chkmem1(trans.i.options)                            
          defaultOptions = 'defaultOptions="'trans.i.options',' ||,             
                          'SSI(@{ssi_info})" '                                  
        End                                                                     
        When (trans.i.options = '' & trans.i.parmx <> 'PARMX') Then             
        Do                                                                      
          trans.i.parmx   = chkmem1(trans.i.parmx)                              
          defaultOptions = 'defaultOptions="&amp;'trans.i.parmx ||,             
                                         ',SSI(@{ssi_info})" '                  
        End                                                                     
        Otherwise                                                               
          Nop                                                                   
      End                                                                       
    End                                                                         
    Else                                                                        
    Do                                                                          
      defaultOptions = ''                                                       
      Select                                                                    
        When (trans.i.options <> '' & trans.i.parmx <> 'PARMX') Then            
        Do                                                                      
          trans.i.options = chkmem1(trans.i.options)                            
          trans.i.parmx   = chkmem1(trans.i.parmx)                              
          defaultOptions = 'defaultOptions="'trans.i.options',' ||,             
                                          '&amp;'trans.i.parmx'" '              
        End                                                                     
        When (trans.i.options <> '' & trans.i.parmx = 'PARMX') Then             
        Do                                                                      
          trans.i.options = chkmem1(trans.i.options)                            
          defaultOptions = 'defaultOptions="'trans.i.options'" '                
        End                                                                     
        When (trans.i.options = '' & trans.i.parmx <> 'PARMX') Then             
        Do                                                                      
          trans.i.parmx   = chkmem1(trans.i.parmx)                              
          defaultOptions = 'defaultOptions="&amp;'trans.i.parmx'" '             
        End                                                                     
        Otherwise                                                               
          Nop                                                                   
      End                                                                       
    End                                                                         
                                                                                
    xcnt = xcnt + 1                                                             
    If trans.i.callMethod = '0' Then                                            
      xml.xcnt = '    <ld:translator callMethod="'trans.i.callMethod'" ' ||,    
                 'name="${resource.def.prefix}'trans.i ||,                      
                       '${resource.def.suffix}" ' ||,                           
                 'dataSetDefinition="${resource.def.prefix}'trans.i.dsdef ||,   
                                    '${resource.def.suffix}" ' ||,              
                 defaultOptions ||,                                             
                 ddNames  ||,                                                   
                 linkedit ||,                                                   
                 'maxRC="'trans.i.maxrc'" ' ||,                                 
                 '>'                                                            
    Else                                                                        
    Do                                                                          
      trans.i.commandMember = chkmem1(trans.i.commandMember)                    
      xml.xcnt = '    <ld:translator callMethod="'trans.i.callMethod'" ' ||,    
                 'name="${resource.def.prefix}'trans.i ||,                      
                       '${resource.def.suffix}" ' ||,                           
                 'commandMember="'trans.i.commandMember'" ' ||,                 
                 'maxRC="'trans.i.maxrc'" ' ||,                                 
                 '>'                                                            
    End                                                                         
                                                                                
    /* Concatonations */                                                        
    doConcat = 'CC'                                                             
    Do j = 1 to trans.i.DDcnt                                                   
      If trans.i.j.ddtype = doConcat then                                       
      Do                                                                        
        xcnt = xcnt + 1                                                         
        xml.xcnt = '      <ld:concatenation name="'trans.i.j.ddname'">'         
        If trans.i.binder <> '' & j = 4 Then                                    
        Do                                                                      
          /* This is a binder SYSLIB, so add the OBJ types */                   
          Do k = 1 to lecCnt                                                    
            xcnt = xcnt + 1                                                     
            xml.xcnt = '        <ld:allocation ' ||,                            
                       'dataSetDefinition="${resource.def.prefix}' ||,          
                                          lecSys.k                 ||,          
                                          '${resource.def.suffix}"/> '          
          End                                                                   
        End                                                                     
        Do k = 1 to trans.i.j.DSNcnt                                            
          xcnt = xcnt + 1                                                       
          xml.xcnt = '        <ld:allocation ' ||,                              
                     'dataSetDefinition="${resource.def.prefix}' ||,            
                                        trans.i.j.k.dsdef        ||,            
                                        '${resource.def.suffix}"/> '            
        End                                                                     
        xcnt = xcnt + 1                                                         
        xml.xcnt = '      </ld:concatenation>'                                  
      End                                                                       
    End                                                                         
                                                                                
    /* Normal DDs     */                                                        
    doConcat = 'DD'                                                             
    varcnt = 0                                                                  
    Drop var.                                                                   
    ddvcnt = 0                                                                  
    Drop ddVar.                                                                 
    Do j = 1 to trans.i.DDcnt                                                   
      If trans.i.j.ddtype = doConcat &,                                         
         trans.i.j.ddname <> 'Not Applicable' Then                              
      Do                                                                        
        Do k = 1 to trans.i.j.DSNcnt                                            
          Parse var trans.i.j.k.dsdef firstBit '&&' tempDD                      
          If tempDD <> '' Then                                                  
            tempDD = '&amp;&amp;'tempDD                                         
          trans.i.j.k.dsdef = firstBit||tempDD                                  
                                                                                
          dataSetDef = ''                                                       
          If trans.i.j.k.dsdef <> '' Then                                       
            dataSetDef = 'dataSetDefinition="${resource.def.prefix}' ||,        
                                            trans.i.j.k.dsdef        ||,        
                                            '${resource.def.suffix}" '          
                                                                                
          inStreamDD = ''                                                       
          If trans.i.j.k.instream <> '' Then                                    
            inStreamDD = 'instream="'trans.i.j.k.instream'" '                   
                                                                                
          appendMember = ''                                                     
          If trans.i.j.k.member  <> '' Then                                     
            appendMember = 'member="'trans.i.j.k.member'" '                     
                                                                                
          outputMember = ''                                                     
          If trans.i.j.k.output  <> '' Then                                     
            outputMember = 'output="'trans.i.j.k.output'" '                     
                                                                                
          outputPattern = ''                                                    
          If trans.i.j.k.outputName <> '' Then                                  
          Do                                                                    
            outputPattern = 'outputNameKind="USE_VARIABLE" ' ||,                
                            'outputName="'trans.i.j.k.outputName'" '            
            varcnt = varcnt + 1                                                 
            var.varcnt = trans.i.j.k.outputName                                 
          End                                                                   
          Else                                                                  
          Do                                                                    
            If trans.i.j.k.pattern <> '' Then                                   
              outputPattern = 'outputNameKind="USE_PATTERN" ' ||,               
                              'outputName="'trans.i.j.k.pattern'" '             
          End                                                                   
                                                                                
          publishOutput = ''                                                    
          If trans.i.j.k.publish <> '' Then                                     
            publishOutput = 'publish="'trans.i.j.k.publish'" '                  
                                                                                
          DDcondition = ''                                                      
          If trans.i.j.k.condition <> '' Then                                   
          Do                                                                    
            Parse var trans.i.j.k.condition 'name=' varName ' value=' varValue  
            varName = '@{var.Tran'i'.'varName'}'                                
            DDcondition = 'condition="&lt;equals ' ||,                          
                          'arg1=&quot;'varName'&quot; '   ||,                   
                          'arg2=&quot;'varValue'&quot;/&gt;"'                   
                                                                                
            /* Need to store varNames to create variables at the end */         
            /* Only store it for one of the conditions               */         
            Do z = 1 to ddvcnt While (varName <> ddVar.z.Name)                  
            End                                                                 
            If z > ddvcnt Then                                                  
            Do                                                                  
              ddvcnt = ddvcnt + 1                                               
              ddVar.ddvcnt.Name = varName                                       
              ddVar.ddvcnt.defVal = trans.i.j.k.conddefVal                      
            End                                                                 
          End                                                                   
                                                                                
          keepAlloc = ''                                                        
          If trans.i.j.k.keep <> '' Then                                        
            keepAlloc = 'keep="'trans.i.j.k.keep'" '                            
                                                                                
          hfsOptions = ''                                                       
          If trans.i.j.k.hfsopts <> '' Then                                     
            hfsOptions = trans.i.j.k.hfsopts' '                                 
                                                                                
          If trans.i.j.k.dsdef = '<INPUT>' Then                                 
          Do                                                                    
            xcnt = xcnt + 1                                                     
            xml.xcnt = '      <ld:allocation ' ||,                              
                         'name="'trans.i.j.ddname'" ' ||,                       
                         'input="true"/>'                                       
          End                                                                   
          Else                                                                  
          Do                                                                    
            xcnt = xcnt + 1                                                     
            xml.xcnt = '      <ld:allocation ' ||,                              
                         'name="'trans.i.j.ddname'" ' ||,                       
                         dataSetDef    ||,                                      
                         inStreamDD    ||,                                      
                         appendMember  ||,                                      
                         outputMember  ||,                                      
                         outputPattern ||,                                      
                         publishOutput ||,                                      
                         DDcondition   ||,                                      
                         keepAlloc     ||,                                      
                         hfsOptions    ||,                                      
                         '/>'                                                   
          End                                                                   
        End                                                                     
      End                                                                       
    End                                                                         
                                                                                
    /* Create a translator variable for parmx if it exists */                   
    If trans.i.callMethod = '0' &,                                              
       trans.i.parmx <> 'PARMX' Then                                            
    Do                                                                          
      xcnt = xcnt + 1                                                           
      xml.xcnt = '    <ld:variable name="'trans.i.parmx'" '||,                  
                      'value=""/>'                                              
    End                                                                         
                                                                                
    /* Create a translator variable for parm  if it exists */                   
    If trans.i.callMethod = '0' &,                                              
       trans.i.parm <> '' Then                                                  
    Do                                                                          
      xcnt = xcnt + 1                                                           
      xml.xcnt = '    <ld:variable name="'trans.i.parm'" '||,                   
                      'value=""/>'                                              
    End                                                                         
                                                                                
    /* Create a translator variables for DD conditions     */                   
    Do v = 1 to ddvcnt                                                          
      parse var ddVar.v.Name '@{var.'step'.'varName'}'                          
      xcnt = xcnt + 1                                                           
      xml.xcnt = '    <ld:variable name="'varName'" '||,                        
                      'value="'ddVar.v.defVal'"/>'                              
    End                                                                         
                                                                                
    /* now create variables for output member name  */                          
    Do v = 1 to varcnt                                                          
      xcnt = xcnt + 1                                                           
      xml.xcnt = '    <ld:variable name="'var.v'" value="*"/>'                  
    End                                                                         
                                                                                
    xcnt = xcnt + 1                                                             
    xml.xcnt = '    </ld:translator>'                                           
  End                                                                           
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt   = '  </target>'                                                    
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML Language Definitions                                      */             
/*---------------------------------------------------------------*/             
xmlLangs :                                                                      
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  '                                                               
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  <!-- Create language definitions -->'                           
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  <target name="langdefs" ' ||,                                   
                       'description="Create language definitions">'             
                                                                                
  /* Loop through one to take a punt at the copybook languages        */        
  Do i = 1 to ld                                                                
    tranNumbers = Translate(langs.i.translators,' ',',')                        
    If (Words(tranNumbers) = 0 | Word(tranNumbers,1) = tr) &,                   
        langs.i.languageCode <> 'LNK' Then                                      
      Call DoLangsXML                                                           
  End                                                                           
  /* Loop through a 2nd time to take a punt at the other languages    */        
  Do i = 1 to ld                                                                
    tranNumbers = Translate(langs.i.translators,' ',',')                        
    If (Words(tranNumbers) <> 0 & Word(tranNumbers,1) <> tr) &,                 
        langs.i.languageCode = 'OTH' Then                                       
      Call DoLangsXML                                                           
  End                                                                           
  /* Loop through a 3rd time to take a punt at the ASM languages      */        
  Do i = 1 to ld                                                                
    tranNumbers = Translate(langs.i.translators,' ',',')                        
    If (Words(tranNumbers) <> 0 & Word(tranNumbers,1) <> tr) &,                 
        langs.i.languageCode = 'ASM' Then                                       
      Call DoLangsXML                                                           
  End                                                                           
  /* Loop through a 4th time to take a punt at the compiler languages */        
  Do i = 1 to ld                                                                
    tranNumbers = Translate(langs.i.translators,' ',',')                        
    If (Words(tranNumbers) <> 0 & Word(tranNumbers,1) <> tr) &,                 
       (langs.i.languageCode <> 'LNK' & langs.i.languageCode <> 'ASM' &,        
        langs.i.languageCode <> 'OTH') Then                                     
      Call DoLangsXML                                                           
  End                                                                           
  /* Loop through a 5th time to take a punt at the binder languages   */        
  Do i = 1 to ld                                                                
    tranNumbers = Translate(langs.i.translators,' ',',')                        
    If langs.i.languageCode = 'LNK' Then                                        
      Call DoLangsXML                                                           
  End                                                                           
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt   = '  </target>'                                                    
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML Language Definitions - build XML                          */             
/*---------------------------------------------------------------*/             
DoLangsXML :                                                                    
                                                                                
  DTL = 0                                                                       
  Select                                                                        
    When (langs.i.languageCode = 'ASM' |,                                       
          langs.i.languageCode = 'COB' |,                                       
          langs.i.languageCode = 'PLI' |,                                       
          langs.i.languageCode = 'C')  Then                                     
    Do                                                                          
      scanner = 'com.ibm.teamz.metadata.scanner.default'                        
    End                                                                         
    When (langs.i.languageCode = 'REX' |,                                       
          langs.i.languageCode = 'PAS')  Then                                   
    Do                                                                          
      langs.i.languageCode = 'OTH'                                              
      scanner = 'com.ibm.teamz.metadata.scanner.default'                        
    End                                                                         
    When (langs.i.languageCode = 'DTL') Then                                    
    Do                                                                          
      langs.i.languageCode = 'OTH'                                              
      DTL = 1                                                                   
      scanner = 'com.ibm.teamz.scanner.dtlscanner.id'                           
    End                                                                         
    Otherwise                                                                   
      scanner = 'com.ibm.team.enterprise.scanner.registration.scanner'          
  End                                                                           
                                                                                
  /* Need to change translator numbers to names */                              
  tranWords = ''                                                                
  tranNumbers = Translate(langs.i.translators,' ',',')                          
  Do j = 1 to Words(tranNumbers)                                                
    tn = Word(tranNumbers,j)                                                    
    tranWords = tranWords || '${resource.def.prefix}'trans.tn ||,               
                             '${resource.def.suffix},'                          
  End                                                                           
                                                                                
  iefbr14 = 0                                                                   
  /* If no translators add an IEFBR14 */                                        
  If Words(tranNumbers) = 0 Then                                                
  Do                                                                            
                                                                                
    iefbr14 = 1                                                                 
    langs.i.translators  = tr                                                   
    langs.i.languageCode = 'OTH'                                                
    tranWords = tranWords || '${resource.def.prefix}'trans.tr ||,               
                             '${resource.def.suffix},'                          
  End                                                                           
                                                                                
  tranWords = Strip(tranWords,T,',')                                            
                                                                                
  /* Need to add stepnames. As translators will probably have   */              
  /* some conditional processing and a translator might be      */              
  /* used by multiple language definitions, we need to use      */              
  /* the translator number in the stepname, and name every step */              
  stepNames = ''                                                                
  tranNumbers = Translate(langs.i.translators,' ',',')                          
  Do j = 1 to Words(tranNumbers)                                                
    tn = Word(tranNumbers,j)                                                    
    stepNames = stepNames || 'Tran'tn','                                        
  End                                                                           
  stepNames = 'stepNames="'Strip(stepNames,T,',')'" '                           
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <ld:langdef ' ||,                                             
                 'name="${resource.def.prefix}'langs.i ||,                      
                 '${resource.def.suffix}" ' ||,                                 
                 'translators="'tranWords'" ' ||,                               
                 stepNames ||,                                                  
                 'defaultExtension="" ' ||,                                     
                 'languageCode="'langs.i.languageCode'" ' ||,                   
                 'defaultscanner="false">'                                      
                                                                                
  If DTL Then                                                                   
  Do                                                                            
    xcnt = xcnt + 1                                                             
    xml.xcnt = '      <ld:dependencytype name="DTLINC"/>'                       
  End                                                                           
  Else                                                                          
  Do                                                                            
    depTran = ''                                                                
    depCnt  = 0                                                                 
    Do j = 1 to Words(tranNumbers)                                              
      tn = Word(tranNumbers,j)                                                  
      Do k = 1 to ns While (tn <> noSyslib.k)                                   
      End                                                                       
      If k > ns Then                                                            
      Do                                                                        
        /* Not found so add to list */                                          
        depCnt = depCnt + 1                                                     
        depTran = depTran || '${resource.def.prefix}'trans.tn ||,               
                             '${resource.def.suffix},'                          
      End                                                                       
    End                                                                         
                                                                                
    If depCnt = Words(tranNumbers) Then                                         
      depTran = ''                                                              
    Else                                                                        
    Do                                                                          
      depTran = Strip(depTran,T,',')                                            
      depTran = 'translators="'deptran'" '                                      
    End                                                                         
                                                                                
    xcnt = xcnt + 1                                                             
    xml.xcnt = '      <ld:dependencytype name="COPY" 'depTran'/>'               
    xcnt = xcnt + 1                                                             
    xml.xcnt = '      <ld:dependencytype name="SQL INCLUDE" 'depTran'/>'        
    xcnt = xcnt + 1                                                             
    xml.xcnt = '      <ld:dependencytype name="MACRO" 'depTran'/>'              
  End                                                                           
  xcnt = xcnt + 1                                                               
  xml.xcnt = '      <ld:scanner name="'scanner'"/>'                             
  If iefbr14 Then                                                               
  Do                                                                            
    xcnt = xcnt + 1                                                             
    xml.xcnt = '      <ld:scopedProperty ' ||,                                  
                      'name="team.enterprise.build.noBuildmap" value="true"/>'  
  End                                                                           
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    </ld:langdef>'                                                
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML Footer information                                        */             
/*---------------------------------------------------------------*/             
xmlFooter :                                                                     
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = ' <target depends="dsdefs,translators,langdefs"' ||,               
          ' description="full build" name="all"/>'                              
  xcnt = xcnt + 1                                                               
  xml.xcnt = '</project>'                                                       
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Generate the file metadata information                        */             
/*---------------------------------------------------------------*/             
fileMetaData :                                                                  
                                                                                
  membfile = outputDir'/members.xml'                                            
  Address syscall "readfile (membfile) member."                                 
  If rc <> 0 | projdefs.0 = 0 Then                                              
  Do                                                                            
    rc = repLine(' ' ' ')                                                       
    rc = repLine('E' 'Member information file 'membfile ||,                     
                        ' does not exist or is empty. ' ||,                     
                        'BLZMEMS must be run first')                            
    migRc = 8                                                                   
  End                                                                           
  If migRc = 0 Then                                                             
  Do                                                                            
                                                                                
    Drop xml.                                                                   
    xcnt = 0                                                                    
                                                                                
    Call xmlMetaHead                                                            
    Call xmlMetaFold                                                            
    Call xmlMetaLang                                                            
    Call xmlMetaFile                                                            
    Call xmlMetaFoot                                                            
  End                                                                           
                                                                                
  xml.0 = xcnt                                                                  
  xmlfile = outputDir'/fileMetaData.xml'                                        
  Address syscall "writefile (xmlfile) 755 xml."                                
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML Header information                                        */             
/*---------------------------------------------------------------*/             
xmlMetaHead :                                                                   
                                                                                
  xml.1  = '<?xml version="1.0"?>'                                              
  xml.2  = '<!--'                                                               
  xml.3  = '    Licensed Materials - Property of IBM'                           
  xml.4  = '    (c) Copyright IBM Corporation 2012,2013. All Rights Reserved.'  
  xml.5  = '    Note to U.S. Government Users Restricted Rights:'               
  xml.6  = '    Use, duplication or disclosure restricted by GSA ADP Schedule'  
  xml.7  = '    Contract with IBM Corp.'                                        
  xml.8  = ' -->'                                                               
  xml.9  = '<project name="'defzProject'" default="all" ' ||,                   
           'xmlns:ld="antlib:com.ibm.team.enterprise.zos.' ||,                  
           'systemdefinition.toolkit">'                                         
  xml.10 = '  <description>SCLM Migration system definition XML</description>'  
  xml.11 = ''                                                                   
  xml.12 = '  <property name="resource.def.prefix" value="'proj'-" />'          
  xml.13 = '  <property name="resource.def.suffix" value="" />'                 
  xml.14 = ''                                                                   
  xml.15 = '  <ld:init password="${password}" projectArea="${projectArea}" ' ||,
           'repositoryAddress="${repositoryAddress}" userId="${userId}"/>'      
  xml.16 = ''                                                                   
  xml.17 = '  <target name="resolvemetadata">'                                  
  xml.18 = ''                                                                   
                                                                                
  xcnt = 18                                                                     
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML zFolder metadata                                          */             
/*---------------------------------------------------------------*/             
xmlMetaFold:                                                                    
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <!-- zFolder metadata -->'                                    
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <ld:resolvemetadata projectRoot="../'defzProject'">'          
                                                                                
  Do i = 1 to t1                                                                
    xcnt = xcnt + 1                                                             
    xml.xcnt = '      <ld:dsdefrule match="'Strip(Typet1.i)'" '||,              
               'dataSetDefinition="${resource.def.prefix}'Strip(Typet1.i) ||,   
               '${resource.def.suffix}"/>'                                      
  End                                                                           
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = ''                                                                 
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML language metadata                                         */             
/*---------------------------------------------------------------*/             
xmlMetaLang:                                                                    
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <!-- Language metadata -->'                                   
                                                                                
  Do i = 1 to member.0                                                          
    If Pos('<member name=',member.i) <> 0 Then                                  
    Do                                                                          
                                                                                
      Parse var member.i '<member name="'membname'"' . ,                        
                         'type="'type'"' . 'language="'lang'">'                 
                                                                                
      Parse var membname membname'.'extension                                   
                                                                                
      Do ll = 1 to ld While (lang <> langs.ll)                                  
      End                                                                       
      If ll > ld Then                                                           
      Do                                                                        
        rc = repLine(' ' ' ')                                                   
        rc = repLine('E' 'Language Definition - 'lang' for member '  ||,        
                         membname' was not found. Skipping processing ' ||,     
                         'for member 'membname'.'extension)                     
        Iterate                                                                 
      End                                                                       
                                                                                
      /* Going to check if the user has changed the ext in the langexts file */ 
      Do j = 1 to langdefs.0                                                    
        If langdefs.j  = lang & langdefs.j.dfltsrc = type Then                  
          extension = langdefs.j.ext                                            
      End                                                                       
                                                                                
      xcnt = xcnt + 1                                                           
      xml.xcnt = '      <ld:langdefrule match=".*/'membname'\.'extension'$" '||,
                   'languageDefinition="${resource.def.prefix}'lang ||,         
                   '${resource.def.suffix}"/>'                                  
    End                                                                         
  End                                                                           
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    </ld:resolvemetadata>'                                        
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML File meta data                                            */             
/*---------------------------------------------------------------*/             
xmlMetaFile:                                                                    
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <!-- File override metadata -->'                              
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    <ld:filemetadata projectRoot="../'defzProject'">'             
                                                                                
  Do i = 1 to member.0                                                          
    If Pos('<member name=',member.i) <> 0 Then                                  
    Do                                                                          
      Parse var member.i '<member name="'membname'"' . 'language="'lang'">'     
      Parse var membname membname'.'extension                                   
                                                                                
      Do ll = 1 to ld While (lang <> langs.ll)                                  
      End                                                                       
      If ll > ld Then                                                           
      Do                                                                        
        rc = repLine(' ' ' ')                                                   
        rc = repLine('E' 'Language Definition - 'lang' for member '  ||,        
                         membname' was not found. Skipping processing ' ||,     
                         'for member 'membname'.'extension)                     
        Iterate                                                                 
      End                                                                       
                                                                                
      i = i + 1                                                                 
      Do i = i to member.0,                                                     
                 While (Pos('<member name=',member.i) = 0 &,                    
                        Pos('</member>',member.i) = 0)                          
        If Pos('<keyword name=',member.i) <> 0 Then                             
        Do                                                                      
          /* Found a keyword. Need to see how many types these are */           
          Parse var member.i '<keyword name="'keyWord'">'                       
          Select                                                                
            /* PARM keyword  */                                                 
            When (keyWord = 'PARM') Then                                        
            Do                                                                  
              /* In SCLM the PARM keyword is appended to the option in */       
              /* all translators. Need to create an options variable   */       
              /* for the member to override the defaults.              */       
              i = i + 1                                                         
              Parm = ''                                                         
              Do i = i to member.0,                                             
                       While (Pos('<keyword name=',member.i) = 0 &,             
                              Pos('<member name=',member.i) = 0 &,              
                              Pos('</keyword>',member.i) = 0 &,                 
                              Pos('</member>',member.i) = 0)                    
                If Pos('<values ',member.i) <> 0 Then                           
                Do                                                              
                  Parse var member.i '<values 'name'="'value'"/>'               
                  langtrans = Translate(langs.ll.translators,' ',',')           
                  Do j = 1 to Words(langtrans)                                  
                    tt = Word(langtrans,j)                                      
                    If trans.tt.callMethod = '0' Then                           
                    Do                                                          
                      stepName = 'Tran'tt                                       
                      xcnt = xcnt + 1                                           
                      xml.xcnt = '      <ld:filemetadatarule ' ||,              
                             'match=".*/'membname'\.'extension'$" '||,          
                             'name="team.enterprise.build.var.' ||,             
                             'PARM'||tt'" ' ||,                                 
                             'value="'value'"/>'                                
                                                                                
                      /* SCLM adds PARM after options before PARMx */           
                      If Pos('PARM'tt,trans.tt.options) = 0 Then                
                      Do                                                        
                        trans.tt.options = trans.tt.options',&PARM'tt           
                        trans.tt.parm    = 'PARM'tt                             
                      End                                                       
                    End                                                         
                    Else                                                        
                    Do                                                          
                      rc = repLine(' ' ' ')                                     
                      rc = repLine('W' 'Member ' membname 'has PARM override'||,
                                       ' for TSO/ISPF translator. This is'||,   
                                       ' not currently supported in the'  ||,   
                                       ' migration.')                           
                    End                                                         
                  End                                                           
                End                                                             
              End                                                               
            End                                                                 
            /* PARMx keyword */                                                 
            When (Substr(keyWord,1,4) = 'PARM') Then                            
            Do                                                                  
              /* In SCLM the PARMx keyword is appended after the PARM  */       
              /* In the migration we have created a separate PARMx     */       
              /* variable to maintain similarity to SCLM.              */       
              i = i + 1                                                         
              Do i = i to member.0,                                             
                       While (Pos('<keyword name=',member.i) = 0 &,             
                              Pos('<member name=',member.i) = 0 &,              
                              Pos('</keyword>',member.i) = 0 &,                 
                              Pos('</member>',member.i) = 0)                    
                If Pos('<values ',member.i) <> 0 Then                           
                Do                                                              
                  Parse var member.i '<values 'name'="'value'"/>'               
                  langtrans = Translate(langs.ll.translators,' ',',')           
                  Do j = 1 to Words(langtrans)                                  
                    tt = Word(langtrans,j)                                      
                    /* Translator contains the PARMx value */                   
                    If keyWord = trans.tt.parmx Then                            
                    Do                                                          
                      Parse var keyWord 'PARM' parmNum                          
                      keyWord = 'PARMX'parmNum                                  
      /*              trans.tt.parmx = keyWord */                               
                      stepName = 'Tran'tt                                       
                      xcnt = xcnt + 1                                           
                      xml.xcnt = '      <ld:filemetadatarule ' ||,              
                             'match=".*/'membname'\.'extension'$" '||,          
                             'name="team.enterprise.build.var.'keyWord'" ' ||,  
                             'value="'value'"/>'                                
                    End                                                         
                  End                                                           
                End                                                             
              End                                                               
            End                                                                 
            Otherwise                                                           
            Do                                                                  
              /* Need to get the translator number to build the step name */    
              /* Need to see which translator is using this variable      */    
              langtrans = Translate(langs.ll.translators,' ',',')               
              Do j = 1 to Words(langtrans)                                      
                tt = Word(langtrans,j)                                          
                keyNotFound = 1                                                 
                Do k = 1 to trans.tt.DDcnt While (keyNotFound)                  
                  Do l = 1 to trans.tt.k.DSNcnt While (keyNotFound)             
                                                                                
                    If Pos(keyword'TYPE',trans.tt.k.l.condition) <> 0 Then      
                    Do                                                          
                      keyNotFound = 0                                           
                      stepName = 'Tran'tt                                       
                      dflttyp = trans.tt.k.l.conddefVal                         
                    End                                                         
                  End                                                           
                End                                                             
              End                                                               
              i = i + 1                                                         
              Do i = i to member.0,                                             
                       While (Pos('<keyword name=',member.i) = 0 &,             
                              Pos('<member name=',member.i) = 0 &,              
                              Pos('</keyword>',member.i) = 0 &,                 
                              Pos('</member>',member.i) = 0)                    
                If Pos('<values ',member.i) <> 0 Then                           
                Do                                                              
                  Parse var member.i '<values 'name'="'value'"/>'               
                  Select                                                        
                    /* We have a conditional type */                            
                    When (name = 'type' & keyNotFound = 0) Then                 
                    Do                                                          
                      /* Only need to create a variable for non default */      
                      If value <> dflttyp Then                                  
                      Do                                                        
                        xcnt = xcnt + 1                                         
                        xml.xcnt = '      <ld:filemetadatarule match=".*/' ||,  
                                    membname'\.'extension'$" ' ||,              
                                   'name="team.enterprise.build.var.' ||,       
                                   keyWord'TYPE" value="'value'"/>'             
                                                                                
                      End                                                       
                    End                                                         
                    When (name = 'member' & value <> membname) Then             
                    Do                                                          
                      nextLine = i + 1                                          
                      Parse var member.nextLine '<values 'name'="'type'"/>'     
                      langtrans = Translate(langs.ll.translators,' ',',')       
                      Do j = 1 to Words(langtrans)                              
                        tt = Word(langtrans,j)                                  
                        memNotFound = 1                                         
                        Do k = 1 to trans.tt.DDcnt While (memNotFound)          
                          Do l = 1 to trans.tt.k.DSNcnt While (memNotFound)     
                            If type = trans.tt.k.l.dsdef Then                   
                            Do                                                  
                              memNotFound = 0                                   
                              stepName = 'Tran'tt                               
                              If trans.tt.k.l.outputName <> '' Then             
                              Do                                                
                                xcnt = xcnt + 1                                 
                                xml.xcnt = '      <ld:filemetadatarule ' ||,    
                                       'match=".*/'membname'\.'extension'$" '||,
                                       'name="team.enterprise.build.var.' ||,   
                                       trans.tt.k.l.outputName'" ' ||,          
                                       'value="'value'"/>'                      
                              End                                               
                              Else                                              
                              Do                                                
                                /* Need to update the translator to have */     
                                /* a variable                            */     
                                trans.tt.k.l.outputName = 'OUT'keyWord||tt      
                                xcnt = xcnt + 1                                 
                                xml.xcnt = '      <ld:filemetadatarule ' ||,    
                                       'match=".*/'membname'\.'extension'$" '||,
                                       'name="team.enterprise.build.var.' ||,   
                                       'OUT'keyWord||tt'" ' ||,                 
                                       'value="'value'"/>'                      
                                rc = repLine(' ' ' ')                           
                                rc = repLine('I' 'Translator - 'trans.tt,       
                                       'DD - 'trans.tt.k.ddname,                
                                       ' has an ARCHDEF override for an ' ||,   
                                       ' output Member name.' ||,               
                                       ' Created a translator variable '  ||,   
                                       ' of "'outname'" to handle output name.')
                              End                                               
                            End                                                 
                          End                                                   
                        End                                                     
                      End                                                       
                    End                                                         
                    Otherwise                                                   
                      Nop                                                       
                  End                                                           
                End                                                             
              End                                                               
            End                                                                 
          End                                                                   
        End                                                                     
      End                                                                       
    End                                                                         
  End                                                                           
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '    </ld:filemetadata>'                                           
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* XML Footer information                                        */             
/*---------------------------------------------------------------*/             
xmlMetaFoot :                                                                   
                                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = '  </target>'                                                      
  xcnt = xcnt + 1                                                               
  xml.xcnt = ' '                                                                
  xcnt = xcnt + 1                                                               
  xml.xcnt = ' <target depends="resolvemetadata"' ||,                           
          ' description="full build" name="all"/>'                              
  xcnt = xcnt + 1                                                               
  xml.xcnt = '</project>'                                                       
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Add IEFBR14 dsdef and translator                              */             
/*---------------------------------------------------------------*/             
                                                                                
doiefbr14:                                                                      
                                                                                
  Do ty = 1 to t3 While('IEFBR14' <> typet3.ty)                                 
  End                                                                           
  If ty > t3 Then                                                               
  Do                                                                            
    t3 = t3 + 1                                                                 
    Typet3.t3 = '(IEFBR14)'                                                     
                                                                                
    tr = tr + 1                                                                 
    trans.tr = 'Do nothing translator'                                          
    trans.tr.callMethod = '0'                                                   
    trans.tr.dsdef = Typet3.t3                                                  
    trans.tr.commandMember = ''                                                 
    trans.tr.maxRc   = '0'                                                      
    trans.tr.porder  = ''                                                       
    trans.tr.options = ''                                                       
    trans.tr.parmx   = ''                                                       
    trans.tr.parm    = ''                                                       
    trans.tr.binder  = ''                                                       
    trans.tr.DDcnt = 0                                                          
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/*---------------------------------------------------------------*/             
/* Convert dodgy XML characters                                  */             
/*---------------------------------------------------------------*/             
                                                                                
chkmem1: Procedure                                                              
                                                                                
  /* change characters that may cause problems in the xml */                    
                                                                                
  Parse arg string                                                              
  Quote = '"'                                                                   
  Apos  = "'"                                                                   
  Dodgychars = '<&>'Quote||Apos                                                 
                                                                                
  newString = ''                                                                
                                                                                
  Do mm = 1 to Length(string)                                                   
    char = Substr(string,mm,1)                                                  
    If Pos(char,Dodgychars) > 0 Then                                            
    Do                                                                          
      Select                                                                    
        When (char = '<') Then newString = newString||'&lt;'                    
        When (char = '&') Then newString = newString||'&amp;'                   
        When (char = '>') Then newString = newString||'&gt;'                    
        When (char = '"') Then newString = newString||'&quot;'                  
        When (char = "'") Then newString = newString||'&apos;'                  
        Otherwise                                                               
          Nop                                                                   
      End                                                                       
                                                                                
    End                                                                         
    Else                                                                        
      newString = newString||char                                               
  End                                                                           
                                                                                
Return newString                                                                
                                                                                
/*---------------------------------------------------------------*/             
/* Create debug lines                                            */             
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
