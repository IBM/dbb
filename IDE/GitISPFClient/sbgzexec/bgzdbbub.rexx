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
/*                                                                   */         
/* MDalb 04/17/20 Enhancements to support zAppBuild                  */         
/*                                                                   */         
/*********************************************************************/         
   Parse Arg BGZUSREP BGZUSLOC BGZUSSDR BGZUSFIL                                
   /* Need to create a permanent ISPF table to store dbb user build options */  
   Address ISPEXEC                                                              
/* 'TBERASE BGZDBBUB'                                                           
   'TBERASE BGZPROPS' */                                                        
   'TBOPEN BGZDBBUB'                                                            
   /* If it didn't exist yet, create it. */                                     
   If RC = 8 Then                                                               
   Do                                                                           
     'TBCREATE BGZDBBUB',                                                       
     'KEYS(BGZUSREP,BGZUSLOC)',                                                 
     'NAMES(BGZBLSCR,BGZBLSAN,BGZBLWRK,BGZBLHLQ,BGZBLLOG) WRITE'                
   End                                                                          
   'VGET (BGZJAVAH,BGZDBBH) SHARED'                                             
   groovyz = BGZDBBH'/bin/groovyz'                                              
   x = lastPos('/',BGZUSLOC)                                                    
   sandbox = Substr(BGZUSLOC,1,x-1)                                             
   appname = Substr(BGZUSLOC,x+1)                                               
   fullPath = BGZUSSDR'/'BGZUSFIL                                               
   relativePathIndex = Pos(appname, fullpath)                                   
   relativePath = Substr(fullpath, relativePathIndex)                           
/* SAY 'Sandbox' sandbox                                                        
   SAY 'Appname' appname                                                        
   SAY 'Fullpath' fullpath                                                      
   SAY 'RelativePath' relativePath                                              
   x = lastPos('/',BGZUSSDR)                                                    
   appname = Substr(BGZUSSDR,1,x-1)                                             
   builddir  = appname'/build'                                                  
   script  = builddir'/build.sh'     */                                         
/* 'TBVCLEAR BGZDBBUB' */                                                       
   BGZBLSCR = ''                                                                
   BGZBLSAN = sandbox                                                           
   BGZBLWRK = ''                                                                
   BGZBLHLQ = ''                                                                
   BGZBLLOG = '/'                                                               
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
       /* DBB user build options */                                             
       script  = BGZBLSCR                                                       
       sandbox = BGZBLSAN                                                       
       workdir = BGZBLWRK                                                       
       hlq     = BGZBLHLQ                                                       
       BGZFILE = relativePath                                                   
       shellcmd  = ''                                                           
       shellcmd  = shellcmd || 'cd' sandbox';'                                  
       shellcmd  = shellcmd || 'export JAVA_HOME='BGZJAVAH';'                   
       shellcmd  = shellcmd ||  groovyz script                                  
       /* Enter pressed without command S - navigate to Script Parameters */    
       If BGZCMD = '' Then                                                      
       Do                                                                       
         /* BGZPROPS table to be displayed on Script Parameters panel */        
         'TBOPEN BGZPROPS'                                                      
         /* If it didn't exist yet, create it. */                               
         If RC = 8 Then                                                         
         Do                                                                     
           'TBCREATE BGZPROPS',                                                 
           'KEYS(BGZUSREP,BGZUSLOC,BGZPROW)',                                   
           'NAMES(BGZPNAME,BGZPVAL,BGZPMCMD) WRITE'                             
         End                                                                    
/*       'TBCREATE BGZPROPS',                                                   
           'KEYS(BGZPROW) NAMES(BGZPNAME,BGZPVAL,BGZPMCMD)',                    
           'REPLACE NOWRITE' */                                                 
         'TBGET BGZPROPS'                                                       
         'TBSORT BGZPROPS FIELDS(BGZPROW)'                                      
         'TBTOP BGZPROPS'                                                       
         BGZPROW = '000001'                                                     
         'TBDELETE BGZPROPS'                                                    
         BGZPNAME = '--sourceDir'                                               
         BGZPVAL  = sandbox                                                     
         BGZPMCMD = ''                                                          
         'TBADD BGZPROPS ORDER'                                                 
         BGZPROW = '000002'                                                     
         'TBDELETE BGZPROPS'                                                    
         BGZPNAME = '--workDir'                                                 
         BGZPVAL  = workdir                                                     
         BGZPMCMD = ''                                                          
         'TBADD BGZPROPS ORDER'                                                 
         BGZPROW = '000003'                                                     
         'TBDELETE BGZPROPS'                                                    
         BGZPNAME = '--hlq'                                                     
         BGZPVAL  = hlq                                                         
         BGZPMCMD = ''                                                          
         'TBADD BGZPROPS ORDER'                                                 
         'TBBOTTOM BGZPROPS'                                                    
         nbparm = BGZPROW                                                       
         DoReq = 0                                                              
         PRPROW = '000001'                                                      
         Do Until DoReq > 0                                                     
           PRPROW = '000001'                                                    
           'TBTOP BGZPROPS'                                                     
           'TBSKIP BGZPROPS NUMBER('PRPROW')'                                   
           'TBDISPL BGZPROPS PANEL(BGZDBBPM)'                                   
           TB_RC = RC                                                           
           'VGET (ZVERB)'                                                       
           If TB_RC = 8 | ZVERB = 'CANCEL' Then                                 
           Do                                                                   
             DoReq = -1                                                         
             Leave                                                              
           End                                                                  
           If ZTDSELS = 0 Then                                                  
             PRPROW = ZTDTOP                                                    
           Do While ZTDSELS > 0                                                 
             'TBMOD BGZPROPS ORDER'                                             
             'TBGET BGZPROPS POSITION(PRPROW)'                                  
             If ZTDSELS = 1 Then                                                
               ZTDSELS = 0                                                      
             Else                                                               
               'TBDISPL BGZPROPS'                                               
           End                                                                  
           If ZVERB <> ' ' Then                                                 
             Iterate                                                            
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
             'TBSORT BGZPROPS FIELDS(BGZPROW)'                                  
           End                                                                  
           /* S command on BGZDBBPM panel  */                                   
           If BGZCMD = 'S' Then                                                 
           Do                                                                   
             DoReq = 1                                                          
             'TBTOP BGZPROPS'                                                   
             'TBSKIP BGZPROPS'                                                  
             Do While(RC = 0)                                                   
               shellcmd=shellcmd || '' BGZPNAME BGZPVAL                         
               'TBSKIP BGZPROPS'                                                
             End                                                                
             shellcmd=shellcmd || ' --userBuild' BGZFILE                        
             'TBTOP BGZPROPS'                                                   
             BGZPROW = '000001'                                                 
             'TBDELETE BGZPROPS'                                                
             BGZPROW = '000002'                                                 
             'TBDELETE BGZPROPS'                                                
             BGZPROW = '000003'                                                 
             'TBDELETE BGZPROPS'                                                
             If GetDBB_RC = 0 Then                                              
               'TBMOD BGZDBBUB'                                                 
             Else                                                               
               'TBADD BGZDBBUB'                                                 
             BGZFLOG = BGZBLWRK'/dbbub.log'                                     
             'VPUT (BGZFLOG) SHARED'                                            
             DBB_rc = BGZCMD('dbbub' shellcmd)                                  
             BGZCMD = ''                                                        
             Iterate                                                            
           End                                                                  
           'TBTOP BGZPROPS'                                                     
           'TBSKIP BGZPROPS POSITION(TEMPROW)'                                  
                                                                                
           Do While RC = 0                                                      
             If BGZPMCMD = '/' Then                                             
             Do                                                                 
               PRPROW = TEMPROW                                                 
               BGZPMCMD = GetPMCMD(BGZBLSCR)                                    
             End                                                                
             Select                                                             
               When BGZPMCMD = 'D' Then                                         
               Do                                                               
                 PRPROW = TEMPROW                                               
                 'TBDELETE BGZPROPS'                                            
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
                     'TBMOD BGZPROPS'                                           
                   End                                                          
                   Else                                                         
                   Do                                                           
                     'TBDELETE BGZPROPS'                                        
                     BGZPNAME = BGZPRNME                                        
                     BGZPVAL  = BGZPRVAL                                        
                     'TBADD BGZPROPS'                                           
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
               'TBMOD BGZPROPS'                                                 
             End                                                                
             'TBSKIP BGZPROPS POSITION(TEMPROW)'                                
           End                                                                  
         End                                                                    
         'TBCLOSE BGZPROPS'                                                     
       End                                                                      
       /* S command on BGZDBBUB panel  */                                       
       If BGZCMD = 'S' Then                                                     
       Do                                                                       
         DoReq = 1                                                              
         shellcmd=shellcmd || ' --sourceDir' sandbox                            
         shellcmd=shellcmd || ' --workDir' workdir                              
         shellcmd=shellcmd || ' --hlq' hlq                                      
         shellcmd=shellcmd || ' --userBuild' BGZFILE                            
         If GetDBB_RC = 0 Then                                                  
           'TBMOD BGZDBBUB'                                                     
         Else                                                                   
           'TBADD BGZDBBUB'                                                     
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
         UPPERCASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                               
         lowercase = 'abcdefghijklmnopqrstuvwxyz'                               
         input= filename                                                        
         input_upper = translate(input, uppercase, lowercase)                   
         filename = input_upper                                                 
         BGZUSLOG = filename'.log'                                              
         BGZFLOG = BGZBLWRK'/'BGZUSLOG                                          
         BGZEMIX = 'NO'                                                         
         'VGET (ZDBCS) SHARED'                                                  
         If ZDBCS = 'YES' THEN BGZEMIX = 'YES'                                  
         "CONTROL ERRORS RETURN"                                                
         "BROWSE File(BGZFLOG) MIXED("BGZEMIX")"                                
         BR_RC = RC                                                             
         "CONTROL ERRORS CANCEL"                                                
         If BR_RC = 20 Then                                                     
           'SETMSG MSG(BGZC039)'                                                
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
