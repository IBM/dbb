/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/*                                                                   */         
/* NAME := BGZCOMIT                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client git commit command            */         
/*                                                                   */         
/* FUNCTION :=                                                       */         
/*                                                                   */         
/*                                                                   */         
/* CALLED BY : BGZREPOS BGZUDLST                                     */         
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
/*                                                                   */         
/*********************************************************************/         
                                                                                
   Parse Arg BGZREPOS BGZUSDIR                                                  
   Address ISPEXEC                                                              
                                                                                
   /* Git commit command  */                                                    
   /* we need to request a git status first and parse the BGZTEMP               
      table returned to select only Modified files  */                          
   Git_rc = 0                                                                   
   'VGET BGZENVIR SHARED'                                                       
   /* Need to craeet a temporary ISPF table to hold info */                     
   'TBCREATE BGZTEMP KEYS(BGZROW),                                              
                     NAMES(BGZCMCMD,BGZLINE,BGZSTAT,BGZFILE) NOWRITE'           
   shellcmd  = ''                                                               
     shellcmd = shellcmd || BGZENVIR                                            
                                                                                
   shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,                                    
          'git status --porcelain'                                              
   Git_rc = BGZCMD('stage' shellcmd)                                            
                                                                                
   /* Now read the Status list */                                               
   'TBTOP BGZTEMP'                                                              
   'TBSKIP BGZTEMP'                                                             
   Do While RC = 0                                                              
     staged = Substr(BGZLINE,1,1)                                               
     unstaged = Substr(BGZLINE,2,1)                                             
     fullname = Substr(BGZLINE,4)                                               
                                                                                
     lastslsh = Lastpos('/',fullname)                                           
     BGZUSFIL = Substr(fullname,lastslsh+1)                                     
                                                                                
     If staged = ' ' | staged = '?' Then                                        
     Do                                                                         
       'TBDELETE BGZTEMP'                                                       
       'TBSKIP BGZTEMP'                                                         
       Iterate                                                                  
     End                                                                        
     If staged <> ' ' & staged <> '?' Then                                      
     Do                                                                         
       BGZSTAT = Substr(BGZLINE,1,3)                                            
       BGZFILE = fullname                                                       
       'TBMOD BGZTEMP'                                                          
     End                                                                        
                                                                                
     'TBSKIP BGZTEMP'                                                           
   End                                                                          
                                                                                
   /* Code to display modfile on panel  */                                      
   ReturnAdd = 0                                                                
   Do Until (ReturnAdd <> 0)                                                    
     'TBTOP BGZTEMP'                                                            
     'TBSKIP BGZTEMP'                                                           
     'TBDISPL BGZTEMP PANEL(BGZCOMIT)'                                          
     TB_RC = RC                                                                 
     'VGET (ZVERB)'                                                             
     If TB_RC = 8 | Zverb = 'CANCEL' Then                                       
     Do                                                                         
      ReturnAdd = -1                                                            
       'TBCLOSE BGZTEMP'                                                        
       Iterate                                                                  
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
       Iterate                                                                  
     End                                                                        
                                                                                
     /* Git reset command if files selected to unstage */                       
     ToReset = '0'                                                              
     Git_rc = 0                                                                 
     'VGET BGZENVIR SHARED'                                                     
     shellcmd  = ''                                                             
     shellcmd = shellcmd || BGZENVIR                                            
                                                                                
     shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,                                  
                  'git reset HEAD '                                             
                                                                                
     'TBTOP BGZTEMP'                                                            
     'TBSKIP BGZTEMP'                                                           
                                                                                
     Do While RC = 0                                                            
                                                                                
       /* build fullname without double quotes when necessary  */               
       namelng = length(BGZFILE)                                                
       If verify(BGZFILE,'"') = 1 Then                                          
         withquote = 0                                                          
       Else                                                                     
         withquote = 1                                                          
       If withquote = 1 Then                                                    
       Do                                                                       
         BGZFILE = Substr(BGZFILE,2,namelng)                                    
         filename = '"'BGZUSDIR'/'BGZFILE                                       
       End                                                                      
       Else                                                                     
         filename = BGZUSDIR'/'BGZFILE                                          
                                                                                
       If BGZCMCMD = 'R' Then                                                   
       Do                                                                       
         ToReset = '1'                                                          
         /* Add selected file to Git add command */                             
         shellcmd = shellcmd || ' 'filename                                     
       End                                                                      
                                                                                
       BGZCMCMD = ''                                                            
       'TBMOD  BGZTEMP ORDER'                                                   
                                                                                
       'TBSKIP BGZTEMP'                                                         
                                                                                
     End                                                                        
     If ToReset = '1' Then                                                      
     Do                                                                         
       Git_rc = BGZCMD('add' shellcmd)                                          
       If Git_rc= 0 Then                                                        
       Do                                                                       
         ReturnAdd = -1                                                         
       End                                                                      
     End                                                                        
                                                                                
     ReturnAdd = -1                                                             
     'TBCLOSE BGZTEMP'                                                          
                                                                                
     /* Git Commit command  */                                                  
     If BGZCOMM /= '' Then                                                      
     Do                                                                         
       Git_rc = 0                                                               
       'VGET BGZENVIR SHARED'                                                   
       shellcmd  = ''                                                           
       shellcmd = shellcmd || BGZENVIR                                          
                                                                                
       shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,                                
              'git commit -m' ||'"'BGZCOMM'"'                                   
       Git_rc = BGZCMD('commit' shellcmd)                                       
       BGZCOMM = ''                                                             
     End                                                                        
                                                                                
   End   /* End Do until ReturnAdd <> 0 */                                      
                                                                                
Return Git_rc                                                                   
