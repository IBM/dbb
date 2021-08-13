/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZCOMMP                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client git command prompt            */         
/*                                                                   */         
/* FUNCTION :=                                                       */         
/*                                                                   */         
/*                                                                   */         
/* CALLED BY : BGZMAIN                                               */         
/*                                                                   */         
/* PARAMETERS :                                                      */         
/*                                                                   */         
/* OUTPUT := None                                                    */         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* XH    04/02/19 Initial version                                    */         
/*                                                                   */         
/*********************************************************************/         
                                                                                
   Parse Arg BGZREPOS BGZUSDIR                                                  
                                                                                
   /* Need to create a permanent ISPF table to store git commands history */    
   Address ISPEXEC                                                              
   'TBOPEN BGZCHIST'                                                            
   /* If it didn't exist yet, create it. */                                     
   If RC = 8 Then                                                               
   Do                                                                           
     'TBCREATE BGZCHIST',                                                       
     'KEYS(BGZCRET) NAMES(BGZCGITC) WRITE'                                      
   End                                                                          
                                                                                
   BGZGIT  = ''                                                                 
   rsel    = ''                                                                 
   DoGitcmd = 0                                                                 
   lastcom = 0                                                                  
   Do Until DoGitcmd <> 0                                                       
     /* Now read the BGZCHIST table to populate the 10 point&shot variables */  
     /* panel BGZgitc to enter a Git command */                                 
     ztcret = 1                                                                 
     'TBSORT BGZCHIST FIELDS(BGZCRET,N,D)'                                      
     'TBTOP BGZCHIST'                                                           
     'TBSKIP BGZCHIST'                                                          
     If RC = 0 Then                                                             
       lastcom = BGZCRET                                                        
                                                                                
     Do While RC = 0                                                            
      If ztcret > 10 Then                                                       
      Do                                                                        
       'TBDELETE BGZCHIST'                                                      
      End                                                                       
      If ztcret = 1 Then                                                        
        ztcret01 = BGZCGITC                                                     
      If ztcret = 2 Then                                                        
        ztcret02 = BGZCGITC                                                     
      If ztcret = 3 Then                                                        
        ztcret03 = BGZCGITC                                                     
      If ztcret = 4 Then                                                        
        ztcret04 = BGZCGITC                                                     
      If ztcret = 5 Then                                                        
        ztcret05 = BGZCGITC                                                     
      If ztcret = 6 Then                                                        
        ztcret06 = BGZCGITC                                                     
      If ztcret = 7 Then                                                        
        ztcret07 = BGZCGITC                                                     
      If ztcret = 8 Then                                                        
        ztcret08 = BGZCGITC                                                     
      If ztcret = 9 Then                                                        
        ztcret09 = BGZCGITC                                                     
      If ztcret = 10 Then                                                       
        ztcret10 = BGZCGITC                                                     
                                                                                
      ztcret = ztcret + 1                                                       
      'TBSKIP BGZCHIST'                                                         
     End                                                                        
                                                                                
     /* Point and Shoot fields ztcretnn returning command depending on  */      
     /* cursor position  */                                                     
                                                                                
     BGZGIT  = ''                                                               
     selfirst = 0                                                               
     If rsel = 1 Then                                                           
     Do                                                                         
       selfirst = 1                                                             
       BGZGIT = ztcret01                                                        
     End                                                                        
     If rsel = 2 Then                                                           
       BGZGIT = ztcret02                                                        
     If rsel = 3 Then                                                           
       BGZGIT = ztcret03                                                        
     If rsel = 4 Then                                                           
       BGZGIT = ztcret04                                                        
     If rsel = 5 Then                                                           
       BGZGIT = ztcret05                                                        
     If rsel = 6 Then                                                           
       BGZGIT = ztcret06                                                        
     If rsel = 7 Then                                                           
       BGZGIT = ztcret07                                                        
     If rsel = 8 Then                                                           
       BGZGIT = ztcret08                                                        
     If rsel = 9 Then                                                           
       BGZGIT = ztcret09                                                        
     If rsel = 10 Then                                                          
       BGZGIT = ztcret10                                                        
                                                                                
     rsel = ''                                                                  
                                                                                
     'DISPLAY PANEL(BGZGITC)'                                                   
     TB_RC = RC                                                                 
     'VGET (ZVERB)'                                                             
     If TB_RC = 8 | ZVERB = 'CANCEL' Then                                       
       DoGitcmd = -1                                                            
     If DoGitcmd <> -1 Then                                                     
     Do                                                                         
                                                                                
       If BGZGIT <> ' ' Then                                                    
       Do                                                                       
         /* Full Git command  */                                                
         Git_rc = 0                                                             
         'VGET BGZENVIR SHARED'                                                 
         shellcmd  = ''                                                         
            shellcmd = shellcmd || BGZENVIR                                     
                                                                                
         shellcmd=shellcmd || 'cd' BGZUSDIR';' || BGZGIT                        
         Git_rc = BGZCMD('gitcmd' shellcmd)                                     
                                                                                
         BGZCRET = lastcom + 1                                                  
         BGZCGITC = BGZGIT                                                      
         If selfirst = 0 Then                                                   
           'TBADD BGZCHIST'                                                     
         BGZGIT  = ''                                                           
       End                                                                      
     End                                                                        
   End                                                                          
   'TBCLOSE BGZCHIST'                                                           
                                                                                
Return Git_rc                                                                   
