/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZBRANC                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client git branch                    */         
/*                                                                   */         
/* FUNCTION :=                                                       */         
/*                                                                   */         
/*                                                                   */         
/* CALLED BY : BGZREPOS                                              */         
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
                                                                                
   Parse Arg BGZREPOS BGZUSDIR NoDisp                                           
                                                                                
   ReturnBranch = -1                                                            
   Rebuild = 1                                                                  
                                                                                
   Address ISPEXEC                                                              
                                                                                
   Do Until (ReturnBranch = 0)                                                  
     If Rebuild = 1 Then                                                        
     Do                                                                         
       Rebuild = 0                                                              
       /* Git branch command  */                                                
       Git_rc = 0                                                               
       'VGET BGZENVIR SHARED'                                                   
       /* Need to create a temporary ISPF table to hold info */                 
       'TBCREATE BGZTEMP KEYS(BGZLINE),                                         
                         NAMES(BGZBRCMD) NOWRITE'                               
                                                                                
       shellcmd  = ''                                                           
       shellcmd = shellcmd || BGZENVIR                                          
                                                                                
       shellcmd = shellcmd || 'cd' BGZUSDIR';' ||,                              
              'git branch -a'                                                   
       Git_rc = BGZCMD('branch' shellcmd)                                       
                                                                                
       /* NO Delete keep the full list returned from git branch -a              
       /* Now read the Branch list */                                           
       /* we need to delete rows starting by remotes/origin/  */                
       'TBTOP BGZTEMP'                                                          
       'TBSKIP BGZTEMP'                                                         
       Do While RC = 0                                                          
         branch = Substr(BGZLINE,1,17)                                          
         If Verify(branch,'  remotes/origin/') = 0 Then                         
         Do                                                                     
           'TBDELETE BGZTEMP'                                                   
         End                                                                    
         'TBSKIP BGZTEMP'                                                       
       End                                                                      
       --------------------------- */                                           
       /* Just using this module to get the current branch */                   
       If NoDisp = 'noDisplay' Then                                             
         Return Git_rc                                                          
     End                                                                        
                                                                                
     /* Code to display branch on panel  */                                     
     'TBTOP BGZTEMP'                                                            
     'TBSKIP BGZTEMP'                                                           
     'TBDISPL BGZTEMP PANEL(BGZBRANC)'                                          
     TB_RC = RC                                                                 
     'VGET (ZVERB)'                                                             
     If TB_RC = 8 | Zverb = 'CANCEL' Then                                       
     Do                                                                         
       ReturnBranch = 0                                                         
       'TBCLOSE BGZTEMP'                                                        
       Iterate                                                                  
     End                                                                        
                                                                                
     /* New branch to create entered on top line */                             
     If BGZNBRAN <> '' Then                                                     
     Do                                                                         
                                                                                
       /* Git checkout operation  */                                            
       Git_rc = 0                                                               
       'VGET BGZENVIR SHARED'                                                   
       shellcmd  = ''                                                           
       shellcmd = shellcmd || BGZENVIR                                          
                                                                                
       shellcmd=shellcmd || 'cd' BGZUSDIR';'||,                                 
         'git checkout -b ' BGZNBRAN                                            
                                                                                
       Git_rc = BGZCMD('crbran' shellcmd)                                       
                                                                                
       If Git_rc = 0 Then                                                       
       Do                                                                       
                                                                                
         BGZBRANC = BGZNBRAN                                                    
         BGZNEWBR = 'Y'                                                         
         'VPUT (BGZBRANC) PROFILE'                                              
         'VPUT (BGZNEWBR) PROFILE'                                              
         BGZLINE = BGZNBRAN                                                     
         'TBADD BGZTEMP ORDER'                                                  
         'TBCLOSE BGZTEMP'                                                      
         BGZNBRAN = ''                                                          
         Rebuild = 1                                                            
         Iterate                                                                
       End                                                                      
       Else                                                                     
       Do                                                                       
         Iterate                                                                
       End                                                                      
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
       Rebuild = 0                                                              
       Iterate                                                                  
     End                                                                        
                                                                                
     'TBTOP BGZTEMP'                                                            
     'TBSKIP BGZTEMP'                                                           
                                                                                
     Do While RC = 0                                                            
                                                                                
       If BGZBRCMD = '/' Then                                                   
         BGZBRCMD = GetBrCMD(BGZREPOS BGZUSDIR BGZLINE)                         
                                                                                
       /* Capture branch name from BGZLINE */                                   
       BGZBRAN = Substr(BGZLINE,3)                                              
                                                                                
       Select                                                                   
                                                                                
         When BGZBRCMD = 'CB' Then                                              
         Do                                                                     
           /* Change working branch */                                          
           /* Git Checkout <branch> */                                          
           Git_rc = 0                                                           
           'VGET BGZENVIR SHARED'                                               
           shellcmd  = ''                                                       
           shellcmd = shellcmd || BGZENVIR                                      
                                                                                
           Parse var BGZBRAN 'remotes/origin/'Branch                            
                                                                                
           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,                            
                  'git checkout ' ||'"'Branch'"'                                
           Git_rc = BGZCMD('checkout' shellcmd)                                 
                                                                                
           If Git_rc= 0 Then                                                    
           Do                                                                   
             BGZBRANC = Branch                                                  
             BGZNEWBR = 'N'                                                     
             'VPUT (BGZBRANC) PROFILE'                                          
             'VPUT (BGZNEWBR) PROFILE'                                          
             Rebuild = 1                                                        
             ReturnBranch = -1                                                  
           End                                                                  
                                                                                
         End                                                                    
                                                                                
         When BGZBRCMD = 'PB' Then                                              
         Do                                                                     
           /* Push branch to origin */                                          
           Git_rc = 0                                                           
           'VGET BGZENVIR SHARED'                                               
           shellcmd  = ''                                                       
             shellcmd = shellcmd || BGZENVIR                                    
                                                                                
           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,                            
                  'git push origin ' BGZBRAN                                    
           Git_rc = BGZCMD('pushbra' shellcmd)                                  
           If Git_rc= 0 Then                                                    
           Do                                                                   
             ReturnBranch = -1                                                  
             Rebuild = 1                                                        
           End                                                                  
                                                                                
         End                                                                    
                                                                                
         When BGZBRCMD = 'DB' Then                                              
         Do                                                                     
           /* Delete local branch */                                            
           Git_rc = 0                                                           
           'VGET BGZENVIR SHARED'                                               
           shellcmd  = ''                                                       
           shellcmd = shellcmd || BGZENVIR                                      
                                                                                
           shellcmd=shellcmd || 'cd' BGZUSDIR';' ||,                            
                  'git branch -d ' BGZBRAN                                      
           Git_rc = BGZCMD('delbran' shellcmd)                                  
           If Git_rc= 0 Then                                                    
           Do                                                                   
             ReturnBranch = -1                                                  
             Rebuild = 1                                                        
           End                                                                  
                                                                                
         End                                                                    
                                                                                
         When BGZBRCMD = 'JU' Then                                              
         Do                                                                     
           /* Jump to USS directory - call BGZUDLST  */                         
           BGZUSSDR = BGZUSDIR                                                  
           'VPUT (BGZUSSDR) PROFILE'                                            
           BGZUSREP = BGZREPOS                                                  
           'VPUT (BGZUSREP) PROFILE'                                            
           BGZBRANC = BGZBRAN                                                   
           'VPUT (BGZBRANC) PROFILE'                                            
           Call BGZUSLST                                                        
           Rebuild = 0                                                          
           ReturnBranch = 0                                                     
           RC = -1                                                              
           Iterate                                                              
         End                                                                    
                                                                                
         Otherwise NOP                                                          
       End                                                                      
                                                                                
       BGZBRCMD = ''                                                            
       'TBMOD  BGZTEMP ORDER'                                                   
                                                                                
       'TBSKIP BGZTEMP'                                                         
                                                                                
     End                                                                        
                                                                                
       If Rebuild = 1 Then                                                      
         'TBCLOSE BGZTEMP'                                                      
                                                                                
   End   /* End Do until ReturnBranch <> 0 */                                   
                                                                                
Return Git_rc                                                                   
                                                                                
GetBrCMD: PROCEDURE                                                             
  Parse Arg BGZREPOS BGZUSDIR BGZLINE                                           
  'ADDPOP'                                                                      
  'DISPLAY PANEL(BGZSLBRA)'                                                     
  'VGET (ZVERB)'                                                                
  if ZVERB = 'CANCEL' | ZVERB = 'EXIT' Then                                     
    BGZSEL = ''                                                                 
                                                                                
  Select;                                                                       
    When BGZSEL = '1' Then                                                      
      Cmd = 'CB'                                                                
    When BGZSEL = '2' Then                                                      
      Cmd = 'PB'                                                                
    When BGZSEL = '3' Then                                                      
      Cmd = 'DB'                                                                
    When BGZSEL = '4' Then                                                      
      Cmd = 'JU'                                                                
    Otherwise                                                                   
      Cmd = ''                                                                  
  End                                                                           
  'REMPOP'                                                                      
Return Cmd                                                                      
