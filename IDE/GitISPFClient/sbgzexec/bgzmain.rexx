/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZMAIN                                                   */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client main module                   */         
/*                                                                   */         
/* FUNCTION := The ISPF Client main module displays the primary      */         
/*             options menu and controls calling the modules         */         
/*             for the other menu functions                          */         
/*                                                                   */         
/*                                                                   */         
/* CALLED BY : BGZ - Invocation EXEC                                 */         
/*                                                                   */         
/* PARAMETERS :                                                      */         
/*                                                                   */         
/* OUTPUT := None                                                    */         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* XH    10/01/19 Initial version                                    */         
/*                                                                   */         
/*********************************************************************/         
                                                                                
   Parse Arg Module ZCMD '/' .                                                  
                                                                                
   /* When called from compiled REXX Module contains the module */              
   /* name. However when called interpretively Module is not    */              
   /* set, so ZCMD is in wrong variable, so need to move it.    */              
   If Datatype(Module,'W') = 1 Then                                             
     ZCMD = Module                                                              
                                                                                
   Address ISPEXEC                                                              
   'VGET (BGZTRACE) SHARED'                                                     
                                                                                
   'TBOPEN BGZPREFS SHARE'                                                      
   TB_RC = RC                                                                   
   'CONTROL ERRORS CANCEL'                                                      
   If TB_RC = 8 Then                                                            
   Do                                                                           
     'TBCREATE BGZPREFS',                                                       
       'KEYS(BGZPRID) NAMES(BGZPRNAM,BGZPRVAL) WRITE'                           
     'TBSORT BGZPREFS FIELDS(BGZPRNAM)'                                         
     BGZPRID  = 'git.code.page'                                                 
     'GETMSG MSG(BGZC013) LONGMSG(BGZPRNAM)'                                    
     BGZPRVAL = 'IBM-1047'                                                      
     'TBADD BGZPREFS ORDER'                                                     
     BGZPRID  = 'git.point.file'                                                
     'GETMSG MSG(BGZC010) LONGMSG(BGZPRNAM)'                                    
     BGZPRVAL = 'Y'                                                             
     'TBADD BGZPREFS ORDER'                                                     
     BGZPRID  = 'git.user.email'                                                
     'GETMSG MSG(BGZC011) LONGMSG(BGZPRNAM)'                                    
     BGZPRVAL = ''                                                              
     'TBADD BGZPREFS ORDER'                                                     
     BGZPRID  = 'git.user.name'                                                 
     'GETMSG MSG(BGZC012) LONGMSG(BGZPRNAM)'                                    
     BGZPRVAL = ''                                                              
     'TBADD BGZPREFS ORDER'                                                     
     BGZPRID  = 'git.password'                                                  
     'GETMSG MSG(BGZC020) LONGMSG(BGZPRNAM)'                                    
     BGZPRVAL = 'N'                                                             
     'TBADD BGZPREFS ORDER'                                                     
                                                                                
                                                                                
   End                                                                          
  'TBCLOSE BGZPREFS'                                                            
                                                                                
   Select                                                                       
     When Zcmd = 0 Then                                                         
       'SELECT CMD(BGZPREFS) NEST LANG(CREX)'                                   
                                                                                
     When Zcmd = 1 Then                                                         
       'SELECT CMD(BGZINIT) NEST LANG(CREX)'                                    
                                                                                
     When Zcmd = 2 Then                                                         
     Do                                                                         
       'SELECT CMD(BGZREPOS) NEST LANG(CREX)'                                   
     End                                                                        
                                                                                
     /* Generate new ssh key option to add later   */                           
 /*  When Zcmd = 1 Then                                                         
       'SELECT CMD(BGZSSHK) NEST LANG(CREX)'   */                               
                                                                                
     When Zcmd = X Then                                                         
     Do                                                                         
       Exit 0                                                                   
     End                                                                        
                                                                                
     Otherwise NOP                                                              
   End /* Select; */                                                            
                                                                                
Exit                                                                            
