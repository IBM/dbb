/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZEDIT                                                   */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client edit module                   */         
/*                                                                   */         
/*                                                                   */         
/* CALLED BY : BGZUDLST                                              */         
/*                                                                   */         
/* PARAMETERS :                                                      */         
/*                                                                   */         
/* OUTPUT := None                                                    */         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* XH    15/01/19 Initial version                                    */         
/*                                                                   */         
/*********************************************************************/         
                                                                                
  Parse Arg EDVWOPT MODE OBJECT                                                 
                                                                                
  Parse var OBJECT ObjName'<>' NewExist                                         
                                                                                
  EditRC   = 0                                                                  
  Panel    = ''                                                                 
  iMacro   = ''                                                                 
  Profile  = ''                                                                 
  Format   = ''                                                                 
  Reclen   = ''                                                                 
  Mixed    = ''                                                                 
  Confirm  = ''                                                                 
  Encod    = ''                                                                 
  BGZCFMCN = ' '                                                                
  BGZENCD = ''                                                                  
  BGZEMIX  = ''                                                                 
  Address ISPEXEC                                                               
                                                                                
  'VGET (BGZBYEDF BGZBYEUF) PROFILE'                                            
  'VGET (BGZEUTF) SHARED'                                                       
                                                                                
  If Substr(ObjName,1,1) = '/' Then    /* Unix file */                          
  Do                                                                            
    If Mode = 'ASCII' Then BGZENCD = '1'                                        
    If Mode = 'UTF-8' Then BGZENCD = '2'                                        
    If Mode = 'UTF8'  Then BGZENCD = '2'                                        
    If BGZBYEUF <> 'ON' then                                                    
    Do                                                                          
      BGZPATH = ObjName                                                         
      'ADDPOP'                                                                  
      If BGZEUTF = 'TRUE' Then                                                  
         'DISPLAY PANEL(BGZUEDTU)'                                              
      Else                                                                      
         'DISPLAY PANEL(BGZUEDT2)'                                              
      If RC >= 4 | ZVERB = 'CANCEL' | ZVERB = 'EXIT' Then                       
        EditRC = 4                                                              
      'REMPOP'                                                                  
    End                                                                         
    If EditRC = 0 Then                                                          
    Do                                                                          
      'VGET (BGZEMAC BGZEPROF BGZEFORM BGZEPANL) PROFILE'                       
      'VGET (BGZERECL) SHARED'                                                  
      If BGZERECL = ' '  Then BGZERECL = '0'                                    
      If BGZEPANL <> ''  Then Panel   = 'PANEL('BGZEPANL')'                     
      If BGZEMAC  <> ''  Then iMacro  = 'MACRO('BGZEMAC')'                      
      If BGZEPROF <> ''  Then Profile = 'PROFILE('BGZEPROF')'                   
      If BGZEFORM <> ''  Then Format  = 'FORMAT('BGZEFORM')'                    
      If BGZERECL <> '0' Then Reclen  = 'RECLEN('BGZERECL')'                    
      If BGZCFMCN <> ''  Then Confirm = 'CONFIRM(YES)'                          
      If BGZEMIX = '' Then                                                      
      Do                                                                        
        'VGET (ZDBCS) SHARED'                                                   
        If ZDBCS = 'YES' THEN BGZEMIX = 'YES'                                   
      End                                                                       
      If BGZEMIX = 'YES' Then Mixed   = 'MIXED('BGZEMIX')'                      
      If BGZENCD = '1' Then Encod    = 'ASCII'                                  
      If BGZENCD = '2' Then Encod    = 'UTF8'                                   
      If Substr(EDVWOPT,1,1) = 'E' Then                                         
      Do                                                                        
        'CONTROL ERRORS RETURN'                                                 
        'EDIT FILE(ObjName) 'Panel iMacro  Profile Format Reclen,               
                             Mixed Confirm Encod                                
        EditRC = rc                                                             
        If Edit_rc /= 0 Then                                                    
        Do                                                                      
          BGZEDITM = ZERRLM                                                     
          'VPUT (BGZEDITM) SHARED'                                              
        End                                                                     
        'CONTROL ERRORS CANCEL'                                                 
      End                                                                       
      If Substr(EDVWOPT,1,1) = 'V' Then                                         
      Do                                                                        
        'CONTROL ERRORS RETURN'                                                 
        'VIEW FILE(ObjName) 'Panel iMacro  Profile Format Reclen,               
                             Mixed Confirm Encod                                
        EditRC = rc                                                             
        If Edit_rc /= 0 Then                                                    
        Do                                                                      
          BGZEDITM = ZERRLM                                                     
          'VPUT (BGZEDITM) SHARED'                                              
        End                                                                     
        'CONTROL ERRORS CANCEL'                                                 
      End                                                                       
    End                                                                         
  End                                                                           
                                                                                
Return EditRC   /* EditProc */                                                  
