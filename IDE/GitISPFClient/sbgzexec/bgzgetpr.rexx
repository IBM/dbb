/* REXX */                                                                      
/*%STUB CPPLEFPL*/                                                              
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZGETPR                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client get TSO prefix                */         
/*                                                                   */         
/* FUNCTION := This module gets the users TSO prefix to use for      */         
/*             temporary data sets                                   */         
/*                                                                   */         
/* CALLED BY : BGZSTART                                              */         
/*                                                                   */         
/* PARAMETERS : None                                                 */         
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
                                                                                
  profile.0 = 0                                                                 
  USERID    = USERID()                                                          
  BGZTSOPR  = USERID                                                            
  x = outtrap(profile.)                                                         
  Address TSO "PROFILE"                                                         
  x = outtrap('OFF')                                                            
  profline = profile.1                                                          
                                                                                
  If profile.0 <> 0 Then                                                        
  Do                                                                            
    Parse var profline . 'PREFIX(' BGZTSOPR ')' .                               
    If BGZTSOPR = '' Then                                                       
    Do y = 1 to profile.0                                                       
      test1 = POS('PREFIX',profile.y)                                           
      If test1 > 0 Then                                                         
        Parse var profile.y . 'PREFIX(' BGZTSOPR ')' .                          
    End                                                                         
                                                                                
    Select                                                                      
      When (BGZTSOPR = '') Then                                                 
        BGZTSOPR = USERID                                                       
      When (BGZTSOPR = USERID) Then                                             
        BGZTSOPR = USERID                                                       
      Otherwise                                                                 
        BGZTSOPR = BGZTSOPR'.'USERID                                            
    End                                                                         
  End                                                                           
                                                                                
Return BGZTSOPR                                                                 
