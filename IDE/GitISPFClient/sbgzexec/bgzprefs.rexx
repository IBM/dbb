/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZPREFS                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client preferences module            */         
/*                                                                   */         
/* FUNCTION := The ISPF Client main module displays the preferences  */         
/*             options to use with Git                               */         
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
                                                                                
  Address ISPEXEC                                                               
  'TBOPEN BGZPREFS SHARE'                                                       
  'TBTOP  BGZPREFS'                                                             
  'TBDISPL BGZPREFS PANEL(BGZPREFS)'                                            
  TB_RC = RC                                                                    
  Do While (TB_RC < 8)                                                          
    Do While ZTDSELS > 0                                                        
          'TBMOD BGZPREFS'                                                      
      If ZTDSELS = 1 Then                                                       
        ZTDSELS = 0                                                             
      Else                                                                      
        'TBDISPL BGZPREFS'                                                      
    End                                                                         
    'TBDISPL BGZPREFS PANEL(BGZPREFS)'                                          
    TB_RC = RC                                                                  
  End                                                                           
  'TBCLOSE BGZPREFS'                                                            
                                                                                
  BGZSET = 0                                                                    
  'VPUT (BGZSET) SHARED'                                                        
                                                                                
Return 0                                                                        
