/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/*                                                                   */         
/* NAME := BGZABOUT                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client about display panel           */         
/*                                                                   */         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* XH    21/04/20 Initial version                                    */         
/*                                                                   */         
/*********************************************************************/         
                                                                                
   Address ISPEXEC                                                              
   "ADDPOP ROW(2) COLUMN(4)"                                                    
   "DISPLAY PANEL(BGZABOUT)"                                                    
   "REMPOP"                                                                     
                                                                                
Exit 0                                                                          
