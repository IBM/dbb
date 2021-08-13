/* REXX */                                                                      
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
                                                                                
   Arg HLQ LANG                                                                 
                                                                                
   If HLQ = '' Then                                                             
   Do                                                                           
     Parse source srcin                                                         
     Parse var srcin p1 p2 p3 p4 dsnname p5                                     
     Parse var dsnname HLQ'.SBGZEXEC'                                           
     If HLQ = '?' | HLQ = '' Then                                               
     Do                                                                         
       If HLQ = '' Then                                                         
       Do                                                                       
         Say 'Unable to determine HLQ for the SBGZEXEC installed dataset'       
         Say 'Please enter correct HLQ ? (eg: BGZ.V1)'                          
         Pull HLQ                                                               
         If HLQ = '' Then                                                       
         Do                                                                     
           Say 'No High Level Qualifier entered .. processing ends'             
           Exit 8                                                               
         End                                                                    
       End                                                                      
     End                                                                        
   End                                                                          
                                                                                
   If LANG = '' Then LANG = 'ENU'                                               
                                                                                
   /*----------------------------------------------------------------*/         
   /* Perform ISPF libdefs and activate alternate EXEC library       */         
   /*----------------------------------------------------------------*/         
                                                                                
   Address TSO "ALTLIB ACTIVATE APPLICATION(EXEC),                              
                 DATASET('"HLQ".SBGZEXEC')"                                     
                                                                                
   Address ISPEXEC                                                              
   "LIBDEF ISPPLIB DATASET ID('"HLQ".SBGZP"LANG"')"                             
   "LIBDEF ISPMLIB DATASET ID('"HLQ".SBGZM"LANG"')"                             
                                                                                
   "QBASELIB ISPPROF ID(PROFDSN)"                                               
   If rc = 0 Then                                                               
   Do                                                                           
     "LIBDEF ISPTLIB DATASET ID("PROFDSN",'"HLQ".SBGZTLIB')"                    
     "LIBDEF ISPTABL DATASET ID("PROFDSN")"                                     
   End                                                                          
   Else                                                                         
   Do                                                                           
     Say 'problem allocating ISPF profile data set'                             
     "LIBDEF ISPTLIB DATASET ID('"HLQ".SBGZTLIB')"                              
   End                                                                          
                                                                                
   /*----------------------------------------------------------------*/         
   /* Invoke ISPF Git Interface for System z                         */         
   /*----------------------------------------------------------------*/         
   "SELECT CMD(BGZSTART module /) NEWAPPL(BGZ) PASSLIB NEST"                    
                                                                                
   /*----------------------------------------------------------------*/         
   /* Free the ISPF libdefs and deactivate alternate EXEC library    */         
   /*----------------------------------------------------------------*/         
   "LIBDEF ISPPLIB"                                                             
   "LIBDEF ISPMLIB"                                                             
   "LIBDEF ISPLLIB"                                                             
   "LIBDEF ISPTLIB"                                                             
   "LIBDEF ISPTABL"                                                             
   Address TSO "ALTLIB DEACTIVATE APPLICATION(EXEC)"                            
                                                                                
Exit 0                                                                          
