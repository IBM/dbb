/* REXX */  
/*********************************************************************/
/*                                                                   */
/* IBM ISPF Git Interface                                            */
/*                                                                   */
/*********************************************************************/
/*                                                                   */
/* NAME := BGZGIT                                                    */
/*                                                                   */
/* DESCRIPTIVE NAME := Main Line Driver                              */
/*                                                                   */
/* PARAMETERS :                                                      */
/*                                                                   */
/*   HLQ   - High Level Qualifier of the installation datasets.      */
/*           If not provided, this will be discovered.               */
/*                                                                   */
/*   LANG  - Sub-qualifier for the PAnel/Messages Libraries          */
/*           Default = "ENU"                                         */
/*                                                                   */
/*                                                                   */
/* Change History                                                    */
/*                                                                   */
/* Who   When     What                                               */
/* ----- -------- -------------------------------------------------- */
/* XH    24/01/19 Initial version                                    */
/* TLD   31/05/23 FIX: Added view of git command results to correct  */
/*                CLIST variable value overflow exception.           */
/*                                                                   */
/*********************************************************************/                                                                    
                                                                                
   Arg HLQ LANG 
                                                                
   /* Temporary work file allocation attributes */                          
   WrkAtr = "SPACE(5,5) TRACK RECFM(F,B) BLKSIZE(2560) LRECL(256) DSORG(PS)"                                                                                

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

     /* Allocate a temporary work file for the git command output */
     Address TSO "ALLOC F(SBGZWRK) NEW REU UNIT(VIO)" WrkAtr         
                                   
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
   Address TSO "FREE F(SBGZWRK)" /* Free the Temporary Work File */                                                             
   Address TSO "ALTLIB DEACTIVATE APPLICATION(EXEC)"                            
                                                                                
Exit 0                                                                          
