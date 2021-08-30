/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZCNVRT                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := ISPF Client Git codepage conversion routine   */         
/*                                                                   */         
/* FUNCTION := This module converts a character or string between    */         
/*             the specified original and new codepage using iconv.  */         
/*                                                                   */         
/* CALLED BY : BGZCMD                                                */         
/*             BGZENCOD                                              */         
/*                                                                   */         
/* PARAMETERS : CHAR     - Character or string to convert            */         
/*              FROMCP   - From Codepage                             */         
/*              TOCP     - To Codepage                               */         
/*                                                                   */         
/* OUTPUT := The character or string, as represented in the 'to'     */         
/*           codepage.                                               */         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* XH    14/01/19 Initial version                                    */         
/*********************************************************************/         
                                                                                
  Parse Arg FromCP ToCP Char                                                    
                                                                                
  Address ISPEXEC 'VGET (BGZICONV) SHARED'                                      
  parms.1 = BGZICONV                                                            
  parms.2 = '-f'                                                                
  parms.3 = FromCP                                                              
  parms.4 = '-t'                                                                
  parms.5 = ToCp                                                                
  parms.0 = 5                                                                   
                                                                                
  env.0 = 0                                                                     
                                                                                
  RC = SYSCALLS('ON')                                                           
  Address SYSCALL                                                               
  'pipe inpipe.'                                                                
  'pipe outpipe.'                                                               
  len = Length(Char)                                                            
  'write (inpipe.2) Char (len)'                                                 
  'close' inpipe.2                                                              
  map.0 = inpipe.1                                                              
  map.1 = outpipe.2                                                             
  'spawn' parms.1 '2 map. parms. env.'                                          
  spid = RETVAL                                                                 
  'waitpid (spid) waitpid. 0'                                                   
  'close' outpipe.2                                                             
  InFD = outpipe.1                                                              
                                                                                
  Converted = ''                                                                
  'read (InFD) inbytes 20000'                                                   
  Do While RetVal <> 0                                                          
    Converted = Converted || inbytes                                            
    'read (InFD) inbytes 20000'                                                 
  End                                                                           
  'close' inpipe.1                                                              
  'close' outpipe.1                                                             
  RC = SYSCALLS('OFF')                                                          
                                                                                
Return Converted                                                                
