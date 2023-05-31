/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZCMD                                                    */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT command module                            */         
/*                                                                   */         
/*                                                                   */         
/* PARAMETERS : shellcmd                                             */         
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
  Parse Source ENVIR CALLTYPE PGM SPECIFICS;                           
  PGM = SUBSTR(PGM,1,8)                                                
                                                                       
  Debug = "N"           /* (Y|N) Always view the temporary work file */
  CLISTVarMax = 32000   /* Maximum Length a CLIST Variable can hold  */           
                                                                                
  Parse Arg gitcmd shellcmd                                                     
  Address ISPEXEC                                                               
                                                                                
  /* Create hex variables for attribute bytes */                                
  x01='01'x                                                                     
  x02='02'x                                                                     
  x03='03'x                                                                     
  x04='04'x                                                                     
  x05='05'x                                                                     
  x06='06'x                                                                     
  x07='07'x                                                                     
  x0D='0D'x                                                                     
                                                                                
  /* Add in this list the git commands that you want to return */               
  /* the output rather than poping on the git message panel.   */               
  /* Output will be returned in a temporary ISPF table.        */               
  noPop = 'branch stage'                                                        
                                                                                
  /* Set the panel title depending on the command   */                          
  /* gitcmd = dbbub --> panel title : DBB Console   */                          
  /* any git command --> panel title : Git Messages */                          
  If gitcmd = 'dbbub' Then                                                      
  Do                                                                            
    'GETMSG MSG(BGZC037) LONGMSG(BGZTITLE)'                                     
  End                                                                           
  Else                                                                          
  Do                                                                            
    'GETMSG MSG(BGZC038) LONGMSG(BGZTITLE)'                                     
  End                                                                           
                                                                                
  filler   = ' '                                                                
  call runShell                                                                 
                                                                                
Return sh_rc                                                                    
                                                                                
/* --------------------------------------------- */                             
/* Procedure to run git command and display      */                             
/* output on pop up panel                        */                             
/* --------------------------------------------- */                             
runShell :                                                                      
                                                                                
  convErr.0 = 0                                                                 
  sh_rc = bpxwunix(shellcmd,,stdout.,stderr.)                                   
                                                                                
  Call ReadOutput                                                               
                                                                                
  If pos(gitcmd,noPOP) = 0 Then                                                 
  Do                                                                            
    If gitcmd = 'dbbub' Then                                                    
    Do                                                                          
      BGZEMIX = 'NO'                                                            
      'VGET (ZDBCS) SHARED'                                                     
      If ZDBCS = 'YES' THEN BGZEMIX = 'YES'                                     
      "CONTROL ERRORS RETURN"                                                   
      "VIEW File(BGZFLOG) MIXED("BGZEMIX")"                                     
      If rc > 0 Then                                                            
        Say ZERRLM                                                              
      "CONTROL ERRORS CANCEL"                                                   
    End                                                                         
    Else                                                                        
    Do                                                                          
      Call BuildDYN

      /* If we are in Debug Mode, always view the command output that */
      /* was written to the temporary work file.                      */
      If (Debug = "Y") Then                                           
        Do                                                            
          rc = ViewWorkFile()                                         
        End                                                           
                                                                
      /* Check the lenght of the dyndata variable data.  If greater */
      /* then the CLIST maximum (32K), then we have to resort to    */
      /* viewing the command output instead of using the Pop-Up     */
      If (Length(dyndata) > CLISTVarMax) Then                                  
        Do                                                               
          If (Debug = "N") Then                                           
            rc = ViewWorkFile() /* If Debug = Y, we already displayed */ 
        End                                                              
      Else /* Proceed with Pop-Up Display */                             
        Do                                                               
          curline  = 1                                                              
          "ISPEXEC ADDPOP"                                                          
          panel = 'BGZMSG'                                                          
          DispRc = DispPanel()                                                      
          Do while (DispRc = 0)                                                     
            Call Scroll                                                             
            panel = 'BGZMSG'                                                        
            DispRc = DispPanel()                                                    
          End                                                                       
          "ISPEXEC REMPOP"
        End                                                            
    End                                                                         
  End                                                                           
                                                                                
Return sh_rc                                                                    
                                                                                
/* --------------------------------------------- */                             
/* Procedure to build the messages dynamic area  */                             
/* --------------------------------------------- */                             
BuildDYN:                                                                       
                                                                                
  dyndata = ''                                                                  
                                                                                
  /* Initialize maxlines to the number of lines always displayed */             
  maxlines = 0                                                                  
                                                                                
  Do i = 1 to gitLine.0                                                         
    x = length(gitLine.i)                                                       
    If x < 70 Then                                                              
    Do                                                                          
      dyndata  = dyndata||fit2line(gitLine.i)                                   
      maxlines = maxlines + 1                                                   
    End                                                                         
    Else                                                                        
    Do                                                                          
      /* I thing we may need a loop to break the line */                        
      /* looks like it breaks line in 2 only          */                        
      lastBlank = LastPos(' ',Substr(gitLine.i,1,70))                           
      dyndata  = dyndata||fit2line(Substr(gitLine.i,1,lastBlank))               
      dyndata  = dyndata||fit2line(x04||Substr(gitLine.i,lastBlank+1))          
      maxlines = maxlines + 2                                                   
    End                                                                         
  End                                                                           
                                                                                
Return                                                                          
                                                                                
/* --------------------------------------------- */                             
/* Procedure to set the dynamic area and display */                             
/* the dependency options panel                  */                             
/* --------------------------------------------- */                             
DispPanel:                                                                      
                                                                                
                                                                                
  BGZdyn = substr(dyndata,1+(curline-1)*70) /* set dynamic variable */          
  "ISPEXEC DISPLAY PANEL("panel")"                                              
  DispRc = rc                                                                   
                                                                                
Return DispRc                                                                   
                                                                                
/****************************************************************************/  
/*                                                                          */  
/* Read a file and convert to ASCII                                         */  
/*                                                                          */  
/****************************************************************************/  
                                                                                
ReadASCII :                                                                     
                                                                                
Return max_rc                                                                   
                                                                                
/* --------------------------------------------- */                             
/* Procedure to pad a line in the dynamic area   */                             
/* with blanks                                   */                             
/* --------------------------------------------- */                             
Fit2Line: procedure                                                             
                                                                                
  parse arg in                                                                  
  ll=70      /* must = line length */                                           
  l=length(in) // ll                                                            
  If l = 0 Then                                                                 
    out=in                                                                      
  Else                                                                          
    out=in||copies(' ',ll-l)                                                    
                                                                                
return out                                                                      
                                                                                
/* --------------------------------------------- */                             
/* Procedure to pad a field with a number of     */                             
/* blanks                                        */                             
/* --------------------------------------------- */                             
setsize: Procedure                                                              
                                                                                
  parse arg in , l                                                              
  if length(in) < l then return left(in,l)                                      
                                                                                
return left(in||copies(' ',l),l)                                                
                                                                                
/* --------------------------------------------- */                             
/* Procedure to handle the scrolling             */                             
/* --------------------------------------------- */                             
Scroll:                                                                         
                                                                                
  iTopData  = ''                                                                
  iEndData  = ''                                                                
  "ISPEXEC VGET (ZVERB,ZSCROLLA,ZSCROLLN)" /* get scroll values      */         
  Select                                   /* Process scrolling      */         
    When(zverb = 'UP') Then                /* Scroll up              */         
    Do                                                                          
      If zscrolla = 'MAX' Then             /*  if scroll was max     */         
        curline = 1                        /*    scroll to top       */         
      Else                                 /*  else a number is known*/         
        curline = max(1,curline-zscrolln); /* (maximum is top)       */         
      If curline = 1 Then                                                       
        iTopData  = x04||Strip(topData)                                         
    End                                                                         
    When(zverb  = 'DOWN') Then             /* Scroll down            */         
    Do                                                                          
      If zscrolla = 'MAX' Then             /*  if scroll was max     */         
        curline = maxlines                 /*    scroll to bottom    */         
      Else                                 /*  else a number is known*/         
        curline = min(maxlines,curline+zscrolln); /* (max is bottom)   */       
      If curline = maxlines Then                                                
        iEndData  = x04||Strip(endData)                                         
    End                                                                         
    When(zverb = 'LEFT' & scrollFld /= '') Then                                 
    Do                                                                          
      Select                                                                    
        When (ZSCROLLA = 'MAX') then                                            
        Do                                                                      
          scrpos = 1                                                            
          scrdi  = Substr(scrfull,scrpos,lmin)                                  
        End                                                                     
        When (ZSCROLLA = 'CSR') then                                            
        Do                                                                      
          curpos = ZCSROFF - posdi + 1                                          
          scrpos = scrpos - (lmin - curpos)                                     
          If scrpos < 1 then                                                    
             scrpos = 1                                                         
          scrdi   = Substr(scrfull,scrpos,lmin)                                 
        End                                                                     
        When (ZSCROLLA = 'PAGE') then                                           
        Do                                                                      
          scrpos = scrpos - lmin                                                
          If scrpos < 1 then                                                    
             scrpos = 1                                                         
          scrdi   = Substr(scrfull,scrpos,lmin)                                 
        End                                                                     
        When (ZSCROLLA = 'HALF') then                                           
        Do                                                                      
          scrpos = scrpos - trunc(lmin/2)                                       
          If scrpos < 1 then                                                    
             scrpos = 1                                                         
          scrdi   = Substr(scrfull,scrpos,lmin)                                 
        End                                                                     
        Otherwise                                                               
        Do                                                                      
          scrpos = scrpos - ZSCROLLN                                            
          If scrpos < 1 then                                                    
             scrpos = 1                                                         
          scrdi   = Substr(scrfull,scrpos,lmin)                                 
        End                                                                     
      End                                                                       
    End                                                                         
    When(zverb  = 'RIGHT' & scrollFld /= '') Then                               
    Do                                                                          
      Select                                                                    
        When (ZSCROLLA = 'MAX') then                                            
        Do                                                                      
          /* Figure I will use Max to position to last non blank */             
          maxpos = Length(Strip(scrfull,'T'))                                   
          If maxpos > lmin Then                                                 
            scrpos = maxpos - lmin + 1                                          
          Else                                                                  
            scrpos = 1                                                          
          scrdi   = Substr(scrfull,scrpos,lmin)                                 
          scrfull = Substr(scrfull,1,scrpos-1) || scrdi                         
        End                                                                     
        When (ZSCROLLA = 'CSR') then                                            
        Do                                                                      
          curpos  = ZCSROFF - posdi                                             
          If curpos = 0 Then                                                    
            scrpos  = scrpos + lmin                                             
          Else                                                                  
            scrpos  = scrpos + curpos                                           
          scrdi   = Substr(scrfull,scrpos,lmin)                                 
        End                                                                     
        When (ZSCROLLA = 'PAGE') then                                           
        Do                                                                      
          scrpos  = scrpos + lmin                                               
          scrdi   = Substr(scrfull,scrpos,lmin)                                 
        End                                                                     
        When (ZSCROLLA = 'HALF') then                                           
        Do                                                                      
          scrpos  = scrpos + trunc(lmin/2)                                      
          scrdi   = Substr(scrfull,scrpos,lmin)                                 
        End                                                                     
        Otherwise      /* must be a number */                                   
        Do                                                                      
          scrpos = scrpos + ZSCROLLN                                            
          scrdi = Substr(scrfull,scrpos,lmin)                                   
        End                                                                     
      End                                                                       
    End                                                                         
    Otherwise;                                                                  
  End                                                                           
                                                                                
Return                                                                          
/****************************************************************************/  
/*                                                                          */  
/* Process stdout and stderr                                                */  
/*                                                                          */  
/****************************************************************************/  
                                                                                
ReadOutput :                                                                    
                                                                                
  BGZPRVAL = 'IBM-037'                                                          
  Address ISPEXEC                                                               
  'CONTROL ERRORS RETURN'                                                       
  'TBOPEN BGZPREFS SHARE'                                                       
  TB_RC = RC                                                                    
  'CONTROL ERRORS CANCEL'                                                       
  If TB_RC = 0 | TB_RC = 12 Then                                                
  Do                                                                            
    BGZPRID = 'git.code.page'                                                   
    'TBGET BGZPREFS'                                                            
    If TB_RC = 0 Then                                                           
      'TBCLOSE BGZPREFS'                                                        
  End                                                                           
                                                                                
  HEX15 = X2C('15')                                                             
  gitLine. = ''                                                                 
  k = 0                                                                         
                                                                                
  If stderr.0 > 0 Then                                                          
  Do                                                                            
    HEX20 = X2C('20')                                                           
    Do i = 1 to stderr.0                                                        
      /* looking to see if stderr is ascii */                                   
      If Pos(HEX20,stderr.i) <> 0 Then                                          
      Do                                                                        
        converted = ''                                                          
        Do j = i to stderr.0 while(Pos(HEX20,stderr.i) <> 0)                    
          converted = converted || BGZCNVRT('UTF-8' BGZPRVAL stderr.j)          
        End                                                                     
        /* Need to break the lines at the hex '15' */                           
        If Pos(HEX15,converted) <> 0 Then                                       
        Do                                                                      
          Do While (converted <> '')                                            
            x = Pos(HEX15,converted)                                            
            k = k + 1                                                           
            gitLine.k = Substr(converted,1,x-1)                                 
            converted = Substr(converted,x+1)                                   
          End                                                                   
        End                                                                     
      End                                                                       
      Else                                                                      
      Do                                                                        
        k = k + 1                                                               
        gitLine.k = stderr.i                                                    
      End                                                                       
    End                                                                         
  End                                                                           
                                                                                
  If stdout.0 > 0 Then                                                          
  Do                                                                            
    HEX20 = X2C('20')                                                           
    Do i = 1 to stdout.0                                                        
      /* looking to see if stderr is ascii */                                   
      If Pos(HEX20,stdout.i) <> 0 Then                                          
      Do                                                                        
        converted = ''                                                          
        Do j = 1 to stdout.0 while(Pos(HEX20,stdout.i) <> 0)                    
          converted = converted || BGZCNVRT('UTF-8' BGZPRVAL stdout.j)          
        End                                                                     
        /* Need to break the lines at the hex '15' */                           
        If Pos(HEX15,converted) <> 0 Then                                       
        Do                                                                      
          Do While (converted <> '')                                            
            x = Pos(HEX15,converted)                                            
            k = k + 1                                                           
            gitLine.k = Substr(converted,1,x-1)                                 
            converted = Substr(converted,x+1)                                   
          End                                                                   
        End                                                                     
      End                                                                       
      Else                                                                      
      Do                                                                        
        k = k + 1                                                               
        gitLine.k = stdout.i                                                    
      End                                                                       
    End                                                                         
  End                                                                           
                                                                                
  HEX02 = X2C('02')                                                             
  HEX05 = X2C('05')                                                             
  HEX0D = X2C('0D')                                                             
  Do gl = 1 to k                                                                
    /* Translate any x'05' to x'02' for display purposes */                     
    gitLine.gl = Translate(gitLine.gl,HEX02,HEX05)                              
    /* Translate any x'0D' to blanks */                                         
    gitLine.gl = x04||Translate(gitLine.gl,' ',HEX0D)                           
  End                                                                           
                                                                                
  /* For specific commands need to add a message at the end */                  
  Select                                                                        
    When (gitcmd = 'clone') Then                                                
    Do                                                                          
      If sh_rc = 0 Then                                                         
      Do                                                                        
        k = k + 1                                                               
        gitLine.k = x04||'Cloning completed'                                    
      End                                                                       
      Else                                                                      
      Do                                                                        
        k = k + 1                                                               
        gitLine.k = x04||'Cloning completed with errors'                        
      End                                                                       
    End                                                                         
    When (gitcmd = 'add') Then                                                  
    Do                                                                          
      If sh_rc = 0 Then                                                         
      Do                                                                        
        k = k + 1                                                               
        gitLine.k = x04||'Add files to staging completed'                       
      End                                                                       
      Else                                                                      
      Do                                                                        
        k = k + 1                                                               
        gitLine.k = x04||'Add files to staging completed with errors'           
      End                                                                       
    End                                                                         
    When (gitcmd = 'branch') | (gitcmd = 'stage') Then                          
    Do                                                                          
      Do i = 1 to k                                                             
        /* Need to get rid of the attribute byte */                             
        BGZROW = i                                                              
        BGZLINE = Substr(gitLine.i,2)                                           
        'ISPEXEC TBADD BGZTEMP ORDER'                                           
        If Substr(BGZLINE,1,1) = '*' Then                                       
        Do                                                                      
          /* Indicates current Branch */                                        
          BGZBRANC = Substr(BGZLINE,3)                                          
          'VPUT (BGZBRANC) PROFILE'                                             
        End                                                                     
      End                                                                       
    End                                                                         
    Otherwise                                                                   
      Nop                                                                       
  End                                                                           
  gitLine.0 = k                                                                 
                                                                                
  If gitcmd = 'dbbub' Then                                                      
  Do                                                                            
    'VGET (BGZFLOG) SHARED'                                                     
    confile  = BGZFLOG                                                          
    Address syscall "writefile (confile) 755 gitline."                          
  End 
                                                                          
/* Write the git command output collected in gitline to the temporary work  */
/* file. We are doing this because sometimes the returned data may exceed   */
/* the CLIST Variable Value length of 32,756.  If it does exceed this       */
/* length, the ISPF Pop-Up to display the git command output cannot be used.*/
/* By capturing the results in a temporary file, the ISPEXEC VIEW will be   */
/* used to display the output. NOTE: If trunacation occurs (EXECIO RC=1),   */
/* update the WrkAtr variable in BGZGIT with a larger LRECL/BLKSIZE and/or  */
/* SPACE.                                                                   */
                                                                              
Do WLCV = 1 to gitLine.0 While (rc = 0)                                       
                                                                              
  /* Remove the hex character from beginning of the line */                   
  wrkLine.WLCV = SUBSTR(gitLine.WLCV,2)                                       
                                                                              
  Address TSO("EXECIO "WLCV" DISKW SBGZWRK" ,                                 
    " (STEM wrkLine. FINIS)")                                                
    If (rc > 0) then                                                          
      Say PGM": Error writing to work file. rc="rc                            
                                                                              
End 
                                                                                                                                                         
Return                                                                          
/****************************************************************************/
/* VIEW WORK FILE                                                           */
/*                                                                          */
/*   This function is use to view the work file that was created for the    */
/* command data output extracted from stderr and stdout.                    */
/*                                                                          */
/* Linkage:                                                                 */
/*                                                                          */
/*   rc = ViewWorkFile()                                                    */
/*                                                                          */
/* Returns:                                                                 */
/*                                                                          */
/*     rc      - as defined the Services.                                   */
/*                                                                          */
/****************************************************************************/
ViewWorkFile:                                                                 
                                                                              
rc         = 0                                                                
LMINITDone = 0                                                                
                                                                              
"LMINIT DATAID(GZWORK) DDNAME(SBGZWRK)"                                       
                                                                              
If (rc = 0) Then                                                              
  Do                                                                          
    LMINITDone = 1                                                            
                                                                              
    "VIEW DATAID(&GZWORK)"                                                    
                                                                              
    If (rc <> 0) Then                                                         
      Do;                                                                     
        Say PGM": VIEW of Work File Failed. rc="rc                            
        Say PGM":   "ZERRLM                                                   
      End                                                                     
  End                                                                         
Else                                                                          
  Do                                                                          
    Say PGM": LMINIT of Work File Failed. rc="rc                              
    Say PGM":   "ZERRLM                                                       
  End                                                                         
                                                                              
Save_rc = rc  

If (LMINIT_Done <> 0) Then                        
  Do                                              
                                                  
    "LMFREE DATAID(&GZWORK)"                      
                                                  
    If (rc <> 0) Then                             
      Do                                          
        Say PGM": LMFREE Work File Failed. rc="rc 
        Say PGM":   "ZERRLM                       
      End                                         
    Else                                          
      LMINITDone = 0                              
                                                  
  End                                             
                                                  
rc = MAX(Save_rc,rc)                              
                                                  
Return rc                                                                                                         
