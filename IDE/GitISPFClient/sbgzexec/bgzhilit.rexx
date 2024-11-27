/* REXX */                                                                      
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZHILIT                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF highlighting                         */         
/*                                                                   */         
/* FUNCTION := The ISPF Client Git highlighting module will colour   */         
/*             the viewed file with git like colours.                */         
/*                                                                   */         
/*                                                                   */         
/* CALLED BY : BGZCMD                                                */         
/*                                                                   */         
/* PARAMETERS :                                                      */         
/*                                                                   */         
/* OUTPUT := None                                                    */         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* LD    11/09/24 Initial version                                    */         
/*                                                                   */         
/*********************************************************************/         
  call isprexpx('I')                                                            
/*Say 'In bgzhilit. Called from' panelsection*/                                 
                                                                                
  I = 0                                                                         
  Line_data.    = ''                                                            
  Shadow_data.  = ''                                                            
                                                                                
  Line_width = ZWIDTH                                                           
                                                                                
  /*-----------------------------------------------------------------*/         
  /* Once we know the line width we split the ZDATA and ZSHADOW      */         
  /* variables into stem arrays.                                     */         
  /*-----------------------------------------------------------------*/         
                                                                                
  Total_length = 0                                                              
  Do While (Total_length < Length(ZDATA))                                       
     I = I + 1                                                                  
     String_start  = 1 + (Line_width * (I-1))                                   
     Line_data.I   = Substr(ZDATA,String_start,Line_width)                      
     Shadow_data.I = Substr(ZSHADOW,String_start,Line_width)                    
     Total_length = Total_length + Line_width                                   
  End                                                                           
                                                                                
  /*-----------------------------------------------------------------*/         
  /* We then use the ZDATA stem array to check for Git stuff         */         
  /* and set the ZSHADOW stem array accordingly                      */         
  /*-----------------------------------------------------------------*/         
                                                                                
  Line_data.0   = I                                                             
  Shadow_data.0 = I                                                             
/*Say ZTITLE*/                                                                  
  filename = ZTITLE                                                             
  shadowfile = filename || '.shadow'                                            
                                                                                
  Address syscall "readfile (filename) filedata."                               
  Address syscall "readfile (shadowfile) shadowdata."                           
                                                                                
  Do I = 1 to Line_data.0                                                       
    If (filedata.0 = shadowdata.0) Then                                         
    Do                                                                          
      /* shadow data present and presumably matches output */                   
      linenum = Strip(Substr(Line_data.I,2,6),LEADING,'0')                      
      If Verify(linenum,'0123456789') Then Iterate                              
                                                                                
      partLine = Substr(Line_data.I,9)                                          
      wholeLine = filedata.linenum                                              
                                                                                
      ending = pos('0'X,partLine)                                               
      if (ending > 0) Then                                                      
        partLine = Substr(partLine,1,ending-1)                                  
                                                                                
      position = pos(partLine,wholeLine)                                        
      if (position = 0) then do                                                 
        Shadow_data.I = '        ' || left(' ',Line_width-8,' ')                
        Iterate                                                                 
      End                                                                       
      shadow = Substr(shadowdata.linenum,position)                              
                                                                                
      shadow = '        ' || Left(shadow,Line_width-8,' ')                      
      Shadow_data.I = shadow                                                    
    End                                                                         
    Else                                                                        
    Do                                                                          
                                                                                
      If Pos('Changes to be committed:',Line_data.I) /= 0 Then                  
        Colour = 'G'                                                            
      If Pos('Changes not staged for commit:',Line_data.I) /= 0 Then            
        Colour = 'R'                                                            
                                                                                
      Select                                                                    
        When (Substr(Line_data.I,1,2) = '==') |,                                
             (Substr(Line_data.I,1,6) = '******') then                          
        Do                                                                      
           NOP                                                                  
        End                                                                     
        /* Git Diff command   */                                                
        When (Substr(Line_data.I,9,10) = 'diff --git') |,                       
             (Substr(Line_data.I,9,6)   = 'index ')     |,                      
             (Substr(Line_data.I,9,4)   = '--- ')       |,                      
             (Substr(Line_data.I,9,4)   = '+++ ')       |,                      
             (Pos('-STARTING EXECUTION-',Line_data.I) /= 0) |,                  
             (Pos('** Build finished',Line_data.I) /= 0) then                   
           Shadow_data.I = '        ' || left('Y',Line_width-8,'Y')             
                                                                                
        When (Substr(Line_data.I,9,3) = '@@ ')          |,                      
             (Pos('Building file',Line_data.I) /= 0) then                       
           Shadow_data.I = '        ' || left('T',Line_width-8,'T')             
                                                                                
        When (Substr(Line_data.I,9,1) = '-')            |,                      
             (Substr(Line_data.I,9,22) = '** Build State : ERROR') |,           
             (Substr(Line_data.I,9,2) = '*!') |,                                
             (Substr(Line_data.I,9,8) = '__RC=1__') then                        
           Shadow_data.I = '        ' || left('R',Line_width-8,'R')             
                                                                                
        When (Substr(Line_data.I,9,1) = '+')            |,                      
             (Substr(Line_data.I,9,22) = '** Build State : CLEAN') |,           
             (Substr(Line_data.I,9,8) = '__RC=0__') |,                          
             (Pos('return code',Line_data.I) /= 0) then                         
           Shadow_data.I = '        ' || left('G',Line_width-8,'G')             
                                                                                
        /* Git status command */                                                
        When (Substr(Line_data.I,10,9) = 'modified:') then                      
           Shadow_data.I = '        ' || left(Colour,Line_width-8,Colour)       
                                                                                
        Otherwise                                                               
           Shadow_data.I = '        ' || left('W',Line_width-8,'W')             
      End                                                                       
      /* Shadow file missing or wasn't updated */                               
                                                                                
      If Substr(Line_data.I,8,1) = '?' then                                     
         Shadow_data.I = Substr(Shadow_data.I,1,9) || 'Z' ||,                   
                         Substr(Shadow_data.I,11)                               
    End                                                                         
  End                                                                           
                                                                                
  /*-----------------------------------------------------------------*/         
  /* We then assign the ZSHADOW variable back                        */         
  /*-----------------------------------------------------------------*/         
                                                                                
  ZSHADOW = ''                                                                  
  Do I = 1 to Shadow_data.0                                                     
     ZSHADOW = ZSHADOW || Shadow_data.I                                         
  End                                                                           
                                                                                
  call isprexpx('T')                                                            
Exit                                                                            
                                                                                
