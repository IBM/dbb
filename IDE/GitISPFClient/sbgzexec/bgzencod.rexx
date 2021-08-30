/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZENCOD                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client encoding module               */         
/*                                                                   */         
/* FUNCTION := The ISPF Client main module displays the preferences  */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* XH    14/01/19 Initial version                                    */         
/*                                                                   */         
/*********************************************************************/         
                                                                                
  Parse Arg Str                                                                 
                                                                                
  OPTIONS EXMODE                                                                
  Result = ''                                                                   
  FoundCP = 0                                                                   
  Do While Str <> ''                                                            
    Parse Var Str 1 First 2 Str                                                 
    Select;                                                                     
      When DATATYPE(First, 'Alphanumeric') = 1 Then NOP                         
      When First = '-' Then First = '%2D'                                       
      When First = '_' Then First = '%5F'                                       
      When First = '.' Then First = '%2E'                                       
      When First = '/' Then First = '%2F'                                       
      When First = ' ' Then First = '+'                                         
      When First = '%' Then First = '%25'                                       
      When First = '"' Then First = '%22'                                       
      When First = '#' Then First = '%23'                                       
      When First = '$' Then First = '%24'                                       
      When First = '*' Then First = '%2A'                                       
      When First = '&' Then First = '%26'                                       
      When First = "'" Then First = '%27'                                       
      When First = '(' Then First = '%28'                                       
      When First = ')' Then First = '%29'                                       
      When First = '+' Then First = '%2B'                                       
      When First = ',' Then First = '%2C'                                       
      When First = ':' Then First = '%3A'                                       
      When First = ';' Then First = '%3B'                                       
      When First = '<' Then First = '%3C'                                       
      When First = '=' Then First = '%3D'                                       
      When First = '>' Then First = '%3E'                                       
      When First = '?' Then First = '%3F'                                       
      When First = '@' Then First = '%40'                                       
      When First = '[' Then First = '%5B'                                       
      When First = '\' Then First = '%5C'                                       
      When First = ']' Then First = '%5D'                                       
      When First = '^' Then First = '%5E'                                       
      When First = '`' Then First = '%60'                                       
      When First = '{' Then First = '%7B'                                       
      When First = '|' Then First = '%7C'                                       
      When First = '}' Then First = '%7D'                                       
      When First = '~' Then First = '%7E'                                       
      Otherwise                                                                 
      Do                                                                        
        BGZPRVAL = 'IBM-037'                                                    
        If FoundCP = 0 Then                                                     
        Do                                                                      
          FoundCP = 1                                                           
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
        End                                                                     
                                                                                
        If BGZPRVAL <> '' Then                                                  
        Do                                                                      
          Converted = BGZCNVRT(BGZPRVAL 'UTF-8' First)                          
          First = ''                                                            
          Do While Converted <> ''                                              
            Parse Var Converted 1 ByteOne 2 Converted                           
            First = First || '%' || C2X(ByteOne)                                
          End                                                                   
        End                                                                     
      End                                                                       
    End                                                                         
    Result = Result || First                                                    
  End                                                                           
                                                                                
  Return Result                                                                 
