<!DOCTYPE DM SYSTEM>                                                            
<:-- Translation requirements                                        -->        
<:--   The translated text in the SMSG variable must not exceed      -->        
<:--   24 bytes                                                      -->        
                                                                                
<copyr>             COPYRIGHT IBM CORP 2019                                     
                                                                                
<VARCLASS NAME=msgcls TYPE='char 80'>                                           
<VARLIST>                                                                       
<VARDCL NAME=bgzlnger VARCLASS=msgcls>                                          
</VARLIST>                                                                      
                                                                                
<MSGMBR NAME=bgzC02>                                                            
                                                                                
<MSG SUFFIX=0 MSGTYPE=Warning ALARM=YES HELP=*>                                 
Specify Userid/Password                                                         
                                                                                
<MSG SUFFIX=1 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Invalid command ">                                                   
Only S, L, LOC, LOCATE, REF, or REFRESH commands allowed                        
                                                                                
<MSG SUFFIX=2 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Not a UNIX directory">                                               
The L line command can only be used against a UNIX directory                    
                                                                                
<MSG SUFFIX=3 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Invalid UNIX file type">                                             
Edit is only valid for regular UNIX files                                       
                                                                                
<MSG SUFFIX=4 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Invalid select code     ">                                           
Codes E (Edit), EA (Edit ASCII), EU (Edit UTF-8),                               
      V (View), VA (View ASCII), VU (View UTF-8),                               
      B (Browse), D (Delete) R (Rename),                                        
ST (Status), AD (Add), CO (Commit), PS (Push), CP (Commit & Push), PL (Pull),   
CM (Command Prompt) UB (DBB user build) and UL (View DBB log) allowed           
                                                                                
<MSG SUFFIX=5  MSGTYPE=Warning ALARM=YES HELP=*                                 
     SMSG="Edit/View failed">                                                   
<VARSUB VAR=BGZLNGER>                                                           
                                                                                
<MSG SUFFIX=6 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Invalid select code     ">                                           
Codes E (Edit), EA (Edit ASCII), V (View), VA (View ASCII), B (Browse),         
D (Delete, R (Rename),                                                          
ST (Status), AD (Add), CO (Commit), PS (Push), CP (Commit & Push), PL (Pull),   
CM (Command Prompt) UB (DBB user build) and UL (View DBB log) allowed           
                                                                                
<MSG SUFFIX=7  MSGTYPE=Info ALARM=NO HELP=*>                                    
File                                                                            
                                                                                
<MSG SUFFIX=8  MSGTYPE=Info ALARM=NO HELP=*>                                    
Dir                                                                             
                                                                                
<MSG SUFFIX=9 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Invalid select code     ">                                           
Code L (List),                                                                  
ST (Status), AD (Add), CO (Commit), PU (Push), CP (Commit & Push), PL (Pull),   
and CM (Command Prompt) allowed                                                 
                                                                                
</MSGMBR>                                                                       
<:--              COPYRIGHT IBM CORP 2019       -->                             
