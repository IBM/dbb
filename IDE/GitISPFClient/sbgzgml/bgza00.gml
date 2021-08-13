<!DOCTYPE DM SYSTEM>                                                            
<:-- Translation requirements                                        -->        
<:--   The translated text in the SMSG variable must not exceed      -->        
<:--   24 bytes                                                      -->        
                                                                                
<copyr>             COPYRIGHT IBM CORP 2019                                     
                                                                                
<VARCLASS NAME=msgcls TYPE='char 80'>                                           
<VARLIST>                                                                       
<VARDCL NAME=bgzlnger VARCLASS=msgcls>                                          
</VARLIST>                                                                      
                                                                                
<MSGMBR NAME=bgzA00>                                                            
                                                                                
<MSG SUFFIX=1 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Invalid Command     ">                                               
The primary command entered is invalid                                          
                                                                                
<MSG SUFFIX=2 MSGTYPE=Warning ALARM=YES HELP=*                                  
     SMSG="Invalid command         ">                                           
Primary command 0, 1, 2 or X allowed                                            
                                                                                
<MSG SUFFIX=3 MSGTYPE=Warning ALARM=YES HELP=*                                  
     SMSG="System Error            ">                                           
<VARSUB VAR=BGZLNGER>                                                           
                                                                                
<MSG SUFFIX=4 MSGTYPE=Warning ALARM=YES HELP=*                                  
     SMSG="Invalid initial option  ">                                           
Initial option 1, 2, 3, 4, 5 allowed                                            
                                                                                
</MSGMBR>                                                                       
<:--              COPYRIGHT IBM CORP 2019       -->                             
