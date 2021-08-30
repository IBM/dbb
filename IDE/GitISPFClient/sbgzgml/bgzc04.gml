<!DOCTYPE DM SYSTEM>                                                            
<:-- Translation requirements                                        -->        
<:--   The translated text in the SMSG variable must not exceed      -->        
<:--   24 bytes                                                      -->        
                                                                                
<copyr>             COPYRIGHT IBM CORP 2019                                     
                                                                                
<VARCLASS NAME=msgcls TYPE='char 80'>                                           
<VARLIST>                                                                       
<VARDCL NAME=bgzlnger VARCLASS=msgcls>                                          
</VARLIST>                                                                      
                                                                                
<MSGMBR NAME=bgzC04>                                                            
                                                                                
<MSG SUFFIX=0 MSGTYPE=Warning ALARM=YES HELP=*                                  
     SMSG="Invalid Git repository">                                             
Invalid Git repository specified. A Git repository ends with .git               
                                                                                
<MSG SUFFIX=1 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Invalid select code     ">                                           
Codes E (Edit) and D (Delete) allowed                                           
                                                                                
                                                                                
</MSGMBR>                                                                       
<:--              COPYRIGHT IBM CORP 2019       -->                             
