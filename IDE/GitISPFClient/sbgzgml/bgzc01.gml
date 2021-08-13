<!DOCTYPE DM SYSTEM>                                                            
<:-- Translation requirements                                        -->        
<:--   The translated text in the SMSG variable must not exceed      -->        
<:--   24 bytes                                                      -->        
                                                                                
<copyr>             COPYRIGHT IBM CORP 2019                                     
                                                                                
<VARCLASS NAME=msgcls TYPE='char 80'>                                           
<VARLIST>                                                                       
<VARDCL NAME=bgzlnger VARCLASS=msgcls>                                          
</VARLIST>                                                                      
                                                                                
<MSGMBR NAME=bgzC01>                                                            
                                                                                
<MSG SUFFIX=0 MSGTYPE=Warning ALARM=YES HELP=*>                                 
Suppress .files from USS list                                                   
                                                                                
<MSG SUFFIX=1 MSGTYPE=WARNING ALARM=YES HELP=*>                                 
Git user.email                                                                  
                                                                                
<MSG SUFFIX=2 MSGTYPE=Warning ALARM=YES HELP=*>                                 
Git user.name                                                                   
                                                                                
<MSG SUFFIX=3 MSGTYPE=Warning ALARM=YES HELP=*>                                 
Client Code Page                                                                
                                                                                
<MSG SUFFIX=4 MSGTYPE=Warning ALARM=YES HELP=*                                  
     SMSG="No repository specified">                                            
A Git repository and a z/OS UNIX directory must be entered for Git clone        
                                                                                
<MSG SUFFIX=5 MSGTYPE=Warning ALARM=YES HELP=*                                  
     SMSG="Invalid zOS UNIX dir">                                               
The zOS UNIX directory must start with the / character                          
                                                                                
<MSG SUFFIX=6 MSGTYPE=Warning ALARM=YES HELP=*                                  
     SMSG="Invalid select code     ">                                           
Codes JU (Jump USS), ST (Status), AD (add), CO (Commit), PS (Push),             
CP (Commit & Push), PL (Pull), BR (Branch), CM (Git Command) and                
RM (Remove Working Directory) allowed                                           
                                                                                
<MSG SUFFIX=7 MSGTYPE=WARNING ALARM=YES HELP=*>                                 
_TAG_REDIR_ERR                                                                  
                                                                                
<MSG SUFFIX=8 MSGTYPE=WARNING ALARM=YES HELP=*>                                 
_TAG_REDIR_IN                                                                   
                                                                                
<MSG SUFFIX=9 MSGTYPE=WARNING ALARM=YES HELP=*>                                 
_TAG_REDIR_OUT                                                                  
                                                                                
</MSGMBR>                                                                       
<:--              COPYRIGHT IBM CORP 2019       -->                             
