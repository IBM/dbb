<!DOCTYPE DM SYSTEM>                                                            
<:-- Translation requirements                                        -->        
<:--   The translated text in the SMSG variable must not exceed      -->        
<:--   24 bytes                                                      -->        
                                                                                
<copyr>             COPYRIGHT IBM CORP 2019                                     
                                                                                
<VARCLASS NAME=msgcls TYPE='char 80'>                                           
<VARLIST>                                                                       
<VARDCL NAME=bgzlnger VARCLASS=msgcls>                                          
</VARLIST>                                                                      
                                                                                
<MSGMBR NAME=bgzC03>                                                            
                                                                                
<MSG SUFFIX=0 MSGTYPE=Warning ALARM=YES HELP=*                                  
     SMSG="Specify user & password">                                            
User name must be specified in preferences and password must be entered         
when specify password is selected in preferences.                               
                                                                                
<MSG SUFFIX=1 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Invalid select code     ">                                           
Codes CB (Change working branch), PB (Push branch on github),                   
DB (Delete local branch) and JU (Jump to working directory) allowed             
                                                                                
<MSG SUFFIX=2 MSGTYPE=Warning ALARM=YES HELP=*                                  
     SMSG="Remove dir failed">                                                  
<VARSUB VAR=BGZLNGER>                                                           
                                                                                
<MSG SUFFIX=3 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="File not US ASCII       ">                                           
The file being edited has a codepage not equal to Microsoft cp1252 or           
ISO-8859-1. As such certain characters may not be displayed correctly.          
                                                                                
<MSG SUFFIX=4 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="Invalid z/OS Unix file">                                             
When creating a new z/OS Unix file the file name must be entered                
                                                                                
<MSG SUFFIX=5 MSGTYPE=Info ALARM=NO HELP=*                                      
     SMSG="Unix File delete        ">                                           
The Unix file has been deleted                                                  
                                                                                
<MSG SUFFIX=6 MSGTYPE=Info ALARM=NO HELP=*                                      
     SMSG="Unix File rename        ">                                           
The Unix file has been renamed                                                  
                                                                                
<MSG SUFFIX=7  MSGTYPE=Info ALARM=NO HELP=*>                                    
DBB Console                                                                     
                                                                                
<MSG SUFFIX=8  MSGTYPE=Info ALARM=NO HELP=*>                                    
Git Messages                                                                    
                                                                                
<MSG SUFFIX=9 MSGTYPE=WARNING ALARM=YES HELP=*                                  
     SMSG="DBB build log not found">                                            
No DBB build log exists for this file                                           
                                                                                
</MSGMBR>                                                                       
<:--              COPYRIGHT IBM CORP 2019       -->                             
