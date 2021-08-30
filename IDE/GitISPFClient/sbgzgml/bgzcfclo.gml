<!doctype dm system (                                                           
                                                                                
  <:ENTITY bgzcfclb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
 <:ENTITY panel_title "Confirm Action on Clone operation">                      
                                                                                
<:-- window title text follows - maximum length = 53 bytes          -->         
 <:ENTITY window_title " ">                                                     
                                                                                
<:ENTITY bgzrepos_prompt "Git Repository    :">                                 
<:ENTITY bgzusdir_prompt "Working Directory :">                                 
<:-- panel instruction text line - maximum text length = 53 bytes   -->         
 <:ENTITY group_header_1 "Instructions:">                                       
 <:ENTITY panel_instruct_1                                                      
    "The working directory already exists. This could be caused by an incorrect 
disconnection from ISPF following a previous clone operation.">                 
 <:ENTITY panel_instruct_2                                                      
    "Select an action to request for this clone.">                              
 <:ENTITY action_prompt "Clone Action">                                         
 <:ENTITY reconnect_prompt                                                      
    "Reconnect the clone table with Git repository">                            
 <:ENTITY replace_prompt                                                        
    "Delete the working directory and redo clone action">                       
                                                                                
<:-- panel fields prompt text follows                               -->         
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzcfclb;                                                                      
 <:--              COPYRIGHT IBM CORP 2020       -->                            
