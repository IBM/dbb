<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzgitb system -- common panel body file imbed -->                   
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
                                                                                
<:ENTITY pan_title "Git Command">                                               
<:ENTITY bgzrepos_prompt "Git Repository    :">                                 
<:ENTITY bgzusdir_prompt "Working Directory :">                                 
<:ENTITY bgzbranc_prompt "On Branch ....... :">                                 
                                                                                
<:-- panel fields prompt text follows                               -->         
<:ENTITY panel_instruct_1 space                                                 
'Enter a Git command - Use Git help for commands description'>                  
<:ENTITY panel_instruct_2 space                                                 
'Place cursor on choice and press enter to Retrieve command'>                   
<:ENTITY comment_prompt "Command">                                              
<:ENTITY cmde_prompt "=>">                                                      
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzgitb;                                                                       
 <:--              COPYRIGHT IBM CORP 2019       -->                            
