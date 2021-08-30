<!doctype dm system (                                                           
                                                                                
  <:ENTITY bgzcfreb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
 <:ENTITY panel_title "Confirm Remove Working Directory">                       
                                                                                
<:-- window title text follows - maximum length = 53 bytes          -->         
 <:ENTITY window_title " ">                                                     
                                                                                
<:ENTITY bgzrepos_prompt "Git Repository    :">                                 
<:ENTITY bgzusdir_prompt "Working Directory :">                                 
<:-- panel instruction text line - maximum text length = 53 bytes   -->         
 <:ENTITY group_header_1 "Instructions:">                                       
 <:ENTITY panel_instruct_1                                                      
    "Press<hp>ENTER</hp>to confirm remove.">                                    
 <:ENTITY panel_instruct_2                                                      
    "Press<hp>CANCEL</hp>or<hp>EXIT</hp>to cancel remove.">                     
 <:ENTITY delete_1_text                                                         
    "(The Cloned repository will be removed from your working directory.">      
 <:ENTITY delete_2_text                                                         
    "Request a Git Status command before to be sure you are up-to-date with     
    the repository.)">                                                          
                                                                                
<:-- panel fields prompt text follows                               -->         
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzcfreb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
