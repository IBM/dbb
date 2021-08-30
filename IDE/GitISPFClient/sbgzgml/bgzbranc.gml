<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY ispdutil system>                                                     
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzbranb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
<:ENTITY pan_title "Git Branches">                                              
                                                                                
<:ENTITY window_title "  ">                                                     
                                                                                
<:ENTITY bgzrepos_prompt "Git Repository    :">                                 
<:ENTITY bgzusdir_prompt "Working Directory :">                                 
<:-- panel fields prompt text follows                               -->         
<:ENTITY panel_instruct_1 space                                                 
'Enter new branch to create or<hp>"/"</hp>against existing branch               
for options'>                                                                   
<:ENTITY branch_prompt "Branch Name">                                           
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzbranb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
