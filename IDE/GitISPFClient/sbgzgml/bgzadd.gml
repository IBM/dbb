<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY ispdutil system>                                                     
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzaddb system -- common panel body file imbed -->                   
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
<:ENTITY pan_title "Git Add Files">                                             
                                                                                
<:ENTITY window_title "  ">                                                     
                                                                                
<:ENTITY bgzrepos_prompt "Git Repository    :">                                 
<:ENTITY bgzusdir_prompt "Working Directory :">                                 
<:ENTITY bgzbranc_prompt "On Branch ....... :">                                 
<:-- panel fields prompt text follows                               -->         
<:ENTITY panel_instruct_1 space                                                 
'Select by<hp>"/"</hp>the files to add to staging'>                             
<:ENTITY status_prompt "Reason">                                                
<:ENTITY modfile_prompt "Changes not staged for commit">                        
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzaddb;                                                                       
 <:--              COPYRIGHT IBM CORP 2019       -->                            
