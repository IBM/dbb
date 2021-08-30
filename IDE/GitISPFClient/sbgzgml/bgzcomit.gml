<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzcomib system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
                                                                                
<:ENTITY pan_title "Git Commit">                                                
                                                                                
<:ENTITY window_title "  ">                                                     
                                                                                
<:-- panel fields prompt text follows                               -->         
<:ENTITY bgzrepos_prompt "Git Repository    :">                                 
<:ENTITY bgzusdir_prompt "Working Directory :">                                 
<:ENTITY bgzbranc_prompt "On Branch ....... :">                                 
<:-- panel fields prompt text follows                               -->         
<:ENTITY status_prompt "Reason">                                                
<:ENTITY modfile_prompt "Changes to be committed">                              
<:ENTITY panel_instruct_1 space                                                 
'Enter a comment to associate to your commit and/or<hp>"R"</hp>against files    
you want to reset to unstage'>                                                  
<:ENTITY comment_prompt "Comment">                                              
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzcomib;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
