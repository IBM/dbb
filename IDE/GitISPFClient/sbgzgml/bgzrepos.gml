<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY ispdutil system>                                                     
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzrepob system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
<:ENTITY pan_title "Git Repository">                                            
                                                                                
<:ENTITY window_title "  ">                                                     
                                                                                
<:-- panel fields prompt text follows                               -->         
<:ENTITY panel_instruct_1 space                                                 
'Enter new repository to clone or<hp>"/"</hp>against existing cloned            
repository for options'>                                                        
<:ENTITY gitag "Git">                                                           
<:ENTITY repository_prompt "Repository Address">                                
<:ENTITY workdir_prompt  "Working Directory     ">                              
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzrepob;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
