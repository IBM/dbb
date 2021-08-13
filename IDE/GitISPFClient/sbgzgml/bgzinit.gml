<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzinitb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
<:ENTITY pan_title "Create a new local repository">                             
                                                                                
<:ENTITY window_title "  ">                                                     
                                                                                
<:-- panel fields prompt text follows                               -->         
<:ENTITY panel_instruct_1 space                                                 
'Enter working directory where to initialize the new local repository.'>        
<:ENTITY directory_prompt  "z/OS UNIX Directory   ">                            
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzinitb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
