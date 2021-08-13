<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzsshkb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
<:ENTITY pan_title "Generate a new SSH key">                                    
                                                                                
<:ENTITY window_title "  ">                                                     
                                                                                
<:-- panel fields prompt text follows                               -->         
<:ENTITY panel_instruct_1 space                                                 
'Verify or substitute your e-mail address, and press enter to generate          
a new SSH key'>                                                                 
<:ENTITY generate_prompt  "Keygen command ">                                    
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzsshkb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
