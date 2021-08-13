<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzslreb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
  <:ENTITY panel_title                                                          
     "Git Repository List Actions">                                             
                                                                                
  <:ENTITY window_title                                                         
     "  ">                                                                      
<:-- SIZE LIMIT: none -->                                                       
<:ENTITY bgzrepos_prompt "Git Repository    :">                                 
<:ENTITY bgzusdir_prompt "Working Directory :">                                 
                                                                                
<:-- SIZE LIMIT: none -->                                                       
<:ENTITY selfld_01_text "Git Repository Action">                                
                                                                                
<:-- SIZE LIMIT: none -->                                                       
<:ENTITY choice_01_text "Jump to Working Directory">                            
<:ENTITY choice_02_text "Git Status">                                           
<:ENTITY choice_03_text "Git Add">                                              
<:ENTITY choice_04_text "Git Commit">                                           
<:ENTITY choice_05_text "Git Push">                                             
<:ENTITY choice_06_text "Git Commit and Push">                                  
<:ENTITY choice_07_text "Git Pull">                                             
<:ENTITY choice_08_text "Git Branch">                                           
<:ENTITY choice_09_text "Git Command Prompt">                                   
<:ENTITY choice_10_text "Remove Working Directory">                             
                                                                                
<:-- panel instruction text line - maximum text length = 68 bytes   -->         
 <:ENTITY panel_instruct1                                                       
  "Select a choice and press<hp>ENTER</hp>to process Git repository action.">   
                                                                                
<:-- End of translatable panel text section                          -->        
<:ENTITY panel_width "71">                                                      
                                                                                
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzslreb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
