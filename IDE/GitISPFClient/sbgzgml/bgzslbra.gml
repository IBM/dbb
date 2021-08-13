<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzslbrb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
  <:ENTITY panel_title                                                          
     "Git Branch List Actions">                                                 
                                                                                
  <:ENTITY window_title                                                         
     "  ">                                                                      
<:-- SIZE LIMIT: none -->                                                       
<:ENTITY bgzrepos_prompt "Git Repository    :">                                 
<:ENTITY bgzusdir_prompt "Working Directory :">                                 
<:ENTITY bgzbranch_prompt "Branch Name       :">                                
                                                                                
<:-- SIZE LIMIT: none -->                                                       
<:ENTITY selfld_01_text "Git Branch Action">                                    
                                                                                
<:-- SIZE LIMIT: none -->                                                       
<:ENTITY choice_01_text "Checkout to working branch">                           
<:ENTITY choice_02_text "Push branch to origin">                                
<:ENTITY choice_03_text "Delete local branch">                                  
<:ENTITY choice_04_text "Jump to working directory">                            
                                                                                
<:-- panel instruction text line - maximum text length = 68 bytes   -->         
 <:ENTITY panel_instruct1                                                       
  "Select a choice and press<hp>ENTER</hp>to process Git branch action.">       
                                                                                
<:-- End of translatable panel text section                          -->        
<:ENTITY panel_width "71">                                                      
                                                                                
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzslbrb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
