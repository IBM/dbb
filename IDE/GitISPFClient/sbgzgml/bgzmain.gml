<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzmainb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
  <:ENTITY panel_title                                                          
     "Git Interface Primary Option Menu">                                       
<:-- part 1 - point and shoot - primary description follows         -->         
<:-- pad short text with blanks, aligning the ending quote mark     -->         
<:--     all text strings must be the same length, including blanks -->         
  <:ENTITY choice_0_pnts  "Preferences">                                        
  <:ENTITY choice_1_pnts  "Git Init   ">                                        
  <:ENTITY choice_2_pnts  "Repository ">                                        
  <:ENTITY choice_4_pnts  "Exit       ">                                        
<:-- part 2 - additional descriptive text                           -->         
  <:ENTITY choice_0_text                                                        
      "Terminal and user preferences  ">                                        
  <:ENTITY choice_1_text                                                        
      "Create a Git Repository        ">                                        
  <:ENTITY choice_2_text                                                        
      "Work with cloned Git repository">                                        
  <:ENTITY choice_4_text                                                        
      "Exit client                    ">                                        
                                                                                
  <:ENTITY option_prompt  "Option">                                             
<:-- End of translatable panel text section                      -->            
    <:ENTITY sel_width "48">                                                    
    <:ENTITY t_size    "12">                                                    
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzmainb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
