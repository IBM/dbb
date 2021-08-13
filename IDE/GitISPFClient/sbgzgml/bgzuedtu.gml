<:-- BGZEDTU panel -->                                                          
                                                                                
<:doctype dm system(                                                            
                                                                                
  <:ENTITY bgzuedtb system -- common panel file imbed -->                       
                                                                                
<:-- Start of translatable panel text section                        -->        
<:--   text delimited by " is to be translated                       -->        
<:--   text should end with '">' as shown.                           -->        
<:--     the '">' can be moved to the right for text expansion       -->        
                                                                                
 <:-- panel title text follows - maximum length = 58 bytes           -->        
  <:ENTITY panel_title "Edit Entry Panel">                                      
                                                                                
 <:-- window title text follows - maximum length = 58 bytes          -->        
  <:ENTITY window_title " ">                                                    
                                                                                
 <:-- choice selection text entries follow                           -->        
 <:-- the length of the text should be limited to 44 bytes           -->        
 <:-- pad short text with blanks, aligning the ending quote mark     -->        
                                                                                
  <:ENTITY % bgz@ent1 system>   <:-- common ENTITY definitions imbed -->        
           %bgz@ent1;           <:-- activate common definitions     -->        
                                                                                
<:-- panel formatting controls follow                                -->        
  <:ENTITY panel_width      "60">                                               
  <:ENTITY dcol_pmtwidth    "18">                                               
  <:ENTITY sfld_selwidth    "36">                                               
                                                                                
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- panel file imbed -->                                                       
&bgzuedtb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
