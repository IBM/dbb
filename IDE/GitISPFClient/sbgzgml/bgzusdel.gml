<!doctype dm system (                                                           
                                                                                
 <:ENTITY bgzusdeb system -- common panel body file imbed -->                   
 <:ENTITY % bgz@ent1 system>   <:-- common ENTITY definitions imbed -->         
            %bgz@ent1;           <:-- activate common definitions     -->       
 <:-- Start of translatable panel text section                        -->       
 <:--   text delimited by " is to be translated                       -->       
 <:--   text should end with '">' as shown.                           -->       
 <:--     the '">' can be moved to the right for text expansion       -->       
 <:-- panel title text follows - maximum length = 53 bytes           -->        
  <:ENTITY panel_title "Confirm Delete ">                                       
                                                                                
 <:-- window title text follows - maximum length = 53 bytes          -->        
  <:ENTITY window_title " ">                                                    
                                                                                
 <:-- panel instruction text line - maximum text length = 53 bytes   -->        
  <:ENTITY group_header_1 "Instructions:">                                      
  <:ENTITY panel_instruct_1                                                     
     "Press<hp>ENTER</hp>to confirm delete.">                                   
  <:ENTITY panel_instruct_2                                                     
     "Press<hp>CANCEL</hp>or<hp>EXIT</hp>to cancel delete.">                    
                                                                                
 <:-- panel fields prompt text follows                               -->        
  <:ENTITY ufile_prompt   "File :">                                             
 <:-- End of translatable panel text section                          -->       
 )>         <:-- DO NOT DELETE THIS LINE -->                                    
 <:-- common panel body file imbed -->                                          
 &bgzusdeb;                                                                     
 <:--              COPYRIGHT IBM CORP 2019       -->                            
