 <!doctype dm system (                                                          
                                                                                
    <:ENTITY bgzusreb system -- common panel body file imbed -->                
    <:ENTITY % bgz@ent1 system>   <:-- common ENTITY definitions imbed -->      
            %bgz@ent1;           <:-- activate common definitions     -->       
 <:-- Start of translatable panel text section                        -->       
 <:--   text delimited by " is to be translated                       -->       
 <:--   text should end with '">' as shown.                           -->       
 <:--     the '">' can be moved to the right for text expansion       -->       
 <:-- panel title text follows - maximum length = 53 bytes           -->        
  <:ENTITY panel_title "File Rename">                                           
                                                                                
 <:-- window title text follows - maximum length = 53 bytes          -->        
  <:ENTITY window_title " ">                                                    
                                                                                
 <:-- panel instruction text line - maximum text length = 53 bytes   -->        
  <:ENTITY ussdir_prompt "Pathname">                                            
  <:ENTITY group_header_1 "Instructions:">                                      
  <:ENTITY panel_instruct_1                                                     
     "Press<hp>ENTER</hp>to rename file.">                                      
  <:ENTITY panel_instruct_2                                                     
     "Press<hp>CANCEL</hp>or<hp>EXIT</hp>to cancel rename.">                    
  <:ENTITY panel_instruct_3                                                     
     "Enter a new file name:">                                                  
                                                                                
  <:ENTITY old_prompt    "Old Name">                                            
  <:ENTITY new_prompt    "New Name">                                            
 <:-- End of translatable panel text section                          -->       
 )>         <:-- DO NOT DELETE THIS LINE -->                                    
 <:-- common panel body file imbed -->                                          
 &bgzusreb;                                                                     
 <:--              COPYRIGHT IBM CORP 2019       -->                            
