<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzdbbpb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
<:ENTITY window_title "  ">                                                     
                                                                                
<:-- SIZE LIMIT: none -->                                                       
<:ENTITY pan_title "Script Parameters">                                         
                                                                                
<:-- SIZE LIMIT: none -->                                                       
<:ENTITY insfld_01_text "Submit DBB user build">                                
<:ENTITY panel_instruct_1 space                                                 
     'Enter new parameter name to create or<hp>"e"</hp>to edit, <hp>"d"</hp>    
to delete'>                                                                     
                                                                                
<:-- panel fields prompt text follows                               -->         
<:ENTITY bgzscript_prompt "Build Script">                                       
<:ENTITY grpfld_01_text "Build Script Parameters">                              
<:ENTITY selfld_01_text "Name                    ">                             
<:ENTITY selfld_02_text "Value ">                                               
<:-- End of translatable panel text section                          -->        
                                                                                
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzdbbpb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
