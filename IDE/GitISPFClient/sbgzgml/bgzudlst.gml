<!doctype dm system (                                                           
  <:ENTITY bgzmenu system>                                                      
  <:ENTITY ispdutil system>                                                     
  <:ENTITY bgzhelp system>                                                      
                                                                                
  <:ENTITY bgzudlsb system -- common panel body file imbed -->                  
                                                                                
  <:-- Start of translatable panel text section                      -->        
  <:--   text delimited by " is to be translated                     -->        
  <:--   text should end with '">' as shown.                         -->        
  <:--     the '">' can be moved to the right for text expnsion      -->        
                                                                                
<:-- panel title text follows - maximum length = 78 bytes           -->         
<:ENTITY pan_title                                                              
  "Working Directory List">                                                     
                                                                                
<:ENTITY window_title                                                           
  "  ">                                                                         
<:-- panel fields prompt text follows                               -->         
<:-- IMPORTANT NOTE for PII translation : in selfld_04_text    -->              
<:-- Perm abbreviation is corresponding to Permission word     -->              
<:ENTITY bgzrepos_prompt "Git Repository    :">                                 
<:ENTITY bgzbranc_prompt "On Branch ....... :">                                 
<:ENTITY ussdir_prompt "Pathname">                                              
<:ENTITY filename_prompt "Filename">                                            
<:ENTITY selfld_02_text "Git Status">                                           
<:ENTITY selfld_03_text "Type">                                                 
<:ENTITY selfld_04_text "Perm">                                                 
<:-- End of translatable panel text section                          -->        
)>         <:-- DO NOT DELETE THIS LINE -->                                     
                                                                                
<:-- common panel body file imbed -->                                           
&bgzudlsb;                                                                      
 <:--              COPYRIGHT IBM CORP 2019       -->                            
