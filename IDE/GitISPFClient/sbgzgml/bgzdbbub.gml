 <!doctype dm system (                                                          
                                                                                
    <:ENTITY bgzdbbtb system -- common panel body file imbed -->                
    <:ENTITY % bgz@ent1 system>   <:-- common ENTITY definitions imbed -->      
            %bgz@ent1;           <:-- activate common definitions     -->       
 <:-- Start of translatable panel text section                        -->       
 <:--   text delimited by " is to be translated                       -->       
 <:--   text should end with '">' as shown.                           -->       
 <:--     the '">' can be moved to the right for text expansion       -->       
 <:-- panel title text follows - maximum length = 53 bytes           -->        
  <:ENTITY panel_title "">                                                      
                                                                                
 <:-- window title text follows - maximum length = 53 bytes          -->        
  <:ENTITY window_title "DBB User Build ">                                      
                                                                                
 <:-- panel instruction text line - maximum text length = 53 bytes   -->        
  <:ENTITY usfil_prompt "Build file">                                           
  <:ENTITY group_header_1 "Instructions:">                                      
  <:ENTITY panel_instruct_1                                                     
     "Enter<hp>S</hp>Command to request DBB user build.">                       
  <:ENTITY panel_instruct_2                                                     
     "Press<hp>ENTER</hp>to set additional parameters to be used in script.">   
  <:ENTITY panel_instruct_3                                                     
     "Press<hp>CANCEL</hp>or<hp>EXIT</hp>to cancel DBB user build.">            
  <:ENTITY panel_instruct_4                                                     
     "Enter options for user build operation :">                                
                                                                                
  <:ENTITY script_prompt    "Build script to use  ">                            
  <:ENTITY sandbox_prompt   "Build sandbox folder ">                            
  <:ENTITY logfile_prompt   "Log file location    ">                            
  <:ENTITY hlq_prompt       "Build destination HLQ">                            
  <:ENTITY dsn_no_quotes    "(Fully qualified, no quotes)">                     
  <:ENTITY viewlog_prompt   "View build output on completion">                  
 <:-- End of translatable panel text section                          -->       
 )>         <:-- DO NOT DELETE THIS LINE -->                                    
 <:-- common panel body file imbed -->                                          
 &bgzdbbtb;                                                                     
 <:--              COPYRIGHT IBM CORP 2019       -->                            
