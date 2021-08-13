<:doctype dm system (                                                           
)>                                                                              
<copyr> © COPYRIGHT IBM CORP    2021                                            
<help name=BGZA0013 keylist=ISRHLP2 applid=ISR depth=20                         
      width=70 WINTITLE="">Help for Edit Entry Panel                            
<area depth=1 extend=on>                                                        
<info>                                                                          
<p>Use the fields below to tailor your<hp>Edit</hp>                             
session.  All fields are optional.                                              
<dl tsize=23 break=fit>                                                         
 <DTHD>Field Name                                                               
 <DDHD>Description                                                              
  <DT>Initial Macro                                                             
  <DD>An initial macro that is to be executed after the data is read            
      but before the data is displayed for                                      
      the edit or view session.                                                 
  <DT>Profile Name                                                              
  <DD>A profile name to be in effect for the edit or view session.              
  <DT>Format Name                                                               
  <DD>A format name for the edit or view action, if you are editing             
      or viewing a formatted file.                                              
  <DT>Panel Name                                                                
  <DD>An edit panel name to use in place of the default                         
      edit or view panel.                                                       
  <DT>Confirm Cancel/Move/Replace                                               
  <DD>Enter a<hp>/</hp>to receive a confirmation                                
      panel on cancel, move, or                                                 
      replace primary commands. Remove the<hp>/</hp>or leave the                
      field blank                                                               
      to perform a cancel, move, or replace primary command without             
      receiving a confirmation panel.                                           
  <DT>Edit/View Mixed Mode                                                      
  <DD>You can edit or view unformatted mixed data that contains both            
      EBCDIC (1-byte) characters and Double Byte Character Set (DBCS            
      or 2-byte) characters. To do this, you must specify mixed mode.           
      Enter a<hp>/</hp>for mixed mode.                                          
      Remove the<hp>/</hp>or leave the field blank for standard EBCDIC.         
  <DT>Preserve VB record length                                                 
  <DD>You can preserve the record lengths of individual records in a            
      variable length file by entering a<hp>/</hp>here. This setting has        
      no effect when the dataset has fixed length records.                      
  <DT>Data encoding                                                             
  <DD>This field can be used when the data is stored using a CCSID              
      representing ASCII or UTF-8 characters. The data will                     
      be converted from  ASCII or UTF-8 to the CCSID set by the                 
      terminal prior to display and data entered from the terminal will         
      be converted from the terminal CCSID to ASCII or UTF-8 before             
      being saved in the data set.                                              
      Note: The UTF-8 option is only available with z/OS 2.1 and above          
</dl>                                                                           
</info>                                                                         
</area>                                                                         
</help>                                                                         
<:--  COPYRIGHT IBM CORP 2021       -->                                         
