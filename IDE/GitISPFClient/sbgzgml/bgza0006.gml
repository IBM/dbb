<:doctype dm system (                                                           
)>                                                                              
                                                                                
<copyr> © COPYRIGHT IBM CORP 2019                                               
<help name=bgzA0006 keylist=ISRHLP2 applid=ISR depth=20                         
      width=70 WINTITLE="">Help for Working Directory                           
<area depth=1 extend=on>                                                        
<info width=*>                                                                  
<p>                                                                             
This panel allows you to view, edit, and manage the file that match the         
specified pattern.  Various information about the UNIX file is displayed        
in several columns:                                                             
<P>                                                                             
<HP>Type:</HP> The type of the UNIX file. Can be Dir (for a directory)          
or File.                                                                        
<P>                                                                             
<HP>Git Status:</HP> The Git Status of files which have been modified (changes  
not staged for commit), or staged (changes to be committed), in your local      
working directory.                                                              
<P>                                                                             
Enter a <HP>'/'</HP> in the area next to a file or directory to see a menu      
of actions, or enter an action shortcut character from the following            
list:                                                                           
<DL TSIZE=10 BREAK=FIT>                                                         
 <DTHD>Character                                                                
  <DDHD>Description                                                             
     <DT>E                                                                      
     <DD>Edit this file                                                         
     <DT>EA                                                                     
     <DD>Edit this file in ascii mode                                           
     <DT>EU                                                                     
     <DD>Edit this file in utf-8 mode (Available in z/OS 2.1)                   
     <DT>V                                                                      
     <DD>View this file                                                         
     <DT>VA                                                                     
     <DD>View this file in ascii mode                                           
     <DT>VU                                                                     
     <DD>View this file in utf-8 mode (Available in z/OS 2.1)                   
     <DT>L                                                                      
     <DD>List the files or folders in this directory                            
     <DT>D                                                                      
     <DD>Delete this file                                                       
     <DT>R                                                                      
     <DD>Rename this file                                                       
     <DT>ST                                                                     
     <DD>Request a Git status command for the cloned repository                 
     <DT>AD                                                                     
     <DD>Request a Git add command for the cloned repository                    
     <DT>CO                                                                     
     <DD>Request a Git commit command for the cloned repository                 
     <DT>PS                                                                     
     <DD>Request a Git push command for the cloned repository                   
     <DT>CP                                                                     
     <DD>Request a Git commit and push command for the cloned repository        
     <DT>PL                                                                     
     <DD>Request a Git pull command for the cloned repository                   
     <DT>CM                                                                     
     <DD>Enter any Git command for the selected repository                      
     <DT>UB                                                                     
     <DD>Request a DBB user build of this file                                  
</DL>                                                                           
<P>                                                                             
To create a new file, enter <HP>S FILENAME</HP> on the                          
<HP>Command</HP> line, where FILENAME is the name you want to                   
use for the created file.                                                       
<P>                                                                             
To refresh the status of all files in a listed directory (for example,          
to detect changes to files made outside of this client), enter                  
<HP>REFRESH</HP> on the <HP>Options</HP> line.                                  
<P>                                                                             
To locate a particular file or directory you can use the                        
<HP>Locate</HP> command, abbreviated as<HP>Loc</HP>or<HP>L</HP> to              
locate on the case sensitive file or directory name.                            
</info>                                                                         
</area>                                                                         
</help>                                                                         
<:--    COPYRIGHT IBM CORP 2019       -->                                       
