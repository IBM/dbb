<:doctype dm system (                                                           
)>                                                                              
                                                                                
<copyr> © COPYRIGHT IBM CORP 2019                                               
<help name=bgzA0004 keylist=ISRHLP2 applid=ISR depth=20                         
      width=70 WINTITLE="">Help for Git repository                              
<area depth=1 extend=on>                                                        
<info width=*>                                                                  
<p>                                                                             
This panel allows you to view and manage your cloned repositories.              
Only repositories cloned by the currently z/OS logged in user will              
be displayed.                                                                   
<P>                                                                             
To clone a new repository, enter it's github address and the working directory  
location where you want to have it cloned.                                      
If your repository address or your working directory is greater than the field  
provided then you can scroll left or right or press<HP>PF4</HP>to get a         
scrollable field entry panel.                                                   
<P>                                                                             
A cloned repository may have a <HP>'*'</HP> decorator to their left indicating  
your branch is ahead of 'origin/branch' by n commit(s).                         
You have to use "git push" to publish your local commits.                       
<P>                                                                             
Enter a <HP>'/'</HP> in the area next to a cloned repository to see             
a menu of actions, or enter an action shortcut character from the               
following list:                                                                 
                                                                                
<DL TSIZE=10 BREAK=FIT>                                                         
 <DTHD>Character                                                                
 <DDHD>Description                                                              
   <DT>JU                                                                       
   <DD>Open the z/OS UNIX browser to the UNIX directory list where the          
   selected repository is cloned                                                
   <DT>ST                                                                       
   <DD>Request a Git Status for the selected repository                         
   <DT>AD                                                                       
   <DD>Request a Git Add for the selected repository                            
   <DT>CO                                                                       
   <DD>Request a Git Commit for the selected repository                         
   <DT>PS                                                                       
   <DD>Request a Git Push for the selected repository                           
   <DT>CP                                                                       
   <DD>Request a Git Commit and Push for the selected repository                
   <DT>PL                                                                       
   <DD>Request a Git Pull for the selected repository                           
   <DT>BR                                                                       
   <DD>Request a Git Branch for the selected repository                         
   <DT>CM                                                                       
   <DD>Enter any Git command for the selected repository                        
</DL>                                                                           
</info>                                                                         
</area>                                                                         
</help>                                                                         
<:--    COPYRIGHT IBM CORP 2019       -->                                       
