<:doctype dm system (                                                           
)>                                                                              
                                                                                
<copyr> © COPYRIGHT IBM CORP 2019                                               
<help name=bgzA0009 keylist=ISRHLP2 applid=ISR depth=20                         
      width=70 WINTITLE="">Help for Git Init                                    
<area depth=1 extend=on>                                                        
<info width=*>                                                                  
<p>                                                                             
This panel allows you to create a new local repository.                         
<P>                                                                             
You need to enter the working directory where to initialize the repository.     
Executing git init creates a .git subdirectory in the current working directory,
which contains all of the necessary Git metadata for the new repository.        
<p>                                                                             
A quick note: git init and git clone can be easily confused.                    
At a high level, they can both be used to "initialize a new git repository."    
However, git clone is dependent on git init. git clone is used to create a copy 
of an existing repository.                                                      
Internally, git clone first calls git init to create a new repository. It then  
copies the data from the existing repository, and checks out a new set of       
working files.                                                                  
</info>                                                                         
</area>                                                                         
</help>                                                                         
<:--    COPYRIGHT IBM CORP 2019       -->                                       
