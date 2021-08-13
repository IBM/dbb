/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZINIT                                                   */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client create repository             */         
/*                                                                   */         
/* FUNCTION :=                                                       */         
/*                                                                   */         
/*                                                                   */         
/* CALLED BY : BGZMAIN                                               */         
/*                                                                   */         
/* PARAMETERS :                                                      */         
/*                                                                   */         
/* OUTPUT := None                                                    */         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* XH    04/02/19 Initial version                                    */         
/*                                                                   */         
/*********************************************************************/         
                                                                                
   Address ISPEXEC                                                              
                                                                                
   'TBOPEN BGZCLONE'                                                            
   /* If it didn't exist yet, create it. */                                     
   If RC = 8 Then                                                               
   Do                                                                           
     'TBCREATE BGZCLONE',                                                       
     'KEYS(BGZREPOS,BGZUSDIR) NAMES(BGZRPCMD,BGZSTATU,BGZBRANC) WRITE'          
   End                                                                          
                                                                                
   'TBSORT BGZCLONE FIELDS(BGZREPOS)'                                           
   'VGET (BGZICONV) SHARED'                                                     
                                                                                
   /* panel BGZinit to enter a working directory */                             
   BGZGIT  = ''                                                                 
   DoGitinit = 0                                                                
   Do Until DoGitinit <> 0                                                      
     'DISPLAY PANEL(BGZINIT)'                                                   
     TB_RC = RC                                                                 
     'VGET (ZVERB)'                                                             
     If TB_RC = 8 | ZVERB = 'CANCEL' Then                                       
       DoGitinit = -1                                                           
     If DoGitinit <> -1 Then                                                    
     Do                                                                         
       DoGitinit = 1                                                            
       If BGZUSDIR = '' Then                                                    
       Do                                                                       
         'SETMSG MSG(BGZC014)'                                                  
         DoGitinit = -1                                                         
       End                                                                      
                                                                                
       If Verify(BGZUSDIR,'/') = 1 Then                                         
       Do                                                                       
         'SETMSG MSG(BGZC015)'                                                  
         DoGitinit = -1                                                         
       End                                                                      
                                                                                
       /* Create the USS directory if not already existing */                   
       Address SYSCALL 'readdir 'BGZUSDIR' ls. lsst.'                           
       If ls.0 = 0 Then                                                         
       Do                                                                       
         /* Address SYSCALL 'mkdir (BGZUSDIR)' 750 */                           
         shellcmd = 'mkdir -m 750 -p 'BGZUSDIR                                  
         sh_rc = bpxwunix(shellcmd,,stdout.,stderr.)                            
         If sh_rc > 0 Then                                                      
         Do                                                                     
           Do e = 1 to stderr.0                                                 
             Say stderr.e                                                       
           End                                                                  
           Do e = 1 to stdout.0                                                 
             Say stdout.e                                                       
           End                                                                  
           DoGitinit = 0                                                        
         End                                                                    
       End                                                                      
                                                                                
       If DoGitinit = 1 Then                                                    
       Do                                                                       
         /* Full Git init command  */                                           
         Git_rc = 0                                                             
         'VGET BGZENVIR SHARED'                                                 
         shellcmd  = ''                                                         
         shellcmd = shellcmd || BGZENVIR                                        
                                                                                
         shellcmd=shellcmd || 'cd' BGZUSDIR';'||,                               
                'git init'                                                      
                                                                                
         Git_rc = BGZCMD('gitcmd' shellcmd)                                     
         If Git_rc = 0 Then                                                     
         Do                                                                     
                                                                                
           /* Create BGZCLONE row for the init local repository  */             
                                                                                
           /* Capture last folder of BGZUSDIR as repository name */             
           x = lastPos('/',BGZUSDIR)                                            
           repoName = Substr(BGZUSDIR,x+1)                                      
           BGZREPOS = 'local_repo/'repoName                                     
           BGZSTATU = ''                                                        
           'TBADD BGZCLONE ORDER'                                               
                                                                                
           /* Create and write .gitattributes file  */                          
           call syscalls 'ON'                                                   
           address syscall                                                      
           path = BGZUSDIR'/.gitattributes'                                     
           'open' path,                                                         
                  O_rdwr+O_creat+O_trunc,                                       
                  755                                                           
           if retval=-1 then                                                    
           do                                                                   
             say 'file not opened, error codes' errno errnojr                   
             return                                                             
           end                                                                  
           fd = retval                                                          
           txt = '# line endings' || esc_n                                      
           'write' fd 'txt' length(txt)                                         
           if retval=-1 then                                                    
             say 'record not written, error codes' errno errnojr                
           txt = '* text eol=lf' || esc_n                                       
           'write' fd 'txt' length(txt)                                         
           if retval=-1 then                                                    
              say 'record not written, error codes' errno errnojr               
           txt = ' ' || esc_n                                                   
           'write' fd 'txt' length(txt)                                         
           if retval=-1 then                                                    
             say 'record not written, error codes' errno errnojr                
           txt = '# This is an example of a file encoding definition. ' ||,     
             'You need one of these for each different file extension' ||,      
             esc_n                                                              
           'write' fd 'txt' length(txt)                                         
           if retval=-1 then                                                    
              say 'record not written, error codes' errno errnojr               
                                                                                
           txt = '*.cbl zos-working-tree-encoding=ibm-1047 ' ||,                
                 'git-encoding=utf-8' || esc_n                                  
           'write' fd 'txt' length(txt)                                         
           if retval=-1 then                                                    
              say 'record not written, error codes' errno errnojr               
           'close' fd                                                           
           tempPath = path || '.temp'                                           
           iconvcommand = 'cat' path '|' ||,                                    
                     BGZICONV '-f IBM-1047 -t UTF-8 >' ||,                      
                     tempPath '&& mv' tempPath path                             
           call bpxwunix iconvcommand                                           
           tagchmodcommand = 'chtag -c UTF-8' path ||,                          
                     ' && chmod 755 ' path                                      
           call bpxwunix tagchmodcommand                                        
         End                                                                    
       End                                                                      
     End                                                                        
   End                                                                          
                                                                                
   Address ISPEXEC                                                              
   'TBCLOSE BGZCLONE'                                                           
                                                                                
Return 0                                                                        
