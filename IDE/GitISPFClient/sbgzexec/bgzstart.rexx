/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BGZSTART                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := ISPF Git Client startup module                */         
/*                                                                   */         
/* FUNCTION := The ISPF Client startup module accepts the required   */         
/*             parameters and is started as a newappl (BGZ). It      */         
/*             can then VPUT those paramaters into the shared pool   */         
/*             in the BGZ appl                                       */         
/*                                                                   */         
/* CALLED BY : BGZ - Invocation EXEC                                 */         
/*                                                                   */         
/* PARAMETERS                                                        */         
/*                                                                   */         
/* OUTPUT := None                                                    */         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* XH    10/01/19 Initial version                                    */         
/*                                                                   */         
/*********************************************************************/         
                                                                                
  Address ISPEXEC                                                               
  'VGET (ZENVIR) SHARED'                                                        
                                                                                
  /* Set the location for Java, Rocket and DBB */                               
  Call BGZCONF                                                                  
  'VGET (BGZJAVAH,BGZROCKH,BGZBASH,BGZCGIT,BGZMAN,BGZPERL5,BGZDBBH) SHARED'     
  'VGET (BGZCAINF) SHARED'                                                      
                                                                                
  If substr(ZENVIR,6,3) >= '7.1' Then                                           
     BGZEUTF = 'TRUE'                                                           
  Else                                                                          
     BGZEUTF = 'FALSE'                                                          
  'VPUT (BGZEUTF) SHARED'                                                       
                                                                                
  gitenv.1  = 'export JAVA_HOME='BGZJAVAH                                       
  gitenv.2  = 'export DBB_HOME='BGZDBBH                                         
  gitenv.3  = 'export GIT_SHELL='BGZBASH'/bin/bash'                             
  gitenv.4  = 'export GIT_EXEC_PATH='BGZCGIT'/libexec/git-core'                 
  gitenv.5  = 'export GIT_TEMPLATE_DIR='BGZCGIT'/share/git-core/templates'      
  gitenv.6  = 'export PATH=$PATH:$JAVA_HOME/bin:'BGZROCKH'/bin'                 
  gitenv.7  = 'export MANPATH=$MANPATH:'BGZMAN'/man'                            
  gitenv.8  = 'export PERL5LIB=$PERL5LIB:'BGZPERL5'/lib/perl5'                  
  gitenv.9  = 'export _BPXK_AUTOCVT=ON'                                         
  gitenv.10 = 'export _CEE_RUNOPTS="FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"'        
  gitenv.11 = 'export _TAG_REDIR_ERR=txt'                                       
  gitenv.12 = 'export _TAG_REDIR_IN=txt'                                        
  gitenv.13 = 'export _TAG_REDIR_OUT=txt'                                       
                                                                                
  /*get home directory */                                                       
  sh_rc = bpxwunix('pwd',,stdout.,stderr.)                                      
  home_dir=stdout.1                                                             
  gitenv.14 = 'export HOME='home_dir                                            
                                                                                
  /* Optional parms */                                                          
  o = 14                                                                        
  If BGZCAINF /= '' Then                                                        
  Do                                                                            
    o = o + 1                                                                   
    gitenv.o = 'export GIT_SSL_CAINFO='BGZCAINF                                 
  End                                                                           
  gitenv.0 = o                                                                  
                                                                                
  BGZENVIR = ''                                                                 
  Do i = 1 to gitenv.0                                                          
    BGZENVIR = BGZENVIR || gitenv.i||';'                                        
  End                                                                           
                                                                                
  'VPUT (BGZENVIR) SHARED'                                                      
                                                                                
  Exitconf = 0                                                                  
  BGZPRVAL = ''                                                                 
  Do While (Exitconf = 0)                                                       
    'SELECT PANEL(BGZMAIN) SCRNAME(BGZ) OPT('BGZPRVAL')'                        
    Sel_rc = rc                                                                 
                                                                                
      Exitconf = 1                                                              
    BGZPRVAL = ''                                                               
  End                                                                           
                                                                                
Exit                                                                            
