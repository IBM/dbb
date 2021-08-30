/* REXX */                                                                      
/*%STUB CALLCMD*/                                                               
/*********************************************************************/         
/*                                                                   */         
/* IBM ISPF Git Interface                                            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/*                                                                   */         
/* NAME := BGZuslst                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := GIT ISPF Client z/OS Unix directory list      */         
/*                                                                   */         
/* FUNCTION := The ISPF Client Git z/OS Unix dir allow userid to     */         
/*             work on cloned repository files on USS                */         
/*                                                                   */         
/*                                                                   */         
/* CALLED BY : BGZREPOS                                              */         
/*                                                                   */         
/* PARAMETERS : BGZUSDIR cloned location path                        */         
/*                                                                   */         
/* OUTPUT :=                                                         */         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* XH    15/01/19 Initial version                                    */         
/* XH    14/05/19 Propose Delete Rename and S on uss files           */         
/*                                                                   */         
/*********************************************************************/         
                                                                                
  Address ISPEXEC                                                               
  'VGET (BGZUSSDR) PROFILE'                                                     
  'VGET (BGZUSREP) PROFILE'                                                     
  'VGET (BGZBRANC) PROFILE'                                                     
  BGZPATH = '"'BGZUSSDR'"'                                                      
  BGZUSLOC = BGZUSSDR                                                           
                                                                                
  /* Get the suppress .files from preference (Y or N)  */                       
  'TBOPEN BGZPREFS SHARE'                                                       
  TB_RC = RC                                                                    
  If TB_RC = 0 | TB_RC = 12 Then                                                
  Do                                                                            
    BGZPRID = 'git.point.file'                                                  
    'TBGET BGZPREFS'                                                            
    If RC = 0 Then                                                              
      BGZSPFIL = BGZPRVAL                                                       
  End                                                                           
  If TB_RC = 0 Then                                                             
   'TBCLOSE BGZPREFS'                                                           
                                                                                
                                                                                
  TabNum = 1                                                                    
  Call Dispuss                                                                  
                                                                                
Return 0                                                                        
                                                                                
DISPUSS:                                                                        
                                                                                
  /* Recursive routine to display the contents of a UNIX directory */           
                                                                                
  ReturnCode = -1                                                               
  DSNROW  = '00000001'                                                          
  Refresh = false                                                               
  Rebuild = 1                                                                   
  Do Until(ReturnCode <> -1)                                                    
    If Rebuild = 1 Then                                                         
    Do                                                                          
      Call Getuss                                                               
      Rebuild = 0                                                               
    End                                                                         
    Refresh = false                                                             
    'TBTOP 'TabName                                                             
    'TBSKIP 'TabName' ROW('DSNROW')'                                            
    'TBDISPL 'TabName' PANEL(BGZUDLST)'                                         
    TB_RC = RC                                                                  
    'VGET (ZVERB)'                                                              
                                                                                
    If TB_RC = 8 | ZVERB = 'CANCEL' Then                                        
    Do                                                                          
      ReturnCode = 0                                                            
    End                                                                         
    Else                                                                        
    Do                                                                          
      Select                                                                    
         When (translate(Word(BGZCMD,1)) = 'REF'  |,                            
               translate(Word(BGZCMD,1)) = 'REFRESH') Then                      
         Do                                                                     
           Refresh = true                                                       
           Rebuild = 1                                                          
           BGZCMD = ''                                                          
           Iterate                                                              
         End                                                                    
         When (translate(Word(BGZCMD,1)) = 'L'  |,                              
               translate(Word(BGZCMD,1)) = 'LOC'  |,                            
               translate(Word(BGZCMD,1)) = 'LOCATE') Then                       
         Do                                                                     
           'TBBOTTOM 'TabName' NOREAD'                                          
           Parse Var BGZCMD . BGZUSFIL                                          
           'TBSCAN 'TabName' ARGLIST(BGZUSFIL) PREVIOUS',                       
                  ' NOREAD ROWID(DSNROW) CONDLIST(LE)'                          
           BGZCMD = ''                                                          
           Iterate                                                              
         End                                                                    
         When (translate(Word(BGZCMD,1)) = 'S' ) Then                           
         Do                                                                     
           Parse Var BGZCMD . BGZUSFIL                                          
           FullName = BGZUSSDR'/'BGZUSFIL                                       
                                                                                
           /* Check file name exists */                                         
           If Length(BGZUSFIL) = 0 Then                                         
           Do                                                                   
             'SETMSG MSG(BGZC034)'                                              
           End                                                                  
                                                                                
           Else                                                                 
           Do                                                                   
             RC = BGZEDIT("E" "<>New" FullName)                                 
             If RC = 0 Then                                                     
             Do                                                                 
               'TBADD 'TabName' ORDER'                                          
               'TBSORT   'TabName' FIELDS(BGZUSFIL)'                            
               Call GetLSinf(FullName)                                          
               Call GetUss(BGZUSFIL',')                                         
             End                                                                
             Else                                                               
               If RC > 4 Then                                                   
               Do                                                               
                 'VGET (BGZEDITM) SHARED'                                       
                 BGZLNGER = BGZEDITM                                            
                 'SETMSG MSG(BGZC025)'                                          
               End                                                              
             Iterate                                                            
           End                                                                  
         End                                                                    
         When (BGZCMD <> '') then                                               
         Do                                                                     
           'SETMSG MSG(BGZC021)'                                                
           Iterate                                                              
         End                                                                    
         Otherwise                                                              
           NOP                                                                  
      End                                                                       
                                                                                
      If ZTDSELS = 0 Then                                                       
        DSNROW = ZTDTOP                                                         
      Do While ZTDSELS > 0                                                      
        'TBMOD 'TabName' ORDER'                                                 
        'TBGET 'TabName' ROWID(DSNROW)'                                         
        If ZTDSELS = 1 Then   /* Only one row was updated. */                   
          ZTDSELS = 0                                                           
        Else                  /* We have another row to read. */                
          'TBDISPL 'TabName                                                     
      End                                                                       
                                                                                
      If ZVERB <> ' ' Then                                                      
        Iterate                                                                 
                                                                                
      'TBTOP 'TabName                                                           
      'TBSKIP 'TabName' ROWID(TEMPROW)'                                         
      Do While RC = 0                                                           
        If BGZUSCMD = '/' Then                                                  
        Do                                                                      
          BGZFLNAM = BGZUSSDR'/'BGZUSFIL                                        
          x = LENGTH(BGZUSLOC)                                                  
          BGZFLNAM = Substr(BGZFLNAM,x+2)                                       
                                                                                
          BGZUSCMD = GetUDCMD(strip(BGZUSTYP BGZFLNAM))                         
          If BGZUSCMD = '/' Then                                                
          Do                                                                    
            BGZUSCMD = ' '                                                      
            'TBMOD 'TabName' ORDER'                                             
          End                                                                   
        End                                                                     
                                                                                
        Select;                                                                 
          When BGZUSCMD = 'L' Then                                              
          Do                                                                    
            IF BGZUSTYP = 'File' Then                                           
               'SETMSG MSG(BGZC022)'                                            
            Else                                                                
            /* Recursively display the next directory */                        
            If BGZUSFIL <> '.' Then                                             
            Do                                                                  
              'TBGET 'TabName' ROWID(DSNROW)'                                   
              StoreDir.TabNum  = BGZUSSDR                                       
              StoreFIle.TabNum = BGZUSFIL                                       
              StoreRow.TabNum = DSNROW                                          
              BGZUSSDR = BGZUSSDR'/'BGZUSFIL                                    
              BGZPATH = '"'BGZUSSDR'"'                                          
              TabNum = TabNum + 1                                               
              'CONTROL DISPLAY SAVE'                                            
                                                                                
              Call Dispuss                                                      
                                                                                
              If ZVERB <> 'CANCEL' Then                                         
              Do                                                                
                'CONTROL DISPLAY RESTORE'                                       
                BGZUSFIL = StoreFile.TabNum                                     
                BGZUSSDR = StoreDir.TabNum                                      
                DSNROW   = StoreRow.TabNum                                      
                /* Need to reset returncode so that the current   */            
                /* table is displayed.                            */            
                returncode = -1                                                 
              End                                                               
            End                                                                 
          End                                                                   
          When Substr(BGZUSCMD,1,1) = 'B' Then                                  
          Do                                                                    
            BGZFILE = BGZUSSDR'/'BGZUSFIL                                       
            BGZEMIX = 'NO'                                                      
            'VGET (ZDBCS) SHARED'                                               
            If ZDBCS = 'YES' THEN BGZEMIX = 'YES'                               
            "CONTROL ERRORS RETURN"                                             
            "BROWSE File(BGZFILE) MIXED("BGZEMIX")"                             
            If rc > 0 Then                                                      
            Do                                                                  
              BGZLNGER = ZERRLM                                                 
              'SETMSG MSG(BGZC025)'                                             
            End                                                                 
            "CONTROL ERRORS CANCEL"                                             
            BGZUSCMD = ' '                                                      
            'TBMOD 'TabName' ORDER'                                             
          End                                                                   
          When Substr(BGZUSCMD,1,1) = 'E' |,                                    
               Substr(BGZUSCMD,1,1) = 'V' Then                                  
          Do                                                                    
            BGZCMD = ' '                                                        
            Select                                                              
              When (BGZUSTAG = '-') Then                                        
              Do                                                                
                'SETMSG MSG(BGZC023)'                                           
                BGZUSCMD = ' '                                                  
                'TBMOD 'TabName' ORDER'                                         
              End                                                               
              When (BGZUSTYP <> 'File') Then                                    
              Do                                                                
                'SETMSG MSG(BGZC023)'                                           
                BGZUSCMD = ' '                                                  
                'TBMOD 'TabName' ORDER'                                         
              End                                                               
              Otherwise                                                         
              Do                                                                
                BGZFILE = BGZUSSDR'/'BGZUSFIL                                   
                If BGZUSCMD = 'E' | BGZUSCMD = 'V'  Then                        
                  RC = BGZEDIT(BGZUSCMD "<>Exist" BGZFILE)                      
                If BGZUSCMD = 'EA' | BGZUSCMD = 'VA' Then                       
                Do                                                              
                  If BGZUSCOD /= 'Cp1252' &,                                    
                     BGZUSCOD /= 'ISO-8859-1' &,                                
                     BGZUSCOD /= 'UTF-8' Then                                   
                    'SETMSG MSG(BGZC033)'                                       
                    RC = BGZEDIT(BGZUSCMD "ASCII" BGZFILE)                      
                /*  RC = BGZEDIT(BGZUSCMD "<>Exist ASCII" BGZFILE) */           
                End                                                             
                If BGZUSCMD = 'EU' | BGZUSCMD = 'VU' Then                       
                  RC = BGZEDIT(BGZUSCMD "UTF-8" BGZFILE)                        
           /*     RC = BGZEDIT(BGZUSCMD "<>Exist UTF-8" BGZFILE)  */            
                If RC = 0 Then                                                  
                Do                                                              
                  BGZUSCMD = ' '                                                
                  'TBMOD 'TabName' ORDER'                                       
                  Call GetUSS(BGZUSFIL',')                                      
                End                                                             
                Else                                                            
                  If RC > 4 Then                                                
                  Do                                                            
                    'VGET (BGZEDITM) SHARED'                                    
                    BGZLNGER = BGZEDITM                                         
                    'SETMSG MSG(BGZC025)'                                       
                  End                                                           
              End                                                               
            End                                                                 
          End                                                                   
                                                                                
          When BGZUSCMD = 'D' Then                                              
          Do                                                                    
            MBRROW = TEMPROW                                                    
            FullName = BGZUSSDR'/'BGZUSFIL                                      
            BGZFLNME = FullName                                                 
            'ADDPOP'                                                            
            'DISPLAY PANEL(BGZUSDEL)'                                           
            TB_RC = RC                                                          
            'VGET (ZVERB)'                                                      
            If TB_RC <> 8 & ZVERB <> 'CANCEL' Then                              
            Do                                                                  
              XFile='"'FullName'"'                                              
              Call bpxwunix 'rm -r 'XFile,,list.,stderr.                        
              'REMPOP'                                                          
              Do T=1 to stderr.0                                                
                Say 'stderr:' stderr.T                                          
              End                                                               
              If stderr.0 = 0 then                                              
              Do                                                                
                'SETMSG MSG(BGZC035)'                                           
                                                                                
                'TBDELETE 'TabName                                              
                                                                                
                BGZUSCMD = ' '                                                  
                Call GetUSS(BGZUSFIL',')                                        
              End                                                               
            End                                                                 
            Else                                                                
              'REMPOP'                                                          
          End                                                                   
                                                                                
          When BGZUSCMD = 'R' Then                                              
          Do                                                                    
            MBRROW = TEMPROW                                                    
            FullName = BGZUSSDR'/'BGZUSFIL                                      
            BGZOLDNM = BGZUSFIL                                                 
            'ADDPOP'                                                            
            'DISPLAY PANEL(BGZUSREN)'                                           
            TB_RC = RC                                                          
            'VGET (ZVERB)'                                                      
            If TB_RC <> 8 & ZVERB <> 'CANCEL' Then                              
            Do                                                                  
              OFile='"'FullName'"'                                              
              NewName = BGZUSSDR'/'BGZNEWNM                                     
              NFile='"'NewName'"'                                               
              Call bpxwunix 'mv' OFile NFile,,list.,stderr.                     
              'REMPOP'                                                          
              Do T=1 to stderr.0                                                
                Say 'stderr:' stderr.T                                          
              End                                                               
              If stderr.0 = 0 then                                              
              Do                                                                
                'SETMSG MSG(BGZC036)'                                           
                                                                                
                BGZUSFIL = BGZOLDNM                                             
                'TBDELETE 'TabName                                              
                BGZUSFIL = BGZNEWNM                                             
                'TBADD 'TabName' ORDER'                                         
                BGZUSCMD = ' '                                                  
                Call GetUSS(BGZNEWNM',')                                        
              End                                                               
            End                                                                 
            Else                                                                
              'REMPOP'                                                          
          End                                                                   
                                                                                
          When BGZUSCMD = 'ST' Then                                             
          Do                                                                    
            BGZUSCMD = ''                                                       
            IF BGZUSTYP = 'File' Then                                           
              BGZFLNAM = BGZUSSDR                                               
            Else                                                                
              BGZFLNAM = BGZUSSDR'/'BGZUSFIL                                    
            /* Git Status command  */                                           
            Git_rc = 0                                                          
            'VGET BGZENVIR SHARED'                                              
            shellcmd  = ''                                                      
              shellcmd = shellcmd || BGZENVIR                                   
                                                                                
            shellcmd=shellcmd || 'cd' BGZUSLOC';' ||,                           
                   'git status'                                                 
            Git_rc = BGZCMD('status' shellcmd)                                  
          End                                                                   
                                                                                
          When BGZUSCMD = 'AD' Then                                             
          Do                                                                    
            BGZUSCMD = ''                                                       
                                                                                
            /* Call BGZADD rexx for Git add Command */                          
            Call BGZADD (BGZUSREP BGZUSLOC)                                     
          End                                                                   
                                                                                
          When BGZUSCMD = 'CO' | BGZUSCMD = 'CP' Then                           
          Do                                                                    
                                                                                
            /* Call BGZCOMIT rexx for Git commit Command */                     
            Git_rc = BGZCOMIT(BGZUSREP BGZUSLOC)                                
                                                                                
            If BGZUSCMD = 'CP' & Git_rc = 0 Then                                
            Do                                                                  
              /* Git Push command  */                                           
              Git_rc = 0                                                        
              'VGET BGZENVIR SHARED'                                            
              shellcmd  = ''                                                    
                shellcmd = shellcmd || BGZENVIR                                 
                                                                                
              shellcmd=shellcmd || 'cd' BGZUSLOC';' ||,                         
                     'git push'                                                 
              Git_rc = BGZCMD('push' shellcmd)                                  
            End                                                                 
            BGZUSCMD = ''                                                       
          End                                                                   
                                                                                
          When BGZUSCMD = 'PS' Then                                             
          Do                                                                    
            BGZUSCMD = ''                                                       
            IF BGZUSTYP = 'File' Then                                           
              BGZFLNAM = BGZUSSDR                                               
            Else                                                                
              BGZFLNAM = BGZUSSDR'/'BGZUSFIL                                    
            /* Git Status command  */                                           
            Git_rc = 0                                                          
            'VGET BGZENVIR SHARED'                                              
            shellcmd  = ''                                                      
              shellcmd = shellcmd || BGZENVIR                                   
                                                                                
            shellcmd=shellcmd || 'cd' BGZUSLOC';' ||,                           
                   'git push'                                                   
            Git_rc = BGZCMD('push' shellcmd)                                    
          End                                                                   
                                                                                
          When BGZUSCMD = 'PL' Then                                             
          Do                                                                    
            BGZUSCMD = ''                                                       
            IF BGZUSTYP = 'File' Then                                           
              BGZFLNAM = BGZUSSDR                                               
            Else                                                                
              BGZFLNAM = BGZUSSDR'/'BGZUSFIL                                    
            /* Git Status command  */                                           
            Git_rc = 0                                                          
            'VGET BGZENVIR SHARED'                                              
            shellcmd  = ''                                                      
              shellcmd = shellcmd || BGZENVIR                                   
                                                                                
            shellcmd=shellcmd || 'cd' BGZUSLOC';' ||,                           
                   'git pull'                                                   
            Git_rc = BGZCMD('pull' shellcmd)                                    
          End                                                                   
                                                                                
          When BGZUSCMD = 'CM' Then                                             
          Do                                                                    
            /* Call BGZCOMMP rexx for Git Command */                            
            Call BGZCOMMP (BGZUSREP BGZUSSDR)                                   
          End                                                                   
                                                                                
          When BGZUSCMD = 'UB' Then                                             
          Do                                                                    
            /* Call BGZDBBUB rexx for DBB user build */                         
            Call BGZDBBUB (BGZUSREP BGZUSLOC BGZUSSDR BGZUSFIL)                 
          End                                                                   
                                                                                
          When BGZUSCMD = 'UL' Then                                             
          Do                                                                    
            /* Get BGZDBBUB */                                                  
            'TBOPEN BGZDBBUB'                                                   
            TB_RC = RC                                                          
            If TB_RC = 0 Then                                                   
            Do                                                                  
              GetDBB_RC = TB_RC                                                 
              'TBGET BGZDBBUB'                                                  
              GetDBB_RC = RC                                                    
            End                                                                 
            If GetDBB_RC = 0 Then                                               
            Do                                                                  
              x = Lastpos('.',BGZUSFIL)                                         
              filename = Substr(BGZUSFIL,1,x-1)                                 
              UPPERCASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                          
              lowercase = 'abcdefghijklmnopqrstuvwxyz'                          
              input= filename                                                   
              input_upper = translate(input, uppercase, lowercase)              
              filename = input_upper                                            
              BGZUSLOG = filename'.log'                                         
              BGZFLOG = BGZBLWRK'/'BGZUSLOG                                     
              BGZEMIX = 'NO'                                                    
              'VGET (ZDBCS) SHARED'                                             
              If ZDBCS = 'YES' THEN BGZEMIX = 'YES'                             
              "CONTROL ERRORS RETURN"                                           
              "BROWSE File(BGZFLOG) MIXED("BGZEMIX")"                           
              If rc > 0 Then                                                    
              Do                                                                
                If RC = 20 Then                                                 
                  'SETMSG MSG(BGZC039)'                                         
                Else                                                            
                Do                                                              
                  BGZLNGER = ZERRLM                                             
                  'SETMSG MSG(BGZC025)'                                         
                End                                                             
              End                                                               
              "CONTROL ERRORS CANCEL"                                           
              If RC = 20 Then                                                   
                'SETMSG MSG(BGZC039)'                                           
            End                                                                 
            Else                                                                
            Do                                                                  
              /* Error warning no DBB build log exists for this file  */        
              'SETMSG MSG(BGZC039)'                                             
            End                                                                 
            If TB_RC = 0 Then                                                   
              'TBCLOSE BGZDBBUB'                                                
                                                                                
            BGZUSCMD = ' '                                                      
            'TBMOD 'TabName' ORDER'                                             
          End                                                                   
                                                                                
          Otherwise                                                             
            NOP                                                                 
        End                                                                     
                                                                                
        If ZVERB <> 'CANCEL' Then                                               
        Do                                                                      
          'TBGET 'TabName                                                       
          If RC = 0 Then                                                        
          Do                                                                    
            BGZUSCMD = ''                                                       
            'TBMOD 'TabName' ORDER'                                             
          End                                                                   
          'TBSKIP 'TabName' ROWID(TEMPROW)'                                     
        End                                                                     
        Else                                                                    
          RC = 8                                                                
      End                                                                       
                                                                                
      /* Reset BGZPATH for next bpxwunix cd command  */                         
      BGZPATH = '"'BGZUSSDR'"'                                                  
      /* Refresh list to display correct Git Status  */                         
      If TB_RC <> 8 & ZVERB <> 'CANCEL' Then                                    
      Do                                                                        
        Refresh = true                                                          
        Rebuild = 1                                                             
        BGZCMD = ''                                                             
        Iterate                                                                 
      End                                                                       
                                                                                
    End                                                                         
  End                                                                           
  If TabNum <> 0 Then                                                           
  Do                                                                            
    'TBEND 'TabName                                                             
    /* Decrement the table count */                                             
    TabNum = TabNum - 1                                                         
    TabName = 'T'RIGHT(Tabnum,7,0)                                              
  End                                                                           
    /* End all recursive open tables when RETURN asked (=n command) */          
  If ZVERB = 'RETURN' | ZVERB = 'CANCEL' Then                                   
  Do                                                                            
    Do While TabNum <> 0                                                        
      'TBEND 'TabName                                                           
      TabNum = TabNum - 1                                                       
      TabName = 'T'RIGHT(Tabnum,7,0)                                            
    End                                                                         
    If ZVERB = 'RETURN' Then                                                    
      Exit                                                                      
  End                                                                           
                                                                                
Return                                                                          
/* End DISPuss */                                                               
                                                                                
Getuss:                                                                         
                                                                                
  Parse Arg filestat                                                            
                                                                                
  Parms = 'dataset' BGZENCOD(BGZUSSDR) 'refresh' Refresh                        
                                                                                
  DirList = 'No'                                                                
  If filestat = '' Then                                                         
  Do                                                                            
    DirList = 'Yes'                                                             
    TabName = 'T'RIGHT(Tabnum,7,0)                                              
    'CONTROL ERRORS RETURN'                                                     
    'TBEND    'TabName                                                          
    'CONTROL ERRORS CANCEL'                                                     
    'TBCREATE 'TabName' KEYS(BGZUSFIL)                                          
          NAMES(BGZUSCMD,BGZUSTYP,BGZUSPER,BGZUSTAG,BGZUSLCK,BGZUSCOD)          
                       REPLACE NOWRITE'                                         
    'TBSORT   'TabName' FIELDS(BGZUSFIL)'                                       
    Call bpxwunix 'cd' BGZPATH';ls -EgoA',,list.,stderr.                        
  /* In comment, because symlink that goes to a deleted directory               
     returns an error                                                           
    Do T=1 to stderr.0                                                          
      Say 'stderr:' stderr.T                                                    
    End                                                                         
    If stderr.0 > 0 then exit(16)                                               
  */                                                                            
    /* Data is returned like this: */                                           
    /*                                                                          
    ----+----1----+----2----+----3----+----4----+----5----+----6----+----7      
    drwxr-xr-x        5      8192 Jun  4 05:39 buildsystem                      
    -rw-r-----  --s-  1   6415194 Jun  4 05:42 jazz.war                         
    drwxr-xr-x        4      8192 Jun  4 05:40 license-update-site              
    drwxr-xr-x        4      8192 Jun  4 05:48 license_files                    
    drwxr-xr-x        2      8192 Jun  5 07:20 provision_profiles               
    drwxr-xr-x        3      8192 Jun  4 05:39 repotools                        
    -rw-r--r--  --s-  1      5540 Jun  5 07:33 teamserver.properties            
    drwxr-xr-x        4      8192 Jun  4 05:40 update-site                      
    */                                                                          
                                                                                
    Do L = 1 to list.0                                                          
      Select                                                                    
        When left(list.L,1) = '-' | ,                                           
             left(list.L,1) = 'd' | left(list.L,1) = 'l' then                   
        Do                                                                      
          Parse var list.L Bits 17 . 30 Blank 31 . 44 Part                      
          If Blank <> ' '                           /* extra big file ? */      
            then Part = substr(Part,pos(' ',Part)+1)                            
          filename = Strip(Part)                                                
                                                                                
          If filename = '.jazz5' | filename = '.jazzShed' Then                  
            iterate                                                             
          If substr(filename,1,1) = '.' & BGZSPFIL = 'Y' Then                   
            iterate                                                             
                                                                                
          BGZUSFIL = filename                                                   
          If left(list.L,1) = '-' Then                                          
            BGZUSTYP = 'File'                                                   
          If left(list.L,1) = 'd' Then                                          
            BGZUSTYP = 'Dir'                                                    
          If left(list.L,1) = 'l' Then                                          
          Do                                                                    
            filename = substr(filename, 1, pos(' ', filename))                  
                                                                                
            filename = Strip(filename)                                          
            BGZUSFIL = filename                                                 
            /* Check into list */                                               
            cmd = 'cd' BGZUSSDR';file ' filename                                
                                                                                
            Call bpxwunix cmd,,type.,stderr.                                    
            BGZUSTYP = 'lnk'                                                    
            If (right(type.1, 9) = 'directory') Then                            
              BGZUSTYP = 'Dir'                                                  
            If (right(type.1, 5) = 'empty') Then                                
              BGZUSTYP = 'File'                                                 
            If (right(type.1, 4) = 'text') Then                                 
              BGZUSTYP = 'File'                                                 
            If (right(type.1, 4) = 'data') Then                                 
              BGZUSTYP = 'File'                                                 
          End                                                                   
                                                                                
          BGZUSCMD = ''                                                         
          BGZUSTAG = ''                                                         
                                                                                
          Address ISPEXEC 'TBADD 'TabName' ORDER'                               
        End /* when file or dir */                                              
                                                                                
        When pos(left(list.L,1),'bcelps') > 0 then                              
        Do                                                                      
          say '>> WARNING' list.L 'not processed'                               
        End /* when file */                                                     
                                                                                
        Otherwise nop /* sub-directory or fluff */                              
      End    /* select */                                                       
    End    /* loop L */                                                         
                                                                                
  End                                                                           
  Else                                                                          
  Do                                                                            
    FileParm = ''                                                               
    Do While pos(',', filestat) <> 0                                            
      Parse Var filestat ThisFile ',' filestat                                  
      ThisFile = Strip(ThisFile,'L')                                            
      FileParm = FileParm 'members' BGZENCOD(ThisFile)                          
    End                                                                         
    Parms = Parms FileParm                                                      
  End                                                                           
                                                                                
  /* Git Status on cloned repository to capture the file status to display  */  
  /* Unstaged files to display with "M" Modified Git Status                 */  
  /* Staged files to display with "S" to commit Git Status                  */  
                                                                                
  Git_rc = 0                                                                    
  'VGET BGZENVIR SHARED'                                                        
  /* Need to craeet a temporary ISPF table to hold info */                      
                                                                                
  'TBCREATE BGZTEMP KEYS(BGZROW),                                               
                    NAMES(BGSTDCMD,BGZLINE,BGZFILE) NOWRITE'                    
  shellcmd  = ''                                                                
    shellcmd = shellcmd || BGZENVIR                                             
                                                                                
  shellcmd=shellcmd || 'cd' BGZUSLOC';' ||,                                     
         'git status --porcelain'                                               
  Git_rc = BGZCMD('stage' shellcmd)                                             
                                                                                
  /* Now read the Status list */                                                
  /* we need to capture Unstaged and Staged files    */                         
  'TBTOP BGZTEMP'                                                               
  'TBSKIP BGZTEMP'                                                              
  TEMP_RC = RC                                                                  
  Do While TEMP_RC = 0                                                          
    staged = Substr(BGZLINE,1,1)                                                
    unstaged = Substr(BGZLINE,2,1)                                              
    fullname = Substr(BGZLINE,4)                                                
    /* build fullname without double quotes when necessary  */                  
    namelng = length(fullname)                                                  
    If verify(fullname,'"') = 1 Then                                            
      withquote = 0                                                             
    Else                                                                        
      withquote = 1                                                             
    If withquote = 1 Then                                                       
      fullname = Substr(fullname,2,namelng-2)                                   
                                                                                
    lastslsh = Lastpos('/',fullname)                                            
    BGZUSFIL = Substr(fullname,lastslsh+1)                                      
    If staged <> ' ' & staged <> '?' Then                                       
    Do                                                                          
      'TBGET ' TabName                                                          
      If RC = 0 Then                                                            
      Do                                                                        
        BGZUSTAG = 'Staged'                                                     
        'TBMOD ' Tabname                                                        
      End                                                                       
    End                                                                         
                                                                                
    If unstaged <> ' ' Then                                                     
    Do                                                                          
      'TBGET ' TabName                                                          
      If RC = 0 Then                                                            
      Do                                                                        
        If unstaged = 'M' Then                                                  
          BGZUSTAG = 'Modified'                                                 
        If unstaged = '?' Then                                                  
          BGZUSTAG = 'Ren/Add'                                                  
        'TBMOD ' Tabname                                                        
      End                                                                       
    End                                                                         
                                                                                
    'TBSKIP BGZTEMP'                                                            
    TEMP_RC = RC                                                                
  End                                                                           
    'TBCLOSE BGZTEMP'                                                           
                                                                                
  /* For a full directory refresh suppress broken link in the table */          
  If DirList = 'Yes' Then                                                       
  Do                                                                            
                                                                                
    FILEROW  = '00000001'                                                       
    'TBTOP ' TabName                                                            
    Do Until(ReturnCode <> -1)                                                  
      found = 0                                                                 
      'TBSKIP ' TabName' ROWID(TEMPROW)'                                        
      If RC <> 0 Then  leave                                                    
      If (BGZUSTYP = 'lnk') Then                                                
        Address ISPEXEC 'TBDELETE 'TabName                                      
    End  /* Loop  TabName */                                                    
  End                                                                           
                                                                                
Return                                                                          
/* End Getuss */                                                                
                                                                                
                                                                                
/* Get the file info for a single file and mod the table entry */               
                                                                                
GetLSinf:                                                                       
                                                                                
  parse arg path                                                                
                                                                                
  Call bpxwunix 'ls -EgoA 'path,,list.,stderr.                                  
                                                                                
  Do L = 1 to list.0                                                            
    Select                                                                      
      When left(list.L,1) = '-' | ,                                             
           left(list.L,1) = 'd' | left(list.L,1) = 'l' then                     
      Do                                                                        
        Parse var list.L Bits 17 . 30 Blank 31 . 44 Part                        
        If Blank <> ' '                           /* extra big file ? */        
          then Part = substr(Part,pos(' ',Part)+1)                              
        filename = Strip(Part)                                                  
                                                                                
        If filename = '.jazz5' | filename = '.jazzShed' Then                    
          iterate                                                               
        lastslsh = Lastpos('/',filename)                                        
        filename = Substr(filename,lastslsh+1)                                  
        BGZUSFIL = filename                                                     
        If left(list.L,1) = '-' Then                                            
          BGZUSTYP = 'File'                                                     
        If left(list.L,1) = 'd' Then                                            
          BGZUSTYP = 'Dir'                                                      
        If left(list.L,1) = 'l' Then                                            
        Do                                                                      
          filename = substr(filename, 1, pos(' ', filename))                    
                                                                                
          filename = Strip(filename)                                            
          /* Check into list */                                                 
          cmd = 'cd' BGZUSSDR';file ' filename                                  
                                                                                
          Call bpxwunix cmd,,type.,stderr.                                      
          BGZUSTYP = 'lnk'                                                      
          If (right(type.1, 9) = 'directory') Then                              
            BGZUSTYP = 'Dir'                                                    
          If (right(type.1, 5) = 'empty') Then                                  
            BGZUSTYP = 'File'                                                   
          If (right(type.1, 4) = 'text') Then                                   
            BGZUSTYP = 'File'                                                   
          If (right(type.1, 4) = 'data') Then                                   
            BGZUSTYP = 'File'                                                   
        End                                                                     
   /* Liam - not displaying the permissions */                                  
   /*   PermOct  = XLATBITS(Strip(bits))    */                                  
   /*   BGZUSPER = PermOct                  */                                  
        BGZUSCMD = ''                                                           
                                                                                
        Address ISPEXEC 'TBMOD 'TabName' ORDER'                                 
      End /* when file or dir */                                                
                                                                                
      When pos(left(list.L,1),'bcelps') > 0 then                                
      Do                                                                        
        say '>> WARNING' list.L 'not processed'                                 
      End /* when file */                                                       
                                                                                
      Otherwise nop /* sub-directory or fluff */                                
    End    /* select */                                                         
  End    /* loop L */                                                           
                                                                                
Return                                                                          
                                                                                
XLATBITS:                                                                       
                                                                                
  parse arg Bits                                                                
  /* interpret bits from long format to Octal */                                
  Bits = Substr(Bits,2)                                                         
  PermOct = ''                                                                  
  o    = 0                                                                      
  Do b = 1 to 3                                                                 
     oct  = 0                                                                   
     Bit.b = Substr(Bits,1+o,3)                                                 
     Do x = 1 to 3                                                              
        y = Translate(substr(Bit.b,x,1),'42101010','rwxSsTt-')                  
        oct = oct + y                                                           
     End                                                                        
     PermOct = PermOct || oct                                                   
     o = o + 3                                                                  
  End                                                                           
                                                                                
return PermOct                                                                  
                                                                                
GetUDCMD: PROCEDURE                                                             
                                                                                
  Parse Arg BGZUSTYP BGZFLNAM                                                   
                                                                                
                                                                                
  If BGZUSTYP <> 'Dir' Then                                                     
  Do                                                                            
    'GETMSG MSG(BGZC027) LONGMSG(BGZDITYP)'                                     
    BGZDEFL = 'U'                                                               
  End                                                                           
  Else                                                                          
  Do                                                                            
    'GETMSG MSG(BGZC028) LONGMSG(BGZDITYP)'                                     
    BGZDEFE = 'U'                                                               
  End                                                                           
                                                                                
  'ADDPOP'                                                                      
  'DISPLAY PANEL(BGZSLUDL)'                                                     
  'VGET (ZVERB)'                                                                
  if ZVERB = 'CANCEL' | ZVERB = 'EXIT' Then                                     
    BGZSEL = ''                                                                 
                                                                                
  Select;                                                                       
    When BGZSEL = '1' Then                                                      
      Cmd = 'E'                                                                 
    When BGZSEL = '2' Then                                                      
      Cmd = 'EA'                                                                
    When BGZSEL = '3' Then                                                      
      Cmd = 'EU'                                                                
    When BGZSEL = '4' Then                                                      
      Cmd = 'V'                                                                 
    When BGZSEL = '5' Then                                                      
      Cmd = 'VA'                                                                
    When BGZSEL = '6' Then                                                      
      Cmd = 'VU'                                                                
    When BGZSEL = '7' Then                                                      
      Cmd = 'B'                                                                 
    When BGZSEL = '8' Then                                                      
      Cmd = 'D'                                                                 
    When BGZSEL = '9' Then                                                      
      Cmd = 'R'                                                                 
    When BGZSEL = '10' Then                                                     
      Cmd = 'L'                                                                 
    When BGZSEL = '11' Then                                                     
      Cmd = 'ST'                                                                
    When BGZSEL = '12' Then                                                     
      Cmd = 'AD'                                                                
    When BGZSEL = '13' Then                                                     
      Cmd = 'CO'                                                                
    When BGZSEL = '14' Then                                                     
      Cmd = 'PS'                                                                
    When BGZSEL = '15' Then                                                     
      Cmd = 'CP'                                                                
    When BGZSEL = '16' Then                                                     
      Cmd = 'PL'                                                                
    When BGZSEL = '17' Then                                                     
      Cmd = 'CM'                                                                
    When BGZSEL = '18' Then                                                     
      Cmd = 'UB'                                                                
    When BGZSEL = '19' Then                                                     
      Cmd = 'UL'                                                                
    Otherwise                                                                   
      Cmd = ''                                                                  
  End                                                                           
  'REMPOP'                                                                      
                                                                                
Return Cmd                                                                      