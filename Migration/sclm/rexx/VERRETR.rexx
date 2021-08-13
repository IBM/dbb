/* REXX */                                                                      
/*********************************************************************/         
/*                                                                   */         
/* Rational Team Concert, Version 7, Release 0, Level 0              */         
/*                                                                   */         
/*                                                                   */         
/* Copyright:    Licensed Materials - Property of IBM                */         
/*                                                                   */         
/*               "Restricted Materials of IBM"                       */         
/*                                                                   */         
/*               5724-V04                                            */         
/*                                                                   */         
/*               Copyright IBM Corp. 2016, 2019                      */         
/*                                                                   */         
/*               US Government Users Restricted Rights -             */         
/*               Use, duplication or disclosure restricted by        */         
/*               GSA ADP Schedule Contract with IBM Corp.            */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* NAME := BLZVERRT                                                  */         
/*                                                                   */         
/* DESCRIPTIVE NAME := SCLM version retrieve to data set             */         
/*                                                                   */         
/* FUNCTION := Extract appropriate SCLM member version               */         
/*                                                                   */         
/*********************************************************************/         
/*                                                                   */         
/* Change History                                                    */         
/*                                                                   */         
/* Who   When     What                                               */         
/* ----- -------- -------------------------------------------------- */         
/* SJ    11/05/16 Initial version written by Steve Jordan (SMPO)     */         
/* LD    19/06/19 Renamed from SMPO VERSRTRV                         */         
/*                                                                   */         
/*********************************************************************/         
  Trace o                                       /* trace - o, r      */         
                                                                                
  ARG proj "," group "," type "," member "," datev "," timev "," versdsn        
                                                                                
  prjdf   = proj                                                                
  datev   = STRIP(datev)                                                        
  versdsn = STRIP(versdsn)                                                      
                                                                                
  verrecv_rc = 0                                                                
                                                                                
  Say 'proj='proj                                                               
  Say 'prjdf='prjdf                                                             
  Say 'group='group                                                             
  Say 'type='type                                                               
  Say 'member='member                                                           
  Say 'datev='datev                                                             
  Say 'timev='timev                                                             
  Say 'versdsn='versdsn                                                         
                                                                                
                                                                                
  "FLMCMD VERRECOV,"proj","prjdf","group","type","member",,"||,                 
          timev","versdsn||,                                                    
          ",,,,,"datev                                                          
  verrecv_rc = rc                                                               
                                                                                
  Say 'verrecv_rc='verrecv_rc                                                   
  ZISPFRC = verrecv_rc                                                          
  Address 'ISPEXEC' "VPUT (ZISPFRC) SHARED"                                     
                                                                                
Exit verrecv_rc                                                                 
