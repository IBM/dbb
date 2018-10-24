#! /bin/sh
#
###############################################################################################
##  Sample shell script used to set Rocket's Git environment variables from Jenkins
##
##  The Rocket Git documentation located in /<installDir>/share/doc/git/<version>/README.ZOS
##  instructs users to create and update new and existing and environment variables for 
##  the Git client to run.  Normally this would be achieved by adding them to the user's 
##  .profile login script.  However the Jenkins slave runs in a non-login shell so the .profile 
##  script is not executed automatically.  This shell script can be executed by both the users 
##  .profile script and the Jenkins slave at startup
##  
##  Setup Instructions: 
##  1) Replace all references to "rsusr" with the appropriate install directory name 
##  2) In the Jenkins Agent definition configuration add the path to this file in the 
##     Prefix Start Slave Command field
##
###############################################################################################
# git the environment variables
 export GIT_SHELL=/rsusr/rocket/bin/bash
 export GIT_EXEC_PATH=/rsusr/rocket/libexec/git-core
 export GIT_TEMPLATE_DIR=/rsusr/rocket/share/git-core/templates

#common the environment variables
 export PATH=$PATH:/rsusr/rocket/bin
 export MANPATH=$MANPATH:/rsusr/rocket/man
 export PERL5LIB=$PERL5LIB:/rsusr/rocket/lib/perl5

#ASCII support the environment variables
 export _BPXK_AUTOCVT=ON
 export _CEE_RUNOPTS="FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"
 export _TAG_REDIR_ERR=txt
 export _TAG_REDIR_IN=txt
 export _TAG_REDIR_OUT=txt

#set git editor to create comments on encdoing ISO8859-1
#git config --global core.editor "/bin/vi -W filecodeset=ISO8859-1"
