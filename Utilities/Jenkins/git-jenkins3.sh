#!/bin/sh
#
# Licensed materials - Property of IBM
# 5655-AC5 Copyright IBM Corp. 2018, 2018
# All rights reserved
# US Government users restricted rights  -  Use, duplication or
# disclosure restricted by GSA ADP schedule contract with IBM Corp.
#
# Support credential management in Jenkins for git client plugin < 3.0
#
###################################################################

# Set current dir to direct temp outputs to @tmp dir

currentdir=$(pwd)

# git operating via HTTP/S
# Setting encoding of temporary files for git askpass
# Redirect of output is mandatory
if test -n "$SSH_ASKPASS"; then
  if test -f "$SSH_ASKPASS"; then
     # check the current tag, if ibm-1047 don't iconv
     chtag -p $SSH_ASKPASS | grep -q "IBM-1047" >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
     if [ $? -eq 0 ]; then
       echo 'file already in ibm-1047' >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
     else
     chtag -p $SSH_ASKPASS >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
     chtag -t -c UTF-8 $SSH_ASKPASS >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
     iconv -f utf-8 -T -t IBM-1047 $SSH_ASKPASS > $SSH_ASKPASS.tmp
     mv $SSH_ASKPASS.tmp $SSH_ASKPASS >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
     chmod 700 $SSH_ASKPASS >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
     fi
   fi
fi

# git operating via SSH
if test -n "$GIT_SSH"; then
  if test -f "$GIT_SSH"; then
     chtag -p $GIT_SSH | grep -q "IBM-1047" >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
     if [ $? -eq 0 ]; then
       echo 'file already in ibm-1047' >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
     else
      chtag -p $GIT_SSH >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      chtag -t -c UTF-8 $GIT_SSH >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      iconv -f utf-8 -T -t IBM-1047 $GIT_SSH > $GIT_SSH.tmp
      mv $GIT_SSH.tmp $GIT_SSH >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      chmod 700 $GIT_SSH >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1

      keyFile=$(cat $GIT_SSH | grep "ssh -i" | sed 's/^[^"]*"\([^"]*\)".*/\1/')
      if test -n "$keyFile"; then
       chtag -p $keyFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
       chtag -t -c UTF-8 $keyFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
       iconv -f utf-8 -T -t IBM-1047 $keyFile > $keyFile.tmp
       mv $keyFile.tmp $keyFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
       chmod 600 $keyFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
     fi
    fi
  fi
fi

#git "$@" | iconv -f ibm-1047 -t ibm-1047
git "$@"