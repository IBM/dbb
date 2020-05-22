#!/bin/sh
#
# Licensed materials - Property of IBM
# 5655-AC5 Copyright IBM Corp. 2018, 2018
# All rights reserved
# US Government users restricted rights  -  Use, duplication or
# disclosure restricted by GSA ADP schedule contract with IBM Corp.
#
# Support modified behaviour in Jenkins git client plugin > 3.0
#
###################################################################

# Set current dir to direct temp outputs to @tmp dir

currentdir=$(pwd)

# git operating via SSH
if test -n "$GIT_SSH"; then
  if test -n "$SSH_ASKPASS"; then
    passphraseFile=$(cat $SSH_ASKPASS | grep "phrase" | sed "s/.*@tmp.//" | sed "s/'//")
    if test -n "$passphraseFile"; then
      chtag -t -c UTF-8 $currentdir@tmp/$passphraseFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      chtag -p $currentdir@tmp/$passphraseFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      iconv -f utf-8 -T -t IBM-1047 $currentdir@tmp/$passphraseFile >$currentdir@tmp/$passphraseFile.tmp
      mv $currentdir@tmp/${passphraseFile}.tmp $currentdir@tmp/$passphraseFile
    fi
  fi
fi

# git operating via HTTP/S
# Setting encoding of temporary files for git askpass
# Redirect of output is mandatory
if test -n "$GIT_ASKPASS"; then
  if test -f "$GIT_ASKPASS"; then
  usernameFile=$(cat $GIT_ASKPASS | grep "Username" | sed "s/.*@tmp.//" | sed "s/'.*//") >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
  passwordFile=$(cat $GIT_ASKPASS | grep "Password" | sed "s/.*@tmp.//" | sed "s/'.*//") >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
  if test -n "$usernameFile"; then
    # check the current tag, if ibm-1047 don't iconv
    chtag -p $currentdir@tmp/$usernameFile | grep -q "IBM-1047" >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
    if [ $? -eq 0 ]; then
      echo 'file already in ibm-1047' >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
    else
      chtag -p $currentdir@tmp/$usernameFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      chtag -p $currentdir@tmp/$passwordFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      chtag -t -c UTF-8 $currentdir@tmp/$usernameFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      chtag -t -c UTF-8 $currentdir@tmp/$passwordFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      chtag -p $currentdir@tmp/$usernameFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      chtag -p $currentdir@tmp/$passwordFile >>$currentdir@tmp/git-jenkins-wrapper.log 2>&1
      iconv -f utf-8 -T -t IBM-1047 $currentdir@tmp/$usernameFile >$currentdir@tmp/$usernameFile.tmp
      iconv -f utf-8 -T -t IBM-1047 $currentdir@tmp/$passwordFile >$currentdir@tmp/$passwordFile.tmp
      mv $currentdir@tmp/${usernameFile}.tmp $currentdir@tmp/$usernameFile
      mv $currentdir@tmp/${passwordFile}.tmp $currentdir@tmp/$passwordFile
    fi
    fi
  fi
fi

#git "$@" | iconv -f ibm-1047 -t ibm-1047
git "$@"
