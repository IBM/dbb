#! /bin/sh

if test -n "$GIT_SSH" ; then
  dbbtmp=~/${JOB_NAME}@dbb
  if ! test -d "$dbbtmp" ; then
    mkdir $dbbtmp
    createdDbbTmp=true
  fi
  cat $GIT_SSH | iconv -f utf-8 -T -t IBM-1047 > $dbbtmp/git.ssh.sh
  export GIT_KEY=$(cat $dbbtmp/git.ssh.sh | grep ssh | sed "s/key.*/key/" | sed "s/.* .//")
  cat $GIT_KEY | iconv -f utf-8 -T -t IBM-1047 > $dbbtmp/git.ssh.key
  rm -f $GIT_SSH
  cp $dbbtmp/git.ssh.sh $GIT_SSH
  chmod 700 $GIT_SSH
  rm -f $GIT_KEY
  cp $dbbtmp/git.ssh.key $GIT_KEY
  chmod 600 $GIT_KEY
  if test "$createdDbbTmp" = true ; then
    rm -rf $dbbtmp
  fi
fi
git "$@" | iconv -f ibm-1047 -t ibm-1047
