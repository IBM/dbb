#!/bin/sh

##############################################################################################
#
# This script calls the JCLtoDBB.groovy script to parse the JCL and generate DBB groovy
# scripts representing the JCL.
# 
##############################################################################################
scriptDir=$(dirname $0)
if [[ -z "${DBB_HOME}" ]]; then
  echo "Need to specified the required environment variable 'DBB_HOME'"
  exit 8
fi
NLSPATH=$DBB_HOME/lib/dmhmsg.cat:$NLSPATH

CMD="$DBB_HOME/bin/groovyz $scriptDir/../groovy/JCLtoDBB.groovy $@"

$CMD
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
