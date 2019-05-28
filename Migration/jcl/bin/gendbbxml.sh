#!/bin/sh

##############################################################################################
#
# This script calls the JCLtoDBBXml.groovy script to parse the JCL and generate DBB XML
# to represent the steps, programs, and datasets used in the JCL.
# 
##############################################################################################
scriptDir=$(dirname $0)
if [[ -z "${DBB_HOME}" ]]; then
  echo "Need to specified the required environment variable 'DBB_HOME'"
  exit 8
fi
NLSPATH=$DBB_HOME/bin/dmhmsg.cat:$NLSPATH

CMD="$DBB_HOME/bin/groovyz $scriptDir/../groovy/JCLtoDBBXml.groovy $@"

$CMD
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
