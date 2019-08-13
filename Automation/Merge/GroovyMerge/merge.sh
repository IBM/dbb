#!/bin/sh
#*******************************************************************************
# Licensed Materials - Property of IBM
# (c) Copyright IBM Corporation 2019. All Rights Reserved.
#
# Note to U.S. Government Users Restricted Rights:
# Use, duplication or disclosure restricted by GSA ADP Schedule
# Contract with IBM Corp.
#*******************************************************************************

scriptDir=$(dirname $0)
. ${scriptDir}/setenv.sh
if [ -z "$GROOVY_HOME" ]; then
   if [ -z "$DBB_HOME" ]; then
      echo "DBB_HOME or GROOVY_HOME must be set to run this utility"
      exit 1
   else
      GROOVY_HOME="$DBB_HOME/groovy-2.4.12"
   fi
fi
CMD="${GROOVY_HOME}/bin/groovy -Dlog4j.configuration=${scriptDir}/log4j.properties merge.groovy $@"

$CMD