#! /bin/sh
#
##############################################################################################
##
# *******************************************************************************
# * Licensed Materials - Property of IBM
# * (c) Copyright IBM Corporation 2022. All Rights Reserved.
# * 
# * Note to U.S. Government Users Restricted Rights:  
# * Use, duplication or disclosure restricted by GSA ADP Schedule 
# * Contract with IBM Corp. 
# *******************************************************************************
##
##  DBB Static Report Migration Groovy script execution.
##
#######################################################################

if [ -z "${DBB_HOME:+set}" ]; then
   echo "DBB_HOME must be set"
   exit 1
fi

SCRIPT_DIR=$(dirname "$0")

$DBB_HOME/bin/groovyz $SCRIPT_DIR/../groovy/migrate-list.groovy "$@"
exit $?
