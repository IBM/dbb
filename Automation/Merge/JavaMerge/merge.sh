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

cd ${scriptDir}/src

# Compile the sample
${JAVA_HOME}/bin/javac com/ibm/merge/Merge.java
rc=$?
if [ rc -gt 0 ]; then
	echo "Compile failed for Merge.java, rc=$rc"
	echo "Check environment settings in setenv.sh"
	exit $rc
fi

# Run the sample
CMD="${JAVA_HOME}/bin/java com.ibm.merge.Merge $@"
$CMD
