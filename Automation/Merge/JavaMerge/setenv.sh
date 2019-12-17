#!/bin/sh
#*******************************************************************************
# Licensed Materials - Property of IBM
# (c) Copyright IBM Corporation 2019. All Rights Reserved.
#
# Note to U.S. Government Users Restricted Rights:
# Use, duplication or disclosure restricted by GSA ADP Schedule
# Contract with IBM Corp.
#*******************************************************************************
#
# Set up the environment
# NOTE: Before running this script, you need to have modified this file to match your z/OS system
#
if [ -z ${ZOAUTIL_DIR} ]; then
   export ZOAUTIL_DIR=/usr/lpp/IBM/zoautil
fi

export PATH=${ZOAUTIL_DIR}/bin:$PATH
export JAVA_HOME=/usr/lpp/java/J8.0_64               # Root directory for Java 64-bit
export CLASSPATH=${ZOAUTIL_DIR}/lib/*:.
export LIBPATH=${ZOAUTIL_DIR}/lib
