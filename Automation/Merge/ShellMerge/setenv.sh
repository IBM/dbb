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
export ZOAUTIL_DIR=/usr/lpp/IBM/zoa
export PATH=${ZOAUTIL_DIR}/bin:$PATH
