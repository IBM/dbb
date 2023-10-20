#!/bin/env bash
#===================================================================================
# NAME: wazideploy-deploy.sh
#
# DESCRIPTION: The purpose of this script is to use wazideploy-deploy to 
# deploy a package with a provided Deployment Plan
#
# SYNTAX: See Help() Section Below
#
# OPTIONS: See Help() Section Below
#
# RETURNS:
#
#    rc         - Return Code
#
# RETURN CODES:
#
#    0          - Successful
#    4          - Warning message(s) issued. See Console messages.
#    8          - Error encountered. See Console messages.
#
# NOTE(S):
#
#   None
#
# Maintenance Log
# Date       Who Vers Description
# ---------- --- ---- --------------------------------------------------------------
# 2023/07/18 RBS 1.00 Initial Release
#===================================================================================
Help() {
  echo $PGM" - Deploy a package with Wazi Deploy                                  "
  echo "                                                                          "
  echo "Description: The purpose of this script is to use wazideploy-deploy to    "
  echo "deploy a package with a provided Deployment Plan                          "
  echo "                                                                          "
  echo "Syntax:                                                                   "
  echo "                                                                          "
  echo "       "$PGM" [Options]                                                   "
  echo "                                                                          "
  echo "Options:                                                                  "
  echo "                                                                          "
  echo "       -h                                - Help.                          "
  echo "                                                                          "
  echo "       -w <working directory>            - Path to the working            "
  echo "                                           directory                      "
  echo "                                           Default=None, Required         "
  echo "                                                                          "
  echo "       -p <deployment plan path>         - Path to the Deployment Plan    "
  echo "                                           Default=None, Required         "
  echo "                                                                          "
  echo "       -e <environment file>             - Path to the environment file   "
  echo "                                           Default=None, Required         "
  echo "                                                                          "
  echo "       -i <package input file path>      - Path to the package            "
  echo "                                           to be deployed                 "
  echo "                                           Default=None, Optional         "
  echo "                                                                          "
  echo "       -l <evidence file path>           - Path to the evidence file      "
  echo "                                           Default=None, Optional         "
  echo "                                                                          "
  echo "       -d                                - Debug tracing flag             "
  echo " "
  exit 0
}

#
# Customization
# Configuration file leveraged by the backend scripts
# Either an absolute path or a relative path to the current working directory
SCRIPT_HOME="`dirname "$0"`"
pipelineConfiguration="${SCRIPT_HOME}/pipelineBackend.config"
# Customization - End

#
# Internal Variables
#set -x                  # Uncomment to enable shell script debug
#export BASH_XTRACEFD=1  # Write set -x trace to file descriptor

PGM=$(basename "$0")
PGMVERS="1.00"
USER=$(whoami)
SYS=$(uname -Ia)

rc=0
ERRMSG=""
WorkingDirectory=""
DeploymentPlan=""
EnvironmentFile=""
PackageInputFile=""
EvidenceFile=""
Debug=""
HELP=$1

if [ "$HELP" = "?" ]; then
  Help
fi

# Validate Shell environment
currentShell=$(ps -p $$ | grep bash)
if [ -z "${currentShell}" ]; then
  rc=8
  ERRMSG=$PGM": [ERROR] The scripts are designed to run in bash. You are running a different shell. rc=${rc}. \n. $(ps -p $$)."
  echo $ERRMSG
fi
#

if [ $rc -eq 0 ]; then
  echo $PGM": [INFO] Deploy Package with Wazi Deploy. Version="$PGMVERS
fi

# Read and import pipeline configuration
if [ $rc -eq 0 ]; then
  if [ ! -f "${pipelineConfiguration}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Pipeline Configuration File (${pipelineConfiguration}) was not found. rc="$rc
    echo $ERRMSG
  else
    source $pipelineConfiguration
  fi
fi

#
# Get Options
if [ $rc -eq 0 ]; then
  while getopts "hd:w:p:e:i:l:" opt; do
    case $opt in
    h)
      Help
      ;;

    w)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Working Directory is required. rc="$rc
        echo $ERRMSG
        break
      fi
      WorkingDirectory="$argument"
      ;;

    p)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Path to Deployment Plan is required. rc="$rc
        echo $ERRMSG
        break
      fi
      DeploymentPlan="$argument"
      ;;

    e)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Path to Environment file is required. rc="$rc
        echo $ERRMSG
        break
      fi
      EnvironmentFile="$argument"
      ;;

    i)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Path to Package Input File is required. rc="$rc
        echo $ERRMSG
        break
      fi
      PackageInputFile="$argument"
      ;;

    l)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Path to Evidence file is required. rc="$rc
        echo $ERRMSG
        break
      fi
      EvidenceFile="$argument"
      ;;

    d)
      # Add command to produce debug output
      Debug=" -d"
      ;;

    \?)
      Help
      rc=1
      break
      ;;

    :)
      rc=1
      ERRMSG=$PGM": [ERROR] Option -$OPTARG requires an argument. rc="$rc
      echo $ERRMSG
      break
      ;;

    esac
  done
fi

#
# Validate Options

if [ $rc -eq 0 ]; then
  if [ -z "${WorkingDirectory}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Working Directory is required. rc="$rc
    echo $ERRMSG
  fi
fi

if [ $rc -eq 0 ]; then
  if [ -z "${DeploymentPlan}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Deployment Plan is required. rc="$rc
    echo $ERRMSG
  fi
fi

if [ $rc -eq 0 ]; then
  if [ -z "${EnvironmentFile}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Environment File is required. rc="$rc
    echo $ERRMSG
  fi
fi

if [ $rc -eq 0 ]; then
  if [ -z "${PackageInputFile}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Package Input File is required. rc="$rc
    echo $ERRMSG
  fi
fi

#
# Set up Environment
if [ $rc -eq 0 ]; then
  . $HOME/.profile
fi

#
# Ready to go
if [ $rc -eq 0 ]; then
  echo $PGM": [INFO] **************************************************************"
  echo $PGM": [INFO] ** Start Wazi Deploy Deployment on HOST/USER: ${SYS}/${USER}"
  echo $PGM": [INFO] **               Working Directory:" ${WorkingDirectory}
  echo $PGM": [INFO] **                 Deployment Plan:" ${DeploymentPlan}
  echo $PGM": [INFO] **                Environment File:" ${EnvironmentFile}
  echo $PGM": [INFO] **              Package Input File:" ${PackageInputFile}

  if [ ! -z "${EvidenceFile}" ]; then
    echo $PGM": [INFO] **                   Evidence File:" ${EvidenceFile}
  fi

  if [ ! -z "${Debug}" ]; then
    echo $PGM": [INFO] **         Debug output is enabled."
  else
    echo $PGM": [INFO] **        Debug output is disabled."
  fi

  echo $PGM": [INFO] **************************************************************"
  echo ""
fi

#
# Set up to execute the Wazi Deploy deploy command
if [ $rc -eq 0 ]; then
  CommandLine="wazideploy-deploy -wf "${WorkingDirectory}" -dp "${DeploymentPlan}" -ef "${EnvironmentFile}
  if [ ! -z "${PackageInputFile}" ]; then
    CommandLine+=" -pif "${PackageInputFile}  
  fi 
  if [ ! -z "${EvidenceFile}" ]; then
    CommandLine+=" -efn "${EvidenceFile}  
  fi 
  if [ ! -z "${Debug}" ]; then
    CommandLine+=${Debug}  
  fi   
  echo ${CommandLine} 2>&1 
  ${CommandLine} 2>&1
  rc=$?

  if [ $rc -ne 0 ]; then
    ERRMSG=$PGM": [ERROR] Unable to Deploy package with Wazi Deploy. rc="$rc
    echo $ERRMSG
    rc=8
  fi
fi

exit $rc