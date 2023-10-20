#!/bin/env bash
#===================================================================================
# NAME: wazideploy-generate.sh
#
# DESCRIPTION: The purpose of this script is to use wazideploy-generate to 
# generate a Deployment Plan for Wazi Deploy
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
  echo $PGM" - Generate Wazi Deploy Deployment Plan                               "
  echo "                                                                          "
  echo "Description: The purpose of this script is to use wazideploy-generate to  "
  echo "generate a Deployment Plan for Wazi Deploy.                               "
  echo "                                                                          "
  echo "Syntax:                                                                   "
  echo "                                                                          "
  echo "       "$PGM" [Options]                                                   "
  echo "                                                                          "
  echo "Options:                                                                  "
  echo "                                                                          "
  echo "       -h                                - Help.                          "
  echo "                                                                          "
  echo "       -m <deployment method path>       - Path to the Deployment Method  "
  echo "                                           Default=None, Required         "
  echo "                                                                          "
  echo "       -p <deployment plan path>         - Path to the generated          "
  echo "                                           Deployment Plan                "
  echo "                                           Default=None, Required         "
  echo "                                                                          "
  echo "       -r <deployment plan report>       - Path to the generated          "
  echo "                                           Deployment Plan report         "
  echo "                                           Default=None, Optional         "
  echo "                                                                          "
  echo "       -i <package input file path>      - Path to the package            "
  echo "                                           used as input                  "
  echo "                                           Default=None, Optional         "
  echo "                                                                          "
  echo "       -o <package output file path>     - Path to the package            "
  echo "                                           used as output                 "
  echo "                                           Default=None, Optional         "
  echo "                                                                          "
  echo "       -c <configuration file path>      - Path to configuration file     "
  echo "                                           for the artifact repository    "
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
DeploymentMethod=""
DeploymentPlan=""
DeploymentPlanReport=""
PackageInputFile=""
PackageOutputFile=""
ConfigFile=""
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
  echo $PGM": [INFO] Generate Wazi Deploy Deployment Plan. Version="$PGMVERS
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
  while getopts "hd:m:p:r:i:o:c:" opt; do
    case $opt in
    h)
      Help
      ;;

    m)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Deployment Method is required. rc="$rc
        echo $ERRMSG
        break
      fi
      DeploymentMethod="$argument"
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

    r)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Path to Deployment Plan Report is required. rc="$rc
        echo $ERRMSG
        break
      fi
      DeploymentPlanReport="$argument"
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

    o)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Path to Package Output File is required. rc="$rc
        echo $ERRMSG
        break
      fi
      PackageOutputFile="$argument"
      ;;

    c)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Configuration File is required. rc="$rc
        echo $ERRMSG
        break
      fi
      ConfigFile="$argument"
      ;;


    d)
      # Add command to produce debug output with Wazi Deploy
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
  if [ -z "${DeploymentMethod}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Deployment Method is required. rc="$rc
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
  if [ -z "${PackageInputFile}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Package Input File is required. rc="$rc
    echo $ERRMSG
  fi
fi

if [ $rc -eq 0 ]; then
  if [ ! -z "${PackageOutputFile}" ] && [ -z "${ConfigFile}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Configuration File is required when specifying Package Output File. rc="$rc
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
  echo $PGM": [INFO] ** Start Wazi Deploy Generation on HOST/USER: ${SYS}/${USER}"
  echo $PGM": [INFO] **               Deployment Method:" ${DeploymentMethod}
  echo $PGM": [INFO] **       Generated Deployment Plan:" ${DeploymentPlan}

  if [ ! -z "${DeploymentPlanReport}" ]; then
    echo $PGM": [INFO] **          Deployment Plan Report:" ${DeploymentPlanReport}
  fi

  echo $PGM": [INFO] **              Package Input File:" ${PackageInputFile}

  if [ ! -z "${PackageOutputFile}" ]; then
    echo $PGM": [INFO] **             Package Output File:" ${PackageOutputFile}
  fi

  if [ ! -z "${ConfigFile}" ]; then
    echo $PGM": [INFO] **              Configuration File:" ${ConfigFile}
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
# Set up to execute the Wazi Deploy generate command
if [ $rc -eq 0 ]; then
  CommandLine="wazideploy-generate -dm "${DeploymentMethod}" -dp "${DeploymentPlan}" -pif "${PackageInputFile}
  if [ ! -z "${DeploymentPlanReport}" ]; then
    CommandLine+=" -dpr "${DeploymentPlanReport}  
  fi 
  if [ ! -z "${PackageOutputFile}" ]; then
    CommandLine+=" -pof "${PackageOutputFile}  
  fi 
  if [ ! -z "${ConfigFile}" ]; then
    CommandLine+=" -cdf "${ConfigFile}  
  fi 
  if [ ! -z "${Debug}" ]; then
    CommandLine+=${Debug}  
  fi 
  echo ${CommandLine} 2>&1 
  ${CommandLine} 2>&1
  rc=$?

  if [ $rc -ne 0 ]; then
    ERRMSG=$PGM": [ERROR] Unable to Generate Deployment Plan with Wazi Deploy. rc="$rc
    echo $ERRMSG
    rc=8
  fi
fi

exit $rc
