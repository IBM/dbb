#!/bin/env bash
#===================================================================================
# NAME: DBBUCDDeploy.sh
#
# DESCRIPTION: The purpose of this script is to perform a deploy of z/OS binaries
# using UrbanCode Deploy from a pipeline.
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
  echo $PGM" - Deploy UCD Component                                               "
  echo "                                                                          "
  echo "Description: The purpose of this script is to perform a                   "
  echo "deployment of a UCD component version to a specified runtime              "
  echo "environment.                                                              "
  echo "                                                                          "
  echo "Syntax:                                                                   "
  echo "                                                                          "
  echo "       "$PGM" [Options]                                                   "
  echo "                                                                          "
  echo "Options:                                                                  "
  echo "                                                                          "
  echo "       -h                       - Help.                                   "
  echo "                                                                          "
  echo "       -a <application>         - UCD application name                    "
  echo "                                  Default=None, Required                  "
  echo "                                                                          "
  echo "       -p <application process> - UCD application process name            "
  echo "                                  Default=None, Required                  "
  echo "                                                                          "
  echo "       -e <environment>         - UCD application environment name        "
  echo "                                  Default=None, Required                  "
  echo "                                                                          "
  echo "       -d <versions to deploy>  - Versions to deploy                      "
  echo "                                  Default=None, Required                  "
  echo "             Ex: Component1:latest\nComponent2:rel-2.10                   "
  echo "                                                                          "
  echo "       -t                       - Deployment timeout in seconds           "
  echo "                                  Default 300s                            "
  echo "                                                                          "
  echo "       -s                       - SSL protocols to handle                 "
  echo "                                  Default is TLSv1.2                      "
  echo "             Ex: TLSv1.2,TLSv1.3                                          "
  echo "                                                                          "
  echo "       -k                       - Disable SSL verification flag           "
  echo "                                                                          "
  echo "       -v                       - Verbose tracing flag                    "
  echo " "
  exit 0
}

#
# Customization
# Configuration file leveraged by the backend scripts
# Either an absolute path or a relative path to the current working directory
SCRIPT_HOME="`dirname "$0"`"
pipelineConfiguration="${SCRIPT_HOME}/pipelineBackend.config"
deployScript="${SCRIPT_HOME}/../../../Pipeline/DeployUCDComponentVersion/ucd-deploy.groovy"
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
Application=""
Process=""
Environment=""
Version=""
Timeout=""
SSLProtocol=""
OptionalCommands=""
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
  echo $PGM": [INFO] Deploy UCD Component. Version="$PGMVERS
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
  while getopts "hkvt:s:a:p:e:d:" opt; do
    case $opt in
    h)
      Help
      ;;

    a)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] UCD Application Name is required. rc="$rc
        echo $ERRMSG
        break
      fi
      Application="$argument"
      ;;

    p)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] UCD Application Process Name is required. rc="$rc
        echo $ERRMSG
        break
      fi
      Process="$argument"
      ;;

    e)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] UCD Environment Name is required. rc="$rc
        echo $ERRMSG
        break
      fi
      Environment="$argument"
      ;;

    d)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] UCD Version(s) to deploy is required. rc="$rc
        echo $ERRMSG
        break
      fi
      Version="$argument"
      ;;

    t)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] UCD Timeout value is required. rc="$rc
        echo $ERRMSG
        break
      fi
      Timeout="$argument"
      ;;

    s)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] SSL Protocol value is required. rc="$rc
        echo $ERRMSG
        break
      fi
      SSLProtocol="$argument"
      ;;

    k)
      # Add command to disable SSL verification
      OptionalCommands=${OptionalCommands}" -k"
      ;;

    v)
      # Add command to produce verbose output
      OptionalCommands=${OptionalCommands}" -v"
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
  if [ -z "${Application}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] UCD Application Name is required. rc="$rc
    echo $ERRMSG
  fi
fi

if [ $rc -eq 0 ]; then
  if [ -z "${Process}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] UCD Appliocation Process Name is required. rc="$rc
    echo $ERRMSG
  fi
fi

if [ $rc -eq 0 ]; then
  if [ -z "${Environment}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] UCD Environment Name is required. rc="$rc
    echo $ERRMSG
  fi
fi

if [ $rc -eq 0 ]; then
  if [ -z "${Version}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] UCD Version(s) to deploy is required. rc="$rc
    echo $ERRMSG
  fi
fi

if [ $rc -eq 0 ]; then
  # Validate Packaging script
  if [ ! -f "${deployScript}" ]; then
    rc=8
    ERRMSG=$PGM": [ERR] Unable to locate ${deployScript}. rc="$rc
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
  echo $PGM": [INFO] ** Start UCD Component Deploy on HOST/USER: ${SYS}/${USER}"
  echo $PGM": [INFO] **   Create UCD component version script:" ${deployScript}
  echo $PGM": [INFO] **                  UCD Application Name:" ${Application}
  echo $PGM": [INFO] **                  UCD Environment Name:" ${Environment}
  echo $PGM": [INFO] **                         UCD User Name:" ${ucdUserName}
  echo $PGM": [INFO] **                        UCD Server URL:" ${ucdServerURL}
  echo $PGM": [INFO] **                 UCD Component Version:" ${Version}
  echo $PGM": [INFO] **          UCD Application Process Name:" ${Process}

  if [ ! -z "${Timeout}" ]; then
    echo $PGM": [INFO] **           UCD Timeout (seconds):" ${Timeout}
  fi

  if [ ! -z "${SSLProtocol}" ]; then
    echo $PGM": [INFO] **                    SSL Protocol:" ${SSLProtocol}
  fi

  if [[ "$OptionalCommands" == *"-v"* ]]; then
    echo $PGM": [INFO] **       Verbose output is enabled: TRUE"
  else
    echo $PGM": [INFO] **       Verbose output is enabled: FALSE"
  fi

  if [[ "$OptionalCommands" == *"-k"* ]]; then
    echo $PGM": [INFO] **       SSL Verification disabled: TRUE"
  else
    echo $PGM": [INFO] **       SSL Verification disabled: FALSE"
  fi

  echo $PGM": [INFO] **************************************************************"
  echo ""
fi

#
# Set up to execute the UCD Deploy groovy script
if [ $rc -eq 0 ]; then
  echo $DBB_HOME/bin/groovyz ${deployScript} -a '"'${Application}'"' -e '"'${Environment}'"' -U ${ucdUserName} -P ${ucdPassword} -u ${ucdServerURL} -d '"'${Version}'"' -p ${Process} ${OptionalCommands} 2>&1
  $DBB_HOME/bin/groovyz ${deployScript} -a '"'${Application}'"' -e '"'${Environment}'"' -U ${ucdUserName} -P ${ucdPassword} -u '"'${ucdServerURL}'"' -d '"'${Version}'"' -p ${Process} ${OptionalCommands} 2>&1
  rc=$?

  if [ $rc -ne 0 ]; then
    ERRMSG=$PGM": [ERROR] Unable to Deploy Application ${Application} to environment ${Environment}. rc="$rc
    echo $ERRMSG
    rc=8
  fi
fi

exit $rc
