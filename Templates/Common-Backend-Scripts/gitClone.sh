#!/bin/env bash
#===================================================================================
# NAME: Git-Clone.sh
#
# DESCRIPTION: The purpose of this script is to perform a git clone of a repository
# from within a Pipeline.
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
#
# Date       Who Vers  Description
# ---------- --- ----- --------------------------------------------------------------
# 2023/07/06 TLD 1.0.0 Initial Release
# 2023/09/25 DB  1.1.0 Initial Release
# 2024/03/01 DB  1.2.0 Updated Error Handling
#===================================================================================

Help() {
  echo $PGM" - Clone Repository ("$PGMVERS")                          "
  echo "                                                              "
  echo "Description: The purpose of this script is to perform a git   "
  echo "clone of a repository from within a Pipeline.                 "
  echo "                                                              "
  echo "Syntax:                                                       "
  echo "                                                              "
  echo "       "$PGM" [Options]                                       "
  echo "                                                              "
  echo "Options:                                                      "
  echo "                                                              "
  echo "       -h           - Help.                                   "
  echo "                                                              "
  echo "       -r <repo>    - Application Repository to be cloned.    "
  echo "                      Default=None, Required.                 "
  echo "                                                              "
  echo "         Ex: git@<sshurl>:<reponame>                          "
  echo "                                                              "
  echo "       -w <workspace>      - Directory Path to a unique       "
  echo "                             working directory                "
  echo "                             Either an absolute path          "
  echo "                             or relative path.                "
  echo "                             If a relative path is provided,  "
  echo "                             buildRootDir and the workspace   "
  echo "                             path are combined                "
  echo "                             Default=None, Required.          "
  echo "                                                              "
  echo "                 Ex: MortgageApplication/main/build-1         "
  echo "                                                              "
  echo "         Ex: /../dbb-logs                                     "
  echo "                                                              "
  echo "       -b <Branch>  - Name of the Branch to be cloned.        "
  echo "                      Default=None, Required.                 "
  echo "                                                              "
  echo "       Optional:                                              "
  echo "       -a <Application>    - Folder name to clone the         "
  echo "                             application git repo             "
  echo "                                                              "
  echo "                 Ex: MortgageApplication                      "
  echo "                                                              "
  echo "         Ex: refs/heads/main                                  "
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
PGMVERS="1.2.0"
USER=$(whoami)
SYS=$(uname -Ia)

rc=0
ERRMSG=""
Repo=""
WorkDir=""
Branch=""
application=""
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

# Print script title
if [ $rc -eq 0 ]; then
  echo $PGM": [INFO] Git Clone Wrapper. Version="$PGMVERS
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
  while getopts "h:r:a:w:b:" opt; do
    case $opt in
    h)
      Help
      ;;
    r)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [WARNING] Git Repository URL is required. rc="$rc
        echo $ERRMSG
        break
      fi
      Repo="$argument"
      ;;
    w)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [WARNING] Build Workspace Folder Name is required. rc="$rc
        echo $ERRMSG
        break
      fi
      Workspace="$argument"
      ;;
    a)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [WARNING] Application Name is required. rc="$rc
        echo $ERRMSG
        break
      fi
      application="$argument"
      ;;
    b)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [WARNING] Branch Name is required. rc="$rc
        echo $ERRMSG
        break
      fi
      Branch="$argument"
      ;;
    \?)
      Help
      rc=1
      break
      ;;
    :)
      rc=4
      ERRMSG=$PGM": [WARNING] Option -$OPTARG requires an argument. rc="$rc
      echo $ERRMSG
      break
      ;;
    esac
  done
fi
#

# Validate Options
validateOptions(){

  if [ -z "${Repo}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Application Repository is required. rc="$rc
    echo $ERRMSG
  fi
   
  if [ -z "${Workspace}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Unique Workspace Path is required. rc="$rc
    echo $ERRMSG
  else
    if [[ ${Workspace:0:1} != "/" ]] ; then
      if [ ! -d "${buildRootDir}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Workspace Directory (${buildRootDir}) was not found. rc="$rc
        echo $ERRMSG
      fi
    fi  
  fi 
    
  if [ -z "${Branch}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Branch Name is required. rc="$rc
    echo $ERRMSG
  fi
    
}
#


# Call validate Options
if [ $rc -eq 0 ]; then
  validateOptions
fi
#

# Fix up the Branch and Workspace Name
if [ $rc -eq 0 ]; then

  # Strip the prefix of the branch name for cloning
  BranchID=${Branch##*"refs/heads/"}

  # Isolate the Workspace Name from the Repo Name
  GitDir=$(basename ${Repo})
  GitDir=${GitDir%.*}
fi
#

# Ready to go
if [ $rc -eq 0 ]; then
  echo $PGM": [INFO] **************************************************************"
  echo $PGM": [INFO] ** Start Git Clone on HOST/USER: ${SYS}/${USER}"
  echo $PGM": [INFO] **          Repo:" ${Repo}
  echo $PGM": [INFO] **       WorkDir:" $(getWorkDirectory)
  if [ ! -z "${application}" ]; then
    echo $PGM": [INFO] **        GitDir:" ${application}
  else    
    echo $PGM": [INFO] **        GitDir:" ${GitDir}
  fi
  echo $PGM": [INFO] **           Ref:" ${Branch} "->" ${BranchID}
  echo $PGM": [INFO] **************************************************************"
  echo ""
fi
#
# Set up to perform the clone of the Repo
if [ $rc -eq 0 ]; then
  
  if [[ ${Workspace:0:1} != "/" ]] ; then 
    cd ${buildRootDir}
    rc=$?
  fi

  if [ $rc -eq 0 ]; then
    mkdir -p ${Workspace}
    rc=$?
  fi

  if [ $rc -eq 0 ]; then
    cd ${Workspace}
    rc=$?

    if [ $rc -ne 0 ]; then
      ERRMSG=$PGM": [ERROR] Unable to change to directory ${Workspace}. rc="$rc
      echo $ERRMSG
      rc=8
    fi
  else
    ERRMSG=$PGM": [ERROR] Unable to create directory ${Workspace}. rc="$rc
    echo $ERRMSG
    rc=8
  fi
fi
#

# Clone the Repo to z/OS UNIX System Services with a re-Direct of STDERR to STDOUT
if [ $rc -eq 0 ]; then

  echo $PGM": [INFO] Preforming Git Clone of Repo ${Repo}, Ref ${BranchID} to $(getWorkDirectory)"
  if [ ! -z "${application}" ]; then
    CMD="git clone -b ${BranchID} ${Repo} ${application}"
  else   
    CMD="git clone -b ${BranchID} ${Repo}"
  fi

  echo $PGM": [INFO] ${CMD}"
  ${CMD} 2>&1
  rc=$?

  if [ $rc -ne 0 ]; then
    ERRMSG=$PGM": [ERROR] Unable to Clone Repo ${Repo}, Ref ${BranchID}. rc="$rc
    echo $ERRMSG
    rc=8
  fi
fi
#

# Check status of the cloned Repo
if [ $rc -eq 0 ]; then

  if [ ! -z "${application}" ]; then
    cd ${application}
  else   
    cd ${GitDir}
  fi
  rc=$?

  if [ $rc -eq 0 ]; then

    echo $PGM": [INFO] Git Status for ${GitDir}"
    git status
    rc=$?

    if [ $rc -eq 0 ]; then

      echo $PGM": [INFO] Git Show-Ref for ${GitDir}"
      git show-ref
      rc=$?

      if [ $rc -ne 0 ]; then
        ERRMSG=$PGM": [ERROR] Error show all ref of Git Repo. rc="$rc
        echo $ERRMSG
        rc=8
      fi
    else
      ERRMSG=$PGM": [ERROR] Error determining status of Git Repo. rc="$rc
      echo $ERRMSG
      rc=8
    fi
  else
    ERRMSG=$PGM": [ERROR] Unable to change to directory ${GitDir}. rc="$rc
    echo $ERRMSG
    rc=8
  fi
fi

if [ $rc -eq 0 ]; then
  ERRMSG=$PGM": [INFO] Clone Repository Complete. rc="$rc
  echo $ERRMSG
else 
  ERRMSG=$PGM": [ERROR] Clone Repository Failed. Check Log. rc="$rc
  echo $ERRMSG
fi

exit $rc
