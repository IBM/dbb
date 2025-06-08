#!/bin/env bash
#===================================================================================
# NAME: deleteWorkspace.sh
#
# DESCRIPTION: Deletes the build workspace from Unix System Services
#
#
# SYNTAX: See Help() Section Below
#
# OPTIONS: See Help() Section Below
#
# RETURN CODES:
#
#    0          - Successful
#    4          - Warning message(s) issued. See Console messages.
#    8          - Error encountered. See Console messages.
#
# NOTE(S):
#
#   1. Review the pipeline backend configuration file
#      (pipelineBackend.config)
#
# Maintenance Log
#
# Date       Who Vers Description
# ---------- --- ---- --------------------------------------------------------------
# 2024/01/10 DB  1.00 Initial Release
#===================================================================================
Help() {
    echo "deleteWorkspace.sh ("$PGMVERS")                                     "
    echo "                                                                    "
    echo "DESCRIPTION: Deletes the build workspace from Unix System Services  "
    echo "                                                                    "
    echo "Syntax:                                                             "
    echo "                                                                    "
    echo "       "$PGM" [Options]                                             "
    echo "                                                                    "
    echo "Options:                                                            "
    echo "                                                                    "
    echo "       -h                  - Display this Help.                     "
    echo "                                                                    "
    echo "       -w <workspace>      - Directory Path to a unique             "
    echo "                             working directory                      "
    echo "                             Either an absolute path                "
    echo "                             or relative path.                      "
    echo "                             If a relative path is provided,        "
    echo "                             buildRootDir and the workspace         "
    echo "                             path are combined.                     "
    echo "                             Default=None, Required.                "
    echo "                                                                    "
    echo "                 Ex: MortgageApplication/main/build-1               "
    echo "                                                                    "
    exit 0
}

#
# Customization
# Configuration file leveraged by the backend scripts
# Either an absolute path or a relative path to the current working directory
SCRIPT_HOME="$(dirname "$0")"
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

# Initialized option variables passed to this script
Workspace=""

# Local Variables
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

# Print script info
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] Delete Workspace script. Version="$PGMVERS
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
    while getopts "hw:" opt; do
        case $opt in
        h)
            Help
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
validateOptions() {

    if [ -z "${Workspace}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Unique Workspace parameter (-w) is required. rc="$rc
        echo $ERRMSG
    else

        if [ ! -d "$(getWorkDirectory)" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Workspace Directory ($(getWorkDirectory)) was not found. rc="$rc
            echo $ERRMSG
        fi

        # Check that workspace directory is a reasonable length (>=12 characters) to
        # prevent deletion of high-level directories
        workDirectory="$(getWorkDirectory)"
        workDirectoryLength=${#workDirectory}
        if [ $workDirectoryLength -lt 12 ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Working directory path ($(getWorkDirectory)) is too short ($workDirectoryLength characters). \
            Expected length: 12 or more characters. rc="$rc
            echo $ERRMSG
        fi
    fi

}

# Call validate Options
if [ $rc -eq 0 ]; then
    validateOptions
fi

# Print info
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] **************************************************************"
    echo $PGM": [INFO] ** Started - Delete Workspace on HOST/USER: ${SYS}/${USER}    "
    echo $PGM": [INFO] **          Working Directory:" $(getWorkDirectory)                   
    echo $PGM": [INFO] **          Workspace        :" $Workspace                            
    echo $PGM": [INFO] **************************************************************"
fi

# Delete build directory
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] Deleting working directory $(getWorkDirectory): "
    CMD="rm -PRf $(getWorkDirectory)"
    echo $PGM": [INFO] ${CMD}"
    ${CMD}
    rc=$?
fi

if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] Workspace directory successfully deleted. rc="$rc
else 
    echo $PGM": [ERROR] Deleting workspace directory $(getWorkDirectory) failed. rc="$rc
fi 

exit $rc