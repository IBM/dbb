#!/bin/env bash
#===================================================================================
# NAME: prepare-logs.sh
#
# DESCRIPTION: Gathers DBB build log files into a single tarfile and echoes the filename
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
# 2023/08/28 LI  1.00 Initial Release
#===================================================================================Help() {
Help() {
    echo $PGM" - Prepare Logs ("$PGMVERS")                              "
    echo "                                                              "
    echo "Description: The purpose of this script to tar the build logs "
    echo "                                                              "
    echo "Syntax:                                                       "
    echo "                                                              "
    echo "       "$PGM" [Options]                                       "
    echo "                                                              "
    echo "Options:                                                      "
    echo "                                                              "
    echo "       -h               - Display this Help.                  "
    echo "                                                              "
    echo "       -w <workspace>      - Directory Path to a unique       "
    echo "                             working directory                "
    echo "                             Either an absolute path          "
    echo "                             or relative path.                "
    echo "                             If a relative path is provided,  "
    echo "                             buildRootDir and the workspace   "
    echo "                             path are combined.               "
    echo "                             Default=None, Required.          "
    echo "                                                              "
    echo "                 Ex: MortgageApplication/main/build-1         "
    echo "                                                              "
    echo "         Ex: MortgageApplication                              "
    echo "                                                              "
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
App=""

# Local Variables
# TLD: Always a good idea to initialize any local varables
AppDir=""                  # Derived Application Directory
outDir=""                  # Computed output directory to store build protocols
nestedApplicationFolder="" # Flag to understand a nested repository
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
    echo $PGM": [INFO] Prepare Build logs script. Version="$PGMVERS
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
    while getopts "h:w:" opt; do
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

        # Compute the outDir parameter
        outDir=$(getLogDir)

        if [ ! -d "$(getWorkDirectory)" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Workspace Directory ($(getWorkDirectory)) was not found. rc="$rc
            echo $ERRMSG
        fi
    fi

}

# Call validate Options
if [ $rc -eq 0 ]; then
    validateOptions
fi

# Print info
# Ready to go  TLD: Suggest in the section to echo as much as possible
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] **************************************************************"
    echo $PGM": [INFO] ** Started - Prepare logs on HOST/USER: ${SYS}/${USER}"
    echo $PGM": [INFO] **          Workspace:" $(getWorkDirectory)
    echo $PGM": [INFO] **             LogDir:" ${outDir}
    echo $PGM": [INFO] **************************************************************"
fi

# Start in working directory containing build directory
if [ $rc -eq 0 ]; then

    cd $outDir

    # Get build directory name
    # Assumes only one item prefixed with "build" (i.e. the run's build directory) in the working directory)
    # DBEHM: Remember, that the build directory name is optional for zAppBuild

    # BuildDir=$(ls -d build*)

    # Create tar file name based on logs directory name

    baseDir=$(basename $PWD)
    cd ..

    echo $PGM": [INFO] **  Directory contents:"
    CMD="ls -RlTr $baseDir"
    echo $PGM": [INFO] ${CMD}"
    ${CMD} #TLD: I commented this out for testing purposed
    rc=$?
fi

if [ $rc -eq 0 ]; then

    # Package files via tar so they can be downloaded in pipeline orchestrator's next steps
    CMD="tar -cf $baseDir.tar $baseDir"
    echo $PGM": [INFO] ${CMD}"
    ${CMD} #TLD: I commented this out for testing purposed
    rc=$?
fi

if [ $rc -eq 0 ]; then
    # Pass BuildLogFileName to pipeline orchestrator
    echo $PGM": [INFO] Logs successfully stored at $PWD/$baseDir.tar"
else
    ERRMSG=$PGM": [ERROR] Error during Prepare Logs. Check Console for details. rc="$rc
    echo $ERRMSG
    rc=8
fi

exit $rc
