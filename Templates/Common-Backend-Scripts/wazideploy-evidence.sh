#!/bin/env bash
#===================================================================================
# NAME: wazideploy-evidence.sh
#
# DESCRIPTION: The purpose of this script is to use wazideploy-evidence to
# generate Deployment report from the Wazi Deploy Evidence YAML file
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
# Date       Who  Vers Description
# ---------- ---- ---- --------------------------------------------------------------
# 2023/11/20 MDLB 1.00 Initial Release
# 2023/11/29 DB   1.10 Fixes to relative workspace directory
# 2025/07/24 DB   1.20 Use default query and report renderer
# 2025/08/14 DB   1.21 Fixes - query and report renderer
#===================================================================================
Help() {
    echo $PGM" - Generate deployment reports with Wazi Deploy                       "
    echo "                                                                          "
    echo "Description: The purpose of this script is to use wazideploy-evidence to  "
    echo " generate Deployment report from Wazi Deploy the Evidence YAML file       "
    echo "                                                                          "
    echo "Syntax:                                                                   "
    echo "                                                                          "
    echo "       "$PGM" [Options]                                                   "
    echo "                                                                          "
    echo "Options:                                                                  "
    echo "                                                                          "
    echo "       -h                                - Help.                          "
    echo "                                                                          "
    echo "       -w <workspace>                    - Directory Path to a unique     "
    echo "                                           working directory              "
    echo "                                           Either an absolute path        "
    echo "                                           or relative path.              "
    echo "                                           If a relative path is provided,"
    echo "                                           buildRootDir and the workspace "
    echo "                                           path are combined              "
    echo "                                           Default computed in            "
    echo "                                            wdDeployPackageDir()          "
    echo "                                            in pipelineBackend.config     "
    echo "                                           Default=None, Required.        "
    echo "                                                                          "
    echo "                 Ex: MortgageApplication/main/build-1                     "
    echo "                                                                          "
    echo "       -o <output file path>             - Path to the output file        "
    echo "                                           (Optional)                     "
    echo "                                           Default=                       "
    echo "                                            See wdOutputFile              "
    echo "                                            in pipelineBackend.config     "
    echo "                                           Either an absolute path        "
    echo "                                           or relative path.              "
    echo "                                           If a relative path is provided,"
    echo "                                           buildRootDir and the workspace "
    echo "                                           path are combined              "
    echo "                                           Default computed in            "
    echo "                                            wdDeployPackageDir()          "
    echo "                                            in pipelineBackend.config     "
    echo "                                           Default=None, Required.        "
    echo "                                                                          "
    echo "       -l <evidence file path>           - Path to the evidence file      "
    echo "                                           (Optional)                     "
    echo "                                           Default=                       "
    echo "                                            See wdEvidenceFileName        "
    echo "                                            in pipelineBackend.config     "
    echo "                                           If a relative path is provided,"
    echo "                                           buildRootDir and the workspace "
    echo "                                           path are combined              "
    echo "                                           Default computed in            "
    echo "                                            wdDeployPackageDir()          "
    echo "                                            in pipelineBackend.config     "
    echo "                                           Default=None, Required.        "
    echo " "
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
PGMVERS="1.10"
USER=$(whoami)
SYS=$(uname -Ia)

rc=0
ERRMSG=""
Workspace=""
EvidenceFile=""
EvidenceFolder=""
OutputFile=""
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
    echo $PGM": [INFO] Generate deployment reports with Wazi Deploy. Version="$PGMVERS
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
    while getopts "hw:o:l:" opt; do
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
            Workspace="$argument"
            ;;

        o)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [ERROR] Path to the Output File is required. rc="$rc
                echo $ERRMSG
                break
            fi
            OutputFile="$argument"
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

# Valicate Workspace property
checkWorkspace() {
    if [ -z "${Workspace}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Unique Workspace parameter is required. rc="$rc
        echo $ERRMSG
    fi
}

# Validate Options
validateOptions() {

    # validate workspace
    if [ -z "${Workspace}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Unique Workspace parameter (-w) is required. rc="$rc
        echo $ERRMSG
    else
        # relative workspace directory
        if [[ ! ${Workspace:0:1} == "/" ]]; then
            Workspace="$(getWorkDirectory)"
        fi

        # validate if workspace directory exists
        if [ ! -d "${Workspace}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Workspace Directory (${Workspace}) was not found. rc="$rc
            echo $ERRMSG
        fi
    fi

    # compute output file if not specified
    if [ -z "${OutputFile}" ]; then
        # compute default based on configuration
        OutputFile="$(wdDeployPackageDir)/${wdOutputFile}"
    else
        # relative path
        if [[ ! ${OutputFile:0:1} == "/" ]]; then
            OutputFile="$(wdDeployPackageDir)/${OutputFile}"
        fi
    fi

    # Validate configuration
    if [ -z "${wdQueryTemplate}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Wazi Deploy Query template is required to be defined in pipelineBackend.config (wdQueryTemplate). rc="$rc
        echo $ERRMSG
    fi

    if [ -z "${wdReportRenderer}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Wazi Deploy Renderer is required to be defined in pipelineBackend.config (wdReportRenderer). rc="$rc
        echo $ERRMSG
    fi

    # compute evidence file if not specified
    if [ -z "${EvidenceFile}" ]; then
        # compute default based on configuration
        EvidenceFile="$(wdDeployPackageDir)/${wdEvidenceFileName}"
    else
        # relative path
        if [[ ! ${EvidenceFile:0:1} == "/" ]]; then
            EvidenceFile="$(wdDeployPackageDir)/${EvidenceFile}"
        fi
    fi
    EvidenceFolder="$(dirname "${EvidenceFile}")"
}

# Call validate Options
if [ $rc -eq 0 ]; then
    validateOptions
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
    echo $PGM": [INFO] ** Start Wazi Deploy evidence reporting on HOST/USER: ${SYS}/${USER}"
    echo $PGM": [INFO] **               Working Directory:" ${Workspace}

    if [ ! -z "${EvidenceFile}" ]; then
        echo $PGM": [INFO] **                   Evidence File:" ${EvidenceFile}
    fi

    if [ ! -z "${wdQueryTemplate}" ]; then
        echo $PGM": [INFO] **                  Query Template:" ${wdQueryTemplate}
    fi

    if [ ! -z "${wdReportRenderer}" ]; then
        echo $PGM": [INFO] **                 Report Renderer:" ${wdReportRenderer}
    fi

    if [ ! -z "${OutputFile}" ]; then
        echo $PGM": [INFO] **                     Output File:" ${OutputFile}
    fi

    echo $PGM": [INFO] **************************************************************"
    echo ""
fi

#
# Set up to execute the Wazi Deploy evidence command
if [ $rc -eq 0 ]; then
    CommandLine="wazideploy-evidence --dataFolder ${EvidenceFolder} --index ${Workspace}/${wdIndexFolder} --query ${wdQueryTemplate} --output=${OutputFile} ir renderer_name=${wdReportRenderer}"     
     
    echo ${CommandLine} 2>&1
    ${CommandLine} 2>&1
    rc=$?

    if [ $rc -ne 0 ]; then
        ERRMSG=$PGM": [ERROR] Wazi Deploy Evidence command failed. rc="$rc
        echo $ERRMSG
    fi

fi

exit $rc
