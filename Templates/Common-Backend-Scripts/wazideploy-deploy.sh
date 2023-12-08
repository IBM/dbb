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
# Date       Who  Vers Description
# ---------- ---- ---- --------------------------------------------------------------
# 2023/10/19 MDLB 1.00 Initial Release
# 2023/11/29 DB   1.10 Fixes to relative workspace directory
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
    echo "                                                                          "
    echo "       -p <deployment plan path>         - Path to the generated          "
    echo "                                           Deployment Plan                "
    echo "                                           (Optional)                     "
    echo "                                           Default=                       "
    echo "                                            See wdDeploymentPlanName      "
    echo "                                            in pipelineBackend.config     "
    echo "                                                                          "
    echo "       -e <environment file>             - Path to the environment file   "
    echo "                                           Either an absolute path        "
    echo "                                           or relative path.              "
    echo "                                           If a relative file name is     "
    echo "                                           provided, environment file     "
    echo "                                           is looked up in folder         "
    echo "                                           wdEnvironmentConfigurations    "
    echo "                                           in pipelineBackend.config      "
    echo "                                           Default=None, Required.        "
    echo "                                                                          "
    echo "                 Ex: Integration.yaml                                     "
    echo "                                                                          "
    echo "                                                                          "
    echo "       -i <package input file>           - Path to the package            "
    echo "                                           used as input                  "
    echo "                                           Default=None, Required.        "
    echo "                                           If a relative path is provided,"
    echo "                                           buildRootDir and the workspace "
    echo "                                           path are combined              "
    echo "                                           Default=None, Required.        "
    echo "                                                                          "
    echo "                 Ex: MortgageApplication.tar                              "
    echo "                                                                          "
    echo "       -l <evidence file path>           - Path to the evidence file      "
    echo "                                           (Optional)                     "
    echo "                                           Default=                       "
    echo "                                            See wdEvidenceFileName        "
    echo "                                            in pipelineBackend.config     "
    echo "                                                                          "
    echo "       -d                                - Debug tracing flag             "
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
    while getopts "hdw:p:e:i:l:" opt; do
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
            Debug="--debug"
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

    # compute deployment plan if not specified
    if [ -z "${DeploymentPlan}" ]; then
        # compute default based on configuration
        DeploymentPlan="$(wdDeployPackageDir)/${wdDeploymentPlanName}"
        # if still undefined
        if [ -z "${DeploymentPlan}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Deployment Plan is required. rc="$rc
            echo $ERRMSG
        fi
    fi
    
    # validate that deployment plan exists
    if [ ! -f "${DeploymentPlan}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Wazi Deploy Deployment Plan (${DeploymentPlan}) was not found. rc="$rc
        echo $ERRMSG
    fi

    # validate the environment file
    if [ -z "${EnvironmentFile}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Environment File is required. rc="$rc
        echo $ERRMSG
    else
        # check for relative path
        if [[ ! ${EnvironmentFile:0:1} == "/" ]]; then
            EnvironmentFile="${wdEnvironmentConfigurations}/${EnvironmentFile}"
        fi
    fi
    
    # validate that environment file exists
    if [ ! -f "${EnvironmentFile}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Wazi Deploy Environment File (${EnvironmentFile}) was not found. rc="$rc
        echo $ERRMSG
    fi

    # validate package input file
    if [ -z "${PackageInputFile}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Package Input File is required. rc="$rc
        echo $ERRMSG
    else
        # check for relative path
        if [[ ! ${PackageInputFile:0:1} == "/" ]]; then
            PackageInputFile="$(getLogDir)/${PackageInputFile}"
        fi
    fi

    # validate that package input file exists
    if [ ! -f "${PackageInputFile}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Package Input File (${PackageInputFile}) was not found. rc="$rc
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
    echo $PGM": [INFO] ** Start Wazi Deploy Deployment on HOST/USER: ${SYS}/${USER}"
    echo $PGM": [INFO] **               Working Directory:" ${Workspace}
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
    CommandLine="wazideploy-deploy --workingFolder "${Workspace}" --deploymentPlan "${DeploymentPlan}" --envFile "${EnvironmentFile}
    if [ ! -z "${PackageInputFile}" ]; then
        CommandLine+=" --packageInputFile "${PackageInputFile}
    fi
    if [ ! -z "${EvidenceFile}" ]; then
        CommandLine+=" --evidencesFileName "${EvidenceFile}
    fi
    if [ ! -z "${Debug}" ]; then
        CommandLine+=" ${Debug}"
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
