#!/bin/env bash
#===================================================================================
# NAME: generateCleanupCommands.sh
#
# DESCRIPTION: The purpose of this script is to generate the necessary clenaup
#              instructions to delete DBB Build groups and build datasets that no
#              longer are needed
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
# 2024/12/20 DB  1.0.0 Initial Version to generate Cleanup command file
#===================================================================================

Help() {
    echo $PGM" - Generate Cleanup Command File ("$PGMVERS")             "
    echo "                                                              "
    echo "Description:                                                  "
    echo " The purpose of this script is to generate the necessary      "
    echo " clenaup instructions to delete DBB Build groups and build    "
    echo " datasets that no longer are needed.                          "
    echo "                                                              "
    echo "Syntax:                                                       "
    echo "                                                              "
    echo "       "$PGM" [Options]                                       "
    echo "                                                              "
    echo "Options:                                                      "
    echo "                                                              "
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
    echo "       -a <Application>    - Folder name to clone the         "
    echo "                             application git repo             "
    echo "                                                              "
    echo "                 Ex: MortgageApplication                      "
    echo "                                                              "
    echo "       -p                  - Flag to control if the delete    "
    echo "                             cmd files should be              "
    echo "                             executed in the pipeline         "
    echo "                                                              "
    echo "                                                              "
    echo " "
    exit 0
}

#
# Customization
# Configuration file leveraged by the backend scripts
# Either an absolute path or a relative path to the current working directory
SCRIPT_HOME="$(dirname "$0")"
pipelineConfiguration="${SCRIPT_HOME}/pipelineBackend.config"
buildUtilities="${SCRIPT_HOME}/utilities/dbbBuildUtils.sh"
# Customization - End

#
# Internal Variables
#set -x                  # Uncomment to enable shell script debug
#export BASH_XTRACEFD=1  # Write set -x trace to file descriptor

PGM=$(basename "$0")
PGMVERS="1.0.0"
USER=$(whoami)
SYS=$(uname -Ia)

rc=0
App=""
gitRepoPath=""
executeCleanupCommandScripts="false" # controls if the execute steps should be performed
collectionsToBeDeleted=()            # Array of collections to be deleted
buildgroupsToBeDeleted=()            # Array of build groups to be deleted
hlqListToBeDeleted=()                # Array of all the HLQs that can be deleted
cmdFileDeleteCollections=""          # will be computed
cmdFileDeleteBuildGroups=""          # will be computed
cmdFileDeleteBuildDatasets=""        # will be computed
outputDir="cleanupCmds"              # outputs directory stored in the work space
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
    echo $PGM": [INFO] Generate Cleanup Command File. Version="$PGMVERS
fi

# Read and import pipeline configuration
if [ $rc -eq 0 ]; then

    # Read and source pipeline configuration
    if [ ! -f "${pipelineConfiguration}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Pipeline Configuration File (${pipelineConfiguration}) was not found. rc="$rc
        echo $ERRMSG
    else
        source $pipelineConfiguration
    fi

    # Read and import utilities
    if [ ! -f "${buildUtilities}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] DBB-Build internal utilities (${buildUtilities}) was not found. rc="$rc
        echo $ERRMSG
    else
        source $buildUtilities
    fi

fi

# Get Options
if [ $rc -eq 0 ]; then
    while getopts "h:a:w:p" opt; do
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
        a)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] Application Name is required. rc="$rc
                echo $ERRMSG
                break
            fi
            App="$argument"
            ;;
        p)
            executeCleanupCommandScripts="true"
            ;;
        \?)
            Help
            rc=1
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
        ERRMSG=$PGM": [ERROR] Unique Workspace Path is required. rc="$rc
        echo $ERRMSG
    else
        if [[ ${Workspace:0:1} != "/" ]]; then
            if [ ! -d "${buildRootDir}" ]; then
                rc=8
                ERRMSG=$PGM": [ERROR] Workspace Directory (${buildRootDir}) was not found. rc="$rc
                echo $ERRMSG
            fi
        fi
    fi

    if [ -z "${App}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Application parameter (-a) is required. rc="$rc
        echo $ERRMSG
    else

        AppDir=$(getApplicationDir)

        # Check if application directory contains
        if [ -d "${AppDir}/${App}" ]; then
            echo $PGM": [INFO] Detected the application repository (${application}) within the git repository layout structure."
            echo $PGM": [INFO]  Assuming this as the new application location."
            AppDir="${AppDir}${App}"
            nestedApplicationFolder="true"
        fi

        if [ ! -d "${AppDir}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Application Directory (${AppDir}) was not found. rc="$rc
            echo $ERRMSG
        else
            CMD="git -C $AppDir rev-parse --is-inside-work-tree"
            isGitDir=$(${CMD})
            rc=$?
            if [ ! "${isGitDir}" == "true" ]; then
                rc=8
                ERRMSG=$PGM": [ERROR] $AppDir is not a git directory or the git command ($CMD) failed. Check Log. rc="$rc
                echo $ERRMSG
            fi
        fi
    fi

    if [ ! -f "${deleteScript}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Delete script (${deleteScript}) was not found. rc="$rc
        echo $ERRMSG
    fi

}

# Prepare output files
prepareTaskOutputs() {

    cmdDir="$(getWorkDirectory)/${outputDir}"
    echo $PGM": [INFO] Creating output directory. ${cmdDir}"
    mkdir -p "${cmdDir}"

    cmdFileDeleteCollections="${cmdDir}/deleteStaleCollections.cmd"
    cmdFileDeleteBuildGroups="${cmdDir}/deleteStaleBuildGroups.cmd"
    cmdFileDeleteBuildDatasets="${cmdDir}/deleteStaleBuildDatasets.cmd"
}

# Call validate Options
if [ $rc -eq 0 ]; then
    validateOptions
fi

# Call validate Options
if [ $rc -eq 0 ]; then
    prepareTaskOutputs
fi

# Check this method accepts a collection name and validates if an active branch exits
# If no branch is found it adds the information into lists for further processing in a subsequent stage.

checkGitReference() {
    # cut of the outputs collection name
    collection=${string/%-outputs/}
    pattern="$App-(.*)"
    if [[ $collection =~ $pattern ]]; then
        branchName=${BASH_REMATCH[1]}
        # does the branch exist?
        CMD="git -C $AppDir show-ref $branchName"
        gitRef=$($CMD)
        if [ ! -z "$gitRef" ]; then
            echo $PGM":        For the collection $string a corresponding branch ($branchName) was detected."
        else
            echo $PGM":        DBB Collection $string does not have a corresponding branch ($branchName). It can be deleted."

            # add collection
            collectionsToBeDeleted+=("$string")
            # add build group and hlq if it's not a output collection
            if [ "$string" == "$collection" ]; then
                buildgroupsToBeDeleted+=($string)

                # compute hlq
                Branch=$branchName
                computeBuildConfiguration
                if [ $rc -eq 0 ]; then
                    hlqListToBeDeleted+=("$HLQ")
                else
                    ERRMSG=$PGM": [WARNING] Computation of build HLQ did not complete successfully for branch name $branchName. Please check log. rc="$rc
                    echo $ERRMSG
                    rc=0 # reset RC to continue
                fi

            fi

        fi
    fi
}

# generate command file to delete collections
genDeleteStatementsCollections() {

    echo $PGM": [STAGE] Generate Cmd File with Delete Statements for stale collections for application $App"

    # Cleanup
    if [ -f "${cmdFileDeleteCollections}" ]; then
        rm ${cmdFileDeleteCollections}
        ERRMSG=$PGM": [INFO] Existing file ${cmdFileDeleteCollections} deleted."
        echo $ERRMSG
    fi

    for applicationCollection in ${collectionsToBeDeleted[@]}; do

        echo "dbb collection delete $applicationCollection ${dbbMetadataStoreOptions}" >>${cmdFileDeleteCollections}

    done

    echo $PGM": [INFO] Cmd File ${cmdFileDeleteCollections} created. "

}

# generate command file to delete build groups
genDeleteStatementsBuildGroups() {

    echo $PGM": [STAGE] Generate Cmd File with Delete Statements for stale build groups for application $App"

    # Cleanup
    if [ -f "${cmdFileDeleteBuildGroups}" ]; then
        rm ${cmdFileDeleteBuildGroups}
        ERRMSG=$PGM": [INFO] Existing file ${cmdFileDeleteBuildGroups} deleted."
        echo $ERRMSG
    fi

    for buildGroup in ${buildgroupsToBeDeleted[@]}; do

        echo "dbb build-group delete $buildGroup ${dbbMetadataStoreOptions}" >>${cmdFileDeleteBuildGroups}

    done

    echo $PGM": [INFO] Cmd File ${cmdFileDeleteBuildGroups} created. "

}

# generate command file to delete build datasets
genDeleteStatementsBuildDatasets() {

    echo $PGM": [STAGE] Generate Cmd File with Delete Statements for stale build datasets for application $App"

    # Cleanup
    if [ -f "${cmdFileDeleteBuildDatasets}" ]; then
        rm ${cmdFileDeleteBuildDatasets}
        ERRMSG=$PGM": [INFO] Existing file ${cmdFileDeleteBuildDatasets} deleted."
        echo $ERRMSG
    fi

    for buildHlq in ${hlqListToBeDeleted[@]}; do

        echo "\$DBB_HOME/bin/groovyz ${deleteScript} -h $buildHlq" >>${cmdFileDeleteBuildDatasets}

    done

    echo $PGM": [INFO] Cmd File ${cmdFileDeleteBuildDatasets} created. "

}

# Ready to go
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] **************************************************************"
    echo $PGM": [INFO] ** Start Gen Cleanup Cmds on HOST/USER: ${SYS}/${USER}"
    echo $PGM": [INFO] **                   Workspace:" $(getWorkDirectory)
    echo $PGM": [INFO] **                 Application:" ${App}
    echo $PGM": [INFO] **                      AppDir:" ${AppDir}
    echo $PGM": [INFO] **    Cmd obsolete collections:" ${cmdFileDeleteCollections}
    echo $PGM": [INFO] **   Cmd obsolete build groups:" ${cmdFileDeleteBuildGroups}
    echo $PGM": [INFO] ** Cmd obsolete build datasets:" ${cmdFileDeleteBuildDatasets}
    echo $PGM": [INFO] **      DBB Metadastore Config:" ${dbbMetadataStoreOptions}
    echo $PGM": [INFO] **     Process Cleanup Scripts:" ${executeCleanupCommandScripts}
    echo $PGM": [INFO] **************************************************************"
    echo ""
fi

if [ $rc -eq 0 ]; then
    # Retrieve existing DBB Collections
    echo $PGM": [STAGE] Retrieve all collections with application qualifier $App"
    applicationCollections=$(dbb collection list $dbbMetadataStoreOptions | grep $App)
    rc=$?
    if [ ! $rc -eq 0 ]; then
        ERRMSG=$PGM": [ERROR] Retrieving Collections failed. Check Log. rc="$rc
        echo $ERRMSG
    fi
fi

if [ $rc -eq 0 ]; then
    # Map DBB collections with existing git branches to compute stale build groups
    echo $PGM": [STAGE] Verifying Git references"
    for applicationCollection in ${applicationCollections[@]}; do
        echo $PGM": [INFO] Check if $applicationCollection has a corresponding active Git branch"
        string=$applicationCollection
        checkGitReference
    done
fi

if [ $rc -eq 0 ]; then
    # Generate DELETE statements for DBB collections
    genDeleteStatementsCollections
fi

if [ $rc -eq 0 ]; then
    # Generate DELETE statements for DBB build group
    genDeleteStatementsBuildGroups
fi

if [ $rc -eq 0 ]; then
    # Generate DELETE statements for build datasets
    genDeleteStatementsBuildDatasets
fi

if [ $rc -eq 0 ]; then
    echo $PGM": [STAGE] Executing Cleanup of DBB Metadatastore Objects"
fi

if [ $rc -eq 0 ]; then
    if [ "${executeCleanupCommandScripts}" = "true" ]; then

        if [ $rc -eq 0 -a -f "${cmdFileDeleteCollections}" ]; then
            echo $PGM": [INFO] Executing cleanup script ${cmdFileDeleteCollections}"
            chmod 755 ${cmdFileDeleteCollections}
            ${cmdFileDeleteCollections}
            rc=$?
        fi

        if [ $rc -eq 0 -a -f "${cmdFileDeleteBuildGroups}" ]; then
            echo $PGM": [INFO] Executing cleanup script ${cmdFileDeleteBuildGroups}"
            chmod 755 ${cmdFileDeleteBuildGroups}
            ${cmdFileDeleteBuildGroups}
            rc=$?
        fi

        if [ $rc -eq 0 -a -f "${cmdFileDeleteBuildDatasets}" ]; then
            echo $PGM": [INFO] Executing cleanup script ${cmdFileDeleteBuildDatasets}"
            chmod 755 ${cmdFileDeleteBuildDatasets}
            ${cmdFileDeleteBuildDatasets}
            rc=$?
        fi

    else
        echo $PGM": [INFO] Execution of scripts has been skipped."

    fi
fi

if [ $rc -eq 0 ]; then
    ERRMSG=$PGM": [INFO] Generate Cleanup Cmds Complete. rc="$rc
    echo $ERRMSG
else
    ERRMSG=$PGM": [ERROR] Generate Cleanup Cmds Failed. Check Log. rc="$rc
    echo $ERRMSG
fi

exit $rc
