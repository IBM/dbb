#!/bin/env bash
#===================================================================================
# NAME: fetchDependencies.sh
#
# DESCRIPTION: The purpose of this script is to fetch all build dependencies
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
# 2025/01/30 DB  1.0.0 Initial Release
#===================================================================================

# Internal script to fetch
runFetchLogic() {

    # Read to go to fetch build dependencies configured in application descriptor
    if [ $rc -eq 0 ]; then
        echo $PGM": [INFO] **************************************************************"
        echo $PGM": [INFO] ** Start Fetch Build Dependencies on HOST/USER: ${SYS}/${USER}"
        echo $PGM": [INFO] **                     WorkDir:" $(getWorkDirectory)
        echo $PGM": [INFO] **                 Application:" ${App}
        echo $PGM": [INFO] **                      Branch:" ${Branch}
        echo $PGM": [INFO] **     Application Descriptor :" ${applicationDescriptor}
        echo $PGM": [INFO] **          Use Package Cache :" ${enablePackageCache}
        echo $PGM": [INFO] **     Archive Cache Location :" ${archiveCache}
        echo $PGM": [INFO] **    External Dependency Log :" ${externalDependenciesLogFile}
        echo $PGM": [INFO] **************************************************************"
        echo ""
    fi
    #

    # Create import dir
    if [ $rc -eq 0 ]; then
        if [ ! -d "$(getWorkDirectory)/imports" ]; then
            mkdir -p $(getWorkDirectory)/imports
        fi
    fi

    # Fetch Application Dependencies
    if [ $rc -eq 0 ]; then
        echo $PGM": [INFO] ** Fetch Application Dependencies from Artifact Repository"
        cmd="groovyz ${PIPELINE_SCRIPTS}/utilities/fetchBuildDependencies.groovy -w $(getWorkDirectory) -a ${applicationDescriptor} -p ${pipelineConfiguration} -b ${Branch}"
        #
        if [ ! -z "${externalDependenciesLogFile}" ]; then
            cmd="${cmd} -d ${externalDependenciesLogFile}"
        fi

        if [ ! -z "${archiveCache}" ]; then
            cmd="${cmd} -c ${archiveCache}"
        fi
        echo $PGM": [INFO] ** CMD : ${cmd}"
        ${cmd}
        rc=$?
    fi

    if [ $rc -eq 0 ]; then
        ERRMSG=$PGM": [INFO] Fetch Build Dependencies Completed. rc="$rc
        echo $ERRMSG
        echo ""
    fi

}

#
# Script Logic
#

# extracting external dependencies is based on the application descriptor
applicationDescriptor="$(getApplicationDir)/applicationDescriptor.yml"

# this log file documents the "fetched" dependencies and their version, that is then stored in the package itself (WD application manifest)
if [ ! -z "${externalDependenciesLogName}" ]; then
    externalDependenciesLogFile="$(getLogDir)/${externalDependenciesLogName}"
fi

# create the log dir
mkdir -p "$(getLogDir)"

# Set up to perform the clone of the Repo
if [ ! -f "${applicationDescriptor}" ]; then
    ERRMSG=$PGM": [INFO] Application Descriptor file (${applicationDescriptor}) was not found. rc="
    echo $ERRMSG
else
    runFetchLogic
fi
