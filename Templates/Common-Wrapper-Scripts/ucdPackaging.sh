#!/bin/env bash
#===================================================================================
# NAME: ucdPackaging.sh
#
# DESCRIPTION: The purpose of this scipt is to execute the UCD Buztool to publish
# the artifacts created by the DBB Build from within an Azure Pipeline.
#
# USAGE: ucdPackaging.sh UcdVers UcdComp WorkDir ExtRepoProp PkgPropFile [PipelineUrl] [GitBranch] [PrUrl]
#
# SYNTAX: See Help() section below
#
# RETURNS:
#
#    rc         - Return Code
#
# RETURN CODES:
#
#    0          - Successful
#    4          - Warning message(s) issued.  See Console messages
#    8          - Error encountered.  See Console messages
#
# NOTE(S):
#
#   1. Review and update the Customization Section to reference the
#        central configuration file pipelineBackend.config
#
#   2. Review the Customization Section in the pipelineBackend.config file :
#
#        PackagingScript  - Location of the dbb-ucd-packaging.groovy
#        BuzTool          - Location of the UCD buztool.sh
#
#===================================================================================
Help() {
    echo $PGM" - Invoke DBB Build ("$PGMVERS")                          "
    echo "                                                              "
    echo "Description: The purpose of this scipt is to execute          "
    echo "the UCD Buztool to publish the artifacts created by           "
    echo "the DBB Build from within an Azure Pipeline.                  "
    echo "                                                              "
    echo "Syntax:                                                       "
    echo "                                                              "
    echo "       "$PGM" [Options]                                       "
    echo "                                                              "
    echo "Options:                                                      "
    echo "                                                              "
    echo "       -h                  - Display this Help.               "
    echo "                                                              "
    echo "       -v <ucdVersion>     - The name of the version          "
    echo "                             to create.                       "
    echo "                             Default = None, Required.        "
    echo "                                                              "
    echo "                Ex: Azure Build ID (Build.buildid)            "
    echo "                                                              "
    echo "       -c <ucdComponent>   - The Component Name in            "
    echo "                             IBM UrbanCode Deploy.            "
    echo "                                                              "
    echo "                Ex: Azure Build ID (Build.buildid)            "
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
    echo "       -e <extRepoProp>    - Path to the external artifact    "
    echo "                             repository file for buztool.sh.  "
    echo "                             Default = None, Required.        "
    echo "                                                              "
    echo "       -f <pkgPropFile>    - Path to a properties file for    "
    echo "                             additional configuring of the    "
    echo "                             dbb-ucd-packaging script.        "
    echo "                             Default = None, Required.        "
    echo "                                                              "
    echo "       -u <pipeLineUrl>    - URL to the pipeline to           "
    echo "                             establish link to pipeline       "
    echo "                             build result.                    "
    echo "                                                              "
    echo "       -b <gitBranch>      - Name of the git branch.          "
    echo "                                                              "
    echo "                 Ex: main                                     "
    echo "                                                              "
    echo "       -p <pullRequestURL> - URL to the Pull Request          "
    echo "                                                              "
    echo "                                                              "
    exit 0
}

#
# Customization
# Configuration file leveraged by the backend scripts
# Either an absolute path or a relative path to the current working directory
SCRIPT_HOME="`dirname "$0"`"
pipelineConfiguration="${SCRIPT_HOME}/pipelineBackend.config"
# Customization - End

# Path and File Name to the advanced debug options.
#log4j2="-Dlog4j.configurationFile=file:/../log4j2.properties"

#
# Internal Variables
#set -x                  # Uncomment to enable shell script debug
#export BASH_XTRACEFD=1  # Write set -x trace to file descriptor

PGM=$(basename "$0")
PGMVERS="1.00"
USER=$(whoami)
SYS=$(uname -Ia)

#
# Set initialization
#
rc=0
ERRMSG=""
UcdVers=""
UcdComp=""
Workspace=""
ExtRepoProp=""
PkgPropFile=""
PipelineUrl=""
Branch=""
PrUrl=""
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
    echo $PGM": [INFO] UCD Packaging Wrapper. Version="$PGMVERS
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
    while getopts "h:v:c:w:e:f:u:b:p:" opt; do
        case $opt in
        h)
            Help
            ;;
        v)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] The name of the version to create is required. rc="$rc
                echo $ERRMSG
                break
            fi
            UcdVers="$argument"
            ;;
        c)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] The Component Name in IBM UrbanCode Deploy is required. rc="$rc
                echo $ERRMSG
                break
            fi
            UcdComp="$argument"
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
        e)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] Path to the external artifact repository file for buztool.sh is required. rc="$rc
                echo $ERRMSG
                break
            fi
            ExtRepoProp="$argument"
            ;;
        f)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] Path to a properties file for additional configuring is required. rc="$rc
                echo $ERRMSG
                break
            fi
            PkgPropFile="$argument"
            ;;
        u)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] URL to the pipeline is required. rc="$rc
                echo $ERRMSG
                break
            fi
            PipelineUrl="$argument"
            ;;
        b)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] Name of the git branch is required. rc="$rc
                echo $ERRMSG
                break
            fi
            Branch="$argument"
            ;;
        p)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] URL to the Pull Request is required. rc="$rc
                echo $ERRMSG
                break
            fi
            PrUrl="$argument"
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
# Validate options
validateOptions() {
    if [ -z "${Workspace}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Unique Workspace parameter (-w) is required. rc="$rc
        echo $ERRMSG
    else

        # Compute the logDir parameter
        logDir=$(getLogDir)

        if [ ! -d "$logDir" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Workspace Directory ($logDir) was not found. rc="$rc
            echo $ERRMSG
        fi
    fi

    # Validate Packaging script
    if [ ! -f "${PackagingScript}" ]; then
        rc=8
        ERRMSG=$PGM": [ERR] Unable to locate ${PackagingScript}. rc="$rc
        echo $ERRMSG
    fi

    # Validate BuzTool

    if [ ! -f "${BuzTool}" ]; then
        rc=8
        ERRMSG=$PGM": [ERR] Unable to locate ${BuzTool}. rc="$rc
        echo $ERRMSG
    fi

    # Validate required parameters
    if [ -z "${UcdVers}" ]; then
        rc=8
        ERRMSG=$PGM": [ERR] UCD Version parameter (UcdVers) is required. rc="$rc
        echo $ERRMSG
    fi

    if [ -z "${UcdComp}" ]; then
        rc=8
        ERRMSG=$PGM": [ERR] UCD Component parameter (UcdComp) is required. rc="$rc
        echo $ERRMSG
    fi

    if [ ! -z "${ExtRepoProp}" ]; then
        if [ ! -f "${ExtRepoProp}" ]; then
            rc=8
            ERRMSG=$PGM": [ERR] Unable to locate ${ExtRepoProp}. rc="$rc
            echo $ERRMSG
        fi
    fi

    if [ ! -z "${PkgPropFile}" ]; then
        if [ ! -f "${PkgPropFile}" ]; then
            rc=8
            ERRMSG=$PGM": [ERR] Unable to locate ${PkgPropFile}. rc="$rc
            echo $ERRMSG
        fi
    fi

    # for the optional parameters, we can skip the evaluation, if they are not defined

    #if [ -z "${PipelineUrl}" ]; then
    #    rc=8
    #    ERRMSG=$PGM": [ERR] Pipeline URL is required. Specify NONE to skip passing the option to dbb-ucd-packaging.groovy. rc="$rc
    #    echo $ERRMSG
    #fi

    #if [ -z "${Branch}" ]; then
    #    rc=8
    #    ERRMSG=$PGM": [ERR] Git Branch name of the source of the package. Specify NONE to skip passing the option to dbb-ucd-packaging.groovy. rc="$rc
    #    echo $ERRMSG
    #fi

    #if [ -z "${PrUrl}" ]; then
    #    rc=8
    #    ERRMSG=$PGM": [ERR] Pull Request URL. Specify NONE to skip passing the option to dbb-ucd-packaging.groovy. rc="$rc
    #    echo $ERRMSG
    #fi

}

# Call validate Options
if [ $rc -eq 0 ]; then
    validateOptions
fi

# #
# # Fetch the Artifacts.
# if [ $rc -eq 0 ]; then

#     # Locate the most recent Build Artifact Directory within WorkDir. The Build Artifacts Directories
#     # are Time Stamped. Therefore, the last directory entry will be the most recent Build Artifacts.
#     Artifacts=$(ls -d $WorkDir/logs/build*)
#     rc=$?

#     if [ $rc -ne 0 ]; then
#         ERRMSG=$PGM": [ERR] Unable to locate the last build artifacts. rc="$rc
#         echo $ERRMSG
#         rc=8
#     fi
# fi

#
# Ready to go
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] **************************************************************"
    echo $PGM": [INFO] ** Started - UCD Publish on HOST/USER: ${SYS}/${USER}"
    echo $PGM": [INFO] **                 WorkDir:" ${WorkDir}
    echo $PGM": [INFO] **             UCD Version:" ${UcdVers}
    echo $PGM": [INFO] **           UCD Component:" ${UcdComp}
    echo $PGM": [INFO] **      Artifacts Location:" ${logDir}
    echo $PGM": [INFO] **    PackagingScript Path:" ${PackagingScript}
    echo $PGM": [INFO] **            BuzTool Path:" ${BuzTool}
    echo $PGM": [INFO] ** External Repository cfg:" ${ExtRepoProp}
    echo $PGM": [INFO] **    Packaging properties:" ${PkgPropFile}
    echo $PGM": [INFO] **            Pipeline URL:" ${PipelineUrl}
    echo $PGM": [INFO] **         Git branch name:" ${Branch}
    echo $PGM": [INFO] **        Pull Request URL:" ${PrUrl}
    echo $PGM": [INFO] **                DBB_HOME:" ${DBB_HOME}
    echo $PGM": [INFO] **************************************************************"
    echo ""
fi

#
# Invoke the DBB UCD Packaging
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] Invoking the DBB UCD Packaging script."

    CMD="groovyz ${log4j2} ${PackagingScript} --buztool ${BuzTool} --workDir ${logDir} --component ${UcdComp} --versionName ${UcdVers}"

    # external repository file
    if [ ! -z "${ExtRepoProp}" ]; then
        CMD="${CMD} --propertyFile ${ExtRepoProp}"
    fi

    # packaging properties file
    if [ ! -z "${PkgPropFile}" ]; then
        CMD="${CMD} --packagingPropFiles ${PkgPropFile}"
    fi

    # add optional pipeline url
    if [ ! -z "${PipelineUrl}" ]; then
        CMD="${CMD} --pipelineURL ${PipelineUrl}"
    fi

    # add optional git branch name
    if [ ! -z "${GitBranch}" ]; then
        CMD="${CMD} --gitBranch ${GitBranch}"
    fi

    # add optional pull request URL
    if [ ! -z "${PrUrl}" ]; then
        CMD="${CMD} --pullRequestURL ${PrUrl}"
    fi

    echo $PGM": [INFO] ${CMD}"
    ${CMD}
    rc=$?

    if [ $rc -eq 0 ]; then
        ERRMSG=$PGM": [INFO] DBB UCD Packaging Complete. rc="$rc
        echo $ERRMSG
    else
        ERRMSG=$PGM": [ERR] DBB UCD Packaging Error. Check Console for details. rc="$rc
        echo $ERRMSG
        rc=12
    fi
fi

exit $rc
