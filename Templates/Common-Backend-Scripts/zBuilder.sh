#!/bin/env bash
#
#===================================================================================
# NAME: zBuilder.sh
#
# DESCRIPTION: The purpose of this script is to perform a DBB Build from within
# a Pipeline using the zBuilder framework.
#
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
#   1. Review the pipeline backend configuration file
#      (pipelineBackend.config) which sets general as well as
#      Dependency Based Build values that must be supplied.
#
#   2. This script contains processing logic that is specific to the branch being
#      processed for a pre-defined set of branch names. This processing will
#      determine the following build options values:
#
#        Build lifecycle  - The Lifecycle of build to perform. Refer to zBuilder
#                           documentation and implementation
#
#        HLQ         - High Level data set qualifier used DBB build output.
#
#      Review the "Build Lifecycle Customization" section below for additional information.
#
#   3. This script supports Dependency Base Build V3. and above.
#
# Maintenance Log
#
# Date       Who Vers  Description
# ---------- --- ----- --------------------------------------------------------------
# 2024/12/12 DB  1.0.0 Initial Release
#===================================================================================
Help() {
    echo $PGM" - Invoke DBB zBuilder ("$PGMVERS")                       "
    echo "                                                              "
    echo "Description: The purpose of this script is to                 "
    echo "invoke the central build framework.                           "
    echo "                                                              "
    echo "Syntax:                                                       "
    echo "                                                              "
    echo "       "$PGM" [Options]                                       "
    echo "                                                              "
    echo "Options:                                                      "
    echo "                                                              "
    echo "       -h                  - Display this Help.               "
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
    echo "                                                              "
    echo "       -a <Application>    - Folder name to the DBB Main      "
    echo "                             Application Source located under "
    echo "                             <Workspace>/<Application>.       "
    echo "                                                              "
    echo "                 Ex: MortgageApplication                      "
    echo "                                                              "
    echo "                                                              "
    echo "       -b <Branch>        - Name of the Branch to compute     "
    echo "                            the build dataset qualifier.      "
    echo "                            Default=None, Required.           "
    echo "                                                              "
    echo "                 Ex: main                                     "
    echo "                                                              "
    echo "                                                              "
    echo "       -p <PipelineType>  - Type of the pipeline to           "
    echo "                            control if this builds with       "
    echo "                            optimize or test options          "
    echo "                            also impacts computed HLQ         "
    echo "                            Default=build, Required.          "
    echo "                            Accepted values:                  "
    echo "                            *build* -                         "
    echo "                             perform build with TEST options  "
    echo "                            *release* -                       "
    echo "                             perform build with options for   "
    echo "                             performance optimized            "
    echo "                             executables                      "
    echo "                            *preview* -                       "
    echo "                             perform build build in           "
    echo "                             preview mode                     "
    echo "                                                              "
    echo "                 Ex: build                                    "
    echo "                                                              "
    echo "       -q                 - (Optional) dataset qualifier      "
    echo "                             for build datasets               "
    echo "                             to override the configured       "
    echo "                             dataset qualifer in              "
    echo "                             pipelineBackend.config           "
    echo "                 Ex: USER.APP                                 "
    echo "                                                              "
    echo "       -t                 - (Optional) zBuilder build         "
    echo "                             lifecycle  override              "
    echo "                                                              "
    echo "                 Ex: -t 'full'                                "
    echo "                                                              "
    echo "       -v                 - (Optional) Verbose tracing        "
    echo "                                                              "
    echo "                                                              "
    exit 0

}

#
# Configuration file leveraged by the backend scripts
# Either an absolute path or a relative path to the current working directory
SCRIPT_HOME="$(dirname "$0")"
pipelineConfiguration="${SCRIPT_HOME}/pipelineBackend.config"
buildUtilities="${SCRIPT_HOME}/utilities/dbbzBuilderUtils.sh"


#
# Internal Variables
#set -x                  # Uncomment to enable shell script debug
#export BASH_XTRACEFD=1  # Write set -x trace to file descriptor

PGM=$(basename "$0")
PGMVERS="1.00"
USER=$USER
SYS=$(uname -Ia)

rc=0
ERRMSG=""

# Initialized option variables passed to this script
Workspace=""
App=""
Branch=""
PipelineType=""
HELP=$1

# Local Variables
AppDir=""                  # Derived Application Directory
HLQ=""                     # Derived High Level Qualifier
HLQPrefix=""               # Prefix of HLQ, either specified via the cli option -q or via configuration file
outDir=""                  # Computed output directory to store build protocols
nestedApplicationFolder="" # Flag to understand a nested repository
Lifecycle=""               # Derived zBuilder lifecycle type
userDefinedLifecycle=""    # Flag if the user has provided the lifecycle as argument 
baselineRef=""             # baselineReference that is computed by utilities/dbbzBuilderUtils.sh
zBuilderConfigOverrides="" # Override of default build config for zBuilder
zBuilderLogDir=""          # Path where zBuilder will store the logs
buildListFile=""           # Location of the generate zBuilder buildList
buildlistsize=0            # Used to assess if files got built

DBBLogger=""
zAppBuildVerbose=""
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
    echo $PGM": [INFO] DBB zBuilder Wrapper. Version="$PGMVERS
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

    # Read and import utilities
    if [ ! -f "${buildUtilities}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] DBB-Build internal utilities (${buildUtilities}) was not found. rc="$rc
        echo $ERRMSG
    else
        source $buildUtilities
    fi

    #
    # Get Options
    if [ $rc -eq 0 ]; then
        while getopts "h:w:a:b:q:p:t:v" opt; do
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
                    ERRMSG=$PGM": [WARNING] Application Folder Name is required. rc="$rc
                    echo $ERRMSG
                    break
                fi
                App="$argument"
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
            t)
                argument="$OPTARG"
                # stop if the argument is -[a-z]
                check="^-[a-z]"
                if [ -z "$argument" ] || [[ "$argument" =~ $check ]]; then
                    rc=4
                    ERRMSG=$PGM": [WARNING] Build Lifecycle/configuration is required. rc="$rc
                    echo $ERRMSG
                    break
                fi
                Lifecycle="$argument"
                userDefinedLifecycle=1 # set flag
                ;;
            p)
                argument="$OPTARG"
                nextchar="$(expr substr $argument 1 1)"
                if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                    rc=4
                    INFO=$PGM": [INFO] No Pipeline Lifecycle specified. rc="$rc
                    echo $INFO
                    break
                fi
                PipelineType="$argument"
                ;;
            v)
                # set the pipeline flag to turn on zAppBuild tracing
                Verbose=1
                ;;
            q)
                # dataset qualifier
                argument="$OPTARG"
                nextchar="$(expr substr $argument 1 1)"
                if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                    rc=4
                    INFO=$PGM": [WARNING] Dataset qualifier is required (-q). rc="$rc
                    echo $INFO
                    break
                fi
                HLQPrefix="$argument"
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

        if [ ! -d "${outDir}" ]; then
            mkdir -p $outDir
            ERRMSG=$PGM": [INFO] Created Pipeline Log directory (${outDir}). rc="$rc
            echo $ERRMSG
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
            echo $PGM": [INFO] Detected the application respository (${App}) within the git repository layout structure."
            echo $PGM": [INFO]  Assuming this as the new application location."
            AppDir="${AppDir}/${App}"
            nestedApplicationFolder="true"
        fi

        if [ ! -d "${AppDir}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Application Directory (${AppDir}) was not found. rc="$rc
            echo $ERRMSG
        fi

        # Compute the outDir parameter
        zBuilderLogDir="${AppDir}/logs" # zBuilder convention
    fi

    if [ -z "${Branch}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Branch Name parameter (-b) is required. rc="$rc
        echo $ERRMSG
    fi

    tmp1=$(echo $PipelineType | tr '[:upper:]' '[:lower:]')

    case $tmp1 in
    "build") ;;
    "release") ;;
    "preview") ;;
    *)
        rc=0
        PipelineType="build"
        ERRMSG=$PGM": [INFO] Set default Pipeline Type : ${PipelineType} ."
        echo $ERRMSG
        ;;
    esac

    if [ -z "${dbbMetadataStoreJdbcId}" ]; then
        echo $PGM": [WARNING] Db2 JDBC User not set. It is recommended to use Db2 for the DBB Metadatastore."
    fi

    if [ -z "${dbbMetadataStoreJdbcPwdFile}" ]; then
        echo $PGM": [WARNING] Db2 JDBC Password file not set. It is recommended to use Db2 for the DBB Metadatastore."
    else
        if [ ! -f "${dbbMetadataStoreJdbcPwdFile}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] DBB database password file (${dbbMetadataStoreJdbcPwdFile}) was not found. rc="$rc
            echo $ERRMSG
        fi
    fi

    if [ -z "${DBB_BUILD}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Required environment variable DBB_BUILD is not set. rc="$rc
        echo $ERRMSG
    fi

    if [ ! -d "${DBB_BUILD}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] DBB_BUILD environment variable pointing to (${DBB_BUILD}), but cannot be found. rc="$rc
        echo $ERRMSG
    fi

    #
    # Init override file
    #
    zBuilderConfigOverrides=${outDir}/overrides.yaml

    #
    # Check to see if debug options were requested and set up switches
    # to enable.

    if [ ${LoggerConfig} -eq 1 ]; then
        DBBLogger="-classpath ${AppDir}/application-conf"
    fi

}

# Call validate Options
if [ $rc -eq 0 ]; then
    validateOptions
fi

# Call utilities to compute build Lifecycle based on pipeline
if [ $rc -eq 0 ]; then
    ##DEBUG## echo $PGM": [DEBUG] **************************************************************"
    ##DEBUG## echo $PGM": [DEBUG] ** Started - ComputeBuildConfiguration"

    # Note : Error-handling within the utlities
    computeBuildConfiguration
fi

# Validate hlq length
if [ $rc -eq 0 ]; then
    if [ ${#HLQ} -gt 35 ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] High-Level Qualifier (${HLQ}) exceeds maximum length of 35. rc="$rc
        echo $ERRMSG
    fi
fi

# Ready to go: Suggest in the section to echo as much as possible
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] **************************************************************"
    echo $PGM": [INFO] ** Started - DBB Build on HOST/USER: ${SYS}/${USER}"
    echo $PGM": [INFO] **          Workspace:" $(getWorkDirectory)
    echo $PGM": [INFO] **        Application:" ${App}
    echo $PGM": [INFO] **             Branch:" ${Branch}
    echo $PGM": [INFO] **      Pipeline Type:" ${PipelineType}
    echo $PGM": [INFO] **    Build Lifecycle:" ${Lifecycle}
    if [ -f "${zBuilderConfigOverrides}" ]; then
        echo $PGM": [INFO] **   Config Override :" ${zBuilderConfigOverrides}
    fi
    echo $PGM": [INFO] **                HLQ:" ${HLQ}
    echo $PGM": [INFO] **             AppDir:" ${AppDir}
    echo $PGM": [INFO] **      zBuilder Path:" ${DBB_BUILD}
    echo $PGM": [INFO] **      zBuilder Logs:" ${zBuilderLogDir}
    echo $PGM": [INFO] **           DBB_HOME:" ${DBB_HOME}
    echo $PGM": [INFO] **      DBB JDBC USER:" ${dbbMetadataStoreJdbcId}
    echo $PGM": [INFO] **  DBB JDBC Pwd File:" ${dbbMetadataStoreJdbcPwdFile}
    if [ ! -z "${dbbMetadataStoreJdbcUrl}" ]; then
        echo $PGM": [INFO] **      DBB JDBC Url :" ${dbbMetadataStoreJdbcUrl}
    fi
    if [ ${LoggerConfig} -eq 1 ]; then
        echo $PGM": [INFO] **         DBB Logger:" ${DBBLogger}
    else
        echo $PGM": [INFO] **         DBB Logger: No"
    fi
    echo $PGM": [INFO] **   Pipeline Log Dir:" ${outDir}
    echo $PGM": [INFO] **************************************************************"
    echo ""
fi

# TLD - This is where I left off.  The below was taken directly from the Azure Script copy.
# More updates will need to be performed to support DBB 2.x.  The following code segments
# where derived directly from the Azure version of this script.

#
# Invoke the DBB Build
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] Invoking the zBuilder Build Framework."

    # Assemble build command
    cd ${AppDir}
    # --outDir ${outDir}
    CMD="$DBB_HOME/bin/dbb build ${Lifecycle} --hlq ${HLQ} --log-encoding UTF-8"
    if [ ! -z "${dbbMetadataStoreJdbcId}" ]; then
        CMD="${CMD} --dbid ${dbbMetadataStoreJdbcId}" # Appending JDBC User Id if defined
    fi
    if [ ! -z "${dbbMetadataStoreJdbcPwdFile}" ]; then
        CMD="${CMD} --dbpf ${dbbMetadataStoreJdbcPwdFile}" # Appending JDBC Password file if defined
    fi
    if [ -f "${zBuilderConfigOverrides}" ]; then
        CMD="${CMD} --config ${zBuilderConfigOverrides}" # Appending Config Override file if created
    fi

    echo $PGM": [INFO] ${CMD}"
    ${CMD} # run build
    rc=$?

    if [ $rc -eq 0 ]; then
        cp ${zBuilderLogDir}/* ${outDir}
        ERRMSG=$PGM": [INFO] Copied build logs from ${zBuilderLogDir} to ${outDir}. rc="$rc
        echo $ERRMSG
    fi

    if [ $rc -eq 0 ]; then

        ## Except for the reset mode, check for "nothing to build" condition and throw an error to stop pipeline
        if [ "$Lifecycle" != "reset" ]; then

            # Locate buildList in Build Log Directory within outDir.
            buildListFile="${outDir}/buildList.txt"

            # If "buildList.txt" was found in the last Build Log Directory, determine the character count.
            # wc -c will return the two values; Character Count and Log File Path.  Parse out the Character Count.
            if [ -f ${buildListFile} ]; then
                set $(wc -c <${buildListFile})
                buildlistsize=$1
            else
                buildlistsize=0
            fi

            if [ $buildlistsize = 0 ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] DBB Build Error. No files on build list. rc="$rc
                echo $ERRMSG
            else
                ERRMSG=$PGM": [INFO] DBB Build Complete. rc="$rc
                echo $ERRMSG
            fi
        else
            ERRMSG=$PGM": [INFO] DBB Reset Complete. rc="$rc
            echo $ERRMSG
        fi
    else
        ERRMSG=$PGM": [ERROR] DBB Build Error. Check Console and Build Log for details. rc="$rc
        echo $ERRMSG
        rc=8
    fi
fi

exit $rc
