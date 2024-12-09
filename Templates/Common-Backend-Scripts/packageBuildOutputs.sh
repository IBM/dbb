#!/bin/env bash
#===================================================================================
# NAME: packageBuildOutputs.sh
#
# DESCRIPTION: The purpose of this script is to execute the
# PackageBuildOutputs scripts to store build outputs as a tar file and
# optionally upload it to an Artifact repository.
#
# SYNTAX: See Help() section below for usage
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
#        PackagingScript  - Location of the PackageBuildOutputs.groovy
#
#===================================================================================
Help() {
    echo $PGM" - Invoke Package Build Outputs ("$PGMVERS")              "
    echo "                                                              "
    echo "DESCRIPTION: The purpose of this script is to execute the     "
    echo "PackageBuildOutputs scripts to store build outputs            "
    echo "as a tar file and optionally upload                           "
    echo "it to an Artifact repository.                                 "
    echo "                                                              "
    echo "Syntax:                                                       "
    echo "                                                              "
    echo "       "$PGM" [Options]                                       "
    echo "                                                              "
    echo "Options:                                                      "
    echo "                                                              "
    echo "  Mandatory parameters                                        "
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
    echo "  Optional parameters                                         "
    echo "                                                              "
    echo "       -t <tarFileName>     - Name of the package tar file    "
    echo "                              (Optional)                      "
    echo "                                                              "
    echo "                Ex: package.tar                               "
    echo "                                                              "
    echo "       -u                   - flag to enable uploading        "
    echo "                             outputs to configured            "
    echo "                             artifact repository              "
    echo "                                                              "
    echo "       -a <Application>    - Application name                 "
    echo "                             Used to compute                  "
    echo "                             Artifact repository name.        "
    echo "                                                              "
    echo "                 Ex: MortgageApplication                      "
    echo "                                                              "
    echo "       -p <pipelineType>  - Type of the pipeline to           "
    echo "                            control in which directory builds "
    echo "                            are stored in the artifact repo   "
    echo "                            Accepted values:                  "
    echo "                            build -                           "
    echo "                             development builds               "
    echo "                            release -                         "
    echo "                             builds with options for          "
    echo "                             performance optimized            "
    echo "                             executables for production env   "
    echo "                                                              "
    echo "       -b <gitBranch>      - Name of the git branch.          "
    echo "                                                              "
    echo "                 Ex: main                                     "
    echo "                                                              "
    echo "       -v <artifactVersion>                                   "
    echo "                           - Name of the artifactVersion      "
    echo "                             within the artifact repository   "
    echo "                             Default=None,                    "
    echo "                             Required, when upload=true       "
    echo "                                                              "
    echo "                Ex: Pipeline Build ID (Build.buildid.tar)     "
    echo "                                                              "
    echo "       -s "<sbomAuthor>"    - Name and email of               "
    echo "                              the SBOM author                 "
    echo "                              enclosed with double quotes     "
    echo "                              (Optional)                      "
    echo "                                                              "
    echo "                 Ex: \"Build Engineer <engineer@example.com>\"  "
    echo "                                                              "
    echo "       -h                  - Display this Help.               "
    echo "                                                              "
    exit 0
}

# Customization
# Central configuration file leveraged by the backend scripts
SCRIPT_HOME="$(dirname "$0")"
pipelineConfiguration="${SCRIPT_HOME}/pipelineBackend.config"
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
Workspace=""
App=""
tarFileName=""
PkgPropFile=""
PipelineType=""
Branch=""

addExtension=""

generateSBOM=""
sbomAuthor=""

publish=""
artifactVersionName=""            # required for publishing to artifact repo
artifactRepositoryUrl=""          # required if artifactRepositoryPropertyFile not specified
artifactRepositoryUser=""         # required if artifactRepositoryPropertyFile not specified
artifactRepositoryPassword=""     # required if artifactRepositoryPropertyFile not specified
artifactRepositoryName=""         # required if artifactRepositoryPropertyFile not specified
artifactRepositoryDirectory=""    # required if artifactRepositoryPropertyFile not specified
artifactRepositoryPropertyFile="" # alternative to above cli parms
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

# Script label
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] Package Build Outputs wrapper. Version="$PGMVERS
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
    while getopts ":h:w:a:t:b:v:p:us:" opt; do
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
        t)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] The name of the version to create is required. rc="$rc
                echo $ERRMSG
                break
            fi
            tarFileName="$argument"
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
        u)
            publish="true"
            ;;
        s)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] SBOM Author is required. rc="$rc
                echo $ERRMSG
                break
            fi
            generateSBOM="true"
            sbomAuthor="$argument"
            ;;
        p)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                INFO=$PGM": [INFO] No Pipeline type specified. rc="$rc
                echo $INFO
                break
            fi
            PipelineType="$argument"
            ;;
        v)
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] The name of the artifact version in Artifact repository is required. rc="$rc
                echo $ERRMSG
                break
            fi
            artifactVersionName="$argument"
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
            ERRMSG=$PGM": [ERROR] Build Log Directory ($logDir) was not found. rc="$rc
            echo $ERRMSG
        fi
    fi

    # Validate Packaging script
    if [ ! -f "${PackagingScript}" ]; then
        rc=8
        ERRMSG=$PGM": [ERR] Unable to locate ${PackagingScript}. rc="$rc
        echo $ERRMSG
    fi

    # Validate Properties file
    if [ ! -z "${PkgPropFile}" ]; then
        if [ ! -f "${PkgPropFile}" ]; then
            rc=8
            ERRMSG=$PGM": [ERR] Unable to locate ${PkgPropFile}. rc="$rc
            echo $ERRMSG
        fi
    fi

}

# function to validate publishing options
validatePublishingOptions() {

    if [ -z "${App}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Application parameter (-a) is required. rc="$rc
        echo $ERRMSG
    fi

    if [ -z "${Branch}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Branch Name parameter (-b) is required. rc="$rc
        echo $ERRMSG
    fi

    if [ ! -z "${artifactRepositoryPropertyFile}" ]; then
        if [ ! -f "${artifactRepositoryPropertyFile}" ]; then
            rc=8
            ERRMSG=$PGM": [ERR] Unable to locate ${artifactRepositoryPropertyFile}. rc="$rc
            echo $ERRMSG
        fi
    else

        if [ -z "${artifactVersionName}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Name of the artifact version (artifactVersionName) is required. rc="$rc
            echo $ERRMSG
        fi

        if [ -z "${artifactRepositoryUrl}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] URL to artifact repository (artifactRepositoryUrl) is required. rc="$rc
            echo $ERRMSG
        fi

        if [ -z "${artifactRepositoryUser}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] User to connect to the Artifact repository server (artifactRepositoryUser) is required. rc="$rc
            echo $ERRMSG
        fi

        if [ -z "${artifactRepositoryPassword}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Password to connect to the Artifact repository server (artifactRepositoryPassword) is required. rc="$rc
            echo $ERRMSG
        fi

        if [ -z "${artifactRepositoryName}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] artifact repository name to store the build (artifactRepositoryName) is required. rc="$rc
            echo $ERRMSG
        fi

        if [ -z "${artifactRepositoryDirectory}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Directory path in the repository to store the build (artifactRepositoryDirectory) is required. rc="$rc
            echo $ERRMSG
        fi

        # If pipeline type is specified, evaluate the value
        if [ ! -z "${PipelineType}" ]; then
            tmp1=$(echo $PipelineType | tr '[:upper:]' '[:lower:]')

            case $tmp1 in
            "build")
                PipelineType=$tmp1
                ;;
            "release")
                PipelineType=$tmp1
                ;;
            "preview")
                rc=4
                ERRMSG=$PGM": [WARN] Default Pipeline Type : ${PipelineType} not supported for packaging."
                echo $ERRMSG
                ;;
            *)
                rc=4
                ERRMSG=$PGM": [WARN] Inavlid Pipeline Type : ${PipelineType} specified."
                echo $ERRMSG
                ;;
            esac
        fi

    fi
}

# Call validate input options
if [ $rc -eq 0 ]; then
    validateOptions
fi

# compute parameters
if [ $rc -eq 0 ]; then
    # Compute artifactRepositoryName based on function in packageBuildOutputs.config
    artifactRepositoryName=$(computeArtifactRepositoryName)
    artifactRepositoryDirectory=$(computeArtifactRepositoryDirectory)

    # set default PipelineType for pipelines on main
    if [ "${Branch}" == "main" ] && [ -z "${PipelineType}" ]; then
        PipelineType="build"
    fi
fi

# Call validate publishing options
if [ $rc -eq 0 ] && [ "$publish" == "true" ]; then
    validatePublishingOptions
fi

#
# Ready to go
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] **************************************************************"
    echo $PGM": [INFO] ** Started - Package Build Outputs on HOST/USER: ${SYS}/${USER}"
    echo $PGM": [INFO] **                  WorkDir:" $(getWorkDirectory)
    if [ ! -z "${App}" ]; then
        echo $PGM": [INFO] **              Application:" ${App}
    fi
    if [ ! -z "${Branch}" ]; then
        echo $PGM": [INFO] **                   Branch:" ${Branch}
    fi
    if [ ! -z "${PipelineType}" ]; then
        echo $PGM": [INFO] **         Type of pipeline:" ${PipelineType}
    fi
    if [ ! -z "${tarFileName}" ]; then
        echo $PGM": [INFO] **            Tar file Name:" ${tarFileName}
    fi
    echo $PGM": [INFO] **     BuildReport Location:" ${logDir}
    echo $PGM": [INFO] **     PackagingScript Path:" ${PackagingScript}
    if [ ! -z "${PkgPropFile}" ]; then
        echo $PGM": [INFO] **     Packaging properties:" ${PkgPropFile}
    fi

    if [ ! -z "${artifactVersionName}" ]; then
        echo $PGM": [INFO] **            Artifact name:" ${artifactVersionName}
    fi

    echo $PGM": [INFO] ** Publish to Artifact Repo:" ${publish}
    if [ "$publish" == "true" ]; then
        if [ ! -z "${artifactRepositoryPropertyFile}" ]; then
            echo $PGM": [INFO] **  ArtifactRepo properties:" ${artifactRepositoryPropertyFile}
        fi

        if [ ! -z "${artifactRepositoryUrl}" ]; then
            echo $PGM": [INFO] **         ArtifactRepo Url:" ${artifactRepositoryUrl}
        fi
        if [ ! -z "${artifactRepositoryUser}" ]; then
            echo $PGM": [INFO] **        ArtifactRepo User:" ${artifactRepositoryUser}
        fi
        if [ ! -z "${artifactRepositoryPassword}" ]; then
            echo $PGM": [INFO] **    ArtifactRepo Password: xxxxx"
        fi
        if [ ! -z "${artifactRepositoryName}" ]; then
            echo $PGM": [INFO] **   ArtifactRepo Repo name:" ${artifactRepositoryName}
        fi
        if [ ! -z "${artifactRepositoryDirectory}" ]; then
            echo $PGM": [INFO] **    ArtifactRepo Repo Dir:" ${artifactRepositoryDirectory}
        fi
    fi
    echo $PGM": [INFO] **            Generate SBOM:" ${generateSBOM}
    if [ ! -z "${sbomAuthor}" ]; then
        echo $PGM": [INFO] **              SBOM Author:" ${sbomAuthor}
    fi
    echo $PGM": [INFO] **                 DBB_HOME:" ${DBB_HOME}
    echo $PGM": [INFO] **************************************************************"
    echo ""
fi

#
# Invoke the Package Build Outputs script
if [ $rc -eq 0 ]; then
    echo $PGM": [INFO] Invoking the Package Build Outputs script."
    
    if [ ! -z "${cycloneDXlibraries}" ]; then
    	cycloneDXlibraries="-cp ${cycloneDXlibraries}"
    fi

    CMD="$DBB_HOME/bin/groovyz ${log4j2} ${cycloneDXlibraries} ${PackagingScript} --workDir ${logDir}"

    # add tarfile name
    if [ ! -z "${tarFileName}" ]; then
        CMD="${CMD} --tarFileName ${tarFileName}"
    fi

    # application name
    if [ ! -z "${App}" ]; then
        CMD="${CMD} --application ${App}"
    fi

    # branch name
    if [ ! -z "${Branch}" ]; then
        CMD="${CMD} --branch ${Branch}"
    fi

    # packaging properties file
    if [ ! -z "${PkgPropFile}" ]; then
        CMD="${CMD} --packagingPropertiesFile ${PkgPropFile}"
    fi

    # addExtension
    if [ "$addExtension" == "true" ]; then
        CMD="${CMD} --addExtension"
    fi

    # artifactVersionName
    if [ ! -z "${artifactVersionName}" ]; then
        CMD="${CMD} --versionName ${artifactVersionName}"
    fi

    # publishing options
    if [ "$publish" == "true" ]; then
        CMD="${CMD} --publish"

        if [ ! -z "${artifactRepositoryUrl}" ]; then
            CMD="${CMD} --artifactRepositoryUrl \"${artifactRepositoryUrl}\""
        fi
        if [ ! -z "${artifactRepositoryPropertyFile}" ]; then
            CMD="${CMD} --artifactRepositoryPropertyFile ${artifactRepositoryPropertyFile}"
        fi

        if [ ! -z "${artifactRepositoryUser}" ]; then
            CMD="${CMD} --artifactRepositoryUser ${artifactRepositoryUser}"
        fi
        if [ ! -z "${artifactRepositoryPassword}" ]; then
            CMD="${CMD} --artifactRepositoryPassword ${artifactRepositoryPassword}"
        fi
        if [ ! -z "${artifactRepositoryName}" ]; then
            CMD="${CMD} --artifactRepositoryName ${artifactRepositoryName}"
        fi
        if [ ! -z "${artifactRepositoryDirectory}" ]; then
            CMD="${CMD} --artifactRepositoryDirectory ${artifactRepositoryDirectory}"
        fi
    fi

    # SBOM options
    if [ "$generateSBOM" == "true" ]; then
        CMD="${CMD} --sbom"
	    if [ ! -z "${sbomAuthor}" ]; then
	        CMD="${CMD} --sbomAuthor \"${sbomAuthor}\""
	    fi
    fi


    echo $PGM": [INFO] ${CMD}"
    /bin/env bash -c "${CMD}"
    rc=$?

    if [ $rc -eq 0 ]; then
        ERRMSG=$PGM": [INFO] Package Build Outputs Complete. rc="$rc
        echo $ERRMSG
    else
        ERRMSG=$PGM": [ERR] Package Build Outputs Failed. Check Console for details. rc="$rc
        echo $ERRMSG
        rc=12
    fi
fi

exit $rc
