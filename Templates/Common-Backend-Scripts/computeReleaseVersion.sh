#!/bin/env bash
#
#===================================================================================
# NAME: computeReleaseVersion.sh
#
# DESCRIPTION: The purpose of this script is to perform computation of the version
# for the next release. 
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
#   2. The naming convention of the release version is 'rel-x.y.z'
#      when: x increased for a major release
#            y increased for a minor release
#            z increased for a patch release
#
#===================================================================================
Help() {
    echo $PGM" - Invoke Release Version Computation ("$PGMVERS")              "
    echo "                                                              "
    echo "DESCRIPTION: The purpose of this script is to execute the     "
    echo "computeReleaseVersion scripts to compute the version          "
    echo "of the next release.                                          "
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
    echo "       -a <Application>    - Application name                 "
    echo "                             Used to compute                  "
    echo "                             Artifact repository name.        "
    echo "                                                              "
    echo "                 Ex: MortgageApplication                      "
    echo "                                                              "
    echo "       -b <gitBranch>      - Name of the git branch.          "
    echo "                                                              "
    echo "                 Ex: main                                     "
    echo "                                                              "
    echo "       -r <releaseType>    - Type of the release              "
    echo "                             to calculate the version.        "
    echo "                             Accepted values:                 "
    echo "                             - Major                          "
    echo "                             - Minor (Default)                "
    echo "                             - Patch                          "
    echo "                                                              "
    exit 0
}

# Customization
# Central configuration file leveraged by the backend scripts
SCRIPT_HOME="`dirname "$0"`"
pipelineConfiguration="${SCRIPT_HOME}/pipelineBackend.config"
# Customization - End

# internal veriables
PGM=$(basename "$0")
PGMVERS="1.10"
USER=$USER
SYS=$(uname -Ia)

rc=0
ERRMSG=""

# Initialized option variables passed to this script
Workspace=""
App=""
Branch=""
ReleaseType=""
HELP=$1

# Local Variables
# TLD: Always a good idea to initialize any local varables
AppDir=""        # Derived Application Directory
Type=""          # Derived Build Type
baselineRef=""   # baselineReference that is to be computed
mainBranchSegment=""
secondBranchSegment=""
thirdBranchSegment=""
baselineReferenceFile=""
newVersion=""
releaseVersion=""

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
  echo $PGM": [INFO] Release Version Wrapper. Version=${PGMVERS}"
fi


if [ $rc -eq 0 ]; then
# Read and import pipeline configuration
    if [ ! -f "${pipelineConfiguration}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Pipeline Configuration File (${pipelineConfiguration}) was not found. rc="$rc
        echo $ERRMSG
    else
        echo $PGM": [INFO] Reading pipeline configuration file: ${pipelineConfiguration}"
        source $pipelineConfiguration
    fi
#

# Get Options
    if [ $rc -eq 0 ]; then
        while getopts "h:w:a:b:r:" opt; do
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
            r) 
                # release type
                argument="$OPTARG"
                nextchar="$(expr substr $argument 1 1)"
                if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                  rc=4
                  INFO=$PGM": [WARNING] Release type is required (-r). rc="$rc
                  echo $INFO
                  break
                fi
                ReleaseType="$argument"
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

  echo $PGM": [INFO] Validating Options"
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
  fi

  if [ -z "${App}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Application parameter (-a) is required. rc="$rc
    echo $ERRMSG
  else

    AppDir=$(getApplicationDir)
    echo $PGM": [INFO] Application Directory: ${AppDir}"

    # Check if application directory contains
    if [ -d "${AppDir}" ]; then
      echo $PGM": [INFO] Detected the application respository (${App}) within the git repository layout structure."
      echo $PGM": [INFO]  Assuming this as the new application location."
      AppDir="${AppDir}"
      nestedApplicationFolder="true"
      
      # Locate the baseline reference file based on the baselineReferenceLocation config in pipelineBackend.config
      baselineReferenceFile="${AppDir}/$baselineReferenceLocation"
      if [ ! -f "${baselineReferenceFile}" ]; then
            echo [ERROR] Applications baseline reference configuration file ${baselineReferenceFile} was not found.
            exit 1
      fi

    fi

    if [ ! -d "${AppDir}" ]; then
      rc=8
      ERRMSG=$PGM": [ERROR] Application Directory (${AppDir}) was not found. rc="$rc
      echo $ERRMSG
    fi
  fi

  if [ -z "${Branch}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Branch Name parameter (-b) is required. rc="$rc
    echo $ERRMSG
  fi

  if [ -z "${ReleaseType}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Release Type parameter (-r) is required. Valid release types are 'major', 'minor' or 'patch'. rc="$rc
    echo $ERRMSG
  fi

}
#

# Get baseline reference version from current branch
getBaselineReference() {

    baselineRef=""
    
    case $(echo $mainBranchSegment | tr '[:lower:]' '[:upper:]') in
        "RELEASE" | "EPIC")
            baselineRef=$(cat "${baselineReferenceFile}" | grep "^${mainBranchSegment}/${secondBranchSegment}" | awk -F "=" ' { print $2 }')
         ;;
        "MAIN")
            baselineRef=$(cat "${baselineReferenceFile}" | grep "^${mainBranchSegment}" | awk -F "=" ' { print $2 }') 
         ;;
         "FEATURE")
            rc=4
            ERRMSG=$PGM": [ERROR] Branch name ${Branch} is a feature branch and does not need to compute the baseline reference. rc="$rc
            echo $ERRMSG
         ;;
        *)
            rc=8
            ERRMSG=$PGM": [ERROR] Branch name ${Branch} does not follow the recommended naming conventions to compute the baseline reference. Received '${mainBranchSegment}' which does not fall into the conventions of release, epic or main. rc="$rc
            echo $ERRMSG
         ;;
    esac
    
    if [ $rc -eq 0 ]; then
        if [ -z "${baselineRef}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] No baseline ref was found for branch name ${Branch} in ${baselineReferenceFile}. rc="$rc
            echo $ERRMSG
        fi
     fi

    ##DEBUG ## echo -e "baselineRef \t: ${baselineRef}"    ## DEBUG
}

computeNextReleaseVersion() {
    # Compute the name of the next release based on the releaseType
    case $(echo $ReleaseType | tr '[:upper:]' '[:lower:]') in
        "patch")
            export newVersion=`echo ${baselineRef} | sed 's/^["refs\/tags\/rel-]*//g' | sed 's/-[a-zA-Z0-9]*//g' | awk -F. -v OFS=. '{$3 += 1 ; print}'`
            rc=0
         ;;
        "minor")
            export newVersion=`echo ${baselineRef} | sed 's/^["refs\/tags\/rel-]*//g' | sed 's/-[a-zA-Z0-9]*//g' | awk -F. -v OFS=. '{$2 += 1 ; $3 = 0; print}'`
            rc=0 
         ;;
         "major")
            export newVersion=`echo ${baselineRef} | sed 's/^["refs\/tags\/rel-]*//g' | sed 's/-[a-zA-Z0-9]*//g' | awk -F. -v OFS=. '{$1 += 1 ; $2 = 0; $3 = 0; print}'`
            rc=0
         ;;
        *)
            rc=8
            ERRMSG=$PGM": [ERROR] No valid release type found. Valid release types are 'major', 'minor' or 'patch'. rc="$rc
            echo $ERRMSG
         ;;
    esac

}

# Call validate Options
if [ $rc -eq 0 ]; then
  validateOptions
fi

#
# Ready to go
if [ $rc -eq 0 ]; then
  echo $PGM": [INFO] **************************************************************"
  echo $PGM": [INFO] ** Started Next Release Computation on HOST/USER: ${SYS}/${USER}"
  echo $PGM": [INFO] **                Workspace:" $(getWorkDirectory)
  echo $PGM": [INFO] **              Application:" ${App}
  echo $PGM": [INFO] **                   Branch:" ${Branch}
  echo $PGM": [INFO] **             Release Type:" ${ReleaseType}
  echo $PGM": [INFO] **   Baselinereference file:" ${baselineReferenceFile}
  echo $PGM": [INFO] **************************************************************"
  echo ""

# Extract the current branch from GitLab environment
  export mainBranchSegment=`echo ${Branch} | awk -F "/" '{ print $1 }'`
  export secondBranchSegment=`echo ${Branch} | awk -F "/" '{ print $2 }'`
  export thirdBranchSegment=`echo ${Branch} | awk -F "/" '{ print $3 }'`
  # echo $PGM": [DEBUG] Branch segments: ${mainBranchSegment}, ${secondBranchSegment}, ${thirdBranchSegment}"

# Find base line version of the current branch from the baseLineReferenceFile
  getBaselineReference
  if [ $rc -eq 0 ]; then
      ERRMSG=$PGM": [INFO] Baseline reference: ${baselineRef}"
      echo $ERRMSG
    
      computeNextReleaseVersion
      
      if [ $rc -eq 0 ]; then
        export releaseVersion="rel-"${newVersion}
        ERRMSG=$PGM": [INFO] Compute the next release version complete. The next release version: ${releaseVersion}. rc="$rc
        echo $ERRMSG
      else
        ERRMSG=$PGM": [ERROR] Compute the next release version failed. Check console for details. rc="$rc
        echo $ERRMSG
      fi
  fi
fi

exit $rc