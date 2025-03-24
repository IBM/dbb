#!/bin/env bash
#
#===================================================================================
# NAME: computeReleaseVersion.sh
#
# DESCRIPTION: The purpose of this script is to perform a DBB Build from within
# a Pipeline.
#
# Customization
# Central configuration file leveraged by the backend scripts
SCRIPT_HOME="$(dirname "$0")"
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
PipelineType=""
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
  echo $PGM": [INFO] Release Version Wrapper. Version="$PGMVERS
fi


if [ $rc -eq 0 ]; then
# Read and import pipeline configuration
    if [ ! -f "${pipelineConfiguration}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Pipeline Configuration File (${pipelineConfiguration}) was not found. rc="$rc
        echo $ERRMSG
    else
        source $pipelineConfiguration
    fi
#

# Get Options
    if [ $rc -eq 0 ]; then
        while getopts "h:w:a:b:p" opt; do
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
            p)
                argument="$OPTARG"
                nextchar="$(expr substr $argument 1 1)"
                echo $argument
                if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                echo $nextchar
                INFO=$PGM": [INFO] No Pipeline type specified. rc="$rc
                echo $INFO
                break
                fi
                PipelineType="$argument"
                ;;
            r)
                argument="$OPTARG"
                nextchar="$(expr substr $argument 1 1)"
                if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                INFO=$PGM": [INFO] No Release type specified. rc="$rc
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

    # Check if application directory contains
    if [ -d "${AppDir}/${App}" ]; then
      echo $PGM": [INFO] Detected the application respository (${App}) within the git repository layout structure."
      echo $PGM": [INFO]  Assuming this as the new application location."
      AppDir="${AppDir}/${App}"
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

  tmp2=$(echo $ReleaseType | tr '[:upper:]' '[:lower:]')

  case $tmp2 in
  "major") ;;
  "minor") ;;
  "patch") ;;
  *)
    rc=0
    PipelineType="patch"
    ERRMSG=$PGM": [INFO] Set default Release Type : ${ReleaseType} ."
    echo $ERRMSG
    ;;
  esac

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
        *)
            rc=8
            ERRMSG=$PGM": [ERROR] Branch name ${Branch} does not follow the recommended naming conventions to compute the baseline reference. Received '${mainBranchSegment}' which does not fall into the conventions of release, epic or main. rc="$rc
            echo $ERRMSG
         ;;
    esac
    

    if [ -z "${baselineRef}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] No baseline ref was found for branch name ${Branch} in ${baselineReferenceFile}. rc="$rc
        echo $ERRMSG
    fi

    ##DEBUG ## echo -e "baselineRef \t: ${baselineRef}"    ## DEBUG
}

computeNextReleaseVersion() {
    # Compute the name of the next release based on the releaseType
    if [ "${ReleaseType}" == "patch" ]; then
      export newVersion=`echo ${baselineRef} | sed 's/^["refs\/tags\/rel-]*//g' | sed 's/-[a-zA-Z0-9]*//g' | awk -F. -v OFS=. '{$3 += 1 ; print}'`
    fi
    
    if [ "${ReleaseType}" == "minor" ]; then
      export newVersion=`echo ${baselineRef} | sed 's/^["refs\/tags\/rel-]*//g' | sed 's/-[a-zA-Z0-9]*//g' | awk -F. -v OFS=. '{$2 += 1 ; $3 = 0; print}'`
    fi
            
    if [ "${ReleaseType}" == "major" ]; then
      export newVersion=`echo ${baselineRef} | sed 's/^["refs\/tags\/rel-]*//g' | sed 's/-[a-zA-Z0-9]*//g' | awk -F. -v OFS=. '{$1 += 1 ; $2 = 0; $3 = 0; print}'`
    fi
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
  echo $PGM": [INFO] **              Workspace:" $(getWorkDirectory)
  echo $PGM": [INFO] **            Application:" ${App}
  echo $PGM": [INFO] **                 Branch:" ${Branch}
  echo $PGM": [INFO] **             Build Type:" ${Type}
  echo $PGM": [INFO] ** Baselinereference file:" ${Type}
  echo $PGM": [INFO] **************************************************************"
  echo ""

# Extract the current branch from GitLab environment
  export mainBranchSegment=`echo ${Branch} | awk -F "/" '{ print $1 }'`
  export secondBranchSegment=`echo ${Branch} | awk -F "/" '{ print $2 }'`
  export thirdBranchSegment=`echo ${Branch} | awk -F "/" '{ print $3 }'`
  echo $PGM": [INFO] Branch segments: ${mainBranchSegment}, ${secondBranchSegment}, ${thirdBranchSegment}"

# Find base line version of the current branch from the baseLineReferenceFile
  getBaselineReference
  echo $PGM": [INFO] Baseline reference: ${baselineRef}"

  computeNextReleaseVersion
  echo $PGM": [INFO] Next release version: ${newVersion}"

fi

exit $rc



