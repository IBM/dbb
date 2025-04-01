#!/bin/env bash
#
#===================================================================================
# NAME: dbbBuild.sh
#
# DESCRIPTION: The purpose of this script is to perform a DBB Build from within
# a Pipeline.
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
#        Build Type  - The type of build to perform. Refer to zAppBuild
#                      build.groovy for more information.
#        HLQ         - High Level data set qualifier used DBB build output.
#
#      Review the "Build Type Customization" section below for additional information.
#
#   3. This script supports Dependency Base Build V2.x and above.
#
# Maintenance Log
#
# Date       Who Vers  Description
# ---------- --- ----- --------------------------------------------------------------
# 2023/07/27 TLD 1.0.0 Initial Release
# 2023/08/28  D  1.1.0 Introducing pipelineConfig and dbbBuildUtils.sh
#===================================================================================
Help() {
  echo $PGM" - Invoke DBB Build ("$PGMVERS")                          "
  echo "                                                              "
  echo "Description: The purpose of this script is to perform         "
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
  echo "       -p <pipelineType>  - Type of the pipeline to           "
  echo "                            control if this builds with       "
  echo "                            optimize or test options          "
  echo "                            also impacting HLQ                "
  echo "                            Default=build, Required.          "
  echo "                            Accepted values:                  "
  echo "                            build -                           "
  echo "                             perform build with TEST options  "
  echo "                            release -                         "
  echo "                             perform build with options for   "
  echo "                             performance optimized            "
  echo "                             executables                      "
  echo "                            preview -                         "
  echo "                             perform build zAppBuild option   "
  echo "                             --preview                        "
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
  echo "       -bc                - (Optional) zAppBuild build        "
  echo "                             options to override              "
  echo "                             the computed build configuration "
  echo "                 Ex: --fullBuild                              "
  echo "                                                              "
  echo "       -v                 - (Optional) Verbose tracing        "
  echo "                             flag for zAppBuild               "
  echo "                                                              "
  echo "                                                              "
  exit 0

}

#
# Build Type Customization
# Configuration file leveraged by the backend scripts
# Either an absolute path or a relative path to the current working directory
SCRIPT_HOME="`dirname "$0"`"
pipelineConfiguration="${SCRIPT_HOME}/pipelineBackend.config"
buildUtilities="${SCRIPT_HOME}/utilities/dbbBuildUtils.sh"
# Customization - End

#
# Internal Variables
#set -x                  # Uncomment to enable shell script debug
#export BASH_XTRACEFD=1  # Write set -x trace to file descriptor

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
HELP=$1

# Local Variables
# TLD: Always a good idea to initialize any local varables
AppDir=""        # Derived Application Directory
HLQ=""           # Derived High Level Qualifier
HLQPrefix=""     # Prefix of HLQ, either specified via the cli option -q or via configuration file
Type=""          # Derived Build Type
userDefinedBuildType="" #  Flag if the user has provided the Build Type as argument
baselineRef=""   # baselineReference that is computed by utilities/dbbBuildUtils.sh
propOverrides="" # Override of default build parameters for zAppBuild
#  computed by utilities/dbbBuildUtils.sh
outDir=""                  # Computed output directory to store build protocols
nestedApplicationFolder="" # Flag to understand a nested repository

# Local variables for checking the contents of buildList and deletedFilesList
totalLogListSize=0
buildListFile=""
deletedFilesListFile=""

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
  echo $PGM": [INFO] DBB Build Wrapper. Version="$PGMVERS
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
          ERRMSG=$PGM": [WARNING] Build type/configuration is required. rc="$rc
          echo $ERRMSG
          break
        fi
        Type="$argument"
        userDefinedBuildType=1
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

  BuildGroovy="${zAppBuild}/build.groovy"

  if [ ! -f "${BuildGroovy}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Unable to locate ${BuildGroovy}. rc="$rc
    echo $ERRMSG
  fi

  #
  # Check to see if debug options were requested and set up switches
  # to enable.

  if [ ${Verbose} -eq 1 ]; then
    zAppBuildVerbose="--verbose"
  fi

  if [ ${LoggerConfig} -eq 1 ]; then
    DBBLogger="-classpath ${AppDir}/application-conf"
  fi

}

# Call validate Options
if [ $rc -eq 0 ]; then
  validateOptions
fi

# Call utilities to compute build type based on pipeline
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

# Ready to go  TLD: Suggest in the section to echo as much as possible
if [ $rc -eq 0 ]; then
  echo $PGM": [INFO] **************************************************************"
  echo $PGM": [INFO] ** Started - DBB Build on HOST/USER: ${SYS}/${USER}"
  echo $PGM": [INFO] **          Workspace:" $(getWorkDirectory)
  echo $PGM": [INFO] **        Application:" ${App}
  echo $PGM": [INFO] **             Branch:" ${Branch}
  echo $PGM": [INFO] **         Build Type:" ${Type}
  if [ ! -z "${propOverrides}" ]; then
    echo $PGM": [INFO] ** Property Override :" ${propOverrides}
  fi
  echo $PGM": [INFO] **                HLQ:" ${HLQ}
  echo $PGM": [INFO] **             AppDir:" ${AppDir}
  echo $PGM": [INFO] **             LogDir:" ${outDir}
  echo $PGM": [INFO] **     zAppBuild Path:" ${zAppBuild}
  echo $PGM": [INFO] **           DBB_HOME:" ${DBB_HOME}
  echo $PGM": [INFO] **      DBB JDBC USER:" ${dbbMetadataStoreJdbcId}
  echo $PGM": [INFO] **  DBB JDBC Pwd File:" ${dbbMetadataStoreJdbcPwdFile}

  if [ ! -z "${dbbMetadataStoreJdbcUrl}" ]; then
    echo $PGM": [INFO] **      DBB JDBC Url :" ${dbbMetadataStoreJdbcUrl}
  fi
  if [ ${Verbose} -eq 1 ]; then
    echo $PGM": [INFO] **           Verbose :" ${zAppBuildVerbose}
  else
    echo $PGM": [INFO] **           Verbose : No"
  fi
  if [ ${LoggerConfig} -eq 1 ]; then
    echo $PGM": [INFO] **         DBB Logger:" ${DBBLogger}
  else
    echo $PGM": [INFO] **         DBB Logger: No"
  fi
  echo $PGM": [INFO] **************************************************************"
  echo ""
fi

# TLD - This is where I left off.  The below was taken directly from the Azure Script copy.
# More updates will need to be performed to support DBB 2.x.  The following code segments
# where derived directly from the Azure version of this script.

#
# Invoke the DBB Build
if [ $rc -eq 0 ]; then
  echo $PGM": [INFO] Invoking the zAppBuild Build Framework."

  # Assemble build command
  CMD="$DBB_HOME/bin/groovyz ${DBBLogger} ${BuildGroovy} --workspace $(getWorkDirectory) --application ${App} --outDir ${outDir} --hlq ${HLQ} ${zAppBuildVerbose} --logEncoding UTF-8"
  if [ ! -z "${dbbMetadataStoreJdbcId}" ]; then
    CMD="${CMD} --id ${dbbMetadataStoreJdbcId}" # Appending JDBC User Id if defined
  fi
  if [ ! -z "${dbbMetadataStoreJdbcPwdFile}" ]; then
    CMD="${CMD} --pwFile ${dbbMetadataStoreJdbcPwdFile}" # Appending JDBC Password file if defined
  fi  
  if [ ! -z "${dbbMetadataStoreJdbcUrl}" ]; then
    CMD="${CMD} --url ${dbbMetadataStoreJdbcUrl}" # Appending JDBC URL if defined
  fi
  if [ ! -z "${zAppBuildPropFiles}" ]; then
    CMD="${CMD} --propFiles ${zAppBuildPropFiles}" # Appending propFiles if defined
  fi
  if [ ! -z "${propOverrides}" ]; then
    CMD="${CMD} --propOverwrites ${propOverrides}" # Appending propOverrides if defined
  fi

  CMD="${CMD} ${Type}" # Append zAppBuild Build Type
  echo $PGM": [INFO] ${CMD}"
  ${CMD} #TLD: I commented this out for testing purposed
  rc=$?
  #exit 0

  if [ $rc -eq 0 ]; then

    ## Except for the reset mode, check for "nothing to build" condition and throw an error to stop pipeline
    if [ "$Type" != "--reset" ]; then
      
      # Locate buildList and deletedFilesList in Build Log Directory within outDir, and group them in logListArray
      buildListFile="${outDir}/buildList.txt"
      deletedFilesListFile="${outDir}/deletedFilesList.txt"
      logListArray=(${buildListFile} ${deletedFilesListFile})

      # For each list in logListArray, if found in the last Build Log Directory, get its size (character count), then
      # increase logListSize by that amount.
      for list in ${logListArray[@]}; do
        if [ -f ${list} ]; then
          # wc -c will return the two values; Character Count and Log File Path.  Parse out the Character Count.
          set $(wc -c <${list})
          totalLogListSize=$((${totalLogListSize}+$1))
        fi
      done

      # Error/warning if both build and file list have 0 character count (i.e. are empty)       
      if [ ${totalLogListSize} = 0 ]; then
        rc=4
        ERRMSG=$PGM": [WARNING] DBB Build Error. No source changes detected. rc="$rc
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
