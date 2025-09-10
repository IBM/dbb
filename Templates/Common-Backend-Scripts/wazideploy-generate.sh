#!/bin/env bash
#===================================================================================
# NAME: wazideploy-generate.sh
#
# DESCRIPTION: The purpose of this script is to use wazideploy-generate to
# generate a Deployment Plan for Wazi Deploy
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
# 2025/03/13 DB   1.10 Allow pipelines to compute the artifact location to download
#                      packages via wazideploy-generate
# 2025/09/10 DB   1.20 Streamline arguments. Deprecate -b branch argument.
#===================================================================================
Help() {
  echo $PGM" - Generate Wazi Deploy Deployment Plan                               "
  echo "                                                                          "
  echo "Description: The purpose of this script is to use wazideploy-generate to  "
  echo "generate a Deployment Plan for Wazi Deploy.                               "
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
  echo "                                           path are combined a working    "
  echo "                                           folder. See wdDeployPackageDir "
  echo "                                           in pipelineBackend.config      "
  echo "                                           (Optional)                     "
  echo "                                           Default=None.                  "
  echo "                                                                          "
  echo "                 Ex: MortgageApplication/main/build-1                     "
  echo "                                                                          "
  echo "       -i <package input file>           - Path to the package            "
  echo "                                           used as input                  "
  echo "                                           If a relative path is provided,"
  echo "                                           the log directory is suffixed  "
  echo "                                           Default=None, Required.        "
  echo "                                                                          "
  echo "                 Ex: MortgageApplication.tar                              "
  echo "                                                                          "
  echo "       -m <deployment method path>       - Path to the Deployment Method  "
  echo "                                           (Optional)                     "
  echo "                                           Default=pipelineBackend.config "
  echo "                                                                          "
  echo "       -p <deployment plan path>         - Path to the generated          "
  echo "                                           Deployment Plan                "
  echo "                                           (Optional)                     "
  echo "                                           Default=                       "
  echo "                                            See wdDeploymentPlanName      "
  echo "                                            in pipelineBackend.config     "
  echo "                                                                          "
  echo "       -r <deployment plan report>       - Path to the generated          "
  echo "                                           (Optional)                     "
  echo "                                           Default=                       "
  echo "                                            See wdDeploymentPlanReportName"
  echo "                                            in pipelineBackend.config     "
  echo "                                                                          "
  echo "                                                                          "
  echo "       -o <package output file path>     - Path to the package            "
  echo "                                           used as output                 "
  echo "                                           (Optional)                     "
  echo "                                           Default computed in            "
  echo "                                            wdDeployPackageDir()          "
  echo "                                            in pipelineBackend.config     "
  echo "                                                                          "
  echo "       -c <configuration file path>      - Path to configuration file     "
  echo "                                           for the artifact repository    "
  echo "                                           Default=                       "
  echo "                                            See wdDeployArtifactoryConfig "
  echo "                                            in pipelineBackend.config     "
  echo "                                                                          "
  echo "       -P <pipelineType>              - Type of the pipeline to           "
  echo "                                        control in which directory builds "
  echo "                                        are stored in the artifact repo   "
  echo "                                        Accepted values:                  "
  echo "                                        build -                           "
  echo "                                         development builds               "
  echo "                                        release -                         "
  echo "                                         builds with options for          "
  echo "                                         performance optimized            "
  echo "                                         executables for production env   "
  echo "                                        (optional)                        "
  echo "                                                                          "
  echo "                                                                          "
  echo "       -I <buildIdentifier>              - A unique build identifier      "
  echo "                                           typically the buildID of the   "
  echo "                                           pipeline                       "
  echo "                                           (optional)                     "
  echo "                                                                          "
  echo "       -R <releaseIdentifier>             - The release identifier for    "
  echo "                                           release pipeline builds        "
  echo "                                           (optional)                     "
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
packagingUtilities="${SCRIPT_HOME}/utilities/packagingUtilities.sh"
# Customization - End

#
# Internal Variables
#set -x                  # Uncomment to enable shell script debug
#export BASH_XTRACEFD=1  # Write set -x trace to file descriptor

PGM=$(basename "$0")
PGMVERS="1.20"
USER=$(whoami)
SYS=$(uname -Ia)

rc=0
ERRMSG=""
# Wazi Deploy configuration variables
DeploymentMethod=""
DeploymentPlan=""
DeploymentPlanReport=""
PackageInputFile=""
PackageOutputFile=""
ConfigFile=""

# CBS configuration variables
Workspace=""
App=""          # Application name - takes cli option a
PipelineType="" # takes cli option P

# Package identifier variables
buildIdentifier=""   # takes cli option I
releaseIdentifier="" # takes cli option R
Branch="" # takes cli option b (DEPRECATED)

# 
computeArchiveUrl="true"            # enables the computation of the url
artifactRepositoryAbsoluteUrl=""    # Used to store the computed Url
usePackageUrl=""                    # Internal flag indicating if the package url was computed

Debug=""
HELP=$1

tarFileName="" # variable to store the package tar

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
  echo $PGM": [INFO] Generate Wazi Deploy Deployment Plan. Version="$PGMVERS
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

# Source packaging helper
if [ $rc -eq 0 ]; then
    if [ ! -f "${packagingUtilities}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Packaging Utilities file (${packagingUtilities}) was not found. rc="$rc
        echo $ERRMSG
    else
        source $packagingUtilities
    fi
fi

#
# Get Options
if [ $rc -eq 0 ]; then
  while getopts "hdw:m:p:r:i:o:c:a:I:R:P:b:" opt; do
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
    m)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Deployment Method is required. rc="$rc
        echo $ERRMSG
        break
      fi
      DeploymentMethod="$argument"
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

    r)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Path to Deployment Plan Report is required. rc="$rc
        echo $ERRMSG
        break
      fi
      DeploymentPlanReport="$argument"
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

    o)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Path to Package Output File is required. rc="$rc
        echo $ERRMSG
        break
      fi
      PackageOutputFile="$argument"
      ;;

    c)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [ERROR] Configuration File is required. rc="$rc
        echo $ERRMSG
        break
      fi
      ConfigFile="$argument"
      ;;

    d)
      # Add command to produce debug output with Wazi Deploy
      Debug=" -d"
      ;;

    a)
      # Application argument
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
        ERRMSG=$PGM": [WARNING] Name of the git branch is required. rc="$rc
        echo $ERRMSG
        break
      fi
      echo $PGM": [INFO] The argument -b branch is no longer used. Please remove it from your pipeline definition."
      Branch="$argument"
      ;;
    P)
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
    I)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [WARNING] The name of the version to create is required. rc="$rc
        echo $ERRMSG
        break
      fi
      buildIdentifier="$argument"
      ;;
    R)
      argument="$OPTARG"
      nextchar="$(expr substr $argument 1 1)"
      if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
        rc=4
        ERRMSG=$PGM": [WARNING] The name of the release identifier is required. rc="$rc
        echo $ERRMSG
        break
      fi
      releaseIdentifier="$argument"
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

# if computation of files is setup
checkWorkspace() {
  if [ -z "${Workspace}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Unique Workspace parameter (-w) is required. rc="$rc
    echo $ERRMSG
  fi
}

# Validate Options
validateOptions() {

  # read default deployment method if not specified
  if [ -z "${DeploymentMethod}" ]; then
    #  read from pipelineBackend.config
    DeploymentMethod="${wdDeploymentMethod}"
    if [ -z "${DeploymentMethod}" ]; then
      rc=8
      ERRMSG=$PGM": [ERROR] Deployment Method is required. rc="$rc
      echo $ERRMSG
    fi
  fi
  if [ $rc -eq 0 ]; then
    if [ ! -f "${DeploymentMethod}" ]; then
      rc=8
      ERRMSG=$PGM": [ERROR] Wazi Deploy Deployment Method (${DeploymentMethod}) was not found. rc="$rc
      echo $ERRMSG
    fi
  fi

  # compute deployment plan if not specified
  if [ -z "${DeploymentPlan}" ]; then
    # compute default based on configuration
    checkWorkspace
    DeploymentPlan="$(wdDeployPackageDir)/${wdDeploymentPlanName}"
    # if still undefined
    if [ -z "${DeploymentPlan}" ]; then
      rc=8
      ERRMSG=$PGM": [ERROR] Deployment Plan is required. rc="$rc
      echo $ERRMSG
    fi
  fi
  # if relative path
  if [[ ! ${DeploymentPlan:0:1} == "/" ]]; then
    DeploymentPlan="$(wdDeployPackageDir)/${DeploymentPlan}"
  fi

  # compute deployment plan report if not specified
  if [ -z "${DeploymentPlanReport}" ]; then
    # compute default based on configuration
    checkWorkspace
    DeploymentPlanReport="$(wdDeployPackageDir)/${wdDeploymentPlanReportName}"
  fi
  # if relative path
  if [[ ! ${DeploymentPlanReport:0:1} == "/" ]]; then
    DeploymentPlanReport="$(wdDeployPackageDir)/${DeploymentPlanReport}"
  fi

  # validate package input file
  if [ -z "${PackageInputFile}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] Package Input File is required. rc="$rc
    echo $ERRMSG
  else
    # check for relative path
    if [ ! ${PackageInputFile:0:1} == "/" ] && [ -z "${usePackageUrl}" ]; then
      checkWorkspace
      PackageInputFile="$(getLogDir)/${PackageInputFile}"
    fi
  fi

  # validate config file
  if [ -z "${ConfigFile}" ]; then
    ConfigFile="${wdDeployArtifactoryConfig}"
  fi

  if [ ! -z "${ConfigFile}" ]; then
    if [ ! -f "${ConfigFile}" ]; then
      rc=8
      ERRMSG=$PGM": [ERROR] Specified Wazi Deploy Artifact repository configuration file (${ConfigFile}) was not found. rc="$rc
      echo $ERRMSG
    fi
  fi

  # compute the output file
  if [ -z "${PackageOutputFile}" ] && [ ! -z "${ConfigFile}" ]; then
    checkWorkspace
    PackageOutputFile="$(wdDeployPackageDir)"
  fi
  # if relative path
  if [[ ! ${PackageOutputFile:0:1} == "/" ]] && [[ ! -z "${PackageOutputFile}" ]]; then
    PackageOutputFile="$(wdDeployPackageDir)/${PackageOutputFile}"
  fi
}

# When publishing is enabled, try reading the wdPackageVersionFile
# that needs to be computed before this step.
if [ $rc -eq 0 ] && [ "$publish" == "true" ] && [ ! -z "${buildIdentifier}" ]; then
  checkWorkspace

  # validate options
  if [ -z "${PipelineType}" ]; then
    rc=8
    ERRMSG=$PGM": [ERROR] To compute the Url of the stored package to enable the download of the archive file via Wazi Deploy generate, you need to provide the pipelineType. rc="$rc
    echo $ERRMSG
  fi

  if [ $rc -eq 0 ]; then

    # Call utilities method to compute the Url based on defined conventions shared with packaging step
    getArchiveLocation

    # Set Input and output files for Wazi Deploy
    PackageInputFile="${artifactRepositoryAbsoluteUrl}"
    PackageOutputFile="$(wdDeployPackageDir)/applicationArchive.tar" # shared convention with wazideploy-deploy.sh
    usePackageUrl="true"
  fi
fi

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
  echo $PGM": [INFO] ** Start Wazi Deploy Generation on HOST/USER: ${SYS}/${USER}"
  echo $PGM": [INFO] **               Deployment Method:" ${DeploymentMethod}
  echo $PGM": [INFO] **       Generated Deployment Plan:" ${DeploymentPlan}

  if [ ! -z "${DeploymentPlanReport}" ]; then
    echo $PGM": [INFO] **          Deployment Plan Report:" ${DeploymentPlanReport}
  fi

  echo $PGM": [INFO] **              Package Input File:" ${PackageInputFile}

  if [ ! -z "${PackageOutputFile}" ]; then
    echo $PGM": [INFO] **             Package Output File:" ${PackageOutputFile}
  fi

  if [ ! -z "${ConfigFile}" ]; then
    echo $PGM": [INFO] **              Configuration File:" ${ConfigFile}
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
# Set up to execute the Wazi Deploy generate command
if [ $rc -eq 0 ]; then
  CommandLine="wazideploy-generate --deploymentMethod "${DeploymentMethod}" --deploymentPlan "${DeploymentPlan}" --packageInputFile "${PackageInputFile}
  if [ ! -z "${DeploymentPlanReport}" ]; then
    CommandLine+=" --deploymentPlanReport "${DeploymentPlanReport}
  fi
  if [ ! -z "${PackageOutputFile}" ]; then
    CommandLine+=" --packageOutputFile "${PackageOutputFile}
  fi
  if [ ! -z "${ConfigFile}" ]; then
    CommandLine+=" --configFile "${ConfigFile}
  fi
  if [ ! -z "${Debug}" ]; then
    CommandLine+=${Debug}
  fi
  echo ${CommandLine} 2>&1
  ${CommandLine} 2>&1
  rc=$?

  if [ $rc -ne 0 ]; then
    ERRMSG=$PGM": [ERROR] Unable to Generate Deployment Plan with Wazi Deploy. rc="$rc
    echo $ERRMSG
    rc=8
  fi
fi

exit $rc
