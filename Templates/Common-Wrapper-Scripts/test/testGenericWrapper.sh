#!/usr/bin/env bash
#
#===================================================================================
# NAME: testGenericWrapper.sh
#
# DESCRIPTION: This script is running zowe cli to run a suite of tests for the wrapper scripts.
#
# OPTIONS:
#  commandInterface
#
#  'testGenericWrapper.sh zowe' submits the commands via  the configured Zowe cli profile
#  'testGenericWrapper.sh ssh ' submits the commands via opens a remote ssh session for each invocation
#
# NOTE(S):
# - Review the test script configuration section to specify:
#   . dbbBuildRootDir to specify the UNIX System Services directory as the workspace
#
# - Recommeded to use the DBB File Metadatastore in .dbb
#
#
# CONFIGURATION:
# - See user
#
# MAINTENANCE LOG:
#
# Date       Who Vers  Description
# ---------- --- ----- --------------------------------------------------------------
# 2023/07/27 DN  1.0.0 Initial release of the test script
#====================================================================================

#====================================================================================
## script configuration
#====================================================================================
# build workspace directory for test
# e.g. dbbBuildRootDir="/u/ibmuser/backendWorkspace"
dbbBuildRootDir=""

# ssh connection
# e.g. sshConnection="ibmuser@dev-lpar"
sshConnection=""

# ssh environment profile to execute for non-conversational ssh sessions
# e.g. sshEnvironmentProfile="/u/ibmuser/.profile"
sshEnvironmentProfile=""


# MortageApplication Artifact repository properties for UCD
# e.g. artifactRepoConfig="/var/jenkins/zappbuild_config/MortgageAppArtifactRepository.properties"
artifactRepoConfig=""

#====================================================================================
# Test repository
#====================================================================================
repo="https://github.com/dennis-behm/MortgageApplication.git" # the test repo
application="MortgageApplication"

#====================================================================================
# flags which pipeline steps should be executed in test process
# e.g. for instance if you don't want to run a test for UCD
# please pay to attention to dependencies between steps -
# e.g. you cannot request a deployment without packaging
#====================================================================================

executeClone="true"               # required
executeBuild="true"               # required
executePackageBuildOutputs="true" # optional
executePublishLogs="true"         # optional
executeUcdPackaging="true"        # optional
executeUcdDeploy="true"           # optional

#====================================================================================
## script configuration - end
#====================================================================================

# local variables
PGM=$(basename "$0")
uniqueWorkspaceId="" # calculated
pipelineType=""      # pipeline type - either build or release
branch=""            # branch
verbose=""           # Verbose logging flag
buildTypeOverride="" # Override of zAppBuild build type
ucdVersionName=""    # UCD component version name
timestamp=""         # timestamp for ucd version name or package build outputs
testSummary=""       # String to collect the overall test summary
rc=0                 # Internal RC

commandInterface=$1 #

if [ -z "${commandInterface}" ]; then
    ERRMSG=$PGM": [INFO] Command interface not specified. Ussing ssh configuruation".
    echo $ERRMSG
fi


if [ -z "${dbbBuildRootDir}" ]; then
    rc=12
    ERRMSG=$PGM": [ERROR] dbbBuildRootDir not specified. Please review configuration. Exiting. rc="$rc
    echo $ERRMSG
    exit $rc
fi

echo "$PGM: [INFO] Running test script using $commandInterface."

# Function that submits the commands
submitCmd() {
    if [ "${commandInterface}" == "zowe" ]; then
        echo "$PGM: [COMMAND] zowe zos-uss issue ssh \"${cmdStr}\""
        zowe zos-uss issue ssh "${cmdStr}"
    else
        # Check configuration
        if [ -z "${sshConnection}" ]; then
            rc=12
            ERRMSG=$PGM": [ERROR] SSH connection details not configured in test script. Exiting. rc="$rc
            echo $ERRMSG
        fi

        if [ -z "${sshEnvironmentProfile}" ]; then
            rc=12
            ERRMSG=$PGM": [ERROR] SSH environment profiles not set. Required to run test script. Exiting. rc="$rc
            echo $ERRMSG
        fi

        if [ $rc -eq 0 ]; then
            echo "$PGM: [COMMAND] ssh $sshConnection \". $sshEnvironmentProfile && ${cmdStr}\""
            ssh $sshConnection ". $sshEnvironmentProfile && ${cmdStr}"
        fi
    fi
}

# Reset environment
# deletes the build workspaces, drops DBB collections and DBB build results from Metadatastore
resetWorkspace() {

    echo "$PGM: [INFO] Reset workspace folder $dbbBuildRootDir."

    # cleanup
    cmdStr="rm -Rf $dbbBuildRootDir"
    submitCmd

    cmdStr="mkdir $dbbBuildRootDir"
    submitCmd


    echo "$PGM: Reset dbb collections in File Metadatastore."

    cmdStr="dbb collection list --type file"
    submitCmd

    cmdStr="dbb collection delete MortgageApplication-hotfix/rel-1.0.0/myfix --type file"
    submitCmd
    cmdStr="dbb collection delete MortgageApplication-hotfix/rel-1.0.0/myfix-outputs --type file"
    submitCmd
    cmdStr="dbb collection delete MortgageApplication-feature/setmainbuildbranch --type file"
    submitCmd
    cmdStr="dbb collection delete MortgageApplication-feature/setmainbuildbranch-outputs --type file"
    submitCmd

    cmdStr="dbb collection delete MortgageApplication-epic/implementAI --type file"
    submitCmd
    cmdStr="dbb collection delete MortgageApplication-epic/implementAI-outputs --type file"
    submitCmd

    cmdStr="dbb build-group delete MortgageApplication-hotfix/rel-1.0.0/myfix --type file"
    submitCmd
    cmdStr="dbb build-group delete MortgageApplication-feature/setmainbuildbranch --type file"
    submitCmd
    cmdStr="dbb build-group delete MortgageApplication-epic/implementAI --type file"
    submitCmd

    cmdStr="dbb collection list --type file"
    submitCmd
}

# Clone step
cloneApplicationRepository() {
    cmdStr="gitClone.sh -w $dbbBuildRootDir/$uniqueWorkspaceId -r $repo -b $branch"
    if [ "$executeClone" == "true" ]; then
        submitCmd
    else
        echo "$PGM: [INFO] Git Clone skipped based on configuration."
    fi
}

# InvokeBuild
runDBBBuild() {
    cmdStr="dbbBuild.sh -w $dbbBuildRootDir/$uniqueWorkspaceId -a ${application} -b $branch $verbose"
    if [ ! -z "${pipelineType}" ]; then
        cmdStr="${cmdStr} -p ${pipelineType}"
    fi
    if [ ! -z "${buildTypeOverride}" ]; then
        cmdStr="${cmdStr} -t '${buildTypeOverride}'"
    fi

    if [ "$executeBuild" == "true" ]; then
        submitCmd
    else
        echo "$PGM: [INFO] Build skipped based on configuration."
    fi

}

# a method to get unique ucd component version names ...
setTimestamp() {
    timestamp=$(date +"%Y-%m-%d_%H-%M-%S")
}

# Invoke UCD packaging script
runUcdPackaging() {
    # application name is also the UCD component name
    # couple of comments / future feature of the script :
    # - version name could be calculated in the backend script
    # - artifactRepoConfig file could be calculated to a path from application-conf
    cmdStr="ucdPackaging.sh -v $ucdVersionName.$timestamp -c $application -w $dbbBuildRootDir/$uniqueWorkspaceId  -b $branch -u http://acme.pipeline.org/$ucdVersionName -e $artifactRepoConfig"
    if [ "$executeUcdPackaging" == "true" ]; then
        submitCmd
    else
        echo "$PGM: [INFO] UCD Packaging skipped based on configuration."
    fi
}

# Run package build outputs script
runPackageBuildOutputs() {

    # application name is also the UCD component name
    # couple of comments / future feature of the script :
    # packageBuildOutputs.sh -w MortApp/main/build-1 -t rel-1.0.0.tar -a MortgageApplication -b feature/myCoolFeature -u -v rel-1.0.0.2023-09-21-xx.xx.xx
    # TODO - do we want to compute the new release?
    #      - do we want to read it from a properties file?
    cmdStr="packageBuildOutputs.sh -w $dbbBuildRootDir/$uniqueWorkspaceId -a $application -t package.tar -b $branch -u -v $application.$timestamp"
    if [ ! -z "${pipelineType}" ]; then
        cmdStr="${cmdStr} -p ${pipelineType}"
    fi
    if [ "$executePackageBuildOutputs" == "true" ]; then
        submitCmd
    else
        echo "$PGM: [INFO] Package Build Outputs skipped based on configuration."
    fi

}

# Request UCD Deployment
# requires JAVA11
runUcdDeployment() {
    # some parameters could be better centrally managed (like -p process).
    cmdStr="ucdDeploy.sh -a $application -p appDeployment -e Integration-Test -d $application:latest -k"
    if [ "$executeUcdDeploy" == "true" ]; then
        submitCmd
    else
        echo "$PGM: [INFO] UCD Deploy skipped based on configuration."
    fi
}

# Prepare Logs
pullLogs() {
    cmdStr="prepareLogs.sh -w $dbbBuildRootDir/$uniqueWorkspaceId"
    if [ "$executePublishLogs" == "true" ]; then

        submitCmd | tee prepareLogsOutput.txt

        # read logs
        logsTar=$(cat prepareLogsOutput.txt | grep "Logs successfully stored at " | awk -F "stored at " ' { print $2 }')

        if [ ! -z "${logsTar}" ]; then
            zowe zos-files download uss-file "$logsTar" -f ./logs/$uniqueWorkspaceId/logs.tar -b
        else
            rc=4
            echo "[WARNING] Tar file containing the logs was not found. rc="$rc
        fi
    else
        echo "$PGM: [INFO] Prepare Logs skipped based on configuration."
    fi

}

############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication
############# MortgageApplication

#====================================================================================
# Functions representing and controlling the various test scenarios
#====================================================================================

pipelineType=""

############# MortgageApplication MortApp/build-1
testMortgageApplication-Main-Bld-Build-0() {
    echo "$PGM: [TEST] Executing test scenario testMortgageApplication-Main-Bld-Build-0."
    # test configuration
    branch="main"
    uniqueWorkspaceId="MortApp/${branch}/build-0"
    pipelineType="build"
    buildTypeOverride="--fullBuild"
    # set timestamp
    setTimestamp

    if [ $rc -eq 0 ]; then
        echo "cloneApplicationRepository"
        echo "-------------"
        cloneApplicationRepository
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Perform Build"
        echo "-------------"
        runDBBBuild
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Pull Logs"
        echo "-------------"
        pullLogs
        rc=$?
        echo "rc=$rc"
    fi

    # reset
    buildTypeOverride=""

}

############# MortgageApplication MortApp/build-1
testMortgageApplication-Main-Bld-Build-1() {
    echo "$PGM: [TEST] Executing test scenario testMortgageApplication-Main-Bld-Build-1."
    # test configuration
    branch="main"
    uniqueWorkspaceId="MortApp/${branch}/build-1"
    pipelineType="build"
    verbose="-v"
    ucdVersionName="${application}.build-1"
    # set timestamp
    setTimestamp

    if [ $rc -eq 0 ]; then
        echo "cloneApplicationRepository"
        echo "-------------"
        cloneApplicationRepository
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Perform Build"
        echo "-------------"
        runDBBBuild
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - UCD Packaging"
        echo "-------------"
        runUcdPackaging
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - PackageBuildOutputs"
        echo "-------------"
        runPackageBuildOutputs
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Run Deployment"
        echo "-------------"
        runUcdDeployment
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Pull Logs"
        echo "-------------"
        pullLogs
        rc=$?
        echo "rc=$rc"
    fi

    #unset variables
    verbose=""

}

testMortgageApplication-Main-Rel-Build-2() {
    echo "$PGM: [TEST] Executing test scenario testMortgageApplication-Main-Rel-Build-2."
    ############# MortgageApplication MortApp/build-2
    branch="main"
    uniqueWorkspaceId="MortApp/${branch}/build-2"
    pipelineType="release"
    ucdVersionName="${application}.build-2"
    # set timestamp
    setTimestamp

    if [ $rc -eq 0 ]; then
        echo "cloneApplicationRepository"
        echo "-------------"
        cloneApplicationRepository
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Perform Build"
        echo "-------------"
        runDBBBuild
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - UCD Packaging"
        echo "-------------"
        runUcdPackaging
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - PackageBuildOutputs"
        echo "-------------"
        runPackageBuildOutputs
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Pull Logs"
        echo "-------------"
        pullLogs
        rc=$?
        echo "rc=$rc"
    fi

}

testMortgageApplication-Main-Prev-Build-3() {
    echo "$PGM: [TEST] Executing test scenario testMortgageApplication-Main-Prev-Build-3."
    branch="main"
    uniqueWorkspaceId="MortApp/${branch}/build-3"
    pipelineType="preview"
    #ucdVersionName="${application}.build-3"
    # set timestamp
    setTimestamp

    if [ $rc -eq 0 ]; then
        echo "cloneApplicationRepository"
        echo "-------------"
        cloneApplicationRepository
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Perform Build"
        echo "-------------"
        runDBBBuild
        rc=$?
        echo "rc=$rc"
    fi

    # no packaging because preview!

    # if [ $rc -eq 0 ]; then
    # echo "Package Build outputs"
    # echo "-------------"
    # runUcdPackaging
    # rc=$?
    # echo "rc=$rc"
    # fi

    if [ $rc -eq 0 ]; then
        echo "Pull Logs"
        echo "-------------"
        pullLogs
        rc=$?
        echo "rc=$rc"
    fi

}
############# MortgageApplication release maintenance leg MortApp/build-3
testMortgageApplication-Release-Rel100-Build-1() {
    echo "$PGM: [TEST] Executing test scenario testMortgageApplication-Release-Rel100-Build-1."
    branch="release/rel-1.0.0"
    uniqueWorkspaceId="MortApp/${branch}/build-1"
    pipelineType=""
    ucdVersionName="${application}.rel-1.0.0.fix-1.build-1"
    # set timestamp
    setTimestamp

    if [ $rc -eq 0 ]; then
        echo "cloneApplicationRepository"
        echo "-------------"
        cloneApplicationRepository
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Perform Build"
        echo "-------------"
        runDBBBuild
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - UCD Packaging"
        echo "-------------"
        runUcdPackaging
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - PackageBuildOutputs"
        echo "-------------"
        runPackageBuildOutputs
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Pull Logs"
        echo "-------------"
        pullLogs
        rc=$?
        echo "rc=$rc"
    fi

}

############# MortgageApplication release maintenance leg MortApp/build-3
testMortgageApplication-Hotfix-Release-Rel100-Build-1() {
    echo "$PGM: [TEST] Executing test scenario testMortgageApplication-Hotfix-Release-Rel100-Build-1."
    branch="hotfix/rel-1.0.0/myfix"
    uniqueWorkspaceId="MortApp/${branch}/build-1"
    ucdVersionName="prelim_${application}.rel-1.0.0.myfix.build-1"
    # set timestamp
    setTimestamp

    if [ $rc -eq 0 ]; then
        echo "cloneApplicationRepository"
        echo "-------------"
        cloneApplicationRepository
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Perform Build"
        echo "-------------"
        runDBBBuild
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - UCD Packaging"
        echo "-------------"
        runUcdPackaging
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - PackageBuildOutputs"
        echo "-------------"
        runPackageBuildOutputs
        rc=$?
        echo "rc=$rc"
    fi

    # load logs even with RC=4 (Warning)
    if [ $rc -eq 4 ]; then
        echo "Pull Logs"
        echo "-------------"
        pullLogs
        rc=$?
        echo "rc=$rc"
    fi

    rc=0 #msg will be RC=4 because no files to be build
}

############# MortgageApplication contibution to the next release
testMortgageApplication-Feature-Setmainbuildbranch-Build-1() {
    echo "$PGM: [TEST] Executing test scenario testMortgageApplication-Feature-Setmainbuildbranch-Build-1."
    branch="feature/setmainbuildbranch"
    uniqueWorkspaceId="MortApp/${branch}/build-1"
    pipelineType=""
    ucdVersionName="prelim_${application}.feature.setmainbuildbranch.build-1"
    # set timestamp
    setTimestamp

    if [ $rc -eq 0 ]; then
        echo "cloneApplicationRepository"
        echo "-------------"
        cloneApplicationRepository
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Perform Build"
        echo "-------------"
        runDBBBuild
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - UCD Packaging"
        echo "-------------"
        runUcdPackaging
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - PackageBuildOutputs"
        echo "-------------"
        runPackageBuildOutputs
        rc=$?
        echo "rc=$rc"
    fi

    # load logs even with RC=4 (Warning)
    if [ $rc -eq 4 ]; then
        echo "Pull Logs"
        echo "-------------"
        pullLogs
        rc=$?
        echo "rc=$rc"
    fi

    rc=0 #msg will be RC=4 because no files to be build

}

############# MortgageApplication contibution to the next release
testMortgageApplication-Epic-implementAI-Build-1() {
    echo "$PGM: [TEST] Executing test scenario testMortgageApplication-Epic-implementAI-Build-1."
    branch="epic/implementAI"
    uniqueWorkspaceId="MortApp/${branch}/build-1"
    ucdVersionName="prelim_${application}.epic.implementAI.build-1"
    # set timestamp
    setTimestamp

    if [ $rc -eq 0 ]; then
        echo "cloneApplicationRepository"
        echo "-------------"
        cloneApplicationRepository
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Perform Build"
        echo "-------------"
        runDBBBuild
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - UCD Packaging"
        echo "-------------"
        runUcdPackaging
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - PackageBuildOutputs"
        echo "-------------"
        runPackageBuildOutputs
        rc=$?
        echo "rc=$rc"
    fi

    # load logs even with RC=4 (Warning)
    if [ $rc -eq 4 ]; then
        echo "Pull Logs"
        echo "-------------"
        pullLogs
        rc=$?
        echo "rc=$rc"
    fi

}

############# MortgageApplication contibution to the next release
testMortgageApplication-Epic-Feature-implementAI-Build-1() {
    echo "$PGM: [TEST] Executing test scenario testMortgageApplication-Epic-Feature-implementAI-Build-1."
    branch="implementAI/myFeatureImpl"
    uniqueWorkspaceId="MortApp/${branch}/build-1"
    ucdVersionName="prelim_${application}.epic.implementAI.build-1"
    # set timestamp
    setTimestamp

    if [ $rc -eq 0 ]; then
        echo "cloneApplicationRepository"
        echo "-------------"
        cloneApplicationRepository
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Perform Build"
        echo "-------------"
        runDBBBuild
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - UCD Packaging"
        echo "-------------"
        runUcdPackaging
        rc=$?
        echo "rc=$rc"
    fi

    if [ $rc -eq 0 ]; then
        echo "Package Step - PackageBuildOutputs"
        echo "-------------"
        runPackageBuildOutputs
        rc=$?
        echo "rc=$rc"
    fi

    # load logs even with RC=4 (Warning)
    if [ $rc -eq 4 ]; then
        echo "Pull Logs"
        echo "-------------"
        pullLogs
        rc=$?
        echo "rc=$rc"
    fi

}

#====================================================================================
# Functions representing and controlling the various test scenarios
#====================================================================================

if [ $rc -eq 0 ]; then
    resetWorkspace
    rc=$?
    echo "rc=$rc"
fi

# Main branch pipelines
if [ $rc -eq 0 ]; then
    testMortgageApplication-Main-Bld-Build-0
    if [ $rc -eq 0 ]; then
        testSummary="${testSummary} testMortgageApplication-Main-Bld-Build-0 \t passed , rc=$rc\n"
    else
        testSummary="${testSummary} testMortgageApplication-Main-Bld-Build-0 \t failed , rc=$rc\n"
    fi
fi

if [ $rc -eq 0 ]; then
    testMortgageApplication-Main-Bld-Build-1
    if [ $rc -eq 0 ]; then
        testSummary="${testSummary} testMortgageApplication-Main-Bld-Build-1 \t passed , rc=$rc\n"
    else
        testSummary="${testSummary} testMortgageApplication-Main-Bld-Build-1 \t failed , rc=$rc\n"
    fi
fi

if [ $rc -eq 0 ]; then
    testMortgageApplication-Main-Rel-Build-2
    if [ $rc -eq 0 ]; then
        testSummary="${testSummary} testMortgageApplication-Main-Rel-Build-2 \t passed , rc=$rc\n"
    else
        testSummary="${testSummary} testMortgageApplication-Main-Rel-Build-2 \t failed , rc=$rc\n"
    fi
fi

if [ $rc -eq 0 ]; then
    testMortgageApplication-Main-Prev-Build-3
    if [ $rc -eq 0 ]; then
        testSummary="${testSummary} testMortgageApplication-Main-Prev-Build-3 \t passed , rc=$rc\n"
    else
        testSummary="${testSummary} testMortgageApplication-Main-Prev-Build-3 \t failed , rc=$rc\n"
    fi
fi

# Feature branch pipeline
if [ $rc -eq 0 ]; then
    testMortgageApplication-Feature-Setmainbuildbranch-Build-1
    if [ $rc -eq 0 ]; then
        testSummary="${testSummary} testMortgageApplication-Feature-Setmainbuildbranch-Build-1 \t passed , rc=$rc\n"
    else
        testSummary="${testSummary} testMortgageApplication-Feature-Setmainbuildbranch-Build-1 \t failed , rc=$rc\n"
    fi
fi

# release maintenance pipeline
if [ $rc -eq 0 ]; then
    testMortgageApplication-Release-Rel100-Build-1
    if [ $rc -eq 0 ]; then
        testSummary="${testSummary} testMortgageApplication-Release-Rel100-Build-1 \t passed , rc=$rc\n"
    else
        testSummary="${testSummary} testMortgageApplication-Release-Rel100-Build-1 \t failed , rc=$rc\n"
    fi
fi
# feature branch for release maintenance
if [ $rc -eq 0 ]; then
    testMortgageApplication-Hotfix-Release-Rel100-Build-1
    if [ $rc -eq 0 ]; then
        testSummary="${testSummary} testMortgageApplication-Hotfix-Release-Rel100-Build-1 \t passed , rc=$rc\n"
    else
        testSummary="${testSummary} testMortgageApplication-Hotfix-Release-Rel100-Build-1 \t failed , rc=$rc\n"
    fi
fi

# epic branch
if [ $rc -eq 0 ]; then
    testMortgageApplication-Epic-implementAI-Build-1
    if [ $rc -eq 0 ]; then
        testSummary="${testSummary} testMortgageApplication-Epic-implementAI-Build-1 \t passed , rc=$rc\n"
    else
        testSummary="${testSummary} testMortgageApplication-Epic-implementAI-Build-1 \t failed , rc=$rc\n"
    fi

fi

# epic feature branch
if [ $rc -eq 0 ]; then
    testMortgageApplication-Epic-Feature-implementAI-Build-1
    if [ $rc -eq 0 ]; then
        testSummary="${testSummary} testMortgageApplication-Epic-Feature-implementAI-Build-1 \t passed , rc=$rc\n"
    else
        testSummary="${testSummary} testMortgageApplication-Epic-Feature-implementAI-Build-1 \t failed , rc=$rc\n"
    fi

fi

# print test summary
printf "Test Summary\n"
printf "============\n"
printf "$testSummary"
