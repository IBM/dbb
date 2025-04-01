# internal variables
mainBranchSegment=""
secondBranchSegment=""
rc=0

# Method implementing the conventions in the CBS for packaging using the PackageBuildOutputs.groovy script
computeArchiveInformation() {
    #############################################
    # output environment variables
    #############################################
    artifactRepositoryName=""      # identifier of the artifact repo
    artifactRepositoryDirectory="" # root directory folder in repo
    artifactVersionName=""         # subfolder in repo path identifying version / origin branch
    tarFileName=""                 # computed tarFileName how it is stored in the artifact repository
    archiveIdentifier=""      # Identifier for Wazi Deploy Application Manifest file
    #############################################

    # configuration variable defining the Artifactory repository name pattern
    artifactRepositoryRepoPattern="${App}-${artifactRepositoryNameSuffix}"

    branchConvention=(${Branch//// })

    if [ $rc -eq 0 ]; then
        if [ ${#branchConvention[@]} -gt 3 ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Script is only managing branch name with up to 3 segments (${Branch}) . See recommended naming conventions. rc="$rc
            echo $ERRMSG
        fi
    fi

    if [ $rc -eq 0 ]; then

        # split the segments
        mainBranchSegment=$(echo ${Branch} | awk -F "/" ' { print $1 }')
        secondBranchSegment=$(echo ${Branch} | awk -F "/" ' { print $2 }')
        thirdBranchSegment=$(echo ${Branch} | awk -F "/" ' { print $3 }')

        # remove chars (. -) from the name
        mainBranchSegmentTrimmed=$(echo ${mainBranchSegment} | tr -d '.-' | tr '[:lower:]' '[:upper:]')

        artifactRepositoryName=$(echo "${artifactRepositoryRepoPattern}")

        # evaluate main segment
        case $mainBranchSegmentTrimmed in
        "MASTER" | "MAIN" | REL*)
            if [ "${PipelineType}" == "release" ]; then
                #############################################
                # Conventions for release builds:
                # <artifactRepositoryName>/<artifactRepositoryDirectory>/<releaseIdentifier>/<application>-<releaseIdentifier>-<buildIdentifier>.tar
                # MortgageApplication-repo-local/release/1.2.3/MortgageApplication-1.2.3-1234567890.tar
                #############################################

                # Release builds are captured in the release directory of the artifact repo
                artifactRepositoryDirectory="release"

                # artifactVersionName is second identifier in the folder structure and represents the
                artifactVersionName=${releaseIdentifier}

                # building up the tarFileName
                tarFileName="${App}-${releaseIdentifier}-${buildIdentifier}.tar"
                archiveIdentifier="${releaseIdentifier}-${buildIdentifier}"
            else
                #############################################
                # Conventions for snapshot builds:
                # <artifactRepositoryName>/<artifactRepositoryDirectory>/<branch>/<application>-<buildIdentifier>.tar
                # Mortgage-repo-local/build/feature/123-enhance-something/Mortgage-123456.tar
                #############################################

                artifactRepositoryDirectory="build"
                artifactVersionName=${Branch}
                tarFileName="${App}-${buildIdentifier}.tar"
                archiveIdentifier="${buildIdentifier}"
            fi
            ;;
        *)
            #############################################
            ### similar to snapshot builds
            #############################################
            artifactRepositoryDirectory="build"
            artifactVersionName=${Branch}
            tarFileName="${App}-${buildIdentifier}.tar"
            archiveIdentifier="${buildIdentifier}"
            ;;
        esac

        #############################################
        ### Construct the absolute repository URL (required when downloading the package)
        #############################################

        if [ "${computeArchiveUrl}" == "true" ]; then

            artifactRepositoryHelpersScript="${SCRIPT_HOME}/../../Pipeline/PackageBuildOutputs/ArtifactRepositoryHelpers.groovy"

            # validate options
            if [ ! -f "${artifactRepositoryHelpersScript}" ]; then
                rc=8
                ERRMSG=$PGM": [ERROR] Unable to locate ${artifactRepositoryHelpersScript}. rc="$rc
                echo $ERRMSG
            fi

            #
            # Invoke the Package Build Outputs script
            if [ $rc -eq 0 ]; then
                echo $PGM": [INFO] Invoking the ArtifactRepositoryHelper groovy script to compute Package Url."

                CMD="$DBB_HOME/bin/groovyz ${log4j2} ${artifactRepositoryHelpersScript} --computeArchiveUrl"

                # add tarfile name
                if [ ! -z "${tarFileName}" ]; then
                    CMD="${CMD} --tarFileName ${tarFileName}"
                fi

                # artifactVersionName
                if [ ! -z "${artifactVersionName}" ]; then
                    CMD="${CMD} --versionName ${artifactVersionName}"
                fi

                # Artifact repo options
                if [ ! -z "${artifactRepositoryUrl}" ]; then
                    CMD="${CMD} --artifactRepositoryUrl \"${artifactRepositoryUrl}\""
                fi

                if [ ! -z "${artifactRepositoryName}" ]; then
                    CMD="${CMD} --artifactRepositoryName ${artifactRepositoryName}"
                fi
                if [ ! -z "${artifactRepositoryDirectory}" ]; then
                    CMD="${CMD} --artifactRepositoryDirectory ${artifactRepositoryDirectory}"
                fi

                echo $PGM": [INFO] ${CMD}"
                artifactRepositoryAbsoluteUrl=$(${CMD} | grep "packageUrl=" |  awk -F "=" ' { print $2 }')

                if [ ! -z "${artifactRepositoryAbsoluteUrl}" ]; then
                    ERRMSG=$PGM": [INFO] Computation of Archive Url completed. rc="$rc
                    echo $ERRMSG
                else
                    rc=12
                    ERRMSG=$PGM": [ERR] Computation of Archive Url failed. Check Console for details. rc="$rc
                    echo $ERRMSG

                fi
            fi

        fi

        # unset internal variables
        mainBranchSegment=""
        secondBranchSegment=""
        thirdBranchSegment=""

    fi
}
