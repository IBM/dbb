# internal variables
mainBranchSegment=""
secondBranchSegment=""
rc=0

# Internal method to define release packaging conventions for publishing
getReleasePackageConfiguration() {
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

    # Identifier for the version attribute in Wazi Deploy Application Manifest file
    wdPackageBuildIdentifier="${releaseIdentifier}-${buildIdentifier}"
}

# Internal method to define preliminary/snapshot packaging conventions for publishing

getPreliminaryPackageConfiguration() {
    #############################################
    # Conventions for snapshot builds:
    # <artifactRepositoryName>/<artifactRepositoryDirectory>/<branch>/<application>-<buildIdentifier>.tar
    # Mortgage-repo-local/build/feature/123-enhance-something/Mortgage-123456.tar
    #############################################

    # Preliminary builds are captured in the release directory of the artifact repo
    artifactRepositoryDirectory="build"

    # In packaging phase the branch names defines the artifactVersion
    if [ ! -z "${releaseIdentifier}" ]; then
        artifactVersionName=${releaseIdentifier}
    elif [ ! -z "${Branch}" ]; then
        artifactVersionName=${Branch}
    fi

    # building up the tarFileName
    tarFileName="${App}-${buildIdentifier}.tar"

    # Identifier for the version attribute in Wazi Deploy Application Manifest file
    wdPackageBuildIdentifier="${buildIdentifier}"
}


getArtifactRepositoryName() {

    # configuration variable defining the Artifactory repository name pattern
    artifactRepositoryRepoPattern="${App}-${artifactRepositoryNameSuffix}"
    artifactRepositoryName=$(echo "${artifactRepositoryRepoPattern}")
}

computePackageUrl() {
    # Assemble the absolute artifact repository URL from the variables set by
    # getReleasePackageConfiguration() or getPreliminaryPackageConfiguration().
    #
    # URL pattern:
    #   release: <artifactRepositoryUrl>/<artifactRepositoryName>/release/<releaseIdentifier>/<App>-<releaseIdentifier>-<buildIdentifier>.tar
    #   build:   <artifactRepositoryUrl>/<artifactRepositoryName>/build/<branch>/<App>-<buildIdentifier>.tar

    # First derive the repository name from the application and suffix
    getArtifactRepositoryName

    if [ -z "${artifactRepositoryUrl}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] artifactRepositoryUrl is not configured in pipelineBackend.config. rc="$rc
        echo $ERRMSG
    elif [ -z "${artifactRepositoryName}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] artifactRepositoryName could not be computed. rc="$rc
        echo $ERRMSG
    elif [ -z "${artifactRepositoryDirectory}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] artifactRepositoryDirectory is not set. rc="$rc
        echo $ERRMSG
    elif [ -z "${artifactVersionName}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] artifactVersionName is not set. rc="$rc
        echo $ERRMSG
    elif [ -z "${tarFileName}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] tarFileName is not set. rc="$rc
        echo $ERRMSG
    else
        artifactRepositoryAbsoluteUrl="${artifactRepositoryUrl}/${artifactRepositoryName}/${artifactRepositoryDirectory}/${artifactVersionName}/${tarFileName}"
        echo $PGM": [INFO] Computation of Archive Url completed. Url=${artifactRepositoryAbsoluteUrl} rc=${rc}"
    fi
}

# Method implementing the conventions in the CBS
computeArchiveInformation() {
    #############################################
    # output environment variables
    #############################################
    artifactRepositoryName=""      # identifier of the artifact repo
    artifactRepositoryDirectory="" # root directory folder in repo
    artifactVersionName=""         # subfolder in repo path identifying version / origin branch
    tarFileName=""                 # computed tarFileName how it is stored in the artifact repository
    wdPackageBuildIdentifier=""    # Identifier for the version attribute in Wazi Deploy Application Manifest file
    #############################################

    # call
    getArtifactRepositoryName

    # evaluate conventions based on branch name
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

        # evaluate main segment
        case $mainBranchSegmentTrimmed in
        "MASTER" | "MAIN" | REL*)
            if [ "${PipelineType}" == "release" ]; then
                getReleasePackageConfiguration
            else
                getPreliminaryPackageConfiguration
            fi
            ;;
        *)
            #############################################
            ### similar to snapshot builds
            #############################################
            getPreliminaryPackageConfiguration
            ;;
        esac

        # unset internal variables
        mainBranchSegment=""
        secondBranchSegment=""
        thirdBranchSegment=""

    fi
}

# Method pulling the conventions for the Wazi Deploy generate command to determine the absolute URL of the package

getArchiveLocation() {
    if [ "${PipelineType}" == "release" ]; then
        getReleasePackageConfiguration
    else
        getPreliminaryPackageConfiguration
    fi

    #############################################
    ### Construct the absolute repository URL (required when downloading the package)
    #############################################

    if [ "${computeArchiveUrl}" == "true" ]; then
        computePackageUrl
    fi
}
