# internal variables
mainBranchSegment=""
secondBranchSegment=""
rc=0



computePackageInformation() {
    #############################################
    # output environment variables
    #############################################
    artifactRepositoryName=""           # identifier of the artifact repo
    artifactRepositoryDirectory=""      # root directory folder in repo
    artifactVersionName=""              # subfolder in repo path identifying version / origin branch
    tarFileName=""                      # computed tarFileName how it is stored in the artifact repository
    artifactRepositoryUrl=""            # absolute URL
    #############################################
    
    # configuration variable defining the Artifactory repository name pattern 
    artifactRepositoryRepoPattern="${App}-repo-local"
    
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
            else
                #############################################
                # Conventions for snapshot builds:
                # <artifactRepositoryName>/<artifactRepositoryDirectory>/<branch>/<application>-<buildIdentifier>.tar
                # Mortgage-repo-local/build/feature/123-enhance-something/Mortgage-123456.tar
                #############################################
  
                artifactRepositoryDirectory="build"
                artifactVersionName=${Branch}
                tarFileName="${App}-${buildIdentifier}.tar"
            fi
            ;;
        *)
            #############################################
            ### similar to snapshot builds
            #############################################
            artifactRepositoryDirectory="build"
            artifactVersionName=${Branch}
            tarFileName="${App}-${buildIdentifier}.tar"
            ;;
        esac

        #############################################
        ### Construct the absolute repository URL
        #############################################
        artifactRepositoryUrl="${artifactRepositoryUrl}/${artifactRepositoryName}/${artifactRepositoryDirectory}/${artifactVersionName}/${tarFileName}"

        # unset internal variables
        mainBranchSegment=""
        secondBranchSegment=""
        thirdBranchSegment=""

    fi
}

