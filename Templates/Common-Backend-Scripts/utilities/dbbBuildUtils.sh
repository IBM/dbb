# internal veriables
mainBranchSegment=""
secondBranchSegment=""
baselineReferenceFile=""
segmentName=""
mergeBaseCommit=""
baseBranch=""

computeBuildConfiguration() {

    # unset variables
    HLQ=""
    propOverrides=""

    ### computes the following environment variables for the dbbBuild.sh script
    # Type - the build type, e.q. --impactBuild --baselineRef release/rel-1.1.4
    # HLQ - the high level qualifier to use
    # propOverrides - zAppBuild property overrides, e.q. "mainBuildBranch=release/rel-1.1.4"

    ##DEBUG ## echo -e "App name \t: ${App}"
    ##DEBUG ## echo -e "Branch name \t: ${Branch}"

    # Compute HLQ preix and application name
    HLQ=$(echo ${HLQPrefix}.${App:0:8} | tr '[:lower:]' '[:upper:]' | tr -d '-')

    # Locate the baseline reference file based on the baselineReferenceLocation config in pipelineBackend.config
    baselineReferenceFile="${AppDir}/$baselineReferenceLocation"

    if [ ! -f "${baselineReferenceFile}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Applications baseline reference configuration file (${baselineReferenceFile}) was not found. rc="$rc
        echo $ERRMSG
    fi

    branchConvention=(${Branch//// })

    if [ $rc -eq 0 ]; then
        if [ ${#branchConvention[@]} -gt 3 ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Script is only managing branch name with up to 3 segments (${Branch}) . rc="$rc
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
        REL* | EPIC* | PROJ*)
            # Release maintenance, epic and project branches are integration branches,
            # that derive dependency information from mainBuildBranch configuration.

            # evaluate third segment
            if [ ! -z "${thirdBranchSegment}" ]; then
                rc=8
                ERRMSG=$PGM": [ERROR] Branch (${Branch}) does not follow standard naming conventions  . rc="$rc
                echo $ERRMSG
            else
                # feature branches for next planned release
                computeSegmentName $secondBranchSegment
                HLQ="${HLQ}.${mainBranchSegmentTrimmed:0:1}${segmentName:0:7}"
            fi

            if [ -z "${Type}" ]; then
                Type="--impactBuild"
# obtain the baselineRef from file
                getBaselineReference
                Type="${Type} --baselineRef ${baselineRef}"
                # Release maintenance / epic / project branch clones the dependency information from the main build branch
                # propOverrides="mainBuildBranch=${mainBranchSegment}"
                # appending the --debug flag to compile with TEST options
                Type="${Type} --debug"
            fi
            ;;
        FEATURE*)
            # all feature branche start with F

            # evaluate third segment
            if [ ! -z "${thirdBranchSegment}" ]; then
                # feature branches for EPIC workflow
                
                # // skipped for simplicity
                # computeSegmentName $secondBranchSegment
                # HLQ="${HLQ}.E${segmentName:0:7}"
                computeSegmentName $thirdBranchSegment
                HLQ="${HLQ}.F${segmentName:0:7}"
                
                propOverrides="mainBuildBranch=epic/${secondBranchSegment}"

            else
                # feature branches for next planned release
                computeSegmentName $secondBranchSegment
                HLQ="${HLQ}.F${segmentName:0:7}"
            fi

            if [ -z "${Type}" ]; then
                Type="--impactBuild"
                # appending the --debug flag to compile with TEST options
                Type="${Type} --debug"

                ## evaluate the feature branch build behaviour
                # compute the base branchName
                    if [ ! -z "${thirdBranchSegment}" ]; then
                        # epic branch workflow
                    baseBranch="origin/epic/${secondBranchSegment}"
                    else 
                        # default dev workflow
                    baseBranch="origin/main"
                fi

                # assess the featureBranchBuildBehaviour setting
                case $featureBranchBuildBehaviour in
                cumulative)
                    Type="${Type} --baselineRef $baseBranch"
                    ;;
                merge-base)
                    getMergeBaseCommit
                    Type="${Type} --baselineRef $mergeBaseCommit"
                    ;;
                *)
                    ## nothing to do
                    ;;
                esac

            fi
            ;;
        HOTFIX*)

            # evaluate third segment
            if [ ! -z "${thirdBranchSegment}" ]; then
                # feature branches for hotfix workflow

                # // skipped for simplicity
                # computeSegmentName $secondBranchSegment
                # HLQ="${HLQ}.R${segmentName:0:7}"
                computeSegmentName $thirdBranchSegment
                HLQ="${HLQ}.H${segmentName:0:7}"
            else
                rc=8
                ERRMSG=$PGM": [ERROR] Hotfix branch (${Branch}) does not follow naming conventions  . rc="$rc
                echo $ERRMSG
            fi

            if [ -z "${Type}" ]; then
                Type="--impactBuild"
                propOverrides="mainBuildBranch=release/${secondBranchSegment}"

                # evaluate the feature branch build behaviour
                    if [ ! -z "${thirdBranchSegment}" ]; then
                        # define baseline reference
                    baseBranch="origin/release/${secondBranchSegment}"
                else
                    echo $PGM": [WARNING] [Utilities/dbbBuildUtils.sh/computeBuildConfiguration] The hotfix branch name (${Branch}) does not match any case of the recommended naming conventions for branches. Performing an impact build."
                    echo $PGM":            Read about our recommended naming conventions at https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#naming-conventions ."
                fi

                # assess the featureBranchBuildBehaviour setting
                case $featureBranchBuildBehaviour in
                cumulative)
                    Type="${Type} --baselineRef $baseBranch"
                    ;;
                merge-base)
                    getMergeBaseCommit
                    Type="${Type} --baselineRef $mergeBaseCommit"
                    ;;
                *)
                    ## nothing to do
                    ;;
                esac

            fi
            ;;
        "PROD" | "MASTER" | "MAIN")
            getBaselineReference
            if [ -z "${Type}" ]; then
                Type="--impactBuild"
                Type="${Type} --baselineRef ${baselineRef}"
            fi
            if [ "${PipelineType}" == "release" ]; then
                HLQ="${HLQ}.${mainBranchSegmentTrimmed:0:8}.REL"
            else
                HLQ="${HLQ}.${mainBranchSegmentTrimmed:0:8}.BLD"
                # appending the --debug flag to compile with TEST options
                Type="${Type} --debug"
            fi

            ;;
        *)
            # User did not follow the recommended naming conventions for branches. The branch name does not match any case of the recommended naming conventions.
            # See https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#naming-conventions 
            HLQ="${HLQ}.${mainBranchSegmentTrimmed:0:8}"
            if [ -z "${Type}" ]; then
                Type="--impactBuild"
                # appending the --debug flag to compile with TEST options
                Type="${Type} --debug"
            fi
            echo $PGM": [WARNING] [Utilities/dbbBuildUtils.sh/computeBuildConfiguration] The branch name (${Branch}) does not match any case of the recommended naming conventions for branches. Performing an impact build."
            echo $PGM":            Read about our recommended naming conventions at https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#naming-conventions ."
            ;;
        esac

        # append pipeline preview if specified
        if [ "${PipelineType}" == "preview" ]; then
            if [ -z "${userDefinedBuildType}" ]; then
                Type="${Type} --preview"
            fi
        fi

        # print computed values
        ##DEBUG ## echo -e "Computed hlq \t: ${HLQ}"
        ##DEBUG ## echo -e "Build option \t: ${Type}"

        # unset internal variables
        baselineRef=""
        mainBranchSegment=""
        mainBranchSegmentTrimmed=""
        secondBranchSegment=""
        secondBranchSegmentTrimmed=""
        thirdBranchSegment=""
        thirdBranchSegmentTrimmed=""
        branchConvention=""
        segmentName=""
        mergeBaseCommit=""
        baseBranch=""

    fi
}

# Private method to retrieve the baseline reference from the configuration file

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

# Private method to retrive the merge-base as the baseline reference
# Requires the baseBranch to be computed

getMergeBaseCommit() {

    # Execute Git cmd to obtain merge-base
    if [ -z "${baseBranch}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] To compute the merge base commit, it requires to define the baseBranch variable. rc="$rc
        echo $ERRMSG
    fi

    # Execute Git cmd to obtain merge-base
    CMD="git -C ${AppDir} merge-base ${Branch} ${baseBranch}"
    mergeBaseCommit=$($CMD)
    rc=$?

    if [ $rc -ne 0 ]; then
        ERRMSG=$PGM": [ERROR] Command ($CMD) failed. Git command to obtain the merge base commit failed for feature branch ${Branch}. See above error log. rc="$rc
        echo $ERRMSG
    fi

    if [ $rc -eq 0 ]; then

        if [ -z "${mergeBaseCommit}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Computation of Merge base commit failed for feature branch ${Branch}. rc="$rc
            echo $ERRMSG
        fi
    fi

}

#
# computation of branch segments
# captured cases
#
# - containing numbers, assuming to be an work-item-id
# - containing strings and words separated by dashes, return first characters of each string
# - none of the above - return segment name in upper case w/o underscores
#

computeSegmentName() {

    segmentName=$1
    if [ ! -z $(echo "$segmentName" | tr -dc '0-9') ]; then
        # "contains numbers"
        retval=$(echo "$segmentName" | tr -dc '0-9')
    elif [[ $segmentName == *"-"* ]]; then
        # contains dashes
        segmentNameTrimmed=$(echo "$segmentName" | awk -F "-" '{ for(i=1; i <= NF;i++) print($i) }' | cut -c 1-1)
        segment1=$(echo "$segmentNameTrimmed" | tr -d '\n')
        retval=$(echo "$segment1" | tr '[:lower:]' '[:upper:]')
    else 
        retval=$(echo "$segmentName" | tr -d '_' | tr '[:lower:]' '[:upper:]')
    fi
    segmentName=$(echo "$retval")
}
