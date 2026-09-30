# internal variables
mainBranchSegment=""
secondBranchSegment=""
baselineReferenceFile=""
segmentName=""
mergeBaseCommit=""
baseBranch=""
topicBranchBehaviour=""

computeBuildConfiguration() {

    # unset variables
    HLQ=""

    ### computes the following environment variables for the zBuilder.sh script
    # Lifecycle               - the zBuilder lifecycle configuration,
    #                           e.q. impact --baselineRef release/rel-1.1.4
    # HLQ                     - the high level qualifier to use
    # zBuilderConfigOverrides - Config file containing variable overrides
    #                           e.q. mainBuildBranch

    ##DEBUG ## echo -e "App name \t: ${App}"
    ##DEBUG ## echo -e "Branch name \t: ${Branch}"

    # Compute HLQ prefix and application name
    HLQ=$(echo ${HLQPrefix}.${App:0:8} | tr '[:lower:]' '[:upper:]' | tr -d '-')

    # Locate the baseline reference file based on the baselineReferenceLocation config in pipelineBackend.config
    baselineReferenceFile="${AppDir}/${baselineReferenceLocation}"

    if [ ! -f "${baselineReferenceFile}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] Application baseline reference file (${baselineReferenceFile}) was not found. rc="$rc
        echo $ERRMSG
    fi

    # Read topic-branch-behaviour from baselineRef.yaml
    if [ $rc -eq 0 ]; then
        topicBranchBehaviour=$(catBaselineRefFile "${baselineReferenceFile}" | grep "^topic-branch-behaviour:" | awk -F ': ' '{ print $2 }' | tr -d '"' | tr -d "'" | tr -d '\r' | xargs)
        if [ -z "${topicBranchBehaviour}" ]; then
            topicBranchBehaviour="merge-base"
            echo $PGM": [INFO] topic-branch-behaviour not set in ${baselineReferenceFile}. Defaulting to: ${topicBranchBehaviour}"
        fi
    fi

    branchConvention=(${Branch//// })

    if [ $rc -eq 0 ]; then
        if [ ${#branchConvention[@]} -gt 3 ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Script is only managing branch names with up to 3 segments (${Branch}). rc="$rc
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
            # Release maintenance, epic and project branches are integration branches.

            # evaluate third segment
            if [ ! -z "${thirdBranchSegment}" ]; then
                rc=8
                ERRMSG=$PGM": [ERROR] Branch (${Branch}) does not follow standard naming conventions. rc="$rc
                echo $ERRMSG
            else
                computeSegmentName $secondBranchSegment
                HLQ="${HLQ}.${mainBranchSegmentTrimmed:0:1}${segmentName:0:7}"
            fi

            if [ -z "${Lifecycle}" ]; then
                Lifecycle="impact"
                getBaselineReference
                Lifecycle="${Lifecycle} --baselineRef ${baselineRef}"
            fi
            ;;
        FEATURE*)
            # All feature branches start with feature/

            # evaluate third segment
            if [ ! -z "${thirdBranchSegment}" ]; then
                # feature branch in an epic workflow: feature/<epic-name>/<feature-name>
                computeSegmentName $thirdBranchSegment
                HLQ="${HLQ}.F${segmentName:0:7}"
                writezBuilderOverride "epic/${secondBranchSegment}"
            else
                # feature branch targeting main
                computeSegmentName $secondBranchSegment
                HLQ="${HLQ}.F${segmentName:0:7}"
            fi

            if [ -z "${Lifecycle}" ]; then
                Lifecycle="impact"

                # compute the base branch for merge-base / cumulative behaviour
                if [ ! -z "${thirdBranchSegment}" ]; then
                    # epic branch workflow
                    baseBranch="origin/epic/${secondBranchSegment}"
                else
                    # default dev workflow
                    baseBranch="origin/main"
                fi

                # assess the topic-branch-behaviour setting from baselineRef.yaml
                case $topicBranchBehaviour in
                cumulative)
                    Lifecycle="${Lifecycle} --baselineRef ${baseBranch}"
                    ;;
                merge-base)
                    getMergeBaseCommit
                    Lifecycle="${Lifecycle} --baselineRef ${mergeBaseCommit}"
                    ;;
                *)
                    ## incremental: no --baselineRef; DBB uses the last successful build record
                    ;;
                esac

            fi
            ;;
        HOTFIX*)

            # evaluate third segment - hotfix branches must be: hotfix/<release>/<name>
            if [ ! -z "${thirdBranchSegment}" ]; then
                # hotfix branch: hotfix/<release-name>/<fix-name>
                computeSegmentName $thirdBranchSegment
                HLQ="${HLQ}.H${segmentName:0:7}"
            else
                rc=8
                ERRMSG=$PGM": [ERROR] Hotfix branch (${Branch}) does not follow naming conventions. rc="$rc
                echo $ERRMSG
            fi

            if [ -z "${Lifecycle}" ]; then
                Lifecycle="impact"
                writezBuilderOverride "release/${secondBranchSegment}"

                if [ ! -z "${thirdBranchSegment}" ]; then
                    baseBranch="origin/release/${secondBranchSegment}"
                else
                    echo $PGM": [WARNING] [dbbzBuilderUtils.sh/computeBuildConfiguration] The hotfix branch (${Branch}) does not match the recommended naming conventions. Performing an impact build."
                    echo $PGM":            Read about our recommended naming conventions at https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#naming-conventions ."
                fi

                # assess the topic-branch-behaviour setting from baselineRef.yaml
                case $topicBranchBehaviour in
                cumulative)
                    Lifecycle="${Lifecycle} --baselineRef ${baseBranch}"
                    ;;
                merge-base)
                    getMergeBaseCommit
                    Lifecycle="${Lifecycle} --baselineRef ${mergeBaseCommit}"
                    ;;
                *)
                    ## incremental: no --baselineRef
                    ;;
                esac

            fi
            ;;
        "PROD" | "MASTER" | "MAIN")
            getBaselineReference
            if [ -z "${Lifecycle}" ]; then
                if [ "${PipelineType}" == "release" ]; then
                    Lifecycle="release"
                else
                    Lifecycle="impact"
                fi
                Lifecycle="${Lifecycle} --baselineRef ${baselineRef}"
            fi
            if [ "${PipelineType}" == "release" ]; then
                HLQ="${HLQ}.${mainBranchSegmentTrimmed:0:8}.REL"
            else
                HLQ="${HLQ}.${mainBranchSegmentTrimmed:0:8}.BLD"
            fi
            ;;
        *)
            # Branch name does not match any recommended naming convention.
            # See https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#naming-conventions
            rc=12
            echo $PGM": [ERROR] [dbbzBuilderUtils.sh/computeBuildConfiguration] The branch name (${Branch}) does not match any recommended naming convention. rc="$rc
            echo $PGM":            Read about our recommended naming conventions at https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#naming-conventions ."
            ;;
        esac

        # append pipeline preview flag if specified
        if [ "${PipelineType}" == "preview" ]; then
            if [ -z "${userDefinedLifecycle}" ]; then
                Lifecycle="${Lifecycle} --preview"
            fi
        fi

        ##DEBUG ## echo -e "Computed hlq \t: ${HLQ}"
        ##DEBUG ## echo -e "Build option \t: ${Lifecycle}"

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
        topicBranchBehaviour=""

    fi
}

# Private method to retrieve the baseline reference from baselineRef.yaml
# Reads the value for the current branch from the long-lived-branches block.
# Requires: mainBranchSegment, secondBranchSegment, baselineReferenceFile

getBaselineReference() {

    baselineRef=""

    case $(echo $mainBranchSegment | tr '[:lower:]' '[:upper:]') in
        "RELEASE" | "EPIC" | "PROJ")
            # Lookup: "  release/rel-x.y.z: ..." or "  epic/<name>: ..."
            baselineRef=$(catBaselineRefFile "${baselineReferenceFile}" | grep "^[[:space:]]\{1,\}${mainBranchSegment}/${secondBranchSegment}:" | awk -F ': ' '{ print $2 }' | tr -d '"' | tr -d "'" | tr -d '\r' | xargs)
            ;;
        "MAIN" | "MASTER" | "PROD")
            # Lookup: "  main: ..."
            baselineRef=$(catBaselineRefFile "${baselineReferenceFile}" | grep "^[[:space:]]\{1,\}${mainBranchSegment}:" | awk -F ': ' '{ print $2 }' | tr -d '"' | tr -d "'" | tr -d '\r' | xargs)
            ;;
        *)
            rc=8
            ERRMSG=$PGM": [ERROR] Branch name ${Branch} does not follow the recommended naming conventions to compute the baseline reference. Received '${mainBranchSegment}' which does not match main, release, epic, or proj. rc="$rc
            echo $ERRMSG
            ;;
    esac

    if [ -z "${baselineRef}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] No baseline ref was found for branch '${Branch}' in ${baselineReferenceFile}. rc="$rc
        echo $ERRMSG
    fi

    ##DEBUG ## echo -e "baselineRef \t: ${baselineRef}"
}

# Private method to retrieve the merge-base commit as the baseline reference.
# Requires: baseBranch to be set before calling.

getMergeBaseCommit() {

    if [ -z "${baseBranch}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] To compute the merge base commit, baseBranch must be set. rc="$rc
        echo $ERRMSG
    fi

    CMD="git -C ${AppDir} merge-base ${Branch} ${baseBranch}"
    mergeBaseCommit=$($CMD)
    rc=$?

    if [ $rc -ne 0 ]; then
        ERRMSG=$PGM": [ERROR] Command ($CMD) failed. Git command to obtain the merge base commit failed for branch ${Branch}. See above error log. rc="$rc
        echo $ERRMSG
    fi

    if [ $rc -eq 0 ]; then
        if [ -z "${mergeBaseCommit}" ]; then
            rc=8
            ERRMSG=$PGM": [ERROR] Computation of merge base commit failed for branch ${Branch}. rc="$rc
            echo $ERRMSG
        fi
    fi

}

#
# computeSegmentName
#  Computes a short uppercase segment identifier from a branch name fragment.
#  Captured cases:
#  - contains numbers only         -> keep only digits (work item ID)
#  - contains dashes               -> first character of each dash-separated word
#  - otherwise                     -> uppercase, strip underscores
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

#
# writezBuilderOverride
#  Writes a temporary override YAML to the pipeline log directory.
#  Overrides the mainBuildBranch variable for the MetadataInit task so that
#  the DBB metadata store uses the correct integration branch for dependency
#  lookups on feature/<epic>/* and hotfix/* branches.
#

writezBuilderOverride() {

    mainBuildBranch=$1
    echo "version: 1.0.0" > $zBuilderConfigOverrides
    echo "application:" >> $zBuilderConfigOverrides
    echo "  name: ${App}" >> $zBuilderConfigOverrides
    echo "  tasks:" >> $zBuilderConfigOverrides
    echo "  # override mainBuildBranch" >> $zBuilderConfigOverrides
    echo "  - task: MetadataInit" >> $zBuilderConfigOverrides
    echo "    variables:" >> $zBuilderConfigOverrides
    echo "       - name: mainBuildBranch" >> $zBuilderConfigOverrides
    echo "         value: ${mainBuildBranch}" >> $zBuilderConfigOverrides
    ## debug #cat $zBuilderConfigOverrides
}
