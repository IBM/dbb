# internal veriables
mainBranchSegment=""
secondBranchSegment=""
baselineReferenceFile=""

computeBuildConfiguration() {

    ##DEBUG ## echo -e "App name \t: ${App}"
    ##DEBUG ## echo -e "Branch name \t: ${Branch}"

    # reset variables
    propOverrides=""

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
            ERRMSG=$PGM": [ERROR] Script is only managing branch name with 3 segments (${Branch}) . rc="$rc
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
        secondBranchSegmentTrimmed=$(echo ${secondBranchSegment} | tr -d '.-' | tr '[:lower:]' '[:upper:]')
        thirdBranchSegmentTrimmed=$(echo ${thirdBranchSegment} | tr -d '.-' | tr '[:lower:]' '[:upper:]')

        # evaluate main segment
        case $mainBranchSegmentTrimmed in
        REL* | EPIC* | PROJ*)
            # Release maintenance, epic and project branches are intergration branches,
            # that derive dependency information from mainBuildBranch configuration.
            HLQ="${HLQ}.${mainBranchSegmentTrimmed:0:8}"
            getBaselineReference
            if [ -z "${Type}" ]; then
                Type="--impactBuild"
                Type="${Type} --baselineRef ${baselineRef}"
                # Release maintenance / epic / project branch clones the dependency information from the main build branch
                # propOverrides="mainBuildBranch=${mainBranchSegment}"
                # appending the --debug flag to compile with TEST options
                Type="${Type} --debug"
            fi
            ;;
        FEATURE*)
            HLQ="${HLQ}.FEATURE"
            if [ -z "${Type}" ]; then
                Type="--impactBuild"
                # appending the --debug flag to compile with TEST options
                Type="${Type} --debug"
            fi
            ;;
        HOTFIX*)
            HLQ="${HLQ}.HOTFIX"
            if [ -z "${Type}" ]; then
                Type="--impactBuild"
                propOverrides="mainBuildBranch=release/${secondBranchSegment}"
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
                if [ -z "${Type}" ]; then
                    # appending the --debug flag to compile with TEST options
                    Type="${Type} --debug"
                fi
            fi

            #Type="--fullBuild" # init build environment
            ;;
        *)
            # Treat other branches as feature branch.
            # Assuming <initiative>/<feature> as naming convention
            # Comment: We might want to verify that the epic branch exists.

            HLQ="${HLQ}.${mainBranchSegmentTrimmed:0:8}"
            if [ -z "${Type}" ]; then
                Type="--impactBuild"
                # appending the --debug flag to compile with TEST options
                Type="${Type} --debug"
            fi
            # override the mainBuildBranch to clone the dependency collection for the initative.
            propOverrides="mainBuildBranch=epic/${mainBranchSegment}"

            ;;

            # rc=8
            # ERRMSG=$PGM": [ERROR] [Utilities/dbbBuildUtils.sh(computeBuildConfiguration())] No build conventions/rules for branch name (${Branch}) have been defined. rc="$rc
            # echo $ERRMSG
            # ;;
        esac

        # evaluate second segment
        if [ ! -z "${secondBranchSegment}" ]; then
            HLQ="${HLQ}.${secondBranchSegmentTrimmed:0:8}"
        fi

        # evaluate third segment
        if [ ! -z "${thirdBranchSegment}" ]; then
            HLQ="${HLQ}.${thirdBranchSegmentTrimmed:0:8}"
        fi

        # append pipeline preview if specified
        if [ "${PipelineType}" == "preview" ]; then
            if [ -z "${Type}" ]; then
                Type="${Type} --preview"
            fi
        fi

        # print computed values
        ##DEBUG ## echo -e "Computed hlq \t: ${HLQ}"
        ##DEBUG ## echo -e "Build option \t: ${Type}"

        # unset variables
        baselineRef=""
        mainBranchSegment=""
        mainBranchSegmentTrimmed=""
        secondBranchSegment=""
        secondBranchSegmentTrimmed=""
        thirdBranchSegment=""
        thirdBranchSegmentTrimmed=""
        branchConvention=""

    fi

}

# Private method to retrieve the baseline reference from the configuration file

getBaselineReference() {

    baselineRef=$(cat "${baselineReferenceFile}" | grep "^${mainBranchSegment}" | awk -F "=" ' { print $2 }')

    if [ -z "${baselineRef}" ]; then
        rc=8
        ERRMSG=$PGM": [ERROR] No baseline ref was found for branch name ${Branch} in ${baselineReferenceFile}. rc="$rc
        echo $ERRMSG
    fi

    ##DEBUG ## echo -e "baselineRef \t: ${baselineRef}"    ## DEBUG
}
