#!/usr/bin/env bash

# internal variables
mainBranchSegment=""
secondBranchSegment=""
rc=0

# outputs
artifactRepositoryAbsoluteUrl=""
artifactRepositoryName=""
artifactRepositoryDirectory=""

computePackageInformation() {

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
                artifactRepositoryDirectory=$(echo ${artifactRepositoryRepoDirectoryPatternReleaseBuilds})

            else
               artifactRepositoryDirectory=$(echo ${artifactRepositoryRepoDirectoryPatternSnapshotBuilds})
            fi
            ;;
        *)
            artifactRepositoryDirectory=$(echo ${artifactRepositoryRepoDirectoryPatternSnapshotBuilds})
            ;;
        esac

        # Fixed convention
        tarFileName="${App}-${artifactVersionName}.tar"

        artifactRepositoryAbsoluteUrl="${artifactRepositoryUrl}/${artifactRepositoryName}/${artifactRepositoryDirectory}/${artifactVersionName}/${tarFileName}"

        # unset internal variables
        mainBranchSegment=""
        secondBranchSegment=""
        thirdBranchSegment=""

    fi
}



if [ $rc -eq 0 ]; then
    while getopts ":a:b:p:v:" opt; do
        case $opt in
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
                ERRMSG=$PGM": [WARNING] Name of the git branch is required. rc="$rc
                echo $ERRMSG
                break
            fi
            Branch="$argument"
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
            argument="$OPTARG"
            nextchar="$(expr substr $argument 1 1)"
            if [ -z "$argument" ] || [ "$nextchar" = "-" ]; then
                rc=4
                ERRMSG=$PGM": [WARNING] The name of the artifact version in Artifact repository is required. rc="$rc
                echo $ERRMSG
                break
            fi
            artifactVersionName="$argument"
        ;;
        :)
            rc=4
            ERRMSG=$PGM": [WARNING] Option -$OPTARG requires an argument. rc="$rc
            echo $ERRMSG
            break
        ;;
        esac
    done

    # Invoke package configuration
    if [ ! -z "${artifactRepositoryUrl}" ] && [ ! -z "${App}" ] && [ ! -z "${artifactVersionName}" ] && [ ! -z "${PipelineType}" ] && [ ! -z "${Branch}" ]; then
        computePackageInformation
        echo $artifactRepositoryAbsoluteUrl
        #echo $artifactRepositoryName
        #echo $artifactRepositoryDirectory
    fi

fi