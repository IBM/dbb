#!/usr/bin/env bash


source packageUtils.sh

source dbbBuildUtils.sh

App=MortgageApplication
Branch=feature/42-new-mortgage-calculation
PipelineType=release
artifactVersionName=build.123.123.123
rc=0

artifactRepositoryRepoPattern="${App}-repo-local"
artifactRepositoryRepoDirectoryPatternFeatureBuilds="${Branch}"
artifactRepositoryRepoDirectoryPatternReleaseBuilds="${Branch}/release"
artifactRepositoryRepoDirectoryPatternDevelopmentBuilds="${Branch}/build"
artifactRepositoryUrl=http://10.3.20.231:8081/artifactory



computePackageInformation

echo $artifactRepositoryAbsoluteUrl
echo $artifactRepositoryName
echo $artifactRepositoryDirectory

echo "###"

App=MortgageApplication
Branch=main
PipelineType=release
artifactVersionName=rel-1.0.0-build.123.123.123
rc=0



artifactRepositoryRepoPattern="${App}-repo-local"
artifactRepositoryRepoDirectoryPatternFeatureBuilds="${Branch}"
artifactRepositoryRepoDirectoryPatternReleaseBuilds="${Branch}/release"
artifactRepositoryRepoDirectoryPatternDevelopmentBuilds="${Branch}/build"
artifactRepositoryUrl=http://10.3.20.231:8081/artifactory



computePackageInformation

echo $artifactRepositoryAbsoluteUrl
echo $artifactRepositoryName
echo $artifactRepositoryDirectory

exit

##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=feature/42-new-mortgage-calculation
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Type
echo $propOverrides
echo "####"

##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=feature/new-mortgage-calculation
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Type
echo $propOverrides
echo "####"

##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=feature/new-mortgage-calculation
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Typee
echo "####"

##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=feature/new_mortgage_calculation
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Type
echo $propOverrides
echo "####"

##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=feature/newmortgagecalculation
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Type
echo $propOverrides
echo "####"


##### 

##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=feature/ai-fraud-detection/54-introduce-ai-model-to-mortgage-calculation
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Type
echo $propOverrides
echo "####"




##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=hotfix/rel-2.0.1/52-fix-mortgage-calculation
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Type
echo $propOverrides
echo "####"

##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=release/rel-2.0.1
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Type
echo $propOverrides
echo "####"

##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=release/rel-2.0.1/blabla
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Type
echo $propOverrides
echo "####"

##### 
HLQPrefix=PIPELINE
App=MortgageApplication
Branch=epic/ai-fraud-detection
rc=0

computeBuildConfiguration

echo $Branch
echo $HLQ
echo $Type
echo $propOverrides
echo "####"