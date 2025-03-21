# Contents

This folder contains utility scripts, that are sourced by the primary common backend scripts.

There is one exception that can be used standalone 

## computePackageUrl.sh

Script to compute the url where a package got stored in the Artifact repository. 

### Invocation

The `computePackageUrl.sh` script can be invoked as follows:

```
utilities/computePackageUrl.sh -w MortgageApplication/feature/15-update-mortgage-application/build_7645 -a MortgageApplication -b feature/15-update-mortgage-application -p build -i 7645
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. 
-a `<application>` | **Application name** leveraged to define the artifact repository name. 
-b `<branch>`| Name of the **git branch** turning into a segment of the directory path in the artifact repository. Naming convention rules are implemented in `utilities/packageUtils.sh`.
-p `<build/release>` | **Pipeline type** to indicate a `build` pipeline (build only with test/debug options) or a `release` pipeline (build for  optimized load modules) to determine the directory in the artifact repository for development and pipeline builds.
-r `<releaseIdentifier>` | **Release identifier** to indicate the next planned release name. This is a computed value based on the pipeline templates.
-i `<buildIdentifier>` | **Build identifier** a unique value to identify the tar file. This is a computed value provided by the pipeline templates. Typically the build number or a timestamp. 

On successful completion the below message it displayed, and a file containing the information is stored the workspace log directory:
```
computePackageUrl.sh: [INFO] Compute Package Url completed. Results stored in /u/ibmuser/git/workspace_test/MortgageApplication/feature/15-update-mortgage-application/build_7645/logs/packageVersionConfig.txt. rc=0
```
packageVersionConfig.txt:
```
packageUrl=http://10.3.20.231:8081/artifactory/MortgageApplication-repo-local/build/feature/15-update-mortgage-application/MortgageApplication-7645.tar
```


### Script output

The section below contains the output that is produced by the `utilities/computePackageUrl.sh` script.

<details>
  <summary>Script Output</summary>

```
/u/ibmuser/git/dbb/Templates/Common-Backend-Scripts/utilities/computePackageUrl.sh -w MortgageApplication/feature/15-update-mortgage-application/build_7645 -a MortgageApplication -b feature/15-update-mortgage-application -p build -i 7645
computePackageUrl.sh: [INFO] Compute Package Url. Version=1.00
computePackageUrl.sh: [INFO] **************************************************************
computePackageUrl.sh: [INFO] ** Started - Compute Package Url on HOST/USER: z/OS ZT01 05.00 02 8561/BPXROOT
computePackageUrl.sh: [INFO] **              Application: MortgageApplication
computePackageUrl.sh: [INFO] **                   Branch: feature/15-update-mortgage-application
computePackageUrl.sh: [INFO] **         Type of pipeline: build
computePackageUrl.sh: [INFO] **            Tar file Name: MortgageApplication-7645.tar
computePackageUrl.sh: [INFO] ** Artifact Repository Helpers: /u/ibmuser/git/dbb/Templates/Common-Backend-Scripts/utilities/../../../Pipeline/PackageBuildOutputs/ArtifactRepositoryHelpers.groovy
computePackageUrl.sh: [INFO] **         ArtifactRepo Url: http://10.3.20.231:8081/artifactory
computePackageUrl.sh: [INFO] **   ArtifactRepo Repo name: MortgageApplication-repo-local
computePackageUrl.sh: [INFO] **    ArtifactRepo Repo Dir: build
computePackageUrl.sh: [INFO] **           Output file: /u/ibmuser/git/workspace_test/MortgageApplication/feature/15-update-mortgage-application/build_7645/logs/packageVersionConfig.txt
computePackageUrl.sh: [INFO] **                 DBB_HOME: /usr/lpp/dbb/v3r0
computePackageUrl.sh: [INFO] **************************************************************

computePackageUrl.sh: [INFO] Invoking the Package Build Outputs script to compute Package Url.
computePackageUrl.sh: [INFO] /usr/lpp/dbb/v3r0/bin/groovyz  /u/ibmuser/git/dbb/Templates/Common-Backend-Scripts/utilities/../../../Pipeline/PackageBuildOutputs/ArtifactRepositoryHelpers.groovy --computePackageUrl --tarFileName MortgageApplication-7645.tar --versionName feature/15-update-mortgage-application --artifactRepositoryUrl "http://10.3.20.231:8081/artifactory" --artifactRepositoryName MortgageApplication-repo-local --artifactRepositoryDirectory build
computePackageUrl.sh: [INFO] Compute Package Url completed. Results stored in /u/ibmuser/git/workspace_test/MortgageApplication/feature/15-update-mortgage-application/build_7645/logs/packageVersionConfig.txt. rc=0
```  

</details>