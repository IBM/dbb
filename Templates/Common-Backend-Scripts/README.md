# Common Backend Scripts for Pipeline Implementations

## 1 - Overview

The Common Backend Scripts for Pipeline Implementations is a collection of scripts that deliver central "services" and a simplified interface for pipeline configurations that implement a Git/DBB-based pipeline for Mainframe applications.

Implementing a pipeline configuration, such as an Azure pipeline, a JenkinsFile, or the .gitlab-ci.yml file, requires accommodation of the selected development workflow with Git. To achieve consistency across various applications, rules must be implemented in pipeline code or configurations to address:
* naming conventions of build datasets,
* configuration parameters of the build framework,
* or naming conventions of the binary package



The community is providing a set of [pipeline tasks](../../Pipeline/) that implement the various stages of the pipeline orchestration. The tasks (generally implemented as groovy scripts) accept/require various parameters; some are related to the application, while others represent a technical configuration that is required for the script to operate.

The purpose of these common backend scripts is to reduce the necessary configuration and scripting within a concrete pipeline orchestration logic and let the application and pipeline teams focus on the application specific parameters.

Instead of mixing **orchestration of tasks** and **implementation of the pipeline rules** (such as computation of build HLQ, build type, etc) in the specific pipeline technology (e.g. a Jenkinsfile or a .gitlab-ci.yaml definition), the common backend scripts provide central services to the pipeline orchestrator, independently from the chosen orchestration technology.

By simplifying the invocation of the [pipeline tasks](../../Pipeline/), these scripts help the DevOps engineer to implement the pipeline configuration, especially for pipeline orchestrators that do not have a runner/agent available on z/OS UNIX System Services.

In addition, this repository contains a [test driver](test/) to outline how the scripts can be remotely invoked from non-z/OS systems. Additional templates for the various pipeline orchestrators (such as an azure-pipeline.yaml), is planned to be provided as well, to showcase how the Common Backend Scripts should be used.

This asset implements the rules and conventions of the Git-based workflow outlined in IBMs documentation `The Git-based workflow you need for Mainframe development` with a defined Git branching, build and packaging strategy.

## 2 - Set up

The provided scripts of this asset are implemented as bash scripts and need to be installed on UNIX System Services of the z/OS system that is used to execute the pipeline's tasks.

### 2.1 - Pre-requisites
The following are required to use these scripts:
* DBB v2.x toolkit is installed.
* zAppBuild is set up on Unix Systems Services.
* Git repository which follows the Git-based workflow outlined in IBM's documentation `The Git-based workflow for Mainframe development`.
* Build dependency information is available before performing the build run.


### 2.2 - Installation

* Copy/clone the Common Backend Scripts into z/OS UNIX System Services under a protected directory, e.g. `/usr/dbb/pipelineBackend`.
  * Update the permission of these scripts to allow for `read/execute` to only the users who will invoke the scripts. This is typically the technical user defined for the pipeline orchestrator. 

* The following environment variables need to be defined (for instance within the `.profile`) for the mainframe users who will execute the scripts on UNIX System Services:

  * `PIPELINE_SCRIPTS` - Environment variable to define the path to the Common Backend Scripts. 
  
     Add the directory path where you stored the scripts to the PATH of the pipeline user's profile, to make the scripts available to the consumers without referring to an absolute path within the pipeline configuration; most likely, this is the mainframe technical user used by the pipeline orchestrator. This avoids the need to set the working directory to the scripts when invoking them. In a non-interactive SSH setup, please make sure to initialize the environment variables for instance by executing the user's .profile, or by running a environment setup script.

  * (Optional) `PIPELINE_WORKSPACE` - Environment variable to configure the root workspace directory to run pipeline activities.

     Assumed to be a dedicated zFS filesystem that is in control of the pipeline user. Can be used in pipeline orchestration implementations to locate the path of logs or outputs. If not configured, the pipeline configuration needs to provide an absolute path to the working directory.

The below shows an extract of the pipeline user's `.profile` file:

  ```sh
  # extract of user's .profile to add the pipeline_config
  #
  # env variable to define the path to the backend scripts
  export PIPELINE_SCRIPTS=/var/dbb/common-wrapper-scripts
  export PATH=$PIPELINE_SCRIPTS:$PATH 

  # environment variable to define the pipeline root workspace directory
  export PIPELINE_WORKSPACE=/var/dbb/pipeline-workspace
  ```

### 2.3 - Script configuration

The scripts are designed to be configurable through the [pipelineBackend.config](pipelineBackend.config) file. This configuration file is located in the same directory as all the backend script files. 

Although each script is designed to work independently of the other scripts, they share common properties such as the root workspace directory for the pipeline scripts, and the directory for log files. These common properties are defined in the [pipelineBackend.config](pipelineBackend.config) file which are used across the backend scripts. 

The following are common properties in the [pipelineBackend.config](pipelineBackend.config) file. 

 
Central configuration | Description
---------- | ----------------------------------------------------------------------------------------
buildRootDir | Absolute path to define the root workspace directory for pipeline executions, e.q. `/usr/pipeline/workspace`. Pipeline configurations can only pass a unique relative workspace.
logsDir | A relative directory name for logs and temporary outputs. Default: logs
zAppBuild settings | Multiple settings for zAppBuild, like path and credentials
UCD settings | Multiple settings for UCD server, like URL and credentials
Wazi Deploy settings | Multiple settings for Wazi Deploy Generation, Deployment and Evidence Requests commands

Central function | Description
---------- | ----------------------------------------------------------------------------------------
getWorkDirectory() | Central function to calculate the absolute path of the working directory
getLogDir() | Central function to calculate the absolute path of the log directory
getApplicationDir() | Central function to calculate the absolute path of the application directory (where the application is stored)

The details of the configuration settings are provided in the comments of the configuration file.


### 2.4 - Required workspace directory

All the scripts are designed to have a unique working directory or workspace. The workspace is for managing the clone of the Git repository, and the log and output directories to avoid any conflicts and collisions. When invoking any of the scripts, the workspace is a required parameter which can either be an absolute path or a relative path. 

If a relative path is provided, the value of the workspace parameter is combined with the `buildRootDir` setting that is defined in the [pipelineBackend.config](pipelineBackend.config) as `<buildRootDir>/<workspace>`.

In the sample below, we use a workspace path consisting of 3 segments which are the **application name**, the **branch name** and the **pipeline build id**. This is the recommended approach, to ensure a unique workspace directory on z/OS UNIX System Services:

```
<Application>/<branch>/<pipeline-id>
```
The branch and pipelineID segments are resolved from the pipeline orchestrator via its built-in variables to:
```
MortApp/main/build-1
```

## 3 - Invocation of scripts

Scripts can be invoked from a non-z/OS pipeline runner/agent via 
* SSH connection
* ZOWE CLI
* or natively, to include steps (such as build and packaging phase) in a pipeline configuration that is executed under z/OS UNIX System Services. 

### 3.1 - Invocation samples: non-interactive SSH session

A non-interactive SSH session comes with a lightweight setup and is not fully initialized, like an interactive session can be by automatically loading the user's profile. The environment should be setup through the user's profile. The following snippet requires `bash` to be part of the PATH environment variable:
```
ssh pipelineuser@lpar ". /u/pipelineuser/.profile && dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main"
```

An alternate configuration is to have `bash` defined as the default program in the OMVS segment of the user.

### 3.2 - Invocation samples: ZOWE CLI

Zowe CLI by default initializes the environment with the user's profile:
```
zowe zos-uss issue ssh "dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main
```

## 4 - Script Inventory

Artifact Name |  Description | Script details   
---------- | -----| -----------------------------------------------------
[gitClone.sh](gitClone.sh) | Pipeline Shell Script to perform Git Clone to z/OS UNIX System Services | [script details](README.md#41---gitclonesh)
[dbbBuild.sh](dbbBuild.sh) | Pipeline Shell Script to invoke the Dependency Based Build framework [zAppBuild](https://github.com/IBM/dbb-zappbuild) | [script details](README.md#42---dbbbuildsh)
[utilities/dbbBuildUtils.sh](utilities/dbbBuildUtils.sh) | Utility Shell Script to implement the computation of build configuration, such as HLQ, build type or property overrides. | [script details](README.md#43---script-capabilities--dbbbuildutilssh)
[packageBuildOutputs.sh](packageBuildOutputs.sh) | Pipeline Shell Script to create a Package using the [PackageBuildOutputs groovy script](https://github.com/IBM/dbb/tree/main/Pipeline/PackageBuildOutputs) | [script details](README.md#44---packagebuildoutputssh)
[ucdPackage.sh](ucdPackaging.sh) | Pipeline Shell Script to publish to UCD Code Station binary repository using the [CreateUCDComponentVersion groovy script](https://github.com/IBM/dbb/tree/main/Pipeline/CreateUCDComponentVersion) | [script details](README.md#45---ucdpackagingsh)
[ucdDeploy.sh](ucdDeploy.sh) | Pipeline Shell Script to trigger a UCD Deployment via its REST interface using the [DeployUCDComponentVersion groovy script](https://github.com/IBM/dbb/tree/main/Pipeline/DeployUCDComponentVersion) | [script details](README.md#46---ucddeploysh)
[wazideploy-generate.sh](wazideploy-generate.sh) | Pipeline Shell Script to generate a Deployment Plan to be used with Wazi Deploy | [script details](README.md#47---wazideploy-generatesh)
[wazideploy-deploy.sh](wazideploy-deploy.sh) | Pipeline Shell Script to trigger a deployment of a package based on Deployment Plan with Wazi Deploy | [script details](README.md#48---wazideploy-deploysh)
[wazideploy-evidence.sh](wazideploy-evidence.sh) | Pipeline Shell Script to query the Wazi Deploy Evidence YAML file and create a deployment report | [script details](README.md#49---wazideploy-evidencesh)
[prepareLogs.sh](prepareLogs.sh) | Pipeline Shell Script to prepare a TAR file containing log files that can then be retrieved. | [script details](README.md#410---preparelogssh)


### 4.1 - gitClone.sh

Script to clone a repository to z/OS UNIX System Services. Please note that it is not pulling for updates. 

#### Invocation

The `gitClone.sh` script can be invoked as below:

```
gitClone.sh -w MortApp/main/build-1 -r git@github.com:Organization/MortgageApplication.git -b main
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. 
-r `<repoURL>` | **Git repository URL**, can either be SSH or HTTPS-based. Example: `-r git@github.com:Organization/MortgageApplication.git`
-b `<branch>` | **Git branch** that should be checked out. Example: `-b main`
-a `<application>` | (Optional) **Application name** to specify the directory into which git will clone. This is required for instance in the scenario if the repository name differs from the application name that is used in the build phase. Example: `-a CBSA`.

**Dealing with private repositories**

Although credentials should be managed in the secret vault of your pipeline orchestrator, you can pass credentials via the Git Repository URL, as follows:
```
gitClone.sh -w MortApp/main/build-1 -r https://<personal-access-token>@github.com/user/dbb-zappbuild-private.git -b main 
```

#### Output

The section below contains the output that is produced by the `gitClone.sh` script.

<details>
  <summary>Script Output</summary>

```
gitClone.sh -w MortApp/release/rel-1.0.0/build-1 -r git@github.ibm.com:zDevOps-Acceleration/MortgageApplication.git -b release/rel-1.0.0

gitClone.sh: [INFO] Clone Repository. Version=1.00
gitClone.sh: [INFO] **************************************************************
gitClone.sh: [INFO] ** Start Git Clone on HOST/USER: z/OS ZT01 04.00 02 8561/BPXROOT
gitClone.sh: [INFO] **          Repo: git@github.ibm.com:zDevOps-Acceleration/MortgageApplication.git
gitClone.sh: [INFO] **       WorkDir: /var/dbb/pipelineBackend/workspace/MortApp/release/rel-1.0.0/build-1
gitClone.sh: [INFO] **        GitDir: MortgageApplication
gitClone.sh: [INFO] **        Branch: release/rel-1.0.0 -> release/rel-1.0.0
gitClone.sh: [INFO] **************************************************************

gitClone.sh: [INFO] Preforming Git Clone of Repo git@github.ibm.com:zDevOps-Acceleration/MortgageApplication.git, Branch release/rel-1.0.0 to /var/dbb/pipelineBackend/workspace/MortApp/release/rel-1.0.0/build-1
Cloning into 'MortgageApplication'...
...
Resolving deltas: 100% (344/344), done.
gitClone.sh: [INFO] Git Status for MortgageApplication
On branch release/rel-1.0.0
Your branch is up to date with 'origin/release/rel-1.0.0'.

nothing to commit, working tree clean
gitClone.sh: [INFO] Git Show-Ref for MortgageApplication
5d2b737fbc62cf2f14630ac4d4473e2d027212c0 refs/heads/release/rel-1.0.0
e720027dd4f1afac469484fc1b5c8f8675d62e27 refs/remotes/origin/Development
9f1ce97a11f4bebe07b30a59992052ddf39e71de refs/remotes/origin/HEAD
597012c81d1bc0714d4c0595619833f30581c314 refs/remotes/origin/feature/setmainbuildbranch
5d2b737fbc62cf2f14630ac4d4473e2d027212c0 refs/remotes/origin/hotfix/rel-1.0.0/myfix
9f1ce97a11f4bebe07b30a59992052ddf39e71de refs/remotes/origin/main
5d2b737fbc62cf2f14630ac4d4473e2d027212c0 refs/remotes/origin/release/rel-1.0.0
0cc39e464cdd6fa7a4a7e9bb1381e25daf757d08 refs/tags/rel-1.0.0
c08c90fb9b76d466b5717595b5de0dee9031f9ca refs/tags/rel100

```  

</details>


### 4.2 - dbbBuild.sh

This script implements the invocation of the [zAppBuild](https://github.com/IBM/dbb-zappbuild) framework. As designed, it makes use of the [baselineRef sub-option](https://github.com/IBM/dbb-zappbuild/blob/documentation-review/docs/BUILD.md#perform-impact-build-by-providing-baseline-reference-for-the-analysis-of-changed-files) provided by zAppBuild. The [dbbBuildUtils.sh](utilities/dbbBuildUtils.sh) script is used to compute the build configuration depending on the workflow to define zAppBuild CLI parameters. It leverages the [application baseline configuration](samples/baselineReference.config) file which is expected to be present in the `application-conf`` directory in order to compute the baseline reference and its changes.

#### Git branches naming convention requirements

The build script follows the naming conventions for branches that are outlined in the document in the [solution guide](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev#naming-conventions):

```properties
## integration branches
#
# main Build branch
main
# release maintenance branches to fix a release that is been put to production
release/rel-1.0.0
# project/initiative/epic branches
epic/epic1234
project/project1

## feature branches
#
# feature branches for contributing to the next planned release via main
feature/setmainbuildbranch
feature/43-set-main-build-branch
# release maintenance feature branches
#  second segment is indicating the release
hotfix/rel-1.0.0/fixMortgageApplication
# feature branches for epics / larger development initiatives
#  second segment is referencing the epic context
feature/epic1234/54-my-first-cool-new-feature
```

Details are documented in [dbbBuildUtils.sh](README.md#script-capabilities--dbbbuildutilssh).

#### Invocation

The `dbbBuild.sh` script can be invoked as follows:

```
dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main -p build
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps.
-a `<application>` | **Application name** to be built, which is passed to zAppBuild as the `--application` parameter.
-b `<branch>` | **Git branch** that is built. Used to compute various build properties such as the `--hlq` and build type.
-p `<build/release/preview>` | (Optional) **Pipeline Type** to indicate a `build` pipeline (build only with test/debug options) or a `release` pipeline (build for optimized load modules), or if it runs in `preview` mode.
-v | (Optional) zAppBuild verbose tracing flag.
-t `<buildTypeArgument>` | (Optional) **zAppBuild Build Type** to specify the build type, such as `--fullBuild`, or `--impactBuild`. Arguments must be provided between quotes - e.g.: `-t '--fullBuild'`. Providing this parameter overrides the computation of the build type in the backend scripts. For instance can be used to initialize the DBB Metadatastore. 
-q `<hlqPrefix>` |(Optional) **HLQ prefix**. Default is retrieved from the [pipelineBackend.config](pipelineBackend.config) file, if the configuration file is not modified - the default value is set to the user executing the script.

**Pipeline type**

The type of pipeline (`-p` option), is used to modify the operational behavior of the build framework on producing executables:
* `build` configures the build options for test/debug options. This is the **default**.
* `release` used to indicate to produce executables with the flag for performance-optimized runtime modules. This is required for the release pipelines which include release candidate packages.
* `preview` configures the build process to execute without producing any outputs. It is used to preview what the build will do. The pipeline should not have any subsequent actions.

#### Output

The section below contains the output that is produced by the `dbbBuild.sh` script.

<details>
  <summary>Script Output</summary>

```
dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main -p build

$ dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main -p b
<ster/build-1 -a MortgageApplication -b main -p bu                 ild
dbbBuild.sh: [INFO] Dependency Based Build. Version=1.00
dbbBuild.sh: [INFO] **************************************************************
dbbBuild.sh: [INFO] ** Started - DBB Build on HOST/USER: z/OS ZT01 04.00 02 8561/BPXROOT
dbbBuild.sh: [INFO] **          Workspace: /var/dbb/pipelineBackend/workspace/MortApp/main/build-1
dbbBuild.sh: [INFO] **        Application: MortgageApplication
dbbBuild.sh: [INFO] **             Branch: main
dbbBuild.sh: [INFO] **         Build Type: --impactBuild --baselineRef refs/tags/rel-1.0.0 --debug
dbbBuild.sh: [INFO] **                HLQ: DBEHM.MORTGAGE.MAIN.BLD
dbbBuild.sh: [INFO] **             AppDir: /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/MortgageApplication
dbbBuild.sh: [INFO] **             LogDir: /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs
dbbBuild.sh: [INFO] **     zAppBuild Path: /var/dbb/dbb-zappbuild_300
dbbBuild.sh: [INFO] **           DBB_HOME: /usr/lpp/dbb/v2r0
dbbBuild.sh: [INFO] **      DBB JDBC USER: DBEHM
dbbBuild.sh: [INFO] **  DBB JDBC Pwd File: /var/dbb/config/db2-pwd-file.xml
dbbBuild.sh: [INFO] **           Verbose : No
dbbBuild.sh: [INFO] **         DBB Logger: No
dbbBuild.sh: [INFO] **************************************************************

dbbBuild.sh: [INFO] Invoking the zAppBuild Build Framework.
dbbBuild.sh: [INFO] /usr/lpp/dbb/v2r0/bin/groovyz  /var/dbb/dbb-zappbuild_300/build.groovy --workspace /var/dbb/pipelineBackend/workspace/MortApp/main/build-1 --application MortgageApplication --outDir /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs --hlq DBEHM.MORTGAGE.MAIN.BLD --id DBEHM --pwFile /var/dbb/config/db2-pwd-file.xml  --logEncoding UTF-8 --propFiles /var/dbb/dbb-zappbuild-config/build.properties,/var/dbb/dbb-zappbuild-config/datasets.properties --impactBuild --baselineRef refs/tags/rel-1.0.0 --debug

** Build start at 20230825.043936.039
** Build output located at /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/build.20230825.163936.039
** Build result created for BuildGroup:MortgageApplication-main BuildLabel:build.20230825.163936.039
** Loading DBB scanner mapping configuration dbb.scannerMapping
** --impactBuild option selected. Building impacted programs for application MortgageApplication 
** Writing build list file to /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/build.20230825.163936.039/buildList.txt
** Populating file level properties from individual artifact properties files.
** Invoking build scripts according to build order: BMS.groovy,Cobol.groovy,LinkEdit.groovy
** Building 2 files mapped to Cobol.groovy script
*** (1/2) Building file MortgageApplication/cobol/epsnbrvl.cbl
*** (2/2) Building file MortgageApplication/cobol/epscmort.cbl
** Writing build report data to /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/build.20230825.163936.039/BuildReport.json
** Writing build report to /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/build.20230825.163936.039/BuildReport.html
** Updating build result BuildGroup:MortgageApplication-main BuildLabel:build.20230825.163936.039
** Build ended at Fri Aug 25 16:39:43 GMT+01:00 2023
** Build State : CLEAN
** Total files processed : 2
** Total build time  : 7.025 seconds

** Build finished
dbbBuild.sh: [INFO} LastBuildLog = /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/build.20230825.163936.039/buildList.txt
dbbBuild.sh: [INFO] DBB Build Complete. rc=0
```  

</details>

### 4.3 - Script utility - dbbBuildUtils.sh

The [dbbBuildUtils](utilities/dbbBuildUtils.sh) script is a utility script providing the `computeBuildConfiguration()` method to compute the zAppBuild's options and parameters:

* `build type`, such as `--impactBuild`
* the baseline reference, `--baselineRef xxx`, where *xxx* is retrieved from the baselineReference.config file.
* flag to produce test modules (`--debug` in zAppBuild) or modules improved for performance (production runtime modules).
* the `mainBuildBranch` to configure feature branches to clone the correct dependency metadata collections and to identify the correct offset of changes.

#### Baseline references requirements

The IBM recommended workflow approach leverages Git tags to identify the offset for calculating the changed files for a given deliverable. In this version, this utility script is retrieving the information from the [baselineReference.config](samples/baselineReference.config) file, that has to be maintained by the application team within the `application-conf` directory. It is the application teams' responsibility to maintain these references. 

Note that the location of the baselineReferences.config file can be customized in the [pipelineBackend.config](pipelineBackend.config) file.

[MortgageApplication-baselineReference.config](MortgageApplication-baselineReference.config) is a sample, that indicates the baseline for the `main` and `release maintenance` branch.

### 4.4 - packageBuildOutputs.sh

This script is to execute the `PackageBuildOutputs.groovy` that packages up the build outputs and optionally uploads it to an artifact repository to publish the artifacts created by a DBB build in the pipeline.

#### Invocation

The `packageBuildOutputs.sh` script can be invoked as follows:

- Package only

```
packageBuildOutputs.sh -w MortApp/main/build-1 -t rel-1.0.0.tar
```
- Package and Upload
```
packageBuildOutputs.sh -w MortApp/main/build-1 -t rel-1.0.0.tar -a MortgageApplication -b main -u -p release -v rel-1.0.0.2023-09-22-08.55.20
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
**Packaging options**
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. The `packageBuildOutputs.sh` script is evaluating the logs directory.
-t `<tarFileName>` | (Optional) Name of the **tar file** to create.
**Artifact Upload options**
-u | Flag to enable upload of outputs to the configured artifact repository.
-a `<application>` | **Application name** leveraged to define the artifact repository name. See function `computeArtifactRepositoryName()` in the pipelineBackend.config file. Ex.: `MortgageApplication-repo-local`. 
-b `<branch>`| Name of the **git branch** turning into a segment of the directory path in the artifact repository. See function `computeArtifactRepositoryDirectory()` in the pipelineBackend.config file.
-p `<build/release>` | **Pipeline type** to indicate a `build` pipeline (build only with test/debug options) or a `release` pipeline (build for  optimized load modules) to determine the directory in the artifact repository for development and pipeline builds.
-v `<artifactVersion>` | Label of the **version** in the artifact repository turning into a segment of the directory path in the artifact repo.

#### Script conventions

**Directory Path within the artifact repo**

For uploading, the backend script computes the directory path within the artifact repository :
* **Branch**/**artifactVersion**

If it is the `main` branch, the pipeline type (-p) is evaluated to 
* **Branch**/**pipelineType <build/release>**/**artifactVersion**
 
while **artifactVersion** is appended by the `PackageBuildOutputs.groovy` script.

#### Output 

The section below contains the output that is produced by the `packageBuildOutputs.sh` script.

<details>
  <summary>Script Output</summary>

```
packageBuildOutputs.sh -w MortApp/main/build-1 -a MortgageApplication -t package.tar -b main -u -v MortgageApplication.2023-09-22_13-55-20 -p build

packageBuildOutputs.sh: [INFO] Package Build Outputs wrapper. Version=1.00
packageBuildOutputs.sh: [INFO] **************************************************************
packageBuildOutputs.sh: [INFO] ** Started - Package Build Outputs on HOST/USER: z/OS ZT01 04.00 02 8561/BPXROOT
packageBuildOutputs.sh: [INFO] **                  WorkDir: /var/dbb/pipelineBackend/workspace/MortApp/main/build-1
packageBuildOutputs.sh: [INFO] **              Application: MortgageApplication
packageBuildOutputs.sh: [INFO] **                   Branch: main
packageBuildOutputs.sh: [INFO] **         Type of pipeline: build
packageBuildOutputs.sh: [INFO] **            Tar file Name: package.tar
packageBuildOutputs.sh: [INFO] **     BuildReport Location: /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs
packageBuildOutputs.sh: [INFO] **     PackagingScript Path: /var/dbb/extensions/dbb20/Pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy
packageBuildOutputs.sh: [INFO] **     Packaging properties: /var/dbb/extensions/dbb20/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties
packageBuildOutputs.sh: [INFO] ** Publish to Artifact Repo: true
packageBuildOutputs.sh: [INFO] **            Artifact name: MortgageApplication.2023-09-22_13-55-20
packageBuildOutputs.sh: [INFO] **         ArtifactRepo Url: http://10.3.20.231:8081/artifactory
packageBuildOutputs.sh: [INFO] **        ArtifactRepo User: admin
packageBuildOutputs.sh: [INFO] **    ArtifactRepo Password: xxxxx
packageBuildOutputs.sh: [INFO] **   ArtifactRepo Repo name: MortgageApplication-repo-local
packageBuildOutputs.sh: [INFO] **    ArtifactRepo Repo Dir: main/build
packageBuildOutputs.sh: [INFO] **                 DBB_HOME: /usr/lpp/dbb/v2r0
packageBuildOutputs.sh: [INFO] **************************************************************

packageBuildOutputs.sh: [INFO] Invoking the Package Build Outputs script.
packageBuildOutputs.sh: [INFO] groovyz  /var/dbb/extensions/dbb20/Pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy --workDir /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs --tarFileName package.tar --packagingPropertiesFile /var/dbb/extensions/dbb20/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties --addExtension --publish --artifactRepositoryUrl "http://10.3.20.231:8081/artifactory" --versionName MortgageApplication.2023-09-22_13-55-20 --artifactRepositoryUser admin --artifactRepositoryPassword artifactoryadmin --artifactRepositoryName MortgageApplication-repo-local --artifactRepositoryDirectory main/build
** PackageBuildOutputs start at 20230922.125616.056
** Properties at startup:
   addExtension -> true
   artifactRepository.directory -> main/build
   artifactRepository.password -> xxxxxx 
   artifactRepository.repo -> MortgageApplication-repo-local
   artifactRepository.url -> http://10.3.20.231:8081/artifactory
   artifactRepository.user -> admin
   buildReportOrder -> [/var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/BuildReport.json]
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "JCL": "TEXT"]
   packagingPropertiesFile -> /var/dbb/extensions/dbb20/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties
   publish -> true
   startTime -> 20230922.125616.056
   tarFileName -> package.tar
   verbose -> false
   versionName -> MortgageApplication.2023-09-22_13-55-20
   workDir -> /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs
** Read build report data from /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/BuildReport.json
** Removing Output Records without deployType or with deployType=ZUNIT-TESTCASE 
** Files detected in /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/BuildReport.json
   DBEHM.MORTGAGE.MAIN.BLD.DBRM(EPSCMORT), DBRM
   DBEHM.MORTGAGE.MAIN.BLD.LOAD(EPSCMORT), CICSLOAD
*** Number of build outputs to package: 2
** Copying BuildOutputs to temporary package dir.
     Copying DBEHM.MORTGAGE.MAIN.BLD.DBRM(EPSCMORT) to /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/tempPackageDir/DBEHM.MORTGAGE.MAIN.BLD.DBRM/EPSCMORT.DBRM with DBB Copymode BINARY
     Copying DBEHM.MORTGAGE.MAIN.BLD.LOAD(EPSCMORT) to /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/tempPackageDir/DBEHM.MORTGAGE.MAIN.BLD.LOAD/EPSCMORT.CICSLOAD with DBB Copymode LOAD
** Copying /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/BuildReport.json to temporary package dir as BuildReport.json.
** Copying /var/dbb/extensions/dbb20/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties to temporary package dir.
** Creating tar file at /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/package.tar.
** Package successfully created at /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/package.tar.
** Uploading package to Artifact Repository http://10.3.20.231:8081/artifactory/MortgageApplication-repo-local/main/build/MortgageApplication.2023-09-22_13-55-20/package.tar.
** ArtifactRepositoryHelper started for upload of /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/package.tar to http://10.3.20.231:8081/artifactory/MortgageApplication-repo-local/main/build/MortgageApplication.2023-09-22_13-55-20/package.tar
** Uploading /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs/package.tar to http://10.3.20.231:8081/artifactory/MortgageApplication-repo-local/main/build/MortgageApplication.2023-09-22_13-55-20/package.tar
** Upload completed
** Build finished
packageBuildOutputs.sh: [INFO] Package Build Outputs Complete. rc=0

rc=0

```

</details>



### 4.5 - ucdPackaging.sh

This script is to execute the `dbb-ucd-packaging.groovy` that invokes the Urban Code Deploy (UCD) buztool utility, to publish the artifacts created by the DBB Build from a pipeline.

#### Invocation

The `ucdPackaging.sh` script can be invoked as follows:

```
ucdPacking.sh -v ucdVersion -c ucdComponentName -w workingDirectory -e externalRepositoryFile -f packagingPropertiesFile [-u pipelineURL] [-b gitBranchName] [-p gitPullRequestURL]
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-v `<ucdVersion>` | **Version** name to create.
-c `<ucdComponentName>` | **Component** name in UCD.
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. The `ucdPackaging.sh` script is evaluating the logs directory.
-e `<externalRepositoryFile>` | **Path to external artifact repository file** used by UCD buztool.
-f `<packagingPropertiesFile>` | **Path to a properties file** for additional configuration for the `dbb-ucd-packaging` script.
-u `<pipelineUrl>`| (Optional) URL to the pipeline to establish link to pipeline build result.
-b `<branchName>` | (Optional) Name of the **git branch**.
-p `<prUrl>` | (Optional) URL to the pull request.

#### Output

The section below contains the output that is produced by the `ucdPackaging.sh` script.

<details>
  <summary>Script Output</summary>

```
$ ucdPackaging.sh -v MortgageApplication.ID -c MortgageApplication -w MortApp/release/rel-1.0.0/build-1 -u https://github.com/IBM/dbb/issues/117 -b main -p 'https://github.com/IBM/dbb/issues/117' 
ucdPackaging.sh: [INFO] Deploy UCD Component. Version=1.
ucdPackaging.sh: [INFO] **************************************************************
ucdPackaging.sh: [INFO] ** Started - UCD Publish on HOST/USER: z/OS ZT01 04.00 02 8561/BPXROOT
ucdPackaging.sh: [INFO] **                 WorkDir: /var/dbb/pipelineBackend/test/MortApp/release/rel-1.0.0/build-1
ucdPackaging.sh: [INFO] **             UCD Version: MortgageApplication.ID
ucdPackaging.sh: [INFO] **           UCD Component: MortgageApplication
ucdPackaging.sh: [INFO] **      Artifacts Location: /var/dbb/pipelineBackend/test/MortApp/release/rel-1.0.0/build-1/logs/buildList.txt
ucdPackaging.sh: [INFO] **    PackagingScript Path: /var/dbb/extensions/dbb20/Pipeline/CreateUCDComponentVersion/dbb-ucd-packaging.groovy
ucdPackaging.sh: [INFO] **            BuzTool Path: /var/ucd-agent/bin/buztool.sh
ucdPackaging.sh: [INFO] ** External Repository cfg:
ucdPackaging.sh: [INFO] **    Packaging properties:
ucdPackaging.sh: [INFO] **            Pipeline URL: https://github.com/IBM/dbb/issues/117
ucdPackaging.sh: [INFO] **         Git branch name: main
ucdPackaging.sh: [INFO] **        Pull Request URL: https://github.com/IBM/dbb/issues/117
ucdPackaging.sh: [INFO] **                DBB_HOME: /usr/lpp/dbb/v2r0
ucdPackaging.sh: [INFO] **************************************************************
ucdPackaging.sh: [INFO] Invoking the DBB UCD Packaging.
ucdPackaging.sh: [INFO] groovyz  /var/dbb/extensions/dbb20/Pipeline/CreateUCDComponentVersion/dbb-ucd-packaging.groovy -b /var/ucd-agent/bin/buztool.sh -w /var/dbb/pipelineBackend/test/MortApp/release/rel-1.0.0/build-1/logs/buildList.txt -c  -v  --propertyFile  --packagingPropFiles  --pipelineURL https://github.com/IBM/dbb/issues/117 --gitBranch main   --pullRequestURL https://github.com/IBM/dbb/issues/117
```

</details>


### 4.6 - ucdDeploy.sh

This script is implementing the invocation of the `ucd-deploy.groovy` script to perform Urban Code Deploy (UCD) deployments.


#### Invocation

The `ucdDeploy.sh` script can be invoked as follows:

```
ucdDeploy.sh -a ucdApplicationName -p ucdApplicationProcessName -e ucdEnvironmentName -d ucdComponentName:ucdDeployVersion
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-a `<ucdApplicationName>` | **Application** name defined in UCD containing the component version to be deployed.
-p `<ucdApplicationProcessName>` | **Process** name in UCD associated with the application being deployed.
-e `<ucdEnvironmentName>` | **Environment** name in UCD that the component version will be deployed to.
-d `<ucdComponentName:ucdDeployVersion>` | **Component name and version** to be deployed to the UCD environment.
-t `<timeout>` | (Optional) **Deployment timeout** value in seconds.  Defaults to 300 seconds.
-s `<SSLProtocol>`| (Optional) **SSL protocol** to use. Default is TLSv1.2.
-k | (Optional) Disable SSL verification flag.
-v | (Optional) Verbose tracing flag. Used to produce additional tracing in the groovy script.

#### Output

The section below contains the output that is produced by the `ucdDeploy.sh` script.

<details>
  <summary>Script Output</summary>

```
ucdDeploy.sh -a rbs-retirementCalculator -p rbs-retirementCalculator-process -e rbs-IntegrationTest -d rbs-retirementCalculator:latest
bash: [INFO] Deploy UCD Component. Version=1.00
bash: [INFO] **************************************************************
bash: [INFO] ** Start UCD Component Deploy on HOST/USER: z/OS ZT01 04.00 02 8561/BPXROOT
bash: [INFO] **   Location of ucd-deploy.groovy: /u/brice/groovyScripts
bash: [INFO] **            UCD Application Name: rbs-retirementCalculator
bash: [INFO] **            UCD Environment Name: rbs-IntegrationTest
bash: [INFO] **                   UCD User Name: admin
bash: [INFO] **                  UCD Server URL: http://10.3.20.231:8080/
bash: [INFO] **           UCD Component Version: rbs-retirementCalculator:latest
bash: [INFO] **    UCD Application Process Name: rbs-retirementCalculator-process
bash: [INFO] **                         Verbose: No
bash: [INFO] **                SSL Verification: Yes
bash: [INFO] **************************************************************

/usr/lpp/dbb/v2r0/bin/groovyz /u/brice/groovyScripts/ucd-deploy.groovy -a "rbs-retirementCalculator" -e "rbs-IntegrationTest" -U admin -P ******** -u http://10.3.20.231:8080/ -d "rbs-retirementCalculator:latest" -p rbs-retirementCalculator-process
** Request UCD Deployment start at 20230830.064205.042
** Properties at startup:
   application -> rbs-retirementCalculator 
   environment -> rbs-IntegrationTest 
   user -> admin 
   password -> xxxxxx 
   url -> http://10.3.20.231:8080/ 
   deployVersions -> rbs-retirementCalculator:latest 
   applicationProcess -> rbs-retirementCalculator-process 
**  Deploying component versions: rbs-retirementCalculator:latest
*** Starting deployment process 'rbs-retirementCalculator-process' of application 'rbs-retirementCalculator' in environment 'rbs-IntegrationTest'
*** SSL Verification disabled
*** Follow Process Request: https://ucd.server.com:8443/#applicationProcessRequest/184c812f-605f-5040-ad31-d3a31f87bb3c
Executing ......
*** The deployment result is SUCCEEDED. See the UrbanCode Deploy deployment logs for details.
** Build finished
```  

</details>

### 4.7 - wazideploy-generate.sh

This script invokes the Wazi Deploy Generate command to generate a Deployment Plan based on the content of a package. The package should be created with the `PackageBuildOutputs.groovy` script or through the `packageBuildOutputs.sh` script.


#### Invocation

The `wazideploy-generate.sh` script can be invoked as follows:

```
wazideploy-generate.sh -w  MortApp/main/build-1 -i MortgageApplication.tar                                     
```
Or by fully specifying the settings
```
wazideploy-generate.sh -m deploymentMethod -p deploymentPlan -r deploymentPlanReport -i packageInputFile
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. Optional, if `deploymentPlan`, `deploymentPlanReport` and `packageOutputFile` are fully referenced. 
-i `<packageInputFile>` | **Package Input File** to be used for the generation phase with Wazi Deploy. This is likely the package to be deployed. If providing a relative path, the file is assumed to be located in the directory `<workspace directory>/<logsDir>`. This parameter can either be path to a TAR file on UNIX System Services, or the URL of the TAR file to retrieve (only Artifactory is supported).
-m `<deploymentMethod>` | (Optional) Absolute path to the Wazi Deploy **Deployment Method** stored on UNIX System Services. If not specified, the deployment method file location is obtained from the `pipelineBackend.config`.
-p `<deploymentPlan>` | (Optional) Absolute or relative path to the **Deployment Plan** file, generated based on the content of the input package. If providing a relative path, the file path is prefixed with Wazi Deploy Packaging directory `<wdDeployPackageDir>` configured in `pipelineBackend.config`.  If not specified, the deployment plan location is obtained from the `pipelineBackend.config`.
-r `<deploymentPlanReport>` | (Optional) Absolute or relative path to the **Deployment Plan Report**. If providing a relative path, the file path is prefixed with Wazi Deploy Packaging directory `<wdDeployPackageDir>` configured in `pipelineBackend.config`. If not specified, the deployment plan report location is obtained from the `pipelineBackend.config`.
-o `<packageOutputFile>` | (Optional) Absolute or relative path to the **Package Output File** that specifies the location where to store the downloaded tar file. If providing a relative path, the file path is prefixed with Wazi Deploy Packaging directory `<wdDeployPackageDir>` configured in `pipelineBackend.config`. Only required when wazideploy-generate is used to download the package. This is indicated when a URL is specified for the **Package Input File**.
-c `<configurationFile>` | (Optional) Absolute path to the **Configuration File** that contains information to connect to Artifactory. Only required when wazideploy-generate is used to download the package. This is indicated when a URL is specified for the **Package Input File**.
-d | (Optional) Debug tracing flag. Used to produce additional tracing with Wazi Deploy.

#### Output

The section below contains the output that is produced by the `wazideploy-generate.sh` script.

<details>
  <summary>Script Output</summary>
wazideploy-generate.sh -m /var/WaziDeploy/wazi-deploy-samples-0.10.0/wazi-deploy-sample/plum-samples/external-repos/deployment-method/deployment-method.yml -p /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlan.yaml -r /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlanReport.html -i /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/MortgageApplication.tar
wazideploy-generate.sh: [INFO] Generate Wazi Deploy Deployment Plan. Version=1.00

wazideploy-generate.sh: [INFO] **************************************************************
wazideploy-generate.sh: [INFO] ** Start Wazi Deploy Generation on HOST/USER: z/OS ZT01 04.00 02 8561/***
wazideploy-generate.sh: [INFO] **               Deployment Method: /var/WaziDeploy/wazi-deploy-samples-0.10.0/wazi-deploy-sample/plum-samples/external-repos/deployment-method/deployment-method.yml
wazideploy-generate.sh: [INFO] **       Generated Deployment Plan: /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlan.yaml
wazideploy-generate.sh: [INFO] **          Deployment Plan Report: /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlanReport.html
wazideploy-generate.sh: [INFO] **              Package Input File: /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/MortgageApplication.tar
wazideploy-generate.sh: [INFO] **        Debug output is disabled.
wazideploy-generate.sh: [INFO] **************************************************************

* Build the deployment plan from the deployment method with the deployment plan extension: WdDeploymentStateExtension
** Generic step PACKAGE/PACKAGE/PACKAGE
*** No item found
** Collecting items for DEPLOY_MODULES/ADD/MEMBER_COPY
*** 4 items found [EPSCMORT.CICSLOAD, EPSMORT.MAPLOAD, EPSCMORT.DBRM, EPSMLIS.MAPLOAD]
** Collecting items for DEPLOY_MODULES/UPDATE/MEMBER_VALIDATE
*** No item found
** Collecting items for DEPLOY_MODULES/UPDATE/MEMBER_ARCHIVE
*** No item found
** Collecting items for DEPLOY_MODULES/UPDATE/MEMBER_COPY
*** No item found
** Collecting items for DB2/UPDATE/DB2_BIND_PACKAGE
*** 1 item found [EPSCMORT.DBRM]
** Collecting items for DB2/UPDATE/DB2_BIND_PLAN
*** 1 item found [EPSCMORT.DBRM]
** Collecting items for CICS/ADD/PROG_CREATE
*** No item found
** Collecting items for CICS/UPDATE/PROG_UPDATE
*** 3 items found [EPSCMORT.CICSLOAD, EPSMORT.MAPLOAD, EPSMLIS.MAPLOAD]
** Collecting items for CICS/DELETE/PROG_DELETE
*** No item found
** Collecting items for DELETE_MODULES/DELETE/MEMBER_DELETE
*** No item found
* Save the deployment plan to: /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlan.yaml
* Save the deployment plan report to: /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlanReport.html

</details>

### 4.8 - wazideploy-deploy.sh

This script invokes the Wazi Deploy Deploy (with the Python Translator) command to deploy the content of a provided package with a Deployment Plan.

#### Invocation

The `wazideploy-deploy.sh` script can be invoked as follows:

Only mandatory parameters, and using relative paths:
```
wazideploy-deploy.sh -w  MorgageApplication/main/build-1 -e IntegrationTest.yaml -i MortgageApplication.tar
```
or qualified paths
```
wazideploy-deploy.sh -w /u/ado/workspace/MorgageApplication/main/build-1 -p /u/ado/workspace/MortApp/main/build-1/deploymentPlan.yaml -e /u/ado/deployment/environment-configs/IntegrationTest.yaml -i /u/ado/builds/MortApp/main/build-1/logs/MortgageApplication.tar -l /u/ado/builds/MortApp/main/build-1/logs/evidences.yaml
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. Optional, if `deploymentPlan`, `environmentFile`, `packageInputFile` and `evidenceFile` are fully referenced. 
-p `<deploymentPlan>` | (Optional) Absolute or relative path to the **Deployment Plan** file, generated based on the content of the input package. If not specified, the deployment plan location is obtained from the `pipelineBackend.config`.
-e `<environmentFile>` | (Optional) Absolute or relative path to **Environment File**, that describes the target z/OS environment. If a relative path is provided, the deployment plan is located with based on the configuration from the `pipelineBackend.config`.
-i `<packageInputFile>` | **Package Input File** package that is to be deployed. If a relative file path is provided, the file is assumed to be located in the `<workspace directory>/<logsDir>`.
-l `<evidenceFile>` | (Optional) Absolute or relative path to the **Evidence File** that will contain the logs of all Wazi Deploy tasks. If not specified, evidence file location will be obtained from the `pipelineBackend.config`.
-d | (Optional) Debug tracing flag. Used to produce additional tracing with Wazi Deploy.

#### Output

The section below contains the output that is produced by the `wazideploy-deploy.sh` script.

<details>
  <summary>Script Output</summary>
wazideploy-deploy.sh -w /u/ado/workspace/MortgageApplication/main/build-20231019.13 -p /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlan.yaml -e /var/WaziDeploy/wazi-deploy-samples-0.10.0/wazi-deploy-sample/plum-samples/external-repos/environment-conf/python/EOLEB7-MortgageApplication-Integration.yaml -i /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/MortgageApplication.tar -l /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/evidence.yaml
wazideploy-deploy.sh: [INFO] Deploy Package with Wazi Deploy. Version=1.00

wazideploy-deploy.sh: [INFO] **************************************************************
wazideploy-deploy.sh: [INFO] ** Start Wazi Deploy Deployment on HOST/USER: z/OS ZT01 04.00 02 8561/***
wazideploy-deploy.sh: [INFO] **               Working Directory: /u/ado/workspace/MortgageApplication/main/build-20231019.13
wazideploy-deploy.sh: [INFO] **                 Deployment Plan: /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlan.yaml
wazideploy-deploy.sh: [INFO] **                Environment File: /var/WaziDeploy/wazi-deploy-samples-0.10.0/wazi-deploy-sample/plum-samples/external-repos/environment-conf/python/EOLEB7-MortgageApplication-Integration.yaml
wazideploy-deploy.sh: [INFO] **              Package Input File: /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/MortgageApplication.tar
wazideploy-deploy.sh: [INFO] **                   Evidence File: /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/evidence.yaml
wazideploy-deploy.sh: [INFO] **        Debug output is disabled.
wazideploy-deploy.sh: [INFO] **************************************************************
wazideploy-deploy -wf /u/ado/workspace/MortgageApplication/main/build-20231019.13 -dp /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlan.yaml -ef /var/WaziDeploy/wazi-deploy-samples-0.10.0/wazi-deploy-sample/plum-samples/external-repos/environment-conf/python/EOLEB7-MortgageApplication-Integration.yaml -pif /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/MortgageApplication.tar -efn /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/evidence.yaml

** Reading the deployment file from: /u/ado/workspace/MortgageApplication/main/build-20231019.13/deploymentPlan.yaml
** Reading the target environment file from: /var/WaziDeploy/wazi-deploy-samples-0.10.0/wazi-deploy-sample/plum-samples/external-repos/environment-conf/python/EOLEB7-MortgageApplication-Integration.yaml
*** Validate Deployment Plan before processing it
*** End of Deployment Plan validation
*** Validate /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/MortgageApplication.tar fingerprint
*** Checksum is valid for /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/MortgageApplication.tar
** Registering SMF Record
*! WARNING: The registration of SMF record failed. See full log in evidence file /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/evidence.yaml
** Processing PACKAGE(s)
** Processing PACKAGE/PACKAGE(s)
** Processing PACKAGE/PACKAGE/PACKAGE(s) with package
*** Processing package /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/MortgageApplication.tar
*** Expand the package /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/MortgageApplication.tar to /u/ado/workspace/MortgageApplication/main/build-20231019.13 on system os OS/390
** Processing DEPLOY_MODULES(s)
** Processing DEPLOY_MODULES/ADD(s)
** Processing DEPLOY_MODULES/ADD/MEMBER_COPY(s) with member_copy
*** Copy /u/ado/workspace/MortgageApplication/main/build-20231019.13/***.MORTGAGE.MAIN.BLD.LOAD/EPSCMORT.CICSLOAD to 'WDEPLOY.MORTGAGE.INT.LOAD(EPSCMORT)'
*** Copy /u/ado/workspace/MortgageApplication/main/build-20231019.13/***.MORTGAGE.MAIN.BLD.LOAD/EPSMORT.MAPLOAD to 'WDEPLOY.MORTGAGE.INT.LOAD(EPSMORT)'
*** Copy /u/ado/workspace/MortgageApplication/main/build-20231019.13/***.MORTGAGE.MAIN.BLD.LOAD/EPSMLIS.MAPLOAD to 'WDEPLOY.MORTGAGE.INT.LOAD(EPSMLIS)'
*** Copy /u/ado/workspace/MortgageApplication/main/build-20231019.13/***.MORTGAGE.MAIN.BLD.DBRM/EPSCMORT.DBRM to 'WDEPLOY.MORTGAGE.INT.DBRM(EPSCMORT)'
** Processing DB2(s)
** Processing DB2/UPDATE(s)
** Processing DB2/UPDATE/DB2_BIND_PACKAGE(s) with db2_bind_package
*** Perform BIND PACKAGE on subsys 'DBC1' for package 'MORTGAGE' and qualifier 'MORTGAGE' with template 'db2_bind_package.j2'
**** Perform BIND PACKAGE on 'EPSCMORT.DBRM'
*** Submit jcl /u/ado/workspace/MortgageApplication/main/build-20231019.13/bind_package_1.jcl
** Job JOB03104 submitted with ZOAU Python API.
**** Job JOB03104 finished CC=0000
** Processing DB2/UPDATE/DB2_BIND_PLAN(s) with db2_bind_plan
*** Perform BIND PLAN on subsys 'DBC1' for pklist '*.MORTGAGE.*' and qualifier 'MORTGAGE' with template 'db2_bind_plan.j2'
*** Submit jcl /u/ado/workspace/MortgageApplication/main/build-20231019.13/bind_plan_1.jcl
** Job JOB03105 submitted with ZOAU Python API.
**** Job JOB03105 finished CC=0000
** Processing CICS(s)
** Processing CICS/UPDATE(s)
** Processing CICS/UPDATE/PROG_UPDATE(s) with cics_cmci_prog_update
*** Perform CICS NEWCOPY on WDEPLOY.MORTGAGE.INT.LOAD(EPSCMORT) on sysplex 'CICS01'
*** Perform CICS NEWCOPY on WDEPLOY.MORTGAGE.INT.LOAD(EPSMORT) on sysplex 'CICS01'
*** Perform CICS NEWCOPY on WDEPLOY.MORTGAGE.INT.LOAD(EPSMLIS) on sysplex 'CICS01'
** Evidences saved in /u/ado/workspace/MortgageApplication/main/build-20231019.13/logs/evidence.yaml

</details>


### 4.9 - wazideploy-evidence.sh

This script invokes the Wazi Deploy Evidence command to generate a Deployment report from the Wazi Deploy Evidence YAML file created by the Wazi Deploy Deploy command.

#### Invocation

The `wazideploy-evidence.sh` script can be invoked as follows:

Only mandatory parameters, and using relative paths:
```
wazideploy-evidence.sh -w MortgageApplication/main/build-20231101.15
```
or qualified paths with optional parameters:
```
wazideploy-evidence.sh -w /u/ado/workspace/MortgageApplication/main/build-20231101.15 -l /u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/evidences/evidence.yaml -o /u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/deployment-report.html
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. Optional, if `deploymentPlan`, `environmentFile`, `packageInputFile` and `evidenceFile` are fully referenced. 
-l `<evidenceFile>` | (Optional) Absolute or relative path to the **Evidence File** that contains the logs of all Wazi Deploy tasks. If not specified, evidence file location will be obtained from the `pipelineBackend.config`.
-o `<outputFile>` | (Optional) Absolute or relative path to the **Output File** that will contain the Deployment Report. If not specified, evidence file location will be obtained from the `pipelineBackend.config`.

#### Output

The section below contains the output that is produced by the `wazideploy-evidence.sh` script.

<details>
  <summary>Script Output</summary>
wazideploy-evidence.sh -w /u/ado/workspace/MortgageApplication/main/build-20231101.15 -l /u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/evidences/evidence.yaml -o /u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/deployment-report.html
wazideploy-evidence.sh: [INFO] Generate deployment reports with Wazi Deploy. Version=1.00

wazideploy-evidence.sh: [INFO] **************************************************************
wazideploy-evidence.sh: [INFO] ** Start Wazi Deploy Deployment on HOST/USER: z/OS ZT01 04.00 02 8561/***
wazideploy-evidence.sh: [INFO] **               Working Directory: /u/ado/workspace/MortgageApplication/main/build-20231101.15
wazideploy-evidence.sh: [INFO] **                     Output File: /u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/deployment-report.html
wazideploy-evidence.sh: [INFO] **                   Evidence File: /u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/evidences/evidence.yaml

wazideploy-evidence.sh: [INFO] **************************************************************


wazideploy-evidence --index /u/ado/workspace/MortgageApplication/main/build-20231101.15/index --dataFolder /u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/evidences i

Indexing '/u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/evidences' to '/u/ado/workspace/MortgageApplication/main/build-20231101.15/index' ...
1 new file scanned in 0.30 seconds
Index in '/u/ado/workspace/MortgageApplication/main/build-20231101.15/index':
    File count: 1 (1 new, 0 deleted)
    Field count: 51
    Record count: 64

wazideploy-evidence --index /u/ado/workspace/MortgageApplication/main/build-20231101.15/index --template /var/WaziDeploy/wazi-deploy-samples-0.10.0/wazi-deploy-sample/plum-samples/evidences/templates/full-report.yml --output=/u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/deployment-report.html r renderer=/var/WaziDeploy/wazi-deploy-samples-0.10.0/wazi-deploy-sample/plum-samples/evidences/renderers/full-report.html

1 record extracted to file '/u/ado/workspace/MortgageApplication/main/build-20231101.15/deploy/deployment-report.html' in 0.06 seconds


</details>

### 4.10 - prepareLogs.sh

Script to obtain the logs that were produced as part of the pipeline steps in the *logs* directory. 

#### Invocation

The `prepareLogs.sh` script can be invoked as follows:

```
prepareLogs.sh -w MortApp/main/build-1
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. 

On successful completion, the script writes a message to indicate the output directory with the log file name:
```
Logs successfully stored at /var/dbb/pipelineBackend/workspace/MortApp/feature/setmainbuildbranch/build-1/logs.tar
```

 #### Script output

The section below contains the output that is produced by the `prepareLogs.sh` script.

<details>
  <summary>Script Output</summary>

```
$ prepareLogs.sh -w MortApp/main/build-1
prepareLogs.sh: [INFO] **************************************************************
prepareLogs.sh: [INFO] ** Started - Prepare logs on HOST/USER: z/OS ZT01 04.00 02 8561/BPXROOT
prepareLogs.sh: [INFO] **          Workspace: /var/dbb/pipelineBackend/workspace/MortApp/main/build-1
prepareLogs.sh: [INFO] **             LogDir: /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs
prepareLogs.sh: [INFO] **************************************************************
prepareLogs.sh: [INFO] **  Directory contents:
prepareLogs.sh: [INFO] ls -RlTr logs
logs:
total 16
                    drwxr-xr-x   2 BPXROOT  ZSECURE     8192 Aug 30 11:53 build.20230830.105335.053

logs/build.20230830.105335.053:
total 240
- untagged    T=off -rw-r--r--   1 BPXROOT  ZSECURE       78 Aug 30 11:53 buildList.txt
t UTF-8       T=on  -rw-r--r--   1 BPXROOT  ZSECURE    18444 Aug 30 11:53 EPSNBRVL.cobol.log
t UTF-8       T=on  -rw-r--r--   1 BPXROOT  ZSECURE    59664 Aug 30 11:53 EPSCMORT.cobol.log
t UTF-8       T=on  -rw-r--r--   1 BPXROOT  ZSECURE     9227 Aug 30 11:53 BuildReport.json
t UTF-8       T=on  -rw-r--r--   1 BPXROOT  ZSECURE     7887 Aug 30 11:53 BuildReport.html
prepareLogs.sh: [INFO] tar -cf logs.tar logs
prepareLogs.sh: [INFO] Logs successfully stored at /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs.tar

```  

</details>

#### Download logs to non-z/OS runner/agent environment

While the script `prepareLogs.sh` only creates the TAR file on the workspace directory, the next step is to download the TAR file to the non-z/OS runner/agent environment, in order to attach it to the pipeline results.

The following sample illustrates the invocation of the `prepareLogs.sh` script and inspects the console output for the message `Logs successfully stored at ` indicating that a TAR file was successfully created. 
It _greps_ the information and invokes a download action.

```shell
    cmdStr="prepareLogs.sh -w MortApp/main/build-1"
    echo "$cmdStr"
    zowe zos-uss issue ssh "${cmdStr}" | tee prepareLogsOutput.txt

    # read logs 
    logsTar=$(cat prepareLogsOutput.txt | grep "Logs successfully stored at " | awk -F "stored at " ' { print $2 }')
    
    if [ ! -z "${logsTar}" ]; then
        # use zowe cli to download file to local environment
        # sftp / scp can be alternative download options.
        zowe zos-files download uss-file "$logsTar" -f ./logs/MortApp/main/build-1/logs.tar -b
    else 
        rc=4
        echo "[WARNING] Tar file containing the logs was not found. rc="$rc
    fi
```

### 4.11 - deleteWorkspace.sh

Script delete the workspace and all empty directories in the working tree. 

#### Invocation

The `deleteWorkspace.sh` script can be invoked as follows:

```
deleteWorkspace.sh -w MortApp/main/build-1
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. 


Note that the script deletes all empty folders in the working tree. It supresses the message `EDC5136I Directory not empty.` and handles that as a INFO message.

 #### Script output

The section below contains the output that is produced by the `deleteWorkspace.sh` script.

<details>
  <summary>Script Output</summary>

```
deleteWorkspace.sh: [INFO] Delete Workspace script. Version=1.00
deleteWorkspace.sh: [INFO] **************************************************************
deleteWorkspace.sh: [INFO] ** Started - Delete Workspace on HOST/USER: z/OS ZT01 05.00 02 8561/BPXROOT    
deleteWorkspace.sh: [INFO] **          Working Directory: /var/dbb/pipelineBackend/MortApp/main/build-1
deleteWorkspace.sh: [INFO] **          Workspace        : MortApp/main/build-1
deleteWorkspace.sh: [INFO] **************************************************************
deleteWorkspace.sh: [INFO] Deleting contents in /var/dbb/pipelineBackend/MortApp/main/build-1: 
deleteWorkspace.sh: [INFO] rm -Rfv /var/dbb/pipelineBackend/MortApp/main/build-1/*
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/info/exclude
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/applypatch-msg.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/commit-msg.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/post-update.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/pre-applypatch.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/pre-commit.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/prepare-commit-msg.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/pre-push.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/pre-rebase.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/update.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/hooks/post-checkout.sample
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/description
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/refs/heads/main
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/refs/remotes/origin/HEAD
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/config
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/HEAD
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/objects/pack/pack-ab68cc529d7cc1609635c0d093249af2cca3a49d.pack
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/objects/pack/pack-ab68cc529d7cc1609635c0d093249af2cca3a49d.rev
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/objects/pack/pack-ab68cc529d7cc1609635c0d093249af2cca3a49d.idx
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/logs/refs/remotes/origin/HEAD
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/logs/refs/heads/main
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/logs/HEAD
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/index
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.git/packed-refs
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.gitattributes
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/.project
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/MultibranchPipeline
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/README.md
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/application-conf/BMS.properties
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/application-conf/CRB.properties
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/application-conf/Cobol.properties
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/application-conf/LinkEdit.properties
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/application-conf/README.md
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/application-conf/application.properties
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/application-conf/baselineReference.config
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/application-conf/bind.properties
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/application-conf/file.properties
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/bms/epsmlis.bms
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/bms/epsmort.bms
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/cobol/epscmort.cbl
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/cobol/epscsmrd.cbl
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/cobol/epscsmrt.cbl
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/cobol/epsmlist.cbl
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/cobol/epsmpmt.cbl
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/cobol/epsnbrvl.cbl
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/copybook/epsmortf.cpy
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/copybook/epsmtcom.cpy
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/copybook/epsmtinp.cpy
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/copybook/epsmtout.cpy
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/copybook/epsnbrpm.cpy
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/copybook/epspdata.cpy
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/crb/cics-resourcesDef.yaml
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/link/epsmlist.lnk
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/logs/BuildReport.html
/var/dbb/pipelineBackend/MortApp/main/build-1/MortgageApplication/properties/epsmlist.cbl.properties
/var/dbb/pipelineBackend/MortApp/main/build-1/logs
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/buildList.txt
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/deletedFilesList.txt
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/EPSNBRVL.cobol.log
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/EPSCMORT.cobol.log
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/BuildReport.json
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/BuildReport.html
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/tempPackageDir/DBEHM.MORTGAGE.MAIN.BLD.LOAD/EPSCMORT.CICSLOAD
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/tempPackageDir/DBEHM.MORTGAGE.MAIN.BLD.DBRM/EPSCMORT.DBRM
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/tempPackageDir/buildReportOrder.txt
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/tempPackageDir/BuildReport.json
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/tempPackageDir/packageBuildOutputs.properties
/var/dbb/pipelineBackend/MortApp/main/build-1/logs/package.tar
/var/dbb/pipelineBackend/MortApp/main/build-1/logs.tar
deleteWorkspace.sh: [INFO] Deleting empty directories in working tree.
deleteWorkspace.sh: [INFO] rmdir -p /var/dbb/pipelineBackend/MortApp/main/build-1 2>&1
deleteWorkspace.sh: [INFO] Deleting empty directories stopped at below directory because it is not empty.
deleteWorkspace.sh: [INFO] rmdir: FSUM6404 directory "/var/dbb/pipelineBackend/MortApp/main": EDC5136I Directory not empty. 
deleteWorkspace.sh: [INFO] Workspace directory successfully deleted.
```  

</details>


## Disclaimer

THIS SAMPLE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

