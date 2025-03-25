# Common Backend Scripts for (any) Pipeline implementation

## Overview

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

## Setup

The provided scripts of this asset are implemented as bash scripts and need to be installed on UNIX System Services of the z/OS system that is used to execute the pipeline's tasks.

### Pre-requisites
The following are required to use these scripts:
* DBB v2.x toolkit is installed.
* zAppBuild is set up on Unix Systems Services.
* Git repository which follows the Git-based workflow outlined in IBM's documentation `The Git-based workflow for Mainframe development`.
* Build dependency information is available before performing the build run.


### Installation

* Copy/clone the Common Backend Scripts into z/OS UNIX System Services under a protected directory, e.g. `/usr/dbb/pipelineBackend`.
  * Update the permission of these scripts to allow for `read/execute` to only the users who will invoke the scripts. This is typically the technical user defined for the pipeline orchestrator. 

* The following environment variables need to be defined (for instance within the `.profile`) for the mainframe users who will execute the scripts on UNIX System Services:

  * `PIPELINE_SCRIPTS` - Environment variable to define the path to the Common Backend Scripts. 
  
     Add the directory path where you stored the scripts to the PATH of the pipeline user's profile, to make the scripts available to the consumers without referring to an absolute path within the pipeline configuration; most likely, this is the mainframe technical user used by the pipeline orchestrator. This avoids the need to set the working directory to the scripts when invoking them. In a non-interactive SSH setup, please make sure to initialize the environment variables for instance by executing the user's .profile, or by running a environment setup script.

  * (Optional) `PIPELINE_WORKSPACE` - Environment variable to configure the root workspace directory to run pipeline activities.

     Assumed to be a dedicated zFS filesystem that is in control of the pipeline user. Can be used in pipeline orchestration implementations to locate the path of logs or outputs. If not configured, the pipeline configuration needs to provide an absolute path to the working directory.

The below shows an extract of the pipeline user's `.profile` file:

  ```sh
  # extract of user's .profile to add the pipeline_config- [Common Backend Scripts for Pipeline Implementations](#common-backend-scripts-for-pipeline-implementations)
 
  #
  # env variable to define the path to the backend scripts
  export PIPELINE_SCRIPTS=/var/dbb/common-wrapper-scripts
  export PATH=$PIPELINE_SCRIPTS:$PATH 

  # environment variable to define the pipeline root workspace directory
  export PIPELINE_WORKSPACE=/var/dbb/pipeline-workspace
  ```

### Script configuration

The scripts are designed to be configurable through the [pipelineBackend.config](pipelineBackend.config) file. This configuration file is located in the same directory as all the backend script files. 

Although each script is designed to work independently of the other scripts, they share common properties such as the root workspace directory for the pipeline scripts, and the directory for log files. These common properties are defined in the [pipelineBackend.config](pipelineBackend.config) file which are used across the backend scripts. 

The following are common properties in the [pipelineBackend.config](pipelineBackend.config) file. 

 
Central configuration | Description
---------- | ----------------------------------------------------------------------------------------
buildRootDir | Absolute path to define the root workspace directory for pipeline executions, e.q. `/usr/pipeline/workspace`. Pipeline configurations can only pass a unique relative workspace.
logsDir | A relative directory name for logs and temporary outputs. Default: logs
zAppBuild and zBuilder settings | Multiple settings for zAppBuild, like path and credentials
UCD settings | Multiple settings for UCD server, like URL and credentials
Wazi Deploy settings | Multiple settings for Wazi Deploy Generation, Deployment and Evidence Requests commands

Central function | Description
---------- | ----------------------------------------------------------------------------------------
getWorkDirectory() | Central function to calculate the absolute path of the working directory
getLogDir() | Central function to calculate the absolute path of the log directory
getApplicationDir() | Central function to calculate the absolute path of the application directory (where the application is stored)

The details of the configuration settings are provided in the comments of the configuration file.

### Required workspace directory

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

## Invocation of scripts

Scripts can be invoked from a non-z/OS pipeline runner/agent via 
* SSH connection
* ZOWE CLI
* or natively, to include steps (such as build and packaging phase) in a pipeline configuration that is executed under z/OS UNIX System Services. 

### Invocation samples: non-interactive SSH session

A non-interactive SSH session comes with a lightweight setup and is not fully initialized, like an interactive session can be by automatically loading the user's profile. The environment should be setup through the user's profile. The following snippet requires `bash` to be part of the PATH environment variable:
```
ssh pipelineuser@lpar ". /u/pipelineuser/.profile && dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main"
```

An alternate configuration is to have `bash` defined as the default program in the OMVS segment of the user.

### Invocation samples: ZOWE CLI

Zowe CLI by default initializes the environment with the user's profile:
```
zowe zos-uss issue ssh "dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main
```

# Script Inventory

Artifact Name |  Description | Script details   
---------- | -----| -----------------------------------------------------
[gitClone.sh](gitClone.sh) | Pipeline Shell Script to perform Git Clone to z/OS UNIX System Services | [script details](README.md#41---gitclonesh)
[dbbBuild.sh](dbbBuild.sh) | Pipeline Shell Script to invoke the Dependency Based Build framework [zAppBuild](https://github.com/IBM/dbb-zappbuild) | [script details](#dbbbuildsh-for-zappbuild-frameworkh)
[zBuilder.sh](zBuilder.sh) | Pipeline Shell script to invoke the zBuilder framework [zBuilder](https://www.ibm.com/docs/en/dbb/3.0?topic=building-zos-applications-zbuilder) | [script details](#zbuildersh-for-dbb-zbuilder)
[packageBuildOutputs.sh](packageBuildOutputs.sh) | Pipeline Shell Script to create a Package using the [PackageBuildOutputs groovy script](https://github.com/IBM/dbb/tree/main/Pipeline/PackageBuildOutputs) | [script details](#packagebuildoutputssh)
[ucdPackage.sh](ucdPackaging.sh) | Pipeline Shell Script to publish to UCD Code Station binary repository using the [CreateUCDComponentVersion groovy script](https://github.com/IBM/dbb/tree/main/Pipeline/CreateUCDComponentVersion) | [script details](#ucdpackagingsh)
[wazideploy-generate.sh](wazideploy-generate.sh) | Pipeline Shell Script to generate a Deployment Plan to be used with Wazi Deploy | [script details](#wazideploy-generatesh)
[wazideploy-deploy.sh](wazideploy-deploy.sh) | Pipeline Shell Script to trigger a deployment of a package based on Deployment Plan with Wazi Deploy | [script details](#wazideploy-deploysh)
[wazideploy-evidence.sh](wazideploy-evidence.sh) | Pipeline Shell Script to query the Wazi Deploy Evidence YAML file and create a deployment report | [script details](#wazideploy-generatesh)
[ucdDeploy.sh](ucdDeploy.sh) | Pipeline Shell Script to trigger a UCD Deployment via its REST interface using the [DeployUCDComponentVersion groovy script](https://github.com/IBM/dbb/tree/main/Pipeline/DeployUCDComponentVersion) | [script details](#ucddeploysh)
[prepareLogs.sh](prepareLogs.sh) | Pipeline Shell Script to prepare a TAR file containing log files that can then be retrieved. | [script details](#preparelogssh)
[generateCleanupCommands.sh](generateCleanupCommands.sh) | Pipeline Shell Script to generate necessary DBB Metadatastore cleanup tasks including the deletion of the build datasets. | [script details](#generatecleanupcommandssh)
[deleteWorkspace.sh](deleteWorkspace.sh) | Pipeline Shell Script to delete the working directory on Unix System Services. | [script details](#deleteworkspacesh)


## Clone Repository with gitClone.sh

Script to clone a repository to z/OS UNIX System Services. Please note that it is not pulling for updates. 

### Invocation

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

### Output

The section below contains the output that is produced by the `gitClone.sh` script.

<details>
  <summary>Script Output</summary>

```
gitClone.sh: [INFO] Git Clone Wrapper. Version=1.2.0
gitClone.sh: [INFO] **************************************************************
gitClone.sh: [INFO] ** Start Git Clone on HOST/USER: z/OS ZT01 05.00 02 8561/***
gitClone.sh: [INFO] **          Repo: git@ssh.dev.azure.com:v3/IBM-DAT/retirementCalculator/retirementCalculator
gitClone.sh: [INFO] **       WorkDir: /u/ado/workspace/retirementCalculator/main/build-20240301.1
gitClone.sh: [INFO] **        GitDir: retirementCalculator
gitClone.sh: [INFO] **           Ref: main -> main
gitClone.sh: [INFO] **************************************************************
gitClone.sh: [INFO] Preforming Git Clone of Repo git@ssh.dev.azure.com:v3/IBM-DAT/retirementCalculator/retirementCalculator, Ref main to /u/ado/workspace/retirementCalculator/main/build-20240301.1
gitClone.sh: [INFO] git clone -b main git@ssh.dev.azure.com:v3/IBM-DAT/retirementCalculator/retirementCalculator
Cloning into 'retirementCalculator'...
gitClone.sh: [INFO] Git Status for retirementCalculator
On branch main
Your branch is up to date with 'origin/main'.
nothing to commit, working tree clean
gitClone.sh: [INFO] Git Show-Ref for retirementCalculator
c5fe1f8526fa2b20e7b6add8556aa773c9ab1d19 refs/heads/main
c5fe1f8526fa2b20e7b6add8556aa773c9ab1d19 refs/remotes/origin/HEAD
ca11720bdffc280fc9ddefdae41943bc5b2008fc refs/remotes/origin/feature/implementUnitTests
c5fe1f8526fa2b20e7b6add8556aa773c9ab1d19 refs/remotes/origin/main
8ee95ad28813cca2378463a2ef16958c60abcbd9 refs/tags/rel-1.0.0
f12465b6a9623033b6d4fcc393acbcfd71f8e8d8 refs/tags/rel-1.0.1
c7e7dcf9c65bebca8e3d0427c9ce82cb4127f7da refs/tags/rel-1.0.1_rc00
gitClone.sh: [INFO] Clone Repository Complete. rc=0
```  

</details>


## Build stage

### Conventions and capabilities

#### Git branch naming conventions

The build script follows the naming conventions for branches that are outlined in the document in the [solution guide](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev#naming-conventions):


Integration branches: 
* `main` as the main, long-living branch through which the application team delivers planned releases
* `release/rel-1.0.0` for release maintenance branch of a release that is deployed to production
* `epic/epic1234` for significant projects/initiatives 

Topic branches: 
* `feature/43-my-fancy-feature` for developing features contributing to the next planned release via the default development workflow.
feature/setmainbuildbranch
* `hotfix/rel-1.0.0/fixMortgageApplication` as the release maintenance feature branch, whereas the second segment is indicating the release
* `feature/epic1234/54-my-first-cool-new-feature` for feature development for epics / larger development initiatives


#### Baseline references config

The IBM recommended workflow approach leverages Git tags to identify the offset for calculating the changed files for a given deliverable. In this version, this utility script is retrieving the information from the [baselineReference.config](samples/baselineReference.config) file, that has to be maintained by the application team within the `application-conf` directory. It is the application teams' responsibility to maintain these references. 

Note that the location of the baselineReferences.config file can be customized in the [pipelineBackend.config](pipelineBackend.config) file.

[baselineReference.config](samples/baselineReference.config) is a sample, that indicates the baseline for the `main` and `release maintenance` branches.

#### Fetching build dependencies

The build stage can be enabled to pull external build dependencies into the build workspace based on the dependencies definition specified in the Application Descriptor file.

The Application Descriptor contains metadata about the application itself, but can contain the dependency configuration to other applications versions managed in an artifact repository, which contain necessary inputs to the build process. Additional information about the Application Descriptor can be found at the [dbb-git-migration-modeler](https://github.com/IBM/dbb-git-migration-modeler) project, which documents cross-application dependencies and generates Application Descriptor files.

In the `dependencies` section in the Application Descriptor file, users can configure which application versions should be fetched into the build workspace. The below snippet references the release build of the Cards application with the reference to `rel-1.2.0` and the concrete buildid `build-20241112.1`

```yaml
dependencies: 
- name: ”Cards"
  type: "release"
  reference: "rel-1.2.0"
  buildid: "build-20241112.1"
```

The Application Descriptor file, called `applicationDescriptor.yml`, is expected to be on the root level of the application's Git repository.

Each application version, represented by an archive, can export shared components such as public or shared include files, and build outputs such as object decks or NCAL load modules. The package needs to be created with the [PackageBuildOutputs](../../Pipeline/PackageBuildOutputs/README.md) script and be uploaded to the artifact repository through the Common Backend Scripts. To fetch the dependencies, it uses the subscript [fetchBuildDependenciesUtils.sh](utilities/fetchBuildDependenciesUtils.sh) that is referenced by both dbbBuild.sh and zBuilder.sh. Under the covers, it uses the [fetchBuildDependencies.groovy](utilities/fetchBuildDependencies.groovy) and the [ArtifactoryHelpers](../../Pipeline/PackageBuildOutputs/ArtifactRepositoryHelpers.groovy) script to download the external dependencies into the working directory. The downloaded archives can be stored at a cache location to improve performance. Fetched archives are expanded in the `imports` subfolder of the pipeline's working directory.

#### Fetch baseline package

(Prototype) Along with the fetching of external build dependencies, the fetch phase can retrieve the application's baseline package from the Artifact repository. This is configured through the `baselines` section of the Application Descriptor. Use the baseline if your application architecture uses static calls or required derived build outputs that is an mandatory input to subsequent builds. A good sample for derived build outputs are bms copybooks, that are inputs to CICS programs. Instead of storing the generated bms copybooks, it is made available through the baseline package.

Baseline archives are defined similarly like external dependencies. Under the `baselines` section, the application team manages the references for the corresponding build branch:

```
baselines: 
- branch: ”main"
  type: "release"
  reference: "rel-1.2.0"
  buildid: "build-7656"
```

### dbbBuild.sh for zAppBuild framework

This script implements the invocation of the [zAppBuild](https://github.com/IBM/dbb-zappbuild) framework. 

By design, the script implements the recommended working practice. It makes use of the [baselineRef sub-option](https://github.com/IBM/dbb-zappbuild/blob/main/docs/BUILD.md#perform-impact-build-by-providing-baseline-reference-for-the-analysis-of-changed-files) provided by zAppBuild to set the baseline Git hash. This is used to identify all the committed changes for the upcoming deliverable (that can be a planned release, a emergency fix, or a significant development initiative)

The computation of the build configuration is performed by the [dbbBuildUtils.sh](utilities/dbbBuildUtils.sh) script. It leverages the [application baseline configuration](samples/baselineReference.config) file which is expected to be present in the `application-conf` directory in order to compute the baseline reference.

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

#### Script utility - dbbBuildUtils.sh

The [dbbBuildUtils](utilities/dbbBuildUtils.sh) script is a core utility script providing the `computeBuildConfiguration()` method to compute the zAppBuild's options and parameters according to the branch naming conventions. For instance

* `build type`, such as the `--impactBuild` zAppBuild build option,
  * the baseline reference, `--baselineRef xxx`, where *xxx* is retrieved from the baselineReference.config file for integration branches, 
* the configured topic branch build behavior (see parameter `featureBranchBuildBehaviour` in pipelineBackend.config), that can either be configured as
  * `merge-base` (default) for cumulative builds that include all the changes added to the feature branch that flow to the integration branch. This setting automatically computes the merge-base commit, which defines the commit when the feature branch was forked.
  * `incremental` for standard zAppBuild `--impactBuild` behavior.
  * `cumulative` for computing all the differences between the topic branch and the integration branch by passing the `--baselineRef`. 
* flag to produce test modules (`--debug` in zAppBuild) or modules improved for performance (production runtime modules).
* the `mainBuildBranch` to configure feature branch pipelines to clone the corresponding DBB dependency metadata collections.

### zBuilder.sh for DBB zBuilder

This script implements the invocation of the [zBuilder](https://www.ibm.com/docs/en/dbb/3.0?topic=building-zos-applications-zbuilder) framework.

By design, the script implements the recommended working practice. It makes use of the [baselineRef sub-option](https://github.com/IBM/dbb-zappbuild/blob/documentation-review/docs/BUILD.md#perform-impact-build-by-providing-baseline-reference-for-the-analysis-of-changed-files) provided by zBuilder build lifecycles to set the baseline Git hash. This is used to identify all the committed changes for the upcoming deliverable (that can be a planned release, a emergency fix, or a significant development initiative)

The computation of the build configuration is performed by the [dbbzBuilderUtils.sh](utilities/dbbzBuilderUtils.sh) script. It leverages the [application baseline configuration](samples/baselineReference.config) file which is expected to be present in the `application-conf` directory in order to compute the baseline reference.

#### Invocation

The `zBuilder.sh` script can be invoked as follows:

```
zBuilder.sh -w MortApp/main/build-1 -a MortgageApplication -b main -p build
```

On purpose, it accepts the same input arguments as dbbBuild.sh.

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps.
-a `<application>` | **Application name** to be built, which is passed to zBuilder as the `--application` parameter.
-b `<branch>` | **Git branch** that is built. Used to compute various build properties such as the `--hlq` and build type.
-p `<build/release/preview>` | (Optional) **Pipeline Type** to indicate a `build` pipeline (build only with test/debug options) or a `release` pipeline (build for optimized load modules), or if it runs in `preview` mode.
-v | (Optional) zBuilder verbose tracing flag.
-t `<buildTypeArgument>` | (Optional) **zBuilder Build lifecycle** to override the build type, such as `full`, or `impact`. Arguments must be provided between quotes - e.g.: `-t 'full'`. Providing this parameter overrides the computation of the build type in the backend scripts. For instance can be used to initialize the DBB Metadatastore. 
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
+ zBuilder.sh -w /var/jenkins/workspace/nch_feature_74-test-zbuilder-cbs -a MortgageApplication -b feature/74-test-zbuilder-cbs -p build
zBuilder.sh: [INFO] DBB zBuilder Wrapper. Version=1.00
zBuilder.sh: [INFO] Created Pipeline Log directory (/var/jenkins/workspace/nch_feature_74-test-zbuilder-cbs/logs). rc=0
zBuilder.sh: [WARNING] Db2 JDBC User not set. It is recommended to use Db2 for the DBB Metadatastore.
zBuilder.sh: [WARNING] Db2 JDBC Password file not set. It is recommended to use Db2 for the DBB Metadatastore.
zBuilder.sh: [INFO] **************************************************************
zBuilder.sh: [INFO] ** Started - DBB Build on HOST/USER: z/OS ZT01 05.00 02 8561/
zBuilder.sh: [INFO] **          Workspace: /var/jenkins/workspace/nch_feature_74-test-zbuilder-cbs
zBuilder.sh: [INFO] **        Application: MortgageApplication
zBuilder.sh: [INFO] **             Branch: feature/74-test-zbuilder-cbs
zBuilder.sh: [INFO] **      Pipeline Type: build
zBuilder.sh: [INFO] **    Build Lifecycle: impact --baselineRef origin/main
zBuilder.sh: [INFO] **                HLQ: JENKINS.PIPELINE.MORTGAGE.F74
zBuilder.sh: [INFO] **             AppDir: /var/jenkins/workspace/nch_feature_74-test-zbuilder-cbs/MortgageApplication
zBuilder.sh: [INFO] **      zBuilder Path: /var/dbb/zBuilder/build
zBuilder.sh: [INFO] **      zBuilder Logs: /var/jenkins/workspace/nch_feature_74-test-zbuilder-cbs/MortgageApplication/logs
zBuilder.sh: [INFO] **           DBB_HOME: /usr/lpp/dbb/v3r0
zBuilder.sh: [INFO] **      DBB JDBC USER:
zBuilder.sh: [INFO] **  DBB JDBC Pwd File:
zBuilder.sh: [INFO] **         DBB Logger: No
zBuilder.sh: [INFO] **   Pipeline Log Dir: /var/jenkins/workspace/nch_feature_74-test-zbuilder-cbs/logs
zBuilder.sh: [INFO] **************************************************************

zBuilder.sh: [INFO] Invoking the zBuilder Build Framework.
zBuilder.sh: [INFO] /usr/lpp/dbb/v3r0/bin/dbb build impact --baselineRef origin/main --hlq JENKINS.PIPELINE.MORTGAGE.F74 --log-encoding UTF-8
IBM Dependency Based Build 3.0.0.1

BUILD

Lifecycle: impact
Task: Start
> Build start at 20241213.032138.021
> Started by 'JENKINS' on 'ZT01'
Task: ScannerInit
Task: MetadataInit
Task: ImpactAnalysis
> Changed Files  : 2 
> Impacted Files : 1 
Stage: Languages
Language: Cobol
> Building 'MortgageApplication/cobol/epsnbrvl.cbl'
> Building 'MortgageApplication/cobol/epscmort.cbl'
Task: Finish
> Build ended at 20241213.032142.021
> Duration of build : 00 min, 04 sec
> Total files processed : 2
> Build Status : CLEAN
zBuilder.sh: [INFO] Copied build logs from /var/jenkins/workspace/nch_feature_74-test-zbuilder-cbs/MortgageApplication/logs to /var/jenkins/workspace/nch_feature_74-test-zbuilder-cbs/logs. rc=0
zBuilder.sh: [INFO] DBB Build Complete. rc=0

```  

</details>

#### Script utility - dbbzBuilderUtils.sh

The [dbbzBuilderUtils](utilities/dbbzBuilderUtils.sh) script is a core utility script providing the `computeBuildConfiguration()` method to compute additional zBuilder CLI options and parameters according to the branch naming conventions. For instance

* `build lifecycle`, such as the `impact` zAppBuild build option,
  * the baseline reference, `--baselineRef xxx`, where *xxx* is retrieved from the baselineReference.config file for integration branches, 
* the configured topic branch build behavior (see parameter `featureBranchBuildBehaviour` in pipelineBackend.config), that can either be configured as
  * `merge-base` (default) for cumulative builds that include all the changes added to the feature branch that flow to the integration branch. This setting automatically computes the merge-base commit, which defines the commit when the feature branch was forked.
  * `incremental` for standard zBuilder `--impactBuild` behavior.
  * `cumulative` for computing all the differences between the topic branch and the integration branch by passing the `--baselineRef`. 
<!-- flag to produce test modules (`--debug` in zAppBuild) or modules improved for performance (production runtime modules). -->
* the `mainBuildBranch` to configure feature branch pipelines to clone the corresponding DBB dependency metadata collections by generating a config.yaml that is passed into zBuilder.

## Packaging stage

Depending on the Deployment Manager tool you are using, you can choose from either creating a package with the [PackageBuildOutputs](#packagebuildoutputssh) script that can be used with IBM Wazi Deploy, or the [UCD packaging](#ucdpackagingsh) script that creates the UCD shiplist and UCD component version.

### packageBuildOutputs.sh

This script is to execute the `PackageBuildOutputs.groovy` that packages up the build outputs and optionally uploads it to an artifact repository to publish the artifacts created by a DBB build in the pipeline.

When uploading the archive to an artifact repository, this script implements naming conventions for the repository layout. The conventions are implemented in [utilities/packageUtils.sh](utilities/packageUtils.sh).
The rules for the naming conventions are detailed hereafter.

For any preliminary build (that uses the `pipelineType=build`), the outputs are uploaded into the directory `build/<reference>/<application>-<buildIdentifier>`:

* **build** is defined for any builds, that are considered to some extend temporary and preliminary. 
* **reference** is the name of the branch which the build originates from: for instance, `feature/123-update-mortgage-computation`, `main` or any hotfix and epic branches.
* The archive's file name is computed using the application's name and a unique build identifier (`-i` argument). This parameter is typically the pipeline build number that is passed by the pipeline orchestrator. If a build identifier is not provided, the current timestamp is used.

For release builds (that use the `pipelineType=release`), the archive is uploaded to the directory `release/<reference>/<application>-<buildIdentifier>`:

* **release** is defined for release builds. 
* **reference** is the release name: for instance, `rel-1.2.3` (provided through the mandatory `-r` argument).
The archive's file name is computed using the application's name, the release name (`-r` argument) and a unique build identifier (`-i` argument). This parameter is typically the pipeline build number that is passed by the pipeline orchestrator. If a build identifier is not provided, the current timestamp is used.


#### Invocation

The `packageBuildOutputs.sh` script can be invoked as follows:

- Package only

```
packageBuildOutputs.sh -w MortApp/main/build-1 -t rel-1.0.0.tar
```
- Package and Upload
```
packageBuildOutputs.sh -w MortApp/main/build-1 -t rel-1.0.0.tar -a MortgageApplication -b main -u -p release -r rel-1.0.0 -i 4657
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
**Packaging options**
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. The `packageBuildOutputs.sh` script is evaluating the logs directory.
-t `<tarFileName>` | (Optional) Name of the **tar file** to create.
**Artifact Upload options**
-u | Flag to enable upload of outputs to the configured artifact repository.
-a `<application>` | **Application name** leveraged to define the artifact repository name.
-b `<branch>`| Name of the **git branch** turning into a segment of the directory path in the artifact repository. Naming convention rules are implemented in `utilities/packageUtils.sh`.
-p `<build/release>` | **Pipeline type** to indicate a `build` pipeline (build only with test/debug options) or a `release` pipeline (build for  optimized load modules) to determine the directory in the artifact repository for development and pipeline builds.
-r `<releaseIdentifier>` | **Release identifier** to indicate the next planned release name. This is a computed value based on the pipeline templates.
-i `<buildIdentifier>` | **Build identifier** a unique value, typically the build number provided by the pipeline orchestrator or a timestamp. Used to help qualifying the archive file. This is a computed value provided by the pipeline templates.
-v `<artifactVersion>` **deprecated** | Label of the **version** in the artifact repository turning into a segment of the directory path in the artifact repo. Deprecated - switch to `-r <releaseIdentifier>` and `-i <buildIdentifier>`.
-s `"<sbomAuthor>"` | (Optional) Name and email of the SBOM author enclosed with double quotes. Ex: "Build Engineer \<engineer@example.com\>" 

Check out the pipelineBackend.config to define the `artifactRepositoryNameSuffix` that is appended to the application name to set the repository name in the artifact repository.

#### Script conventions

**SBOM Generation**

The generation of an SBOM is controlled by the `generateSBOM` property defined in the [pipelineBackend.config](pipelineBackend.config) file. The default SBOM Author is also specified in the [pipelineBackend.config](pipelineBackend.config) file in the `sbomAuthor` property, but this property can be overridden with the `-s` parameter of this script. When the SBOM Author is provided as a parameter, it automatically enables the SBOM generation, even if set to `false` in the [pipelineBackend.config](pipelineBackend.config) file.

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

packageBuildOutputs.sh: [INFO] Invoking the ArtifactRepositoryHelper groovy script.
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



### ucdPackaging.sh

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


## Deployment Stage

Depending on the selected Deployment tool, select either from the scripts for IBM Wazi Deploy ([wazideploy-generate.sh](#wazideploy-generatesh), [wazideploy-deploy.sh](#wazideploy-deploysh) and [wazideploy-generate.sh](#wazideploy-evidencesh)) or [UCD deployment](#ucddeploysh) to submit a deployment request in IBM UrbanCode Deploy.


### wazideploy-generate.sh

This script invokes the Wazi Deploy Generate command to generate a Deployment Plan based on the content of a package. The package should be created with the `PackageBuildOutputs.groovy` script or through the `packageBuildOutputs.sh` script.

This script assesses the configuration option `publish` from the `pipelineBackend.config` file. In case the configuration has enabled the upload to the Artifact repository, the script computes the URL where the package is expected to be found, and passes the URL into the wazideploy-generate command. This means that wazideloy-generate will download the package from the Artifact repository and allows to restore the package on a different system. This step leverages the generic utility script computePackageUrl. It requires to pass in the additional arguments `-P`, `-R`, `-B`

#### Invocation

The `wazideploy-generate.sh` script can be invoked as follows:

```
wazideploy-generate.sh -w  MortApp/main/build-1 -i MortgageApplication.tar                                     
```
Or by fully specifying the settings
```
wazideploy-generate.sh -m deploymentMethod -p deploymentPlan -r deploymentPlanReport -i packageInputFile
```

To enable the download based on build and release identifier
```
wazideploy-generate.sh -w  MortgageApplication/feature/15-fetch-application-dependencies/dbb-zappbuild.build_1234 -a MortgageApplication -P build -b feature/15-fetch-application-dependencies -I 1234
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory**, an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. Optional, if `deploymentPlan`, `deploymentPlanReport` and `packageOutputFile` are fully referenced. 
-i `<packageInputFile>` | **Package Input File** to be used for the generation phase with Wazi Deploy. This is likely the package to be deployed. If providing a relative path, the file is assumed to be located in the directory `<workspace directory>/<logsDir>`. This parameter can either be path to an archive (TAR file) on UNIX System Services, or the URL of the archive (TAR file) to retrieve (only Artifactory is supported). <br><br> If the Common Backend Scripts are configured to perform the upload, the scripts computes the location in the artifact repo and overrides the `-i` and `-o` argument and in that case the this arument is not required.
-m `<deploymentMethod>` | (Optional) Absolute path to the Wazi Deploy **Deployment Method** stored on UNIX System Services. If not specified, the deployment method file location is obtained from the `pipelineBackend.config`.
-p `<deploymentPlan>` | (Optional) Absolute or relative path to the **Deployment Plan** file, generated based on the content of the input package. If providing a relative path, the file path is prefixed with Wazi Deploy Packaging directory `<wdDeployPackageDir>` configured in `pipelineBackend.config`.  If not specified, the deployment plan location is obtained from the `pipelineBackend.config`.
-r `<deploymentPlanReport>` | (Optional) Absolute or relative path to the **Deployment Plan Report**. If providing a relative path, the file path is prefixed with Wazi Deploy Packaging directory `<wdDeployPackageDir>` configured in `pipelineBackend.config`. If not specified, the deployment plan report location is obtained from the `pipelineBackend.config`.
-o `<packageOutputFile>` | (Optional) Absolute or relative path to the **Package Output File** that specifies the location where to store the downloaded tar file. If providing a relative path, the file path is prefixed with Wazi Deploy Packaging directory `<wdDeployPackageDir>` configured in `pipelineBackend.config`. Only required when wazideploy-generate is used to download the package. This is indicated when a URL is specified for the **Package Input File**.

-d | (Optional) Debug tracing flag. Used to produce additional tracing with Wazi Deploy.
-- | - when retrieving the tar file from Artifact repo the below options are mandatory -
-b `<branch>`| Name of the **git branch** turning into a segment of the directory path for the location within the artifact repository.
-p `<build/release>` | **Pipeline Type** to indicate a `build` pipeline (build only with test/debug options) or a `release` pipeline (build for optimized load modules for release candidates).
-R `<releaseIdentifier>` | **Release identifier** to indicate the next planned release name. This is a computed value based on the pipeline templates.
-I `<buildIdentifier>` | **Build identifier** a unique value to identify the tar file. This is a computed value provided by the pipeline templates. Typically the build number of the pipeline run.
-c `<configurationFile>` | Absolute path to the Wazi Deploy **Configuration File** that contains information to connect to the artifact repository. See [IBM Wazi Deploy documentation](https://www.ibm.com/docs/en/developer-for-zos/17.0?topic=files-configuration-file)

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

### wazideploy-deploy.sh

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
-p `<deploymentPlan>` | (Optional) Absolute or relative path to the **Deployment Plan** file, generated based on the content of the input package. If not specified, the location of the deployment plan is obtained from the `pipelineBackend.config`.
-e `<environmentFile>` | (Optional) Absolute or relative path to **Environment File**, that describes the target z/OS environment. If a relative path is provided, the environment file is located based on the setting `wdEnvironmentConfigurations` in `pipelineBackend.config`.
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


### wazideploy-evidence.sh

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

### ucdDeploy.sh

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

## Generic stages

When your pipeline setup uses a remote agents that runs outside of the z/OS build machine (like with an agent on an x86 environment), you can benefit from the [prepareLogs.sh](#preparelogssh) script to create an archive of the logs, before transferring it to the remote agent for attaching them to the pipeline run.

Once the pipeline has completed all its tasks, you can use the [deleteWorkspace.sh](#deleteworkspacesh) to cleanup the created workspace on z/OS Unix Services and optionally the [generateCleanupCommands.sh](#generatecleanupcommandssh) for housekeeping activities of the DBB metadataststore. 

### prepareLogs.sh

Script to obtain the logs that were produced as part of the pipeline step in the *logs* directory. 

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

### deleteWorkspace.sh

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

### generateCleanupCommands.sh

Script to generate and run the necessary cleanup steps of DBB Metadatastore collections and build groups (build results), and the deletion of the build datasets using the [DeletePDS.groovy](../../Utilities/DeletePDS/README.md) utility.

The script lists all the existing DBB collections obtained by applying a filter based on the zAppBuild naming conventions. It checks if Git branches corresponding to the provided application name exist in the Git repository. If one or more branches are found, it generates the necessary command files that contain the removal statements. The generated scripts can be can automatically executed, if the `-p` flag is passed to the script.

#### Invocation

The `generateCleanupCommands.sh` script can be invoked as follows:

```
generateCleanupCommands.sh -w MortApp/main/build-1 -a MortApp -p
```

CLI parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory** - an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. 
-a `<application>` | **Application name** to be analyzed for stale DBB Metadatastore objects and build datasets.
-p | Flag to control if the generated commands files should be executed by the pipeline. If the commands are not executed by the script, it is recommended to publish the generated files to the pipeline orchestrator, where an administrator can review and eventually execute them manually.

#### Additional notes

This script can be embedded into a pipeline execution, but can also be used in a standalone setup. For a pipeline implementation, this task can be included in the release process to facilitate the cleanup of stale DBB collections and DBB build groups, and to delete the build datasets as well.

For the standalone implementation, use the following process:
1. Have the Common Backend Scripts installed to z/OS Unix System Services and have them configured. 
2. Clone the application repository including all remote references.
3. Execute the `generateCleanupCommands.sh` script like in the above sample. The user executing the script needs proper permissions on the DBB Metadatastore.

Please note that the script leverages the [utilities/dbbBuildUtils.sh](utilities/dbbBuildUtils.sh) to compute the build high-level qualifier (HLQ).

### Script output

The section below contains the output that is produced by the `generateCleanupCommands.sh` script.

<details>
  <summary>Script Output</summary>

```
". ./.profile && generateCleanupCommands.sh -w /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104 -a MortgageApplication -p"
generateCleanupCommands.sh: [INFO] Generate Cleanup Command File. Version=1.0.0
generateCleanupCommands.sh: [INFO] Creating output directory. /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds
generateCleanupCommands.sh: [INFO] **************************************************************
generateCleanupCommands.sh: [INFO] ** Start Gen Cleanup Cmds on HOST/USER: z/OS ZT01 05.00 02 8561/GITHUB
generateCleanupCommands.sh: [INFO] **                   Workspace: /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104
generateCleanupCommands.sh: [INFO] **                 Application: MortgageApplication
generateCleanupCommands.sh: [INFO] **                      AppDir: /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/MortgageApplication
generateCleanupCommands.sh: [INFO] **    Cmd obsolete collections: /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds/deleteStaleCollections.cmd
generateCleanupCommands.sh: [INFO] **   Cmd obsolete build groups: /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds/deleteStaleBuildGroups.cmd
generateCleanupCommands.sh: [INFO] ** Cmd obsolete build datasets: /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds/deleteStaleBuildDatasets.cmd
generateCleanupCommands.sh: [INFO] **      DBB Metadastore Config: --type file --location /u/github/
generateCleanupCommands.sh: [INFO] **     Process Cleanup Scripts: true
generateCleanupCommands.sh: [INFO] **************************************************************

generateCleanupCommands.sh: [STAGE] Retrieve all collections with application qualifier MortgageApplication
generateCleanupCommands.sh: [STAGE] Verifying Git references
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-main has a corresponding active Git branch
generateCleanupCommands.sh:        For the collection MortgageApplication-main a corresponding branch (main) was detected.
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-main-outputs has a corresponding active Git branch
generateCleanupCommands.sh:        For the collection MortgageApplication-main-outputs a corresponding branch (main) was detected.
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-feature/johnpipelinetesting has a corresponding active Git branch
generateCleanupCommands.sh:        For the collection MortgageApplication-feature/johnpipelinetesting a corresponding branch (feature/johnpipelinetesting) was detected.
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-feature/johnpipelinetesting-outputs has a corresponding active Git branch
generateCleanupCommands.sh:        For the collection MortgageApplication-feature/johnpipelinetesting-outputs a corresponding branch (feature/johnpipelinetesting) was detected.
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-feature/20-some-more-testing has a corresponding active Git branch
generateCleanupCommands.sh:        DBB Collection MortgageApplication-feature/20-some-more-testing does not have a corresponding branch (feature/20-some-more-testing). It can be deleted.
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-feature/20-some-more-testing-outputs has a corresponding active Git branch
generateCleanupCommands.sh:        DBB Collection MortgageApplication-feature/20-some-more-testing-outputs does not have a corresponding branch (feature/20-some-more-testing). It can be deleted.
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-feature/pipeline-trigger-testing has a corresponding active Git branch
generateCleanupCommands.sh:        For the collection MortgageApplication-feature/pipeline-trigger-testing a corresponding branch (feature/pipeline-trigger-testing) was detected.
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-feature/pipeline-trigger-testing-outputs has a corresponding active Git branch
generateCleanupCommands.sh:        For the collection MortgageApplication-feature/pipeline-trigger-testing-outputs a corresponding branch (feature/pipeline-trigger-testing) was detected.
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-feature/18-add-generate-cleanup-instructions has a corresponding active Git branch
generateCleanupCommands.sh:        For the collection MortgageApplication-feature/18-add-generate-cleanup-instructions a corresponding branch (feature/18-add-generate-cleanup-instructions) was detected.
generateCleanupCommands.sh: [INFO] Check if MortgageApplication-feature/18-add-generate-cleanup-instructions-outputs has a corresponding active Git branch
generateCleanupCommands.sh:        For the collection MortgageApplication-feature/18-add-generate-cleanup-instructions-outputs a corresponding branch (feature/18-add-generate-cleanup-instructions) was detected.
generateCleanupCommands.sh: [STAGE] Generate Cmd File with Delete Statements for stale collections for application MortgageApplication
generateCleanupCommands.sh: [INFO] Cmd File /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds/deleteStaleCollections.cmd created. 
generateCleanupCommands.sh: [STAGE] Generate Cmd File with Delete Statements for stale build groups for application MortgageApplication
generateCleanupCommands.sh: [INFO] Cmd File /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds/deleteStaleBuildGroups.cmd created. 
generateCleanupCommands.sh: [STAGE] Generate Cmd File with Delete Statements for stale build datasets for application MortgageApplication
generateCleanupCommands.sh: [INFO] Cmd File /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds/deleteStaleBuildDatasets.cmd created. 
generateCleanupCommands.sh: [STAGE] Executing Cleanup of DBB Metadatastore Objects
generateCleanupCommands.sh: [INFO] Executing cleanup script /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds/deleteStaleCollections.cmd
BGZTK0195I Successfully deleted collection "MortgageApplication-feature/20-some-more-testing"
BGZTK0195I Successfully deleted collection "MortgageApplication-feature/20-some-more-testing-outputs"
generateCleanupCommands.sh: [INFO] Executing cleanup script /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds/deleteStaleBuildGroups.cmd
BGZTK0195I Successfully deleted group "MortgageApplication-feature/20-some-more-testing"
generateCleanupCommands.sh: [INFO] Executing cleanup script /u/github/workspace/IBM-DAT/MortgageApplication/feature/18-add-generate-cleanup-instructions/build_f104/cleanupCmds/deleteStaleBuildDatasets.cmd
** Deleting all datasets filtered with HLQ 'GITHUB.MORTGAGE.F20'
*** Deleting 'GITHUB.MORTGAGE.F20.COBOL'
*** Deleting 'GITHUB.MORTGAGE.F20.COPY'
*** Deleting 'GITHUB.MORTGAGE.F20.DBRM'
*** Deleting 'GITHUB.MORTGAGE.F20.LOAD'
*** Deleting 'GITHUB.MORTGAGE.F20.OBJ'
** Deleted 5 entries.
** Build finished
generateCleanupCommands.sh: [INFO] Generate Cleanup Cmds Complete. rc=0

```  

</details>





# Disclaimer

THIS SAMPLE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

