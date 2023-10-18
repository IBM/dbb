# Common Backend Scripts for Pipeline Implementations

## 1 - Overview

The Common Backend Scripts for Pipeline Implementations is an asset that is delivering central "services" and a simplified interface for pipeline configurations that implement a Git/DBB-based pipeline for Mainframe applications.

Implementing the pipeline configurations such as an Azure pipeline, a JenkinsFile, or the gitlab-ci.yml file requires accommodation of the selected development workflow with Git. Rules must be implemented in pipeline code / configurations to address
* naming conventions of build datasets,
* configuration parameters of the build framework,
* or naming conventions of the binary package

to achieve consistency across the pipelines for the various applications.

The community is providing a set of [pipeline tasks](../../Pipeline/) that implement the various stages of the pipeline orchestration. The tasks being implemented as groovy scripts accept/require various parameters within some being related to the application, while others represent a technical configuration that is required for the script to operate.

The purpose of these common backend scripts is to reduce the necessary configuration and scripting within a concrete pipeline orchestration logic and let the application and pipeline teams focus on the application specific parameters.

Instead of mixing **orchestration of tasks** and **implementation of the pipeline rules** (such as computation of build hlq, build type, etc) in the specific pipeline technology e.g. a Jenkinsfile or a gitlab-ci.yaml definition, the common backend scripts provide central services to the pipeline orchestrator - independent of the chosen orchestration technology.

By simplifying the invocation of the scripts, these scripts support the DevOps engineer to implement the pipeline configurations faster, especially for pipeline orchestrators that do not have a runner/agent available on Unix System Services.

In addition, this repository contains a [test driver](test/) to outline how the scripts can be remotely invoked from a x86 system. Additional templates for the various pipeline orchestrators (such as an azure-pipeline.yaml), is planned to be provided as well, that make use of the Common Backend Scripts.

This asset implements the rules and conventions of the Git-based workflow outlined in IBMs documentation `The Git-based workflow you need for Mainframe development` with a defined Git branching, build and packaging strategy.

## 2 - Set up

The provided scripts of this asset are implemented as bash scripts and need to be installed on UNIX System Services of the z/OS LPAR that is used for the pipeline. 

### 2.1 - Pre-requisites
The following are required to use these scripts.
* DBB v2.x toolkit is installed.
* zAppBuild is set up on Unix Systems Services.
* Git repository which follows the Git-based workflow outlined in IBMs documentation `The Git-based workflow for Mainframe development`.
* Build dependency information is available before performing the build run.


### 2.2 - Installation

* Copy/Clone the backend wrapper scripts into Unix System Services files under a protected directory, E.q. `/usr/dbb/pipelineBackend`.
  * Update the permission of these scripts to allow for `read/execute` to only those users, who will invoke the scripts. This is typically the technical user of the pipeline orchestrator. 

* The following environment variables need to be defined (for instance within the `.profile`) for the mainframe users who will execute the scripts on UNIX System Services:

  * `PIPELINE_SCRIPTS` - Environment variable to define the path to the common backend scripts. 
  
     Add the directory path where you stored the scripts to the PATH of the pipeline users profile, to make the scripts available to the consumers w/o referring to an absolute path within the pipeline configuration - most likely this is the MF technical user of the pipeline orchestrator. This avoids the need to change directory to the scripts to invoke them. In a non-interactive ssh setup, please make sure to initialize the environment variables for instance by executing the users .profile, or by running a environment setup script.

  * (Optional) `PIPELINE_WORKSPACE` - Environment variable to configure the root workspace directory to process pipeline activities.

     Assumed to be a dedicated mounted zFS file system that is in control of the pipeline user. Can be used in pipeline orchestration implementations to locate the path of logs or outputs. If not configured, the pipeline configuration needs to provide an absolute path to the working directory.

The below shows an extract of the pipeline users `.profile`:

  ```sh
  # extract of users .profile to add the pipeline_config
  #
  # env variable to define the path to the backend scripts
  export PIPELINE_SCRIPTS=/var/dbb/common-wrapper-scripts
  export PATH=$PIPELINE_SCRIPTS:$PATH 

  # env variable to define the pipeline root directory
  export PIPELINE_WORKSPACE=/var/dbb/pipeline-workspace
  ```

### 2.3 - Script configuration

The scripts are designed to be configurable through the [pipelineBackend.config](pipelineBackend.config) file. This configuration file is located on the same directory as all the backend script files. 

Although, each script is designed to work independently of the other scripts, they share common properties such as the root directory for the pipeline scripts, and the directory for log files. These common properties are defined in the [pipelineBackend.config](pipelineBackend.config) file which are utilized across the backend scripts. 

The following are common properties in the [pipelineBackend.config](pipelineBackend.config) file. 

 
Central configuration | Description
---------- | ----------------------------------------------------------------------------------------
buildRootDir | Absolute path to define the root directory for pipeline executions, e.q. `/usr/pipeline/workspace`. Pipeline configurations can only pass a unique relative workspace.
logsDir | A relative directory name for logs and temporary outputs. Default: logs
zAppBuild settings | multiple settings for zAppBuild - like path and credentials
UCD settings | multiple settings for ucd server url or credentials

Central function | Description
---------- | ----------------------------------------------------------------------------------------
getWorkDirectory() | central function to calculate the absolute path of the working directory
getLogDir() | central function to calculate the absolute path of the log directory
getApplicationDir() | central function to calculate the absolute path of the application directory (where the application is stored)

The details of the configuration settings are provided in the notes section of the configuration file.


### 2.4 - Required workspace directory

All the scripts are designed to have a unique working directory or workspace. The workspace is for managing the clone of the git repository, and the log and output directories to avoid any conflicts and collisions. When invoking any of the script, the workspace is a required parameter which can either be an absolute path or a relative path. 

If a relative path is provided, the value of the workspace parameter is combined with the `buildRootDir` setting that is defined in the [pipelineBackend.config](pipelineBackend.config) as : `<buildRootDir>/<workspace>`.

In the below sample, we use a workspace path consisting of 3 segments which are the **application name**, the **branch name** and the **pipeline build id**. This is the recommended approach, to ensure a unique workspace directory on Unix System Services:

```
<Application>/<branch>/<pipeline-id>
```
The branch and pipelineID segments are resolved from the pipeline orchestrator via it's built-in variables to:
```
MortApp/main/build-1
```

## 3 - Invocation of scripts

Scripts can be invoked from the distributed pipeline runner via 
* SSH connection
* ZOWE CLI
* or natively, to include steps (such as build and packaging phase) in a pipeline configuration that is executed under Unix System Services. 

### 3.1 - Invocation samples: Non-interactive ssh session

A non-interactive ssh session has a light setup and is not fully initialized like an interactive session that loads the users profile. Please setup the environment through the users profile. That needs bash on its' PATH, like the below:
```
ssh pipelineuser@lpar ". /u/pipelineuser/.profile && dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main"
```

### 3.2 - Invocation samples: ZOWE CLI

Zowe CLI by default initializes  the environment with the users profile.
```
zowe zos-Unix System Services issue ssh "dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main
```

## 4 - Script Inventory

Artifact Name |  Description | Script details   
---------- | -----| -----------------------------------------------------
[gitClone.sh](gitClone.sh) | Pipeline Shell Script to perform Git Clone to Unix System Services | [script details](README.md#41---gitclonesh)
[dbbBuild.sh](dbbBuild.sh) | Pipeline Shell Script to invoke the Dependency Based Build framework [zAppBuild](https://github.com/IBM/dbb-zappbuild) | [script details](README.md#42---dbbbuildsh)
[utilities/dbbBuildUtils.sh](utilities/dbbBuildUtils.sh) | Utility Shell Script to implementing the computation of build configuration, such as HLQ, build type or property overrides. | [script details](README.md#43---script-capabilities--dbbbuildutilssh)
[packageBuildOutputs.sh](packageBuildOutputs.sh) | Pipeline Shell Script to create a Package using the [PackageBuildOutputs groovy script](https://github.com/IBM/dbb/tree/main/Pipeline/PackageBuildOutputs) | [script details](README.md#44---packagebuildoutputssh)
[ucdPackage.sh](ucdPackaging.sh) | Pipeline Shell Script to publish to UCD Code Station binary repository using the [CreateUCDComponentVersion groovy script](https://github.com/IBM/dbb/tree/main/Pipeline/CreateUCDComponentVersion) | [script details](README.md#45---ucdpackagingsh)
[ucdDeploy.sh](ucdDeploy.sh) | Pipeline Shell Script to trigger a UCD Deployment via its REST interface using the [DeployUCDComponentVersion groovy script](https://github.com/IBM/dbb/tree/main/Pipeline/DeployUCDComponentVersion) | [script details](README.md#46---ucddeploysh)
[prepareLogs.sh](prepareLogs.sh) | Pipeline Shell Script to prepare a tar file containing log files that can then be retrieved. | [script details](README.md#47---preparelogssh)


### 4.1 - gitClone.sh

Script to clone a repository to Unix System Services. Please note that it is not pulling for updates. 

#### Invocation

The gitClone.sh script can be invoked as below:

```
gitClone.sh -w MortApp/main/build-1 -r git@github.ibm.com:zDevOps-Acceleration/MortgageApplication.git -b main
```

Cli parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory** an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. 
-r `<repoURL>` | **Git repository url**, can either be ssh or https. Ex. `-r git@github.ibm.com:zDevOps-Acceleration/MortgageApplication.git`
-b `<branch>` | **Git branch** that should be checked out. Ex. `-b main`

**Dealing with Private repositories**

You can pass on the credentials via the invocation as:
```
gitClone.sh -w MortApp/main/build-1 -r https://<personal-access-token>@github.com/dennis-behm/dbb-zappbuild-private.git -b main 
```
while credentials should be manages in the secret vault of your pipeline orchestrator.

#### Output

Expand the section below to view the output that is produced by the `gitClone.sh` script.

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

This script is implementing the invocation of the [zAppBuild](https://github.com/IBM/dbb-zappbuild) framework. Per the design, it leverages the [baselineRef sub-option](https://github.com/IBM/dbb-zappbuild/blob/documentation-review/docs/BUILD.md#perform-impact-build-by-providing-baseline-reference-for-the-analysis-of-changed-files) of zAppBuild. The [dbbBuildUtils.sh](utilities/dbbBuildUtils.sh) script is used to compute the build configuration depending on the workflow to define zAppBuild cli parameters. It leverages the [application baseline configuration](samples/baselineReference.config) file which is expected to be present in the application-conf directory in order to compute the baseline reference and its changes.

#### Git branches naming convention requirements

The build script follows the below conventions on branch names, that are outlined in the document `The Git-based workflow for Mainframe development`:

```properties
# main Build branch
main

# feature branches for contributing to the next planned release via main
feature/setmainbuildbranch

# release maintenance branches to fix a release that is been put to production
release/rel-1.0.0

# release maintenance feature branches
#  second segment is indicating the release
hotfix/rel-1.0.0/fixMortgageApplication

# project/initiative/epic branches
epic/epic1234
project/project1

# project/initiative/epic branches
#  second segment is indicating the project/initiative/epic
epic1234/myfirstcoolnewfeature
project1/myfirstcoolnewfeature
```

The [utilities](utilities/dbbBuildUtils.sh) script implements the rules outlined in IBMs documentation `The Git-based workflow for Mainframe development.` Read [dbbBuildUtils.sh](README.md#script-capabilities--dbbbuildutilssh) for details.

#### Invocation

The dbbBuild.sh script can be invoked as below:

```
dbbBuild.sh -w MortApp/main/build-1 -a MortgageApplication -b main -p build
```

Cli parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory** an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps.
-a `<application>` | **Application name** to be built, which is passed to zAppBuild as the `--application` parameter.
-b `<branch>` | **Git branch** that is built. Used to compute various build properties such as the `--hlq` and build type.
-p  `<build/release/preview>` | (*) **Pipeline Type** to indicate a build pipeline (`build` - only with test/debug options) or a `release` pipeline (build for performance optimized load modules), or if it runs in `preview` mode.
-v | (*) zAppBuild verbose tracing flag.
-t  `<buildTypeArgument>` | (*) **zAppBuild Build Type** to specify the build type, such as `--fullBuild`, or `--impactBuild`. Please provide arguments in quotes `-t '--fullBuild'` . Please note - providing this parameter overrides the computation of the build type in the backend scripts. Might be used to initialize the DBB Metadatastore. 
-q '<hlqPrefix>' |(*) **HLQ prefix**. Default is retrieved from the `pipelineBackend.config` file, which is set to the pipeline user executing the script.

(*) Optional

**Pipeline type**

The type of pipeline (`-p` option), is used to modify the operational behavior of the build framework on producing executables:
* `build` configures the build options for test/debug options. This is the **default**.
* `release` used to indicate to produce executables with the flag for performance optimized runtime modules. This is required for the release pipelines which include packaging of release candidates.
* `preview` configures to run the build process but will not produce any outputs. It is used to preview what the build will do. The pipeline should not have any subsequent actions.

#### Output

Expand the section below to view the output that is produced by the `dbbBuild.sh` script.

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
dbbBuild.sh: [INFO] **                HLQ: DBEHM.MORTGAGE.main.BLD
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
dbbBuild.sh: [INFO] /usr/lpp/dbb/v2r0/bin/groovyz  /var/dbb/dbb-zappbuild_300/build.groovy --workspace /var/dbb/pipelineBackend/workspace/MortApp/main/build-1 --application MortgageApplication --outDir /var/dbb/pipelineBackend/workspace/MortApp/main/build-1/logs --hlq DBEHM.MORTGAGE.main.BLD --id DBEHM --pwFile /var/dbb/config/db2-pwd-file.xml  --logEncoding UTF-8 --propFiles /var/dbb/dbb-zappbuild-config/build.properties,/var/dbb/dbb-zappbuild-config/datasets.properties --impactBuild --baselineRef refs/tags/rel-1.0.0 --debug

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

### 4.3 - Script capabilities / dbbBuildUtils.sh

The [dbbBuildUtils](utilities/dbbBuildUtils.sh) script is a utility script providing a method `computeBuildConfiguration()` to compute the zAppBuild options and parameters:

* `build type` such as `--impactBuild`
* the baseline reference `--baselineRef xxx`, where xxx is retrieved from the baselineReference.config file.
* flag to produce test modules (`--debug` in zAppBuild) or modules improved for performance (production runtime modules).
* the `mainBuildBranch` to configure feature branches to clone the correct dependency metadata collections and to identify the correct offset of changes.

#### Baseline references requirements

The IBM recommended workflow approach leverages Git tags to identify the offset for calculating the changed files for a given deliverable. In this version, this utility script is retrieving the information from the [baselineReference.config](samples/baselineReference.config) file, that has to be maintained by the application team within the `application-conf` directory. It is the application teams' responsibility to maintain these references. 

Note that the location of the baselineReferences.config file can be customized in the [pipelineBackend.config](pipelineBackend.config) file.

[MortgageApplication-baselineReference.config](MortgageApplication-baselineReference.config) is a sample, that indicates the baseline for the `main` and `release maintenance` branch.

For IBM internal purposes there is a sample git repository at [zDevOpsAcceleration/MortgageApplication](https://github.ibm.com/zDevOps-Acceleration/MortgageApplication/branches), that contains the branches and the following git history:

```log
* 597012c (origin/feature/setmainbuildbranch, feature/setmainbuildbranch) change main build branch
* 9f1ce97 (HEAD -> main, origin/main) change submodule
| * 5d2b737 (origin/release/rel-1.0.0, origin/hotfix/rel-1.0.0/myfix) Update epscsmrt.cbl
|/ 
*   0cc39e4 (tag: rel-1.0.0) Merge pull request #6 from zDevOps-Acceleration/Development 
```


### 4.4 - packageBuildOutputs.sh

This script is to execute the `PackageBuildOutputs.groovy` that packages up the build outputs and optionally uploads it to an artifact repository to publish the artifacts created by the DBB Build from a pipeline.

#### Invocation

The packageBuildOutputs.sh script can be invoked as below:

Package only

```
packageBuildOutputs.sh -w MortApp/main/build-1 -t rel-1.0.0.tar
```
Package and Upload
```
packageBuildOutputs.sh -w MortApp/main/build-1 -t rel-1.0.0.tar -a MortgageApplication -b main -u -p release -v rel-1.0.0.2023-09-22-08.55.20
```

Cli parameter | Description
---------- | ----------------------------------------------------------------------------------------
Packaging options | 
-w `<workspace>` | **Workspace directory** an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. The packageBuildOutputs.sh script is evaluating the logs directory.
-t `<tarFileName>` | Name of the **tar file** to create (Optional).
Artifact Upload options |
-u | Flag to enable upload of outputs to the configured artifact repository.
-b | Name of the **git branch** turning into an segment of the directory path in the artifact repo (Optional, but required when publishing). 
-p  `<build/release>` | **Pipeline type** to indicate a build pipeline (`build` - only with test/debug options) or a `release` pipeline (build for performance optimized load modules) to determine the directory in the artifact repository for development and pipeline builds.
-v `artifactVersion` | Label of the **version** in the artifact repository turning into a segment of the directory path in the artifact repo.

#### Script conventions

**Directory Path within the artifact repo**

The backend script computes for the upload to the artifact repository the directory path within repository:
* **Branch**/**artifactVersion**

If it is the `main` branch, the pipeline type (-p) is evaluated to 
* **Branch**/**pipelineType <build/release>**/**artifactVersion**
 
while **artifactVersion** is appended by the `PackageBuildOutputs.groovy` script.

#### Output 

Expand the section below to view the output that is produced by the `packageBuildOutputs.sh` script.

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

This script is to execute the dbb-ucd-packaging.groovy that invokes the UCD Buztool to publish the artifacts created by the DBB Build from a pipeline.

#### Invocation

The uscPackaging.sh script can be invoked as below:

```
ucdPacking.sh -v ucdVersion -c ucdComponentName -w workingDirectory -e externalRepositoryFile -f packagingPropertiesFile [-u pipelineURL] [-b gitBranchName] [-p gitPullRequestURL]
```

Cli parameter | Description
---------- | ----------------------------------------------------------------------------------------
-v `<ucdVersion>` | **Version** name to create.
-c `<ucdComponentName>` | **Component** name in UCD.
-w `<workspace>` | **Workspace directory** an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. The ucdPackaging.sh script is evaluating the logs directory.
-e `<externalRepositoryFile>` | **Path to external artifact repository file** for buzztool.sh.
-f `<packagingPropertiesFile>` |**Path to a properties file** for additional configuration of the dbb-ucd-packaging script.
-u `<pipelineUrl>`|(*) URL to the pipeline to establish link to pipeline build result.
-b |(*) Name of the **git branch**.
-p |(*) URL to the pull request.

(*) Optional

#### Output

Expand the section below to view the output that is produced by the `ucdPacking.sh` script.

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

This script is implementing the invocation of the ucd-deploy.groovy script to perform Urban Code Deploy deployments.


#### Invocation

The ucdDeploy.sh script can be invoked as follows:

```
ucdDeploy.sh -a ucdApplicationName -p ucdApplicationProcessName -e ucdEnvironmentName -d ucdComponentName:ucdDeployVersion
```

Cli parameter | Description
---------- | ----------------------------------------------------------------------------------------
-a `<ucdApplicationName>` | **Application** name defined in UCD containing the component version to be deployed.
-p `<ucdApplicationProcessName>` | **Process** name in UCD associated with the application being deployed.
-e `<ucdEnvironmmentName>` | **Environment** name in UCD that the component version will be deployed to.
-d `<ucdComponentName:ucdDeployVersion>` | **Component name and version** to be deployed to the UCD environment.
-t `<timeout>` |(*) **Deployment timeout** value in seconds.  Defaults to 300 seconds.
-s `<SSLProtocol>`|(*) **SSL protocol** to use.  Default is TLSv1.2.
-k |(*) Disable SSL verification flag.
-v |(*) Verbose tracing flag. Used to produce additional tracing in the groovy script.

(*) Optional

#### Output

Expand the section below to view the output that is produced by the `ucdDeploy.sh` script.

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

### 4.7 - prepareLogs.sh

Script to obtain the logs that were produced as part of the pipeline steps in the work/log directory. 

#### Invocation

The script can be invoked as below:

```
prepareLogs.sh -w MortApp/main/build-1
```

Cli parameter | Description
---------- | ----------------------------------------------------------------------------------------
-w `<workspace>` | **Workspace directory** an absolute or relative path that represents unique directory for this pipeline definition, that needs to be consistent through multiple steps. 

On successful completion, the script is writing a message to indicate the output directory with the log file name
```
Logs successfully stored at /var/dbb/pipelineBackend/workspace/MortApp/feature/setmainbuildbranch/build-1/logs.tar
```

 #### Script output

Expand the section below to view the output that is produced by the `prepareLogs.sh` script.

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

#### Download logs to distributed runner environment

While the script `prepareLogs.sh` only creates the tar file on the workspace directory, you want to download the created tar file to the distributed runner environment to attach it to the pipeline results.

The below sample illustrates the invocation of the prepareLogs script and inspect the console output for the message `Logs successfully stored at ` indicating that a tar file was successfully created. 
It _greps_ the information and invokes a download action.

```shell
    cmdStr="prepareLogs.sh -w MortApp/main/build-1"
    echo "$cmdStr"
    zowe zos-Unix System Services issue ssh "${cmdStr}" | tee prepareLogsOutput.txt

    # read logs 
    logsTar=$(cat prepareLogsOutput.txt | grep "Logs successfully stored at " | awk -F "stored at " ' { print $2 }')
    
    if [ ! -z "${logsTar}" ]; then
        # use zowe cli to download file to local environment
        # sftp / scp can be alternative download options.
        zowe zos-files download Unix System Services-file "$logsTar" -f ./logs/MortApp/main/build-1/logs.tar -b
    else 
        rc=4
        echo "[WARNING] Tar file containing the logs was not found. rc="$rc
    fi
```
## Disclaimer

THIS SAMPLE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

