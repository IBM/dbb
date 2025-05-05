# Gitlab DevOps pipeline template
This template provides an [.gitlab-ci.yml](.gitlab-ci.yml) definition file to setup an Gitlab CI/CD pipeline for applications managed in an Gitlab Git repository.
## Overview and capabilities
This pipeline template is implementing the [Git-based process and branching model for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev) within an Gitlab CI/CD context.

It leverages the [Common Backend scripts](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md) to implement the Setup, Build, Packaging and Deployment stages.

The pipeline implements the following stages
* `Setup` stage to [clone](../Common-Backend-Scripts/README.md#41---gitclonesh) the Git repository to a workspace directory on z/OS Unix System Services. 
* `Build` stage 
  * to invoke the zAppBuild [build](../Common-Backend-Scripts/README.md#42---dbbbuildsh) framework,
  * to [prepare](../Common-Backend-Scripts/README.md#49---preparelogssh) the log files and publish them as Gitlab Artifacts.
  * in case of a release pipeline, to create the release candidate tag.
* `Packaging` stage
  * to create a package (TAR file) based on the [PackageBuildOutputs script](../Common-Backend-Scripts/README.md#44---packagebuildoutputssh)
* `Deploy Integration` stage to deploy to the development / integration test environment that includes:
  * to run the Wazi Deploy and generate deployment plan [generate command](../Common-Backend-Scripts/README.md#47---wazideploy-generatesh)
  * to deploy the package with the Wazi Deploy [deploy command](../Common-Backend-Scripts/README.md#48---wazideploy-deploysh) (Python-based)
  * to run the Wazi Deploy [evidence command](../Common-Backend-Scripts/README.md#49---wazideploy-evidencesh) to generate deployment report and updating the evidence.
  * to publish deployment log files to the Gitlab Artifacts.
  * to store the Wazi Deploy evidence files at a shared location to support later reporting scenarios.
* `Deploy Acceptance` and `Deploy Production` stages to deploy to controlled test environments via the [release pipeline](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline#the-release-pipeline-with-build-packaging-and-deploy-stages) that includes:
  * to deploy the package with the Wazi Deploy to targeted environment [deploy command](../Common-Backend-Scripts/README.md#48---wazideploy-deploysh) (Python-based)
  * to run the Wazi Deploy [evidence command](../Common-Backend-Scripts/README.md#49---wazideploy-evidencesh) to generate deployment report and updating the evidence.
  * to publish deployment log files to the Gitlab Artifacts.
  * to store the Wazi Deploy evidence files at a shared location to support later reporting scenarios.
* `Finalize` stage to create a release tag from [baseline reference file](../Common-Backend-Scripts/samples/baselineReference.config) and create a release maintenance branch as described in the [scaling up gideline](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#scaling-up).
* `Cleanup` stage: 
  * to clean up project directory on Gitlab server, and
  * to [delete the build workspace](../Common-Backend-Scripts/README.md#411---deleteworkspacesh) on z/OS Unix System Services.

Depending on your selected deployment technology, review the definitions and (de-)/activate the appropriate steps.

The pipeline uses the Gitlab concepts: `Stage`and `Jobs`.

![Gitlab Release Pipeline](images/gitlab-pipeline-release.png)

## Prerequisites

To leverage this template, access to a Gitlab CI/CD environment is required, and an Gitlab runner must be configured to connect to your mainframe environment. Please review the setup instructions of this [document](https://www.ibm.com/support/pages/system/files/inline-files/Integrating%20IBM%20zOS%20platform%20in%20CICD%20pipelines%20with%20GitLab%20-%20v1.7_1.pdf).

The template leverages [Zowe CLI](https://docs.zowe.org/stable/user-guide/cli-installcli/) to issue command invoking the [Common Backend scripts](../Common-Backend-Scripts/) on USS. It specifically uses the `issue unix-shell` command which allows streaming of the console outputs back to the Zowe Command-Line Interface, that was introduced in IBM RSE API Plug-in for Zowe CLI version 4.0.0 (see [Changelog](https://marketplace.visualstudio.com/items/IBM.zopeneditor/changelog))

* Zowe `base` and `rse` profiles needs to be configured in the `zowe.config.json` file under `.zowe` directory

    Example of Zowe configuration file:
    ```
    "$schema": "./zowe.schema.json",
    "profiles": {
        "rse": {
            "type": "rse",
            "properties": {
                "port": 6800,
                "basePath": "rseapi",
                "protocol": "https"
            },
            "secure": []
        },
        "base": {
            "type": "base",
            "properties": {
                "host": "mainframe.hostname",
                "rejectUnauthorized": false
            },
            "secure": []
        }
    },
    "defaults": {
        "rse": "rse",
        "base": "base"
    },
    "autoStore": false
    ```

The [Common Backend scripts](../Common-Backend-Scripts/) need to be configured for the selected deployment technologies to operate correctly.

## Installation and setup of template

**Note: Please work with your pipeline specialist to review the below section.**

The `.gitlab-ci.yaml` can be dropped into the root folder of your Gitlab Git repository and will automatically provide pipelines for the specified triggers. Please review the definitions thoroughly with your Gitlab administrator.

Following requirements need to be met:
* Zowe rse profile is configured to connect to z/OS machine as described above.

### Variables configuration
The following variables need to be defined and configured as the environment variables in the GitLab group or project setting:

Variable | Description
--- | ---
AutomationToken | [Group access token](https://docs.gitlab.com/ee/api/rest/#personalprojectgroup-access-tokens) to be used for authentication when invoke Gitlab CLI REST interfaces.
RSEAPI_USER | Username for Zowe RSEAPI server authentication. This username is used when issue shell script command through Zowe.
RSEAPI_PASSWORD | Password for Zowe RSEAPI server authentication. This password is used when issue shell script command through Zowe.
PIPELINE_WORKSPACE | Root directory on z/OS Unix System services to perform builds. E.g. /u/gitlab/workspace
WAZI_DEPLOY_CONFIGDIR | Path to a directory on USS containing Wazi Deploy configuration files. The configuration files can be populated with the [Wazi Deploy samples](https://github.com/jbyibm/cics-genapp/tree/main/wazideploy-samples).

The following variables need to be updated within the pipeline definition file: `.gitlab-ci.yaml`.

Variable | Description
--- | ---
application | Specify the name of your application which will be used to invoke the [Common Backend scripts](../Common-Backend-Scripts/).
wdEnvironmentFileIntegration | Path to a Wazi Deploy configuration file for integration environment.
wdEnvironmentFileAcceptance | Path to a Wazi Deploy configuration file for acceptance environment.
wdEnvironmentFileProduction | Path to a Wazi Deploy configuration file for production environment.
baselineReferenceFile | Path to baselineReference.config file of your application.

## Pipeline usage

The pipeline implements the common build, package, and deploy steps to process various configurations according to the defined conventions.
It is a single Gitlab CI/CD pipeline definition supporting various workflows. The [.gitlab-ci.yml](.gitlab-ci.yml) supports: 

* automated [build pipelines for feature branches](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline#pipeline-build-of-feature-branches) with a clone and build stage,
* the [basic pipeline](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline#the-basic-build-pipeline-for-main-epic-and-release-branches) when changes are merged into the branch `main` and
* a [release pipeline](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline#the-release-pipeline-with-build-packaging-and-deploy-stages) to build and package the release candidate, installation to predefined environments including the production environment.

Please check the pipeline definition to understand the various triggers for which this pipeline is executed and also the conditions when stages and jobs are executed.

Please make yourself familiar with the [Git branching for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#characteristics-of-mainline-based-development-with-feature-branches) documentation.

### Pipeline variables

In a default setup, the basic pipeline is triggered for each new commit.

It allows overriding value of the below variables when manually requesting the pipeline. This is especially useful when the application team want to create a release candidate package for higher test environments and production.

Parameter | Description
--- | ---
pipelineType     | Pipeline type - either build, release or preview. (Default: build)
releaseType      | Release type - major, minor, patch as input to compute the release version and to set the release candidate and release git tags. (Default: patch)
verbose          | Boolean flag to control logging of build framework. (Default: false)

### Feature Branch pipeline

The pipeline for feature branches executes the following steps:

* Clone
* Build
* Package & publish package

This pipeline needs to be run manually with the *pipelineType* variable as `preview`.

Overview of the pipeline:  

![Gitlab Preview Pipeline](images/gitlab-pipeline-preview.png)

### Basic build pipeline for Integration branches

The basic build pipeline for integration branches contains the following stages:
* Clone
* Build
* Package & publish package
* Deployment to the integration test environment with cleanup job (required to be triggered manually)

This is a default pipeline. It runs automatically when there is a new commit to a repository. You can also run this pipeline manually by setting the *pipelineType* variable as `build`.

Overview of the pipeline:

![Gitlab Build Pipeline](images/gitlab-pipeline-basicBuild.png)

### Release pipeline

When the development team agrees to build a release candidate, the release pipeline type is triggered manually.

It covers the followings steps:
* Clone
* Build, tagging of the release candidate
* Package & publish package
* Deployment to the integration test environment
* Deployment to the acceptance test environment (required to be triggered manually)
* Deployment to the production environment, including tagging the production release state (required to be triggered manually)
* Cleanup (required to be triggered manually)

The development team manually requests the pipeline and specifies the *pipelineType* variable as `release`. Along with the *release type*, the pipeline will automatically calculate the release tag based on the information in the [baselineReference.config](../Common-Backend-Scripts/samples/baselineReference.config) file, tag a release candidate and also the final release that is deployed to production.

Overview of the release pipeline:

![Gitlab Release Pipeline](images/gitlab-pipeline-release.png)
