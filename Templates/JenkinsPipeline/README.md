# Jenkins Multibranch pipeline template

This template provides an Jenkinsfile [MultibranchPipeline](MultibranchPipeline) definition as MultiBranch to setup a Jenkins Multibranch pipeline for applications managed in any Git provider.

## Overview and capabilities

This pipeline template is implementing the [Git-based process and branching model for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev) with Jenkins as a the Pipeline Orchestrator.

It leverages the [Common Backend scripts](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md) to implement the Build, Packaging and Deployment stages.

The pipeline implements the following stages
* `Checkout` stage to the Git repository to a workspace directory on z/OS Unix System Services using the integrated Git Plugin of Jenkins.
* `Pipeline Setup` computing the settings for subsequent stages, that are displayed in the `Parameters and Values` stage.
* `Build` stage 
  * to invoke zAppBuild via the [dbbBuild.sh](../Common-Backend-Scripts/README.md#42---dbbbuildsh) Common Backend script,
  * to publish log files to the Jenkins build result.
* `SonarQube Analysis` stage to request a SonarQube scan of the application repository.
* `Packaging` stage
  * to create a UrbanCode Component version via the Common Backend script [ucdPackaging.sh](../Common-Backend-Scripts/README.md#45---ucdpackagingsh),
  * to publish packaging log files to the Jenkins build result,
  * to add links to the UCD Component version.
* `Deploy to INT` stage to deploy to the development / integration test environment that includes:
  * to request the Deployment of the UrbanCode Component version via the Common Backend script [ucdDeploy.sh](../Common-Backend-Scripts/README.md#46---ucddeploysh) to the shared Development/Integration Test environment,
  * to publish deployment log files to the Jenkins build result,
  * to add links to the UCD Deployment request.
 * `Workspace Cleanup` stage to clean up the workspace

Depending on your selected and software analysis and deployment technology, review the definitions and (de-)/activate the appropriate steps.

The pipeline is implemented as a declarative pipeline.

## Prerequisites

The [Common Backend scripts](../Common-Backend-Scripts/) need to be configured for the selected technologies to operate correctly.

## Installation and setup of template

**Note: Please work with your pipeline specialist to review the below section.**

The `MultibranchPipeline` can be dropped into the root folder of your Git repository and will automatically provide pipelines for the specified triggers as soon as the Multibranch pipeline is configured in Jenkins.

Please review the definitions thoroughly with your Jenkins administrator. Ideally, the MultibranchPipeline is converted into a [Jenkins Shared Library](https://www.jenkins.io/doc/book/pipeline/shared-libraries/) to provide stronger central controls.

Step-by-step instructions to configure a Multibranch pipeline is documented the Jenkins books under: https://www.jenkins.io/doc/book/pipeline/multibranch/


## Pipeline usage

The pipeline implements the common build, package and deploy steps to process various configurations according to the defined conventions.
It is a single Jenkins Multibranch pipeline definition supporting various workflows. It supports: 

* automated [build pipelines for feature branches](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline#pipeline-build-of-feature-branches) with a clone and build stage,
* the [basic pipeline](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline#the-basic-build-pipeline-for-main-epic-and-release-branches) when changes are merged into the branch `main` and
* a [release pipeline](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline#the-release-pipeline-with-build-packaging-and-deploy-stages) to build and package the release candidate, that then can be deployed via UCD.

Please check the pipeline definition to understand the various triggers for which this pipeline is executed and also the conditions when stages and jobs are executed.

Please make yourself familiar with the [Git branching for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#characteristics-of-mainline-based-development-with-feature-branches) documentation.

### Pipeline variables

In a default setup, the feature and basic pipelines are triggered for each new commit as soon as the Multibranch integration detects the changes.

The pipeline allows to override the values of the below variables when manually requesting the pipeline. This is especially useful when the application team want to create a release candidate package for higher test environments and production.

Parameter | Description
--- | ---
pipelineType     | Pipeline type - either build, release or preview. (Default: build)
verbose          | Boolean flag to control logging of build framework. (Default: false)
<!-- releaseType      | Release type - major, minor, patch as input to compute the release version and to set the release candidate and release git tags. (Default: patch) //-->

### Feature Branch pipeline

The pipeline for feature branches executes the following steps:

* Setup
* Clone
* Build
* SonarQube Analysis
* Create UCD Component version

This pipeline can be manually requested to set the *pipelineType* variable `preview` to run zAppBuild in preview mode.

Overview of the pipeline:  

![Jenkins Feature Branch Pipeline](images/jenkins-pipeline-feature-branch.png)

Pipelines for Feature, Epic and Release Maintenance branches perform the same steps like a feature branch pipeline. 

### Basic build pipeline when merging into Main

The basic build pipeline for the main branch contains the following stages:

* Setup
* Clone
* Build (with TEST options)
* SonarQube Analysis
* Create UCD Component version
* Request UCD Deployment to the integration test environment

It run automatically when there is a new commit to a repository. The common backend script `dbbBuild.sh` will automatically extract the baseline git tag based on the information in the [baselineReference.config](../Common-Backend-Scripts/samples/baselineReference.config) file that is expected to be maintained in the application's git repository.

You can also run this pipeline manually to override [the pipeline parameters](#pipeline-variables), for instance to set the *pipelineType* variable to `preview` to run zAppBuild in preview mode and skip the packaging steps.

Overview of the pipeline:

![Jenkins Build Pipeline](images/jenkins-pipeline-basicBuild.png)

Please note the links that are created to the Jenkins build result that take the user to the package and the deployment request within UCD:

![Jenkins Build Result](images/jenkins-pipeline-basicBuild-result.png)

### Release pipeline

When the development team agrees to build a release candidate, the release pipeline type is triggered manually for the `main` branch. The development team manually requests the pipeline and specifies the *pipelineType* variable as `release`. Per the recommended branching model, release packages are only created from the `main` branch.

It covers the similar steps steps like before:

* Setup
* Clone
* Build (now with the optimize compile options)
* SonarQube Analysis
* Create UCD Component version
* Request UCD Deployment to the integration test environment

The user can then use UCD to deploy the release candidate package to the higher test environments. At the time of the production deployment, a release tag and a release can be created in the Git repository of choice. This can be automated as part of the deployment process to create a git tag for the commit of the release pipeline build. 

Overview of the release pipeline:

![Jenkins Release Pipeline](images/jenkins-pipeline-basicBuild.png)