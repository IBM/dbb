# GitHub Actions pipeline template 

This template provides multiple .yml files to setup a GitHub Actions pipeline for applications managed in a GitHub repository. 

## Overview & capabilities 

This pipeline template is implementing the [Git branching model for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/) within a GitHub Actions context.

It leverages the [Common Backend scripts](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md) to implement the Setup, Build, Packaging, and Deployment stages.

The pipeline implements the following stages:

 - `Setup` stage: 
   - [Clone](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#41---gitclonesh) the Git repository to a workspace directory on z/OS Unix System Services.
 - `Build` stage:
   - Invoke the zAppBuild [build](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#42---dbbbuildsh) framework.
   - [Prepare](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#410---preparelogssh) the log files and publish them as GitHub Artifacts.
   - In the case of a release pipeline, to create the release candidate tag.
 - `Packaging` stage:
   - Create a .TAR file package based on the [PackageBuildOutput](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#44---packagebuildoutputssh) script.
 - `Deploy Integration` stage to deploy to the development / integration test environment:
   - Generate the deployment command with the Wazi Deploy [generate](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#47---wazideploy-generatesh) command.
   - Deploy the package with the Wazi Deploy [deploy](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#48---wazideploy-deploysh) command.
   - Generate the deployment report and update evidence with the Wazi Deploy [evidence](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#49---wazideploy-evidencesh) command.
   - Publish deployment logs as a GitHub Artifact.
 - `Deploy Acceptance` and `Deploy Production` stages: 
   - Deploy to controlled test environments via the [release pipeline](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline/#the-release-pipeline-with-build-packaging-and-deploy-stages).
   - Generate the deployment command with the Wazi Deploy [generate](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#47---wazideploy-generatesh) command.
   - Deploy the package with the Wazi Deploy [deploy](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#48---wazideploy-deploysh) command.
   - Generate the deployment report and update evidence with the Wazi Deploy [evidence](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#49---wazideploy-evidencesh) command.
   - Retrieve Deployment Reports (`Deploy Production` only).
   - Publish deployment logs as a GitHub Artifact (`Deploy Production` only).
   - Create the Wazi Deploy index using the [createwazideployindex](./actions/createwazideployindex/action.yml) composite action. 
   - Create the production release tag using the [tagging_createproductionreleasetag](./actions/tagging_createproductionreleasetag/action.yml) composite action. (`Deploy Production` only).
 - `Cleanup` Stage:
   - [Delete the build workspace](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md#411---deleteworkspacesh) on z/OS Unix System Services.


Depending on your selected deployment technology, review the definitions and (de-)/activate the appropriate steps.

This pipeline uses the GitHub Actions concepts: `Stage`, `Jobs`, [Composite Actions](https://docs.github.com/en/actions/creating-actions/creating-a-composite-action), and the `GitHub CLI`.


## Prerequsities 
To leverage this template, access to a GitHub Actions environment is required, and a GitHub Actions runner must be configured to connect to your mainframe environment. 
This GitHub Actions runner will also require the [GitHub CLI](https://cli.github.com/), which is automatically included on GitHub hosted runners. 

Additionally, your GitHub repo will need to have the [Common Backend Scripts](https://github.com/IBM/dbb/tree/main/Templates/Common-Backend-Scripts) configured.

## Installation & setup 

**Note: Please work with your pipeline specialist to review this section.**

The `.github` folder, containing the `actions` and `workflows` subfolders can be dropped into the root folder of your GitHub repository, and will automatically provide pipelines for the specified triggers.

The following requirements need to be met:
 - An SSH connection to the z/OS machine. 

### Variable configuration 

The following variables will need to be defined and configured in the GitHub Repo.

Variable | Description
--- | ---
Personal Access Token Secret | A secret containing a personal access token

The following variables will need to be defined and configured in the pipeline definition files

Variable | Description | File(s)
--- | --- | ---
zosHostname | zOS - Host name / IP address for SFTP connection | Feature.yml, Build.yml, Release.yml, Preview.yml, deployToEnv.yml 
zosSFTPUser | zOS - Host user for SFPT connection | Feature.yml, Build.yml, Release.yml, Preview.yml, deployToEnv.yml 
githubAccessToken | A GitHub Personal Access Token used for authentication, matching the Personal Access Token Secret. In the template, this is secrets.SAMPLE_PAT | pipelineController.yml 

## Pipeline usage 

This pipeline implements the common build, package, and deploy steps to process various configurations according to defined conventions.
It is a combination of GitHub Actions pipeline definitions supporting various workflows. 
When used as intented, the combination of pipeline definitions supports:
 - Automated [build pipelines for feature branches](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline/#pipeline-build-of-feature-branches) with a clone and build stage.
 - A [basic pipeline](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline/#the-basic-build-pipeline-for-main-epic-and-release-branches) when changes are merged into the main branch.
 - A [release pipeline](https://ibm.github.io/z-devops-acceleration-program/docs/branching-model-supporting-pipeline/#the-release-pipeline-with-build-packaging-and-deploy-stages) to build and package the release candidate, including installation to predefined environments such as the production environment. 

Unlike other pipeline orchestrators, GitHub Actions encourages the use of multiple workflow files that will interact with each other. This template makes use of the following workflow files:

 - [pipelineController.yml](./workflows/pipelineController.yml)
    - This pipeline is used to trigger other pipelines, as it is the only pipeline with a `workflow_dispatch` trigger. 
 - [Feature.yml](./workflows/Feature.yml)
    - Pipeline for feature branch.
 - [Build.yml](./workflows/Build.yml)
    - The build pipeline.
 - [Release.yml](./workflows/Release.yml)
    - Pipeline for release branch.
 - [Preview.yml](./workflows/Preview.yml)
    - The preview pipeline.
 - [deployToEnv.yml](./workflows/deployToEnv.yml)
    - Reusable pipeline for deploying to a given environment.
 - [actions/createwazideployindex/action.yml](./actions/createwazideployindex/action.yml)
    - A composite action, used to copy the evidence file into the evidence inventory.
 - [actions/tagging_createreleasecandidate/action.yml](./actions/tagging_createreleasecandidate/action.yml)
    - A composite action, used to create the release candidate, and calculate the release version.
 - [actions/tagging_createproductionreleasetag/action.yml](./actions/tagging_createproductionreleasetag/action.yml)
    - A composite action, used to create the production release. 

![pipelineDiagram](pipelineDiagram.png)


Please review the pipeline definitions to understand the various triggers for which these pipelines may be executed, and also the conditions when stages, jobs, or steps are executed. 

Please make yourself familiar with the [Git branching for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#characteristics-of-mainline-based-development-with-feature-branches) documentation. 

### Pipeline variables 

In a default setup, the mainline pipeline is triggered for each new commit. 

When manually requesting a pipeline through the pipeline controller, the following variables can be overwritten:
This is especially useful when the application team want to create a release candidate package for higher test environments and production.

Parameter | Description
--- | ---
pipelineType | Which type of pipeline is being run, either preview, build, or release. Defaults to testing. 
releaseType | Used only for the release pipeline, which type of release is being released, either patch, minor, or major. Defaults to n/a.
verbose (wip) | Boolean flag to control logging of build framework. Defaults to false.

### Feature branch pipeline

The feature branch pipeline peforms the following: 
 - Clone
 - Build 
 - Package & publish package 

This pipeline runs automatically when pushing to a feature branch.

#### Overview of the pipeline:
![Feature Pipeline Diagram](feature-pipeline.png)

### Basic build pipeline for Integration branches 

The basic build pipeline for integration branches performs the following:
 - Clone
 - Build
 - Package & publish package 
 - Deploy to the integration test environment
 - Cleanup

This is the default pipeline, and runs automatically whenever there is a new commit to the main branch. 
Additionally, this pipleine can be run by manually running the pipelineController, with *pipelineType* as `build`.

#### Overview of the pipeline:
![Build Pipeline Diagram](build-pipeline.png)

### Release pipeline

When the development team agrees to build a release candidate, the release pipeline is triggered manually. 

The release pipeline performs the following:
 - Clone
 - Build 
 - Tag the release candidate 
 - Package & publish package 
 - Deploy to the integration test environment 
 - Deploy to the acceptance test environment
   - This needs to be triggered manually 
 - Deploy to the production environment and tag the production release 
   - This needs to be triggered manually 
 - Cleanup
   - This needs to be triggered manually 

This pipeline can be run by manually running the pipelineController, with *pipelineType* as `release`.
Additionally, this pipeline will automatically calculate the release tag based on the information provided in the Baseline Reference file, store the calculated tag in an artifactVersion artifact, tag a release candidate, and the final release deployed to production. 

#### Overview of the pipeline:
![Release Pipeline Diagram](release-pipeline.png)

Note: both the basic build pipeline for Integration branches, and the release pipeline deploy to the integration test environment, but go about it in different ways. 
The release pipeline does this via invoking the deployToEnv.yml workflow, while the build pipeline does it in a single workflow file. 
This is done to highlight how the same task can be completed in 2 different ways. 


## Known issues / WIP / Missing features

This is a non-comprehensive list of items that are either a known issue, a work in progress, or something that still needs to be implemented. 

### High Importance 
- In the release pipeline, deploy-acceptance, deploy-production, and cleanup need manual triggers
  - This will require configuration specific to your environment. You can find the relevant documentation [here.](https://docs.github.com/en/actions/managing-workflow-runs/reviewing-deployments)

### Medium Importance 
- the baseline reference file is not automatically updated when a new version is released
- the `verbose` flag is not implemented on the pipeline controller 

### Low Importance 
- N/A

## Reference & additional resources 

This was made by using the [Azure DevOps template](https://github.com/IBM/dbb/blob/main/Templates/AzureDevOpsPipeline/README.md) alongside the GitHub [Actions importer](https://docs.github.com/en/actions/migrating-to-github-actions/automated-migrations/automating-migration-with-github-actions-importer) and the [GitLab DevOps template](https://github.com/IBM/dbb/blob/main/Templates/GitlabCIPipeline/README.md) as reference.

Additionally, this video on [Creating a DevOps Pipeline with GitHub Actions](https://mediacenter.ibm.com/media/CICD+-+Creating+a+DevOps+Pipeline+with+Github+Actions/1_1qhqzg3l) provides an introduction to GitHub Actions. 