# Pipeline Templates

This category provides templates to implement a CI/CD pipeline covering the entire development lifecycle. These assets are developed based on [The Git-based process you need for Mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev) and help you to quickly setup your pipeline environment. Please make yourself familiar with the outlined Git branching model.

## Table of Contents 
Asset | Description | Documentation Link
--- | --- | ---
Common-Backend-Scripts | Core asset to simplify defining the pipeline orchestration by providing central services for the various stages of the CI/CD pipeline. Especially useful for pipeline architectures which don't provide a native runner .  | [Common-Backend-Scripts/README.md](Common-Backend-Scripts/README.md)
Azure DevOps Pipeline Template | Template to setup a [AzureDevOps pipeline](https://learn.microsoft.com/en-us/azure/devops/pipelines/?view=azure-devops&viewFallbackFrom=azure-pipelines) to build, package and deploy Azure Repos Git. | [AzureDevOpsPipeline/README.md](AzureDevOpsPipeline/README.md)
Gitlab CI/CD Pipeline Template | .gitlab-ci.yml template to setup a [Gitlab CI/CD pipeline](https://docs.gitlab.com/ee/ci/pipelines/) to build, package and deploy Gitlab platform. | [GitlabCIPipeline/README.md](GitlabCIPipeline/README.md)
GitHub Actions Pipeline Template | Template to setup a [GitHub Actions Pipeline](https://docs.github.com/en/actions) to build, package, and deploy a GitHub repository. | [GitHubActionsPipeline/README.md](GitHubActionsPipeline/README.md)
Jenkins Multibranch Pipeline Template | Multibranch pipeline template to setup a [Jenkins CI/CD pipeline](https://www.jenkins.io/doc/book/pipeline/multibranch/) to build, package and deploy with any Git provider. | [JenkinsPipeline/README.md](JenkinsPipeline/README.md)

Please use the [Github discussion](https://github.com/IBM/dbb/discussions) board for feedback and comments on these templates.


## Capabilities of Pipeline Templates 

The below tables provide an overview of the implemented capabilities of the pipeline templates. The information can be used to lookup and understand the variations in the implementations. 

### Feature Branch pipeline implementations

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
Git Clone | (CBS) gitClone.sh | (CBS)  gitClone.sh | (CBS)  gitClone.sh | Jenkins Git Plugin
Build  | (CBS)  dbbBuild.sh | (CBS)  dbbBuild.sh | (CBS)  dbbBuild.sh | (CBS)  dbbBuild.sh
Publish Build Logs  | (CBS)  prepareLogs.sh and sftp to load and attach logs | (CBS)  prepareLogs.sh and sftp to load and attach logs | (CBS)  prepareLogs.sh and zowe cli rse to load and attach logs | Jenkins artifactPublisher Plugin
Packaging  | (CBS) packageBuildOutputs.sh | (CBS) packageBuildOutputs.sh | (CBS) packageBuildOutputs.sh | (CBS) ucdPackaging.sh to create UCD component version
Package Upload | Upload to Azure Artifacts | - | - | UCD buztool configuration
Workspace Cleanup | (CBS) deleteWorkspace.sh | (CBS) deleteWorkspace.sh | (CBS) deleteWorkspace.sh | Jenkins Plugin

### Integration branch pipeline implementations

These steps are implemented when merging changes to the main, release maintenance or an epic branch.

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
Git Clone | (CBS) gitClone.sh | (CBS)  gitClone.sh | (CBS)  gitClone.sh | Jenkins Git Plugin
Build  | (CBS)  dbbBuild.sh | (CBS)  dbbBuild.sh | (CBS)  dbbBuild.sh | (CBS)  dbbBuild.sh
Publish Build Logs  | (CBS)  prepareLogs.sh and sftp to load and attach logs | (CBS)  prepareLogs.sh and sftp to load and attach logs | (CBS)  prepareLogs.sh and zowe cli rse to load and attach logs | Jenkins artifactPublisher Plugin
Packaging  | (CBS) packageBuildOutputs.sh | (CBS) packageBuildOutputs.sh | (CBS) packageBuildOutputs.sh | (CBS) ucdPackaging.sh to create UCD component version <br> Create Link to UCD in Pipeline run
Package Upload | Upload to Azure Artifacts | - | - | UCD buztool configuration
*Deployment to Integration Test environment*  |   |   |   |  
Deployment Integration Test Environment | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh  | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh <br> | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh <br> | (CBS) ucdDeploy.sh
Publish deployment logs | (CBS) prepareLogs.sh <br> and sftp upload | (CBS) prepareLogs.sh <br> and sftp upload  | (CBS) prepareLogs.sh <br> and sftp upload  |  Create UCD Deployment Link in Pipeline run
Workspace Cleanup | (CBS) deleteWorkspace.sh | (CBS) deleteWorkspace.sh | (CBS) deleteWorkspace.sh | Jenkins Plugin

### Release pipeline implementations

These steps are implemented when requesting a release pipeline.

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
Git Clone | (CBS) gitClone.sh | (CBS)  gitClone.sh | (CBS)  gitClone.sh | Jenkins Git Plugin
Build  | (CBS)  dbbBuild.sh | (CBS)  dbbBuild.sh | (CBS)  dbbBuild.sh | (CBS)  dbbBuild.sh
Publish Build Logs  | (CBS)  prepareLogs.sh and sftp to load and attach logs | (CBS)  prepareLogs.sh and sftp to load and attach logs | (CBS)  prepareLogs.sh and zowe cli rse to load and attach logs | Jenkins artifactPublisher Plugin
Creation of the release candidate tag | Computation of the release candidate and planned release name <br> Creation of the release candidate Tag in ADO via the ADO CLI | Computation of the release candidate and planned release name <br> Creation of the release candidate Git tag in Gitlab via REST | Computation of the release candidate and planned release name <br> Creation of a pre-release via GH Cli for the release candidate | Computation of the UCD package name. No tagging in Git (independent of the Git provider).
Packaging  | (CBS) packageBuildOutputs.sh | (CBS) packageBuildOutputs.sh | (CBS) packageBuildOutputs.sh | (CBS) ucdPackaging.sh to create UCD component version <br> Create Link to UCD to Pipeline run
Package Upload (Note - enable upload to enterprise artifact repository) | Upload to Azure Artifacts | - | - | UCD buztool configuration
*Deployment to Integration Test environment*  |   |   |   |  
Deployment Integration Test Environment | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh  | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh <br> | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh <br> | (CBS) ucdDeploy.sh. Create Link to UCD Deployment request to Pipeline run
Publish deployment logs | (CBS) prepareLogs.sh <br> and sftp upload | (CBS) prepareLogs.sh <br> and sftp upload  | (CBS) prepareLogs.sh <br> and sftp upload  |  Create UCD Deployment Link in Pipeline run
*Deployment to Acceptance Test environment*  |   |   |   |  
Retrieve Deployment Package | Download from ADO package registry | The package from pipeline working directory is used | The package from pipeline working directory is used | Not part of the pipeline template
Deployment Acceptance Test Environment | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh  | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh <br> | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh <br> | Not part of the pipeline template
Publish deployment logs | (CBS) prepareLogs.sh <br> and sftp upload | (CBS) prepareLogs.sh <br> and sftp upload  | (CBS) prepareLogs.sh <br> and sftp upload  |  Not part of the pipeline template
*Deployment to Production Test environment*  |   |   |   |  
Retrieve Deployment Package | Download from ADO package registry | The package from pipeline working directory is used | The package from pipeline working directory is used | Not part of the pipeline template
Deployment Production Test Environment | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh  | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh <br> | (CBS) wazideploy-generate.sh <br> (CBS) wazideploy-deploy.sh <br> (CBS) wazideploy-evidence.sh <br> | (Not part of the pipeline template
Publish deployment logs | (CBS) prepareLogs.sh <br> and sftp upload | (CBS) prepareLogs.sh <br> and sftp upload  | (CBS) prepareLogs.sh <br> and sftp upload  |  Not part of the pipeline template
Release Finalisation | Creation of the release candidate Tag in ADO via the ADO CLI | Creation of a Release and Release-tag via GH Cli for the release | Computation of the release candidate and planned release name <br> Creation of the release Git tag in Gitlab via REST <br> Automated update of the `baselineReference.conf` file | No automated tagging
Workspace Cleanup | (CBS) deleteWorkspace.sh | (CBS) deleteWorkspace.sh | (CBS) deleteWorkspace.sh | Jenkins Plugin
