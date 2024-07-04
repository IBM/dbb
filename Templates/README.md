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

### Communication and Tool selection

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
Communication | ssh | zowe cli rse plugin | ssh | zos-agent
Git Provider | Azure DevOps | GitLab | GitHub | Any Git provider
Deployment technology | Wazi Deploy | Wazi Deploy | Wazi Deploy | IBM DevOps Deploy (a.k.a. UCD)
Additional integration technologies | Azure DevOps CLI | GitLab REST interface | GitHub CLI | -

### Feature Branch pipeline implementations

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
Git Clone | :small_blue_diamond: gitClone.sh | :small_blue_diamond:  gitClone.sh | :small_blue_diamond:  gitClone.sh | Jenkins Git Plugin
Build  | :small_blue_diamond:  dbbBuild.sh | :small_blue_diamond:  dbbBuild.sh | :small_blue_diamond:  dbbBuild.sh | :small_blue_diamond:  dbbBuild.sh
Publish Build Logs  | :small_blue_diamond:  prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond:  prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond:  prepareLogs.sh and zowe cli rse to load and attach logs | Jenkins artifactPublisher Plugin
Packaging  | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: ucdPackaging.sh to create UCD component version
Package Upload | Upload to Azure Artifacts | - | - | UCD buztool configuration
Workspace Cleanup | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | Jenkins Plugin

### Integration branch pipeline implementations

These steps are implemented when merging changes to the `main`, `release` maintenance or an `epic` branch.

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
Git Clone | :small_blue_diamond: gitClone.sh | :small_blue_diamond:  gitClone.sh | :small_blue_diamond:  gitClone.sh | Jenkins Git Plugin
Build  | :small_blue_diamond:  dbbBuild.sh | :small_blue_diamond:  dbbBuild.sh | :small_blue_diamond:  dbbBuild.sh | :small_blue_diamond:  dbbBuild.sh
Publish Build Logs  | :small_blue_diamond:  prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond:  prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond:  prepareLogs.sh and zowe cli rse to load and attach logs | Jenkins artifactPublisher Plugin
Packaging  | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: ucdPackaging.sh to create UCD component version <br> Create Link to UCD in Pipeline run
Package Upload | Upload to Azure Artifacts | - | - | UCD buztool configuration
*Deployment to Integration Test environment*  |   |   |   |  
Deployment Integration Test Environment | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh  | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: ucdDeploy.sh
Publish deployment logs | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload  | :small_blue_diamond: prepareLogs.sh <br> and sftp upload  |  Create UCD Deployment Link in Pipeline run
Workspace Cleanup | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | Jenkins Plugin

### Release pipeline implementations

These steps are implemented when requesting a release pipeline.

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
Git Clone | :small_blue_diamond: gitClone.sh | :small_blue_diamond:  gitClone.sh | :small_blue_diamond:  gitClone.sh | Jenkins Git Plugin
Build  | :small_blue_diamond:  dbbBuild.sh | :small_blue_diamond:  dbbBuild.sh | :small_blue_diamond:  dbbBuild.sh | :small_blue_diamond:  dbbBuild.sh
Publish Build Logs  | :small_blue_diamond:  prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond:  prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond:  prepareLogs.sh and zowe cli rse to load and attach logs | Jenkins artifactPublisher Plugin
Creation of the release candidate tag | Computation of the release candidate and planned release name <br> Creation of the release candidate Tag in ADO via the ADO CLI | Computation of the release candidate and planned release name <br> Creation of the release candidate Git tag in Gitlab via REST | Computation of the release candidate and planned release name <br> Creation of a pre-release via GH Cli for the release candidate | Computation of the UCD package name. No tagging in Git (independent of the Git provider).
Packaging  | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: ucdPackaging.sh to create UCD component version <br> Create Link to UCD to Pipeline run
Package Upload (Note - enable upload to enterprise artifact repository) | Upload to Azure Artifacts | - | - | UCD buztool configuration
*Deployment to Integration Test environment*  |   |   |   |  
Deployment Integration Test Environment | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh  | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: ucdDeploy.sh. Create Link to UCD Deployment request to Pipeline run
Publish deployment logs | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload  | :small_blue_diamond: prepareLogs.sh <br> and sftp upload  |  Create UCD Deployment Link in Pipeline run
*Deployment to Acceptance Test environment*  |   |   |   |  
Retrieve Deployment Package | Download from ADO package registry | The package from pipeline working directory is used | The package from pipeline working directory is used | Not part of the pipeline template
Deployment Acceptance Test Environment | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh  | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | Not part of the pipeline template
Publish deployment logs | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload  | :small_blue_diamond: prepareLogs.sh <br> and sftp upload  |  Not part of the pipeline template
*Deployment to Production Test environment*  |   |   |   |  
Retrieve Deployment Package | Download from ADO package registry | The package from pipeline working directory is used | The package from pipeline working directory is used | Not part of the pipeline template
Deployment Production Test Environment | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh  | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | (Not part of the pipeline template
Publish deployment logs | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload  | :small_blue_diamond: prepareLogs.sh <br> and sftp upload  |  Not part of the pipeline template
Release Finalisation | Creation of the release candidate Tag in ADO via the ADO CLI | Creation of a Release and Release-tag via GH Cli for the release | Computation of the release candidate and planned release name <br> Creation of the release Git tag in Gitlab via REST <br> Automated update of the `baselineReference.conf` file | No automated tagging
Workspace Cleanup | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | Jenkins Plugin
