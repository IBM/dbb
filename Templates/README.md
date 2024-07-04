# Pipeline Templates

This category provides templates to implement a CI/CD pipeline covering the entire development lifecycle. These assets are developed based on [The Git-based process you need for Mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev) and help you to quickly setup your pipeline environment. Please make yourself familiar with the outlined Git branching model.

## Table of Contents 
Asset | Description | Documentation Link
--- | --- | ---
Common-Backend-Scripts | Core asset to simplify defining the pipeline orchestration by providing central services for the various stages of the CI/CD pipeline. Especially useful for pipeline architectures which don't provide a native runner . | [Common-Backend-Scripts/README.md](Common-Backend-Scripts/README.md)
Azure DevOps Pipeline Template | Template to setup a [AzureDevOps pipeline](https://learn.microsoft.com/en-us/azure/devops/pipelines/?view=azure-devops&viewFallbackFrom=azure-pipelines) to build, package and deploy Azure Repos Git. | [AzureDevOpsPipeline/README.md](AzureDevOpsPipeline/README.md)
Gitlab CI/CD Pipeline Template | .gitlab-ci.yml template to setup a [Gitlab CI/CD pipeline](https://docs.gitlab.com/ee/ci/pipelines/) to build, package and deploy Gitlab platform. | [GitlabCIPipeline/README.md](GitlabCIPipeline/README.md)
GitHub Actions Pipeline Template | Template to setup a [GitHub Actions Pipeline](https://docs.github.com/en/actions) to build, package, and deploy a GitHub repository. | [GitHubActionsPipeline/README.md](GitHubActionsPipeline/README.md)
Jenkins Multibranch Pipeline Template | Multibranch pipeline template to setup a [Jenkins CI/CD pipeline](https://www.jenkins.io/doc/book/pipeline/multibranch/) to build, package and deploy with any Git provider. | [JenkinsPipeline/README.md](JenkinsPipeline/README.md)

Please use the [Github discussion](https://github.com/IBM/dbb/discussions) board for feedback and comments on these templates.


## Capabilities of Pipeline Templates 

The below tables provide an overview of the implemented capabilities of the pipeline templates. The information can be used to lookup and understand the variations in the implementations. The icon :small_blue_diamond: indicates that this step uses a [Common Backend Script](Common-Backend-Scripts/).

### Communication and Tool selection

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
**Communication between CI platform and mainframe** | ssh | IBM RSE API Plug-in for Zowe CLI | ssh | zos-agent
**Git Provider** | Azure DevOps | GitLab | GitHub | Any Git provider
**Deployment technology** | Wazi Deploy | Wazi Deploy | Wazi Deploy | IBM DevOps Deploy (a.k.a. UCD)
**Additional integration technologies** | Azure DevOps CLI | GitLab REST interface | GitHub CLI | SonarQube sonar-scanner

### Feature Branch pipeline implementations

The purpose of feature branch pipelines is to provide an early feedback to developers about the impacted components of their changes, building a preliminary package that can be tested in an isolated environment.

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
**Clone Git repository** | :small_blue_diamond: gitClone.sh | :small_blue_diamond: gitClone.sh | :small_blue_diamond: gitClone.sh | Jenkins Git plugin
**Build** | :small_blue_diamond: dbbBuild.sh | :small_blue_diamond: dbbBuild.sh | :small_blue_diamond: dbbBuild.sh | :small_blue_diamond: dbbBuild.sh
**Publish Build Logs** | :small_blue_diamond: prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond: prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond: prepareLogs.sh and zowe CLI rse to load and attach logs | Jenkins artifactPublisher plugin
**Code Quality and Scans** | - | - | - | SonarQube Scan
**Create package** | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: ucdPackaging.sh to create UCD component version
**Publish Package** | Upload to Azure Artifacts | Enable upload to enterprise artifact repository | Enable upload to enterprise artifact repository | Depending on UCD buztool configuration
**Workspace Cleanup** | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | Jenkins Workspace plugin

### Integration branch pipeline implementations

These steps are implemented when merging changes into the `main`, `release` maintenance or `epic` branch. It builds with the debug options and automatically installs it into the development test environment.

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
**Clone Git repository** | :small_blue_diamond: gitClone.sh | :small_blue_diamond: gitClone.sh | :small_blue_diamond: gitClone.sh | Jenkins Git Plugin
**Build** | :small_blue_diamond: dbbBuild.sh | :small_blue_diamond: dbbBuild.sh | :small_blue_diamond: dbbBuild.sh | :small_blue_diamond: dbbBuild.sh
**Publish Build Logs** | :small_blue_diamond: prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond: prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond: prepareLogs.sh and ZOWE CLI rse to load and attach logs | Jenkins artifactPublisher plugin
**Code Quality and Scans** | - | - | - | SonarQube Scan
**Create package** | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: ucdPackaging.sh to create UCD component version <br> Create Link to UCD in Pipeline run
**Publish Package** | Upload to Azure Artifacts | - | - | Depending on UCD buztool configuration
*Deployment to Integration Test environment* | | | | 
**Deployment Integration Test environment** | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: ucdDeploy.sh
<br> | <br> | Copy WD Evidence File to Evidence Inventory | Copy WD Evidence File to Evidence Inventory | <br>
**Publish deployment logs** | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | Create UCD Deployment Link in Pipeline run
**Workspace Cleanup** | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | Jenkins Plugin

### Release pipeline implementations

These steps are implemented when requesting a release pipeline.

Capability | Azure DevOps | GitLabCI | GitHub Actions | Jenkins
--- | --- | --- | --- | ---
**Clone Git repository** | :small_blue_diamond: gitClone.sh | :small_blue_diamond: gitClone.sh | :small_blue_diamond: gitClone.sh | Jenkins Git Plugin
**Build** | :small_blue_diamond: dbbBuild.sh | :small_blue_diamond: dbbBuild.sh | :small_blue_diamond: dbbBuild.sh | :small_blue_diamond: dbbBuild.sh
**Publish Build Logs** | :small_blue_diamond: prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond: prepareLogs.sh and sftp to load and attach logs | :small_blue_diamond: prepareLogs.sh and zowe CLI rse to load and attach logs | Jenkins artifactPublisher plugin
**Code Quality and Scans** | - | - | - | SonarQube Scan
**Creation of the release candidate tag** | Computation of the release candidate and planned release name <br> Creation of the release candidate Tag in ADO via the ADO CLI | Computation of the release candidate and planned release name <br> Creation of the release candidate Git tag in Gitlab via REST | Computation of the release candidate and planned release name <br> Creation of a pre-release via GH CLI for the release candidate | Computation of the UCD package name. No tagging in Git (independent of the Git provider).
**Create package** | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: packageBuildOutputs.sh | :small_blue_diamond: ucdPackaging.sh to create UCD component version <br> Create Link to UCD to Pipeline run
**Publish Package** (Recommendation -Enable upload to enterprise artifact repository) | Upload to Azure Artifacts | - | - | Depending on UCD buztool configuration
*Deployment to Integration Test environment* | | | | 
**Deployment Integration Test environment** | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: ucdDeploy.sh. Create Link to UCD Deployment request to Pipeline run
**Publish deployment logs** | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | Create UCD Deployment Link in Pipeline run
<br> | <br> | Copy WD Evidence File to Evidence Inventory | Copy WD Evidence File to Evidence Inventory | <br>
*Deployment to Acceptance Test environment* | | | | 
**Retrieve Deployment Package** | Download from ADO package registry | Package from pipeline working directory is used | Package from pipeline working directory is used | Not part of the pipeline template
**Deployment Acceptance Test environment** | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | Not part of the pipeline template
<br> | <br> | Copy WD Evidence File to Evidence Inventory | Copy WD Evidence File to Evidence Inventory | <br>
**Publish deployment logs** | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | Not part of the pipeline template
*Deployment to Production Test environment* | | | | 
**Retrieve Deployment Package** | Download from ADO package registry | The package from pipeline working directory is used | Package from pipeline working directory is used | Not part of the pipeline template
**Deployment Production environment** | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> Register deployment to environment in ADO | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> Register deployment to environment in GitHub | :small_blue_diamond: wazideploy-generate.sh <br> :small_blue_diamond: wazideploy-deploy.sh <br> :small_blue_diamond: wazideploy-evidence.sh <br> | Not part of the pipeline template
<br> | <br> | Copy WD Evidence File to Evidence Inventory | Copy WD Evidence File to Evidence Inventory | <br>
**Publish deployment logs** | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | :small_blue_diamond: prepareLogs.sh <br> and sftp upload | Not part of the pipeline template
**Release Finalisation** | Creation of a Git tag via ADO CLI | Creation of a Git tag and release via GH CLI | Creation of a Git tag via GitLab via REST <br> Automated update of the `baselineReference.conf` file | No automated tagging
**Workspace Cleanup** | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | :small_blue_diamond: deleteWorkspace.sh | Jenkins Plugin
