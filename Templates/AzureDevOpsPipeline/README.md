# Azure Pipeline Template

The template provides the [azure-pipelines.yaml](azure-pipelines.yml) to setup an Azure Pipeline for applications managed in Azure Git in a repository.

## Overview and Capabilities

This pipeline template is implementing the Git-based process for mainframe development within an Azure DevOps context. It leverages the [Common-Backend scripts](../Common-Backend-Scripts/) to implement the stages Setup, Build, Packaging and Deployment. 

The pipeline implements the following stages

* `Setup` stage to [clone](../Common-Backend-Scripts/README.md#41---gitclonesh) the git repository to a workspace directory on z/OS Unix System Services. 
* `Build` stage 
  * invoking the zAppBuild [build](../Common-Backend-Scripts/README.md#42---dbbbuildsh) framework,
  * [preparing](../Common-Backend-Scripts/README.md#49---preparelogssh) the log files and publish them to the Azure build result.
* `Packaging` stage
  * to create a new [UCD component](../Common-Backend-Scripts/README.md#45---ucdpackagingsh) version (commented out)
  * create a package tar based on [package build outputs script](../Common-Backend-Scripts/README.md#44---packagebuildoutputssh)
  * a sample step to load the package file to the Azure Artifacts
* `Deployment` stage
  * to run the Wazi Deploy [generate command](../Common-Backend-Scripts/README.md#47---wazideploy-generatesh)
  * to deploy the package with the Wazi Deploy [deploy command](../Common-Backend-Scripts/README.md#48---wazideploy-deploysh) (Python-based)

The pipeline uses the concepts `Stage`, `Jobs` and `Tasks`.

## Installation and Setup

The `azure-pipeline.yaml` can be dropped into the root folder of your Azure git repository and will automatically provide pipelines for the specified triggers. Please review the definitions thoroughly with your Azure administrator. 

### Required Pipeline Variables

Variable | Description
--- | ---
  agentPool                            | Agent pool name for Azure Agents to connect to MVS
  zosSSHConnection                     | zOS - SSH connection name
  pipelineWorkspace                    | Root directory on z/OS Unix System services to perform builds. E.g. `/u/ado/workspace`
  wdEnvironmentFile                    | The Wazi Deploy environment file for the deployment into the lowest stage
  zosSFTPHostname                      | zOS - Host name (or Host IP address) for SFTP connection
  zosSFTPUser                          | zOS - Host user for SFTP connection
  azureArtifactFeedID                  | Feed ID of the Azure artifact for publishing the package (when publishing to Azure DevOps Artifacts)
  azureArtifactVersionOption           | Azure artifact version option (when publishing to Azure DevOps Artifacts)

### Pipeline parameters

Parameter | Description
--- | ---
pipelineType     | Pipeline type - either build, release or preview. (Default: build)
verbose          | boolean flag to control logging of build framework. (Default: false) 

