# Azure Pipeline Template

The template provides the [azure-pipelines.yaml](azure-pipelines.yml) to setup an Azure Pipeline for applications managed in Azure Git in a repository.

## Overview and Capabilities

This pipeline template is implementing the [Git-based process and branching model for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev) within an Azure DevOps context. It leverages the [Common-Backend scripts](../Common-Backend-Scripts/) to implement the stages Setup, Build, Packaging and Deployment. 

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
  * to run the Wazi Deploy [evidence command](../Common-Backend-Scripts/README.md#49---wazideploy-evidencesh) to generate deployment report and updating the evidence.
  * [preparing](../Common-Backend-Scripts/README.md#49---preparelogssh) the deployment log files and publish them to the Azure build result.


The pipeline uses the Azure concepts `Stage`, `Jobs` and `Tasks`:

![](images/ado_pipelineOverview.png)

## Installation and Setup

The `azure-pipeline.yaml` can be dropped into the root folder of your Azure git repository and will automatically provide pipelines for the specified triggers. Please review the definitions thoroughly with your Azure administrator. 

Following requirements need to be met:
* [Azure Agent](https://learn.microsoft.com/en-us/azure/devops/pipelines/agents/agents?view=azure-devops)  the pipeline script can connect to the mainframe LPAR
* Azure [SSH Service Connection](https://learn.microsoft.com/en-us/azure/devops/pipelines/library/service-endpoints?view=azure-devops&tabs=yaml#ssh-service-connection) to connect to the mainframe LPAR for the Azure built-in SSH tasks
* The private SSH key of the TSO technical user to login to z/OS Unix System Services from scripts

### Required Pipeline Variables

Variable | Description
--- | ---
  agentPool                            | Agent pool name for Azure Agents to connect to MVS
  zosSSHConnection                     | zOS - The name of the Azure SSH connection name for connecting to z/OS Unix System Services of the LPAR where the build actions will be performed.
  sshZosKnownHost                      | The known host entry for secure shell connections. [See Notes](#obtaining-the-known-host-entry-for-secure-shell-connections).
  pipelineWorkspace                    | Root directory on z/OS Unix System services to perform builds. E.g. `/u/ado/workspace`
  wdEnvironmentFile                    | The Wazi Deploy environment file for the deployment into the lowest stage
  zosSFTPHostname                      | zOS - Host name (or Host IP address) for SFTP connection
  zosSFTPUser                          | zOS - Host user for SFTP connection
  azureArtifactFeedID                  | Feed ID of the Azure artifact for publishing the package (when publishing to Azure DevOps Artifacts)
  azureArtifactVersionOption           | Azure artifact version option (when publishing to Azure DevOps Artifacts)

#### Obtaining the known host entry for secure shell connections

For any actions over secure shell, that are executed outside of the Azure SSH Task, the pipeline is installing the private ssh key of the mainframe user and the entry for the known hosts file

**Obtaining the known host entry**

Open a terminal and issue `ssh-keyscan eoleb7.dat.ibm.com` - with using the LPAR hostname as argument.

 ```
    ssh-keyscan eoleb7.dat.ibm.com          
    # eoleb7.dat.ibm.com:22 SSH-2.0-OpenSSH_7.6
    eoleb7.dat.ibm.com ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEAwzoxzESrEqeWmAZNIa6NWJXh6l+BgX8JlZ3er1tMUAxKBEQ7aBKbCb+64P1m0TbpWhVMEYZBmHhpvAn6N86/4YLWCn8sJmshC9u7bag3dcSorIDO+/el2ochP+Ub4cD/V3DvOxVBsjK+a2nPBDZDbDjI5jdjEDfTC/uXRdQA3Qs=
    # eoleb7.dat.ibm.com:22 SSH-2.0-OpenSSH_7.6
    # eoleb7.dat.ibm.com:22 SSH-2.0-OpenSSH_7.6
```

Select the uncommented line as the value for the `sshZosKnownHost` key of the pipeline variable.

### Configurable pipeline parameters when requesting a pipeline

Parameter | Description
--- | ---
pipelineType     | Pipeline type - either build, release or preview. (Default: build)
verbose          | boolean flag to control logging of build framework. (Default: false) 

