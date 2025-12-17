# Jenkins Deployment Pipeline Template

This template provides a [Jenkinsfile](Jenkinsfile) to set up a **Deployment pipeline** for applications managed in a Git repository.

## Overview and Capabilities

This Jenkins pipeline implements the [Git-based process and branching model for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/branching/git-branching-model-for-mainframe-dev) and leverages [Common Backend Scripts](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md) along with **IBM Wazi Deploy** to automate deployments of pre-built application packages from an artifact repository into integration, acceptance, and production environments on z/OS.

The pipeline supports **manual triggers** and implements the following stages:

* **Validate Parameters**  
  - Validates pipeline input parameters passed during manual trigger or via API.  
  - Determines the appropriate environment configuration file based on the selected target environment.  
  - Ensures required parameters (e.g., `application`, `packageType`, `buildId`) are defined.

* **Generate Plan**  
  - Uses [`wazideploy-generate.sh`](../../Common-Backend-Scripts/wazideploy-generate.sh) to create the deployment plan in z/OS Unix System Services (USS).  
  - Supports both the deployment of preliminary **build** and **release** packages.

* **Deploy**  
  - Executes [`wazideploy-deploy.sh`](../../Common-Backend-Scripts/wazideploy-deploy.sh) to deploy the application to the selected target environment.  
  - Produces initial evidence files and deployment logs.  
  - Uses `returnStatus` for robust error handling (`sh(script: ..., returnStatus: true)`).

* **Report**  
  - Invokes [`wazideploy-evidence.sh`](../../Common-Backend-Scripts/wazideploy-evidence.sh) to generate deployment reports and evidence summaries.  
  - Persists evidence files to centralized directories.  
  - Updates the Wazi Deploy evidence index for traceability.

* **Workspace Cleanup**  
  - Cleans up intermediate files and temporary deployment directories 
---

The pipeline supports controlled deployments to **integration**, **acceptance**, or **production** environments. 
It uses Jenkins **stages**, **steps**, and **post conditions** for robust execution and error handling.

![image.png](./images/image.png)

---

## Prerequisites

To leverage this Jenkinsfile:

* A **Jenkins environment** with agents capable of running shell scripts and connecting to the z/OS UNIX System Services (USS) environment.
* Access to **IBM Wazi Deploy** scripts and **Common Backend Scripts** installed on the deployment agent.
* Appropriate workspace permissions for reading/writing deployment directories and evidence files.

---

## Installation and Setup

* Place the [Jenkinsfile](Jenkinsfile) in the root directory of your project repository.
* Please review the definitions thoroughly with your Jenkins administrator and adjust pipeline environment variables, workspace paths, and script locations according to your environment.

### Required Parameters

| Parameter           |  Description                                                                        |
| ------------------- | ----------------------------------------------------------------------------------- | 
| `application`       | Application name to deploy.                                                         |
| `buildId`           | Build pipeline ID corresponding to the artifact.                                    |
| `packageReference`  | Release version (e.g., `rel-2.6.0`) or branch name(e.g., `main`, `feature`).        |
| `targetEnvironment` | Deployment target (`integration`, `acceptance`, `production`).                      |
| `packageType`       | Package type: `build` or `release`.                                                 |

---

## Pipeline Usage

This Jenkinsfile can be executed:

* **Manually** via Jenkins → Build with Parameters

### Example

1. Open Jenkins → select the job → **Build with Parameters**
2. Enter values:
   Eg:
   ```
    application: retirementCalculator
    buildId: 12247
    packageReference: rel-1.6.1
    targetEnvironment: integration
    packageType: release
    
   ```

3. Click **Build**  

The pipeline will:

1. Validate input parameters  
2. Generate a deployment plan  
3. Deploy the application  
4. Generate deployment report and evidence  
5. Clean up workspace  

---

## Error Handling and Return Codes

* Shell commands are executed with `returnStatus: true` to capture **exit codes** without failing the pipeline immediately.
* Custom logic handles deployment errors, evidence generation failures, and workspace cleanup issues.
* Jenkins `post` blocks ensure **always-run cleanup** and artifact archiving.

---


## Summary

This Jenkinsfile provides a **robust deployment CD pipeline** for mainframe applications:

* Automates deployments of prebuilt packages to multiple environments  
* Maintains auditable deployment evidence and reports  
* Provides detailed logging and error handling  
* Cleans workspace to maintain agent hygiene  

It integrates **Common Backend Scripts** into enterprise Jenkins pipelines for automated, traceable deployment workflows.

