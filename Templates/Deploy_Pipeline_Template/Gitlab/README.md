# GitLab Deployment Pipeline Template

This template provides a [.gitlab-ci.yml](.gitlab-ci.yml) definition file to set up a **GitLab CD (Continuous Deployment)** pipeline for applications managed in a GitLab Git repository.

## Overview and Capabilities

This pipeline template implements the [Git-based process and branching model for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev) within a **GitLab CI/CD deployment-only context**.

It leverages the [Common Backend Scripts](https://github.com/IBM/dbb/blob/main/Templates/Common-Backend-Scripts/README.md) and **IBM Wazi Deploy** to perform automated deployments of pre-built application packages from **Artifactory** into integration, acceptance, and production environments on z/OS.

The pipeline supports **manual and API triggers** and implements the following stages:

* `Validate` 

  * Validates the pipeline input parameters passed during manual or API trigger.
  * Determines the appropriate environment configuration file based on the selected target environment.
  * Ensures all required parameters (e.g., `application`, `buildId`, `artifactoryRepo`) are defined.

* `Generate Plan` 

  * Uses the Wazi Deploy script [`wazideploy-generate.sh`](../../Common-Backend-Scripts/wazideploy-generate.sh) to create the deployment plan on USS.
  * Supports both build-based and release-based deployments depending on the `pipelineType` value.

* `Deploy` 

  * Deploys the application using [`wazideploy-deploy.sh`](../../Common-Backend-Scripts/wazideploy-deploy.sh)
  * Executes the deployment against the selected target environment (integration, acceptance, or production).
  * Produces initial evidence files and deployment logs.

* `Report` 

  * Invokes [`wazideploy-evidence.sh`](../../Common-Backend-Scripts/wazideploy-evidence.sh) to generate a deployment report and evidence summary.
  * Downloads the report and evidence from USS to GitLab artifacts.
  * Updates the centralized Wazi Deploy evidence index for traceability.

* `Cleanup` 

  * Removes the temporary USS workspace using [`deleteWorkspace.sh`](../../Common-Backend-Scripts/deleteWorkspace.sh).
  * Cleans up intermediate files and temporary deployment directories.

---

Depending on the target environment and pipeline type, you can trigger this CD pipeline for:

* Continuous deployment to **integration** (automated verification)
* Controlled deployments to **acceptance** or **production** (manual approval flow)

The pipeline uses the GitLab concepts of `stages` and `jobs`.

![gitlab-cd-pipeline.png](../Gitlab/images/gitlab-cd-pipeline.png)

---

## Prerequisites

To leverage this template, access to a **GitLab CI/CD environment** and a properly configured **GitLab runner** capable of connecting to your z/OS environment is required.

Review the setup instructions provided in the IBM document:
[Integrating IBM z/OS platform in CI/CD pipelines with GitLab](https://www.ibm.com/support/pages/system/files/inline-files/Integrating%20IBM%20zOS%20platform%20in%20CICD%20pipelines%20with%20GitLab%20-%20v1.7_1.pdf).

This pipeline requires **Zowe CLI** to execute commands remotely on z/OS UNIX System Services (USS) using the `issue unix-shell` command, available in **IBM RSE API Plug-in for Zowe CLI version 4.0.0 or later**.

* The Zowe CLI `base` and `rse` profiles must be configured under `.zowe/zowe.config.json`:

```json
{
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
}
```

The [Common Backend Scripts](../Common-Backend-Scripts/) and Wazi Deploy must be properly installed and configured in the z/OS environment.

---

## Installation and Setup of Template

**Note:** Please work with your DevOps or pipeline specialist to configure this template.

1. Place the `.gitlab-ci.yml` file in the **root directory** of your GitLab project.
2. Ensure the **GitLab runner** has access to both the USS environment and the Artifactory repository.
3. Review and adapt the environment paths and variables according to your setup.

### Required Environment Variables (in GitLab → Settings → CI/CD → Variables)

| Variable                                    | Description                                                         |
| ------------------------------------------- | ------------------------------------------------------------------- |
| `RSEAPI_USER`                               | Username for RSE API server authentication (Zowe).                  |
| `RSEAPI_PASSWORD`                           | Password for RSE API user.                                          |
| `RSEAPI_WORKING_DIRECTORY`                  | Working directory path on USS for Zowe commands.                    |
| `PIPELINE_WORKSPACE`                        | Root workspace directory on z/OS USS (e.g., `/u/gitlab/workspace`). |
| `WAZI_DEPLOY_SAMPLES`                       | Base path for environment configuration YAML files.                 |
| `ARTIFACTORY_URL`                           | Base URL of your Artifactory instance.                              |
| `ARTIFACTORY_USER` / `ARTIFACTORY_PASSWORD` | Artifactory credentials for artifact download.                      |

---

## Pipeline Usage

This CD pipeline supports **manual or API triggers** and can be used for both build and release deployments.

It automatically performs validation, plan generation, deployment, evidence generation, and cleanup.

### Pipeline Parameters

| Parameter           | Required                      | Description                                                               |
| ------------------- | ----------------------------- | ------------------------------------------------------------------------- |
| `artifactoryRepo`   |                              | Artifactory repository name containing the build artifact.                 |
| `application`       |                              | Application name to deploy.                                                |
| `buildId`           |                              | Build identifier corresponding to the artifact.                            |
| `releaseVersion`    |  Only for release pipelines  | Release version to deploy (e.g., `rel-2.6.0`).                             |
| `targetEnvironment` |                              | Target deployment environment (`integration`, `acceptance`, `production`). |
| `branchName`        |                              | Source branch name used for the build.                                     |
| `pipelineType`      |                              | Pipeline type: `build` or `release`.                                       |

---

### Example 1 — Manual Trigger (from GitLab UI)

1. Navigate to **CI/CD → Pipelines → Run Pipeline**
2. Choose the appropriate branch (e.g., `main`)
3. Enter pipeline variables:

   ```
   artifactoryRepo: retirementCalculator-gitlab-repo-local
   application: retirementCalculator
   buildId: 12247
   targetEnvironment: integration
   branchName: main
   pipelineType: build
   ```
4. Click **Run pipeline**

---

### Example 2 — API Trigger

```bash
curl --request POST \
  --form "token=<YOUR_TRIGGER_TOKEN>" \
  --form "ref=main" \
  --form "variables[artifactoryRepo]=retirementCalculator-gitlab-repo-local" \
  --form "variables[application]=retirementCalculator" \
  --form "variables[buildId]=12247" \
  --form "variables[targetEnvironment]=acceptance" \
  --form "variables[branchName]=main" \
  --form "variables[pipelineType]=release" \
  --form "variables[releaseVersion]=rel-2.6.0" \
  "https://gitlab.example.com/api/v4/projects/<PROJECT_ID>/trigger/pipeline"
```

---

## Summary

This template provides a **deployment-only GitLab CD pipeline** to promote application builds across environments using IBM Wazi Deploy and Zowe CLI.

It is ideal for:

* Automating deployments of prebuilt packages
* Maintaining auditable evidence across environments
* Integrating mainframe deployments into enterprise DevOps pipelines

---


