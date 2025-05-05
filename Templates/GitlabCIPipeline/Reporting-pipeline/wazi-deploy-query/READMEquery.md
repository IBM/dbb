# Gitlab Wazideploy query template
This template provides a [.gitlab-ci.yml](.gitlab-ci.yml) definition file to trigger an analysis of the evidence file with the Wazi Deploy evidence command and a  [queryTemplate.yml](queryTemplate.yml) template that contains the extraction criteria for the analysis.

## Overview and capabilities
This pipeline template is analyzing the Wazi Deploy deployment process and the content of the target deployment environment to list the names of the deployed artifacts that are stored in your evidence file a result of the deployment. 


The pipeline implements the following stages
* `Query` stage 
   * to refresh Wazi Deploy index for all applications
   * to query the Wazi Deploy Index. 


#Depending on your selected deployment technology, review the definitions and (de-)/activate the appropriate steps.

The pipeline uses the Gitlab concepts: `Stage`and `Jobs`.

<img width="658" alt="image" src="https://github.com/user-attachments/assets/dead3fd1-3bf1-41e6-9c0a-394b5fc6c743" />


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

The `.gitlab-ci.yml` and `.queryTemplate.yml` can be dropped into the wazi-deploy-query folder of your Gitlab Git repository and will automatically provide pipelines for the specified triggers. Please review the definitions thoroughly with your Gitlab administrator.

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

The following variables need to be updated within the pipeline definition file: `.gitlab-ci.yml`.

Variable | Description
--- | ---

templateFile | Path to the query file that contains the extraction criteria for the analysis
rendererFile | Path to renderer file that transforms the analysis results into a specified output format such as HTML, JSON or txt
reportFile | Path to store the output file produced as a result of running the Wazi deploy query


## Pipeline usage

The pipeline template is analyzing the Wazi Deploy deployment process and the content of the target deployment environment to list the names of the deployed artifacts that are mentioned in your evidence files. Listing these names is useful because you can then easily copy and paste them as arguments in the analysis command line when you want to base an analysis on a specified name.


Please check the pipeline definition to understand the various triggers for which this pipeline is executed.

Please make yourself familiar with the [Git branching for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#characteristics-of-mainline-based-development-with-feature-branches) documentation.

### Pipeline variables

In a default setup, the values of all the below parameters are set to '*'. It allows overriding the values of the below variables when manually requesting the pipeline. 

Parameter | Description
--- | ---
application | Specify the name of your application which will be used to invoke the [Common Backend scripts](../Common-Backend-Scripts/).
module |
type | Specify the type of artifacts
environment | Specify the environment


### Implementation of the pipeline

When a developer wants to analyze the Wazi deploy deployment process and the content of the target deployment environment, the pipeline can be triggered manually.

It covers the followings steps:
* Query - Refreshes Wazi deploy index and query it using wazideploy-evidence command 
