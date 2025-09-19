# IBM Wazi Deploy query and reporting pipeline template

This template provides a [.gitlab-ci.yml](.gitlab-ci.yml) definition file to set up a pipeline that lists deployed packages within a specified time window by querying deployment information from Wazi Deploy evidence files.

## Overview and capabilities
This pipeline queries Wazi Deploy evidence files to extract and list details of packages deployed during a specific time window, generating a report of deployed packages within the specified timeframe.

The pipeline leverages a custom query template to filter deployment information based on the time range provided.

The pipeline consists of a single stage:

`Query`
   * Index the evidence files to enable searchability
   * Query the indexed evidence based on the specified time window parameters passed via the pipeline request dialog

The pipeline uses the Gitlab concepts: `Stage`and `Jobs`.



## Prerequisites

This pipeline submits queries against a centrally managed set of Wazi Deploy evidence files expected to be available on the same machine where the pipeline runs.

The directory containing Wazi Deploy evidence files is referenced by the wdEvidencesRoot variable in the .gitlab-ci.yml file.



## Setup and usage of template

This pipeline uses the [wazi-deploy-evidence command](https://www.ibm.com/docs/en/developer-for-zos/17.0.0?topic=commands-wazi-deploy-evidence-command) to perform queries and analysis on previous deployments. 

1. Create a new GitLab project
2. Copy the following files into the root of your new project:
      * [queryTemplate.yml](queryTemplate.yml) 
      * [renderer.yml](renderer.yml) or [renderer.html](renderer.html) 
      * [`Gitlab/`](./Gitlab/) directory  
3. Review the [.gitlab-ci.yml](.gitlab-ci.yml) with your Gitlab administrator. See [CLI Parameters and description](#cli-parameters-and-description)
3. Provide query parameters and trigger the pipeline manually. See [Pipeline parameters](#pipeline-parameters)

The job will output a deployment report either in plain text, YAML or html format (based on the renderer).


## Pipeline variables

The following variables need to be updated within the pipeline definition file: `.gitlab-ci.yml`.

CLI Parameter | Description
--- | ---
templateFile |  path to the query file that contains the extraction criteria for the analysis.
reportFile | path to store the output file produced as a result of running the Wazi deploy query.
rendererFile | (optional) path to the renderer file that transforms the analysis results into a specified output format such as HTML, JSON or txt 

## Pipeline parameters

Please check the below parameters for which this pipeline is executed.

Parameter | Description
--- | ---
application | Specify the name of your application (eg: retirementCalculator, Mortgage-SA)
environment | Specify the environment (eg: integration, acceptance)
deployed_after | Start timestamp (inclusive) for the deployment package listing
deployed_before | End timestamp (inclusive) for the deployment package listing
