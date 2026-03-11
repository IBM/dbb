# IBM Wazi Deploy query and reporting pipeline template

This template provides a [Jenkinsfile](Jenkinsfile) to set up a pipeline that lists deployed packages within a specified time window by querying deployment information from Wazi Deploy evidence files.

## Overview and capabilities
This pipeline queries Wazi Deploy evidence files to extract and list details of packages deployed during a specific time window, generating a report of deployed packages within the specified timeframe.

The pipeline leverages a custom query template to filter deployment information based on the time range provided.

The pipeline has three stages:

`clone`
   * Clone the repository containing all the source code

`Prepare Directories`
   * Create `reportOutputDirectory` to store the output file created as a result of running the query.

`Query`
   * Query the indexed evidence based on the provided search criteria that got passed in via the pipeline request dialogue.



## Prerequisites

This pipeline submits queries against a centrally managed set of Wazi Deploy evidence files expected to be available on the same machine where the pipeline runs.

The directory containing Wazi Deploy evidence files is referenced by the wdEvidencesRoot variable in the Jenkinsfile file.



## Setup and usage of template

This pipeline uses the [wazi-deploy-evidence command](https://www.ibm.com/docs/en/developer-for-zos/17.0.0?topic=commands-wazi-deploy-evidence-command) to perform queries and analysis on previous deployments. 

1. Create a new Jenkins project
2. Copy the following files from the parent directory into the root of your new project:
      * [queryTemplate.yml](../queryTemplate.yml) 
      * [renderer.yml](../renderer.yml) or [renderer.html](../renderer.html) 
      * [`Jenkins/Jenkinsfile`](Jenkinsfile) 
3. Review the [Jenkinsfile](Jenkinsfile) with your Jenkins administrator.
3. Provide query parameters and trigger the pipeline manually. See [Pipeline parameters](#pipeline-parameters)

The job will output a deployment report either in plain text, YAML or html format (based on the renderer).


## Environment variables

The following variables need to be updated within the pipeline definition file: `Jenkinsfile`.

Pipeline variables | Description
--- | ---
templateFile |  Path to the query file that contains the extraction criteria for the analysis.
reportFile | Path to the output file produced as a result of running the Wazi deploy query.
rendererFile | (Optional) Path to the renderer file that transforms the analysis results into a specified output format such as HTML, JSON or TXT
wdEvidencesRoot | Directory on the Jenkins runner environment where the Wazi Deploy evidences from deployments got persisted.
wdEvidencesIndex |  Directory on the Jenkins runner environment to persist Wazi Deploy indexes
reportOutputDirectory | Directory on the Jenkins runner environment to store the output reports.

## Pipeline parameters

Please check the below parameters for which this pipeline is executed.

Parameter | Description
--- | ---
application | Name of the application (eg: retirementCalculator, Mortgage-SA) used for reporting. Use `*` to query on all applications.
environment | Environment (eg: integration, acceptance) used for reporting. Use `*` to query on all environments.
deployed_after | Start timestamp (inclusive) used for reporting on the deployment package listing
deployed_before | End timestamp (inclusive) used for reporting on the deployment package listing
