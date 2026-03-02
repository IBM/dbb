# IBM Wazi Deploy query and reporting pipeline template

This template provides a [Jenkinsfile](Jenkinsfile) to setup a pipeline for query the deployment information from the Wazi Deploy evidence file and create a deployed artifacts report.

## Overview and capabilities
This pipeline template is analyzing the evidence file, to list the details of the deployed artifacts that are created as a result of the deployment and generate a deployment report of the deployed artifacts.

The pipeline leverages the [queryTemplate.yml](../queryTemplate.yml) to query deployed artifacts information from the evidence files.


The pipeline has three stages:

`clone`
   * Clone the repository containing all the source code

`Prepare Directories`
   * Create `reportOutputDirectory` to store the output file created as a result of running the query.

`Query`
   * Query the indexed evidence based on the provided search criteria that got passed in via the pipeline request dialogue.



## Prerequisites

This pipeline submits queries against a centrally managed set of Wazi Deploy evidence files expected to be available on the same machine where the pipeline runs.

The directory of Wazi Deploy evidence files is referenced by within the Jenkinsfile file by the wdEvidencesRoot variable.



## Setup and usage of template

This pipeline implements the [wazi-deploy-evidence command](https://www.ibm.com/docs/en/developer-for-zos/17.0.0?topic=commands-wazi-deploy-evidence-command) for easy use of deployment analysis. 

1. Create a new Jenkins project
2. Copy the following files from the parent directory into the root of your new project:
      * [queryTemplate.yml](../queryTemplate.yml) 
      * [renderer.yml](../renderer.yml) 
      * [`Jenkins/Jenkinsfile`](Jenkinsfile) file
3. Review the [Jenkinsfile](Jenkinsfile) with your Jenkins administrator. 
3. Provide query parameters and trigger the pipeline manually. See [Pipeline parameters](#pipeline-parameters)

The job will output a deployment report either in plain text or YAML format (based on the renderer).


## Environment variables

The following variables need to be updated within the `Jenkinsfile`.

CLI Parameter | Description
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
application | Specify the name of your application (eg: retirementCalculator, Mortgage-SA)
module | The program name
type | Specify the type of artifacts (eg: COBOL,JCL)
environment | Specify the environment (eg: integration, acceptance)
