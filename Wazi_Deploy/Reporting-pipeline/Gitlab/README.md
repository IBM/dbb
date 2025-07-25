# IBM Wazi Deploy query and reporting pipeline template

This template provides a [.gitlab-ci.yml](.gitlab-ci.yml) definition file to setup a pipeline for query the deployment information from the Wazi Deploy evidence file and create a deployed artifacts report.

## Overview and capabilities
This pipeline template is analyzing the evidence file, to list the details of the deployed artifacts that are created as a result of the deployment and generate a deployment report of the deployed artifacts.

The pipeline leverages the [queryTemplate.yml](queryTemplate.yml) to query deployed artifacts information from the evidence files.


The pipeline has only one stage:

`Query`
   * index the evidence files to make them searchable
   * query the index based on the provided search criteria that got passed in via the pipeline request dialogue.

The pipeline uses the Gitlab concepts: `Stage`and `Jobs`.



## Prerequisites

This pipeline is submitting a query against a centrally managed set of Wazi Deploy evidence files that are expected to be collected on the same machine where this pipeline will be executed.

The directory of Wazi Deploy evidence files is referenced by within the .gitlab-ci.yml file by the wdEvidencesRoot variable.



## Setup and usage of template

This pipleine implements the [wazi-deploy-evidence command](https://www.ibm.com/docs/en/developer-for-zos/17.0.0?topic=commands-wazi-deploy-evidence-command) for easy use of deployment analysis. 

1. Create a new GitLab project
2. Copy the following files into the root of your new project:
      * [queryTemplate.yml](queryTemplate.yml) 
      * [renderer.yml](renderer.yml) 
      * [`Gitlab/`](./Gitlab/) directory  
3. Review the [.gitlab-ci.yml](.gitlab-ci.yml) with your Gitlab administrator. See [CLI Parameters and description](#cli-parameters-and-description)
3. Provide query parameters and trigger the pipeline manually. See [Pipeline parameters](#pipeline-parameters)

The job will output a deployment report either in plain text or YAML format (based on the renderer).


## CLI Parameters and description

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
module | The program name
type | Specify the type of artifacts (eg: COBOL,JCL)
environment | Specify the environment (eg: integration, acceptance)
