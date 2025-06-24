# Gitlab IBM Wazi Deploy query template

This template provides a [.gitlab-ci.yml](.gitlab-ci.yml) definition file to setup a pipeline for query the deployment information from the Wazi Deploy evidence file and create a deployed artifacts report.

## Overview and capabilities
This pipeline template is analyzing the evidence file, to list the names of the deployed artifacts that are created as a result of the deployment and generate a deployment report of the deployed artifacts.

The pipeline leverages the [queryTemplate.yml](queryTemplate.yml) to query deployed artifacts information from the evidence files.


The pipeline has only one stage:

`Query`
   * index the evidence files to make them searchable
   * query the index based on the provided search criteria that got passed in via the pipeline request dialogue.

The pipeline uses the Gitlab concepts: `Stage`and `Jobs`.

<img width="658" alt="image" src="https://github.com/user-attachments/assets/dead3fd1-3bf1-41e6-9c0a-394b5fc6c743" />


## Structure and setup of template

This is a standalone pipeline that can be used to query the evidence file. Please review the definitions thoroughly with your Gitlab administrator to define the renderer file in the machine that hosts the GitLab runner. The renderer is optional and can be in various formats like SQL, .csv, html, JSON etc. Incase the renderer is not specified, the retrieved data is presented in a raw YAML format.
please find more documentation here [IBM Wazi Deploy documentation](https://www.ibm.com/docs/en/developer-for-zos/17.0.0?topic=deploy-getting-started-analysis-deployment-results))

### CLI Parameter and description

The following variables need to be updated within the pipeline definition file: `.gitlab-ci.yml`.

CLI Parameter | Description
--- | ---
templateFile |  path to the query file that contains the extraction criteria for the analysis.
reportFile | path to store the output file produced as a result of running the Wazi deploy query.
rendererFile | (optional) path to the renderer file that transforms the analysis results into a specified output format such as HTML, JSON or txt 


## Pipeline usage

This pipleine implements the [wazi-deploy-evidence-command](https://www.ibm.com/docs/en/developer-for-zos/17.0.0?topic=commands-wazi-deploy-evidence-command) for easy use of deployment analysis. 

Please check the below parameters for which this pipeline is executed.

Parameter | Description
--- | ---
application | Specify the name of your application (eg: retirementCalculator, Mortgage-SA)
module | The program name
type | Specify the type of artifacts (eg: COBOL,JCL)
environment | Specify the environment (eg: integration, acceptance)


### Implementation of the pipeline

When a developer wants to analyze the Wazi deploy evidence file to get the list of artifacts that have been deployed, the pipeline can be triggered manually.

### Sample output 
This can be downloaded and viewed after a successful run of the pipeline.
<img width="622" alt="image" src="https://github.com/user-attachments/assets/fbc471c0-d83a-404b-93e1-4ca75af207e9" />

