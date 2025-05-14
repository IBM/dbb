# Gitlab Wazi Deploy query template
This template provides a [.gitlab-ci.yml](.gitlab-ci.yml) definition file to query the evidence file using the [queryTemplate.yml](queryTemplate.yml) template.

## Overview and capabilities
This pipeline template is analyzing the evidence file, to list the names of the deployed artifacts that are stored in it as a result of the deployment. 


The pipeline has only one stage called,

`Query`
   * To refresh Wazi Deploy index for all applications (index is a pointer to latest data available)
   * To query the Wazi Deploy Index. 

The pipeline uses the Gitlab concepts: `Stage`and `Jobs`.

<img width="658" alt="image" src="https://github.com/user-attachments/assets/dead3fd1-3bf1-41e6-9c0a-394b5fc6c743" />


## Structure and setup of template

In the current setting, `.gitlab-ci.yml` and `.queryTemplate.yml` are kept in Wazi-Deploy-Query under the Reporting-pipeline folder of the Gitlab Git repository. 
To be able to query the evidence file successfully , you should be having the above mentioned files along with renderer.yml and evidence file in your Gitlab runner. Please review the definitions thoroughly with your Gitlab administrator.


### Variables configuration

The following variables need to be updated within the pipeline definition file: `.gitlab-ci.yml`.

Variable | Description
--- | ---
templateFile |  Path to the query file that contains the extraction criteria for the analysis
rendererFile | Path to renderer file that transforms the analysis results into a specified output format such as HTML, JSON or txt 
reportFile | Path to store the output file produced as a result of running the Wazi deploy query


## Pipeline usage

The pipeline template is analyzing the evidences file to list the names of the deployed artifacts that are mentioned in your evidence files. Listing these names is useful because you can then easily copy and paste them as arguments in the analysis command line when you want to base an analysis on a specified name.


Please check the pipeline definition to understand the various triggers for which this pipeline is executed.

Please make yourself familiar with the [Git branching for mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev/#characteristics-of-mainline-based-development-with-feature-branches) documentation.

### Pipeline variables

In a default setup, the values of all the below parameters are set to '*'. It allows overriding the values of the below variables when manually requesting the pipeline. 

Parameter | Description
--- | ---
application | Specify the name of your application( eg: retirementCalculator, Mortgage-SA)
module | The program name
type | Specify the type of artifacts (eg: COBOL,JCL)
environment | Specify the environment (eg: integration, acceptance)


### Implementation of the pipeline

When a developer wants to analyze the Wazi deploy evidence file to get the list of artifacts, the pipeline can be triggered manually.

The only step that it covers is, 

* `Query` - Refreshes Wazi deploy index and query the index using wazideploy-evidence command. It is then published into the location stored under variable `reportFile`
