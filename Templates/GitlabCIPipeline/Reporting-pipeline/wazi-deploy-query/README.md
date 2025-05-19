# Gitlab IBM Wazi Deploy query template
This template provides a [.gitlab-ci.yml](.gitlab-ci.yml) definition file to query the evidence file using  [queryTemplate.yml](queryTemplate.yml) template.

## Overview and capabilities
This pipeline template is analyzing the evidence file, to list the names of the deployed artifacts that are stored in it as a result of the deployment. 


The pipeline has only one stage called,

`Query`
   * To refresh Wazi Deploy index for all applications (index is a pointer to the latest data available)
   * To query the Wazi Deploy index. 

The pipeline uses the Gitlab concepts: `Stage`and `Jobs`.

<img width="658" alt="image" src="https://github.com/user-attachments/assets/dead3fd1-3bf1-41e6-9c0a-394b5fc6c743" />


## Structure and setup of template

In the current setting, `.gitlab-ci.yml` and `.queryTemplate.yml` are kept in `Wazi-Deploy-Query` under the `Reporting-pipeline` folder of the Gitlab Git repository. 
This is a standalone pipeline and to be able to query the evidence file successfully , the only things you need to be having are the above mentioned files along with `renderer.yml` and evidence file in your machine that hosts the  Gitlab runner. Please review the definitions thoroughly with your Gitlab administrator.


### Variables configuration

The following variables need to be updated within the pipeline definition file: `.gitlab-ci.yml`.

Variable | Description
--- | ---
templateFile |  Path to the query file that contains the extraction criteria for the analysis
rendererFile | Path to the renderer file that transforms the analysis results into a specified output format such as HTML, JSON or txt 
reportFile | Path to store the output file produced as a result of running the Wazi deploy query


## Pipeline usage

The pipeline template is analyzing the evidence file to list details of the deployed artifacts that are mentioned in your evidence files. It is useful because instead of reading through the evidence file, which is rather complicated, you can instead run the query to get the data in simple HTML, JSON or txt file format.

Please check the pipeline definition to understand the various triggers for which this pipeline is executed.

### Pipeline variables

Below are the variables when manually requesting the pipeline. 

Parameter | Description
--- | ---
application | Specify the name of your application( eg: retirementCalculator, Mortgage-SA)
module | The program name
type | Specify the type of artifacts (eg: COBOL,JCL)
environment | Specify the environment (eg: integration, acceptance)


### Implementation of the pipeline

When a developer wants to analyze the Wazi deploy evidence file to get the list of artifacts, the pipeline can be triggered manually.

The only step that it covers is, 

* `Query` - Refreshes Wazi deploy index and query the index using wazideploy-evidence command. It is then published into the location that you specify under the variable `reportFile`
