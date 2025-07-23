# IBM Wazi Deploy query and reporting template

This template provides a [.gitlab-ci.yml](.gitlab-ci.yml) definition file to setup a pipeline for query the deployment information from the Wazi Deploy evidence file and create a deployed artifacts report.

## Overview and capabilities
This pipeline template is analyzing the evidence file, to list the details of the deployed artifacts that are created as a result of the deployment and generate a deployment report of the deployed artifacts.

The pipeline leverages the [queryTemplate.yml](queryTemplate.yml) to query deployed artifacts information from the evidence files.


The pipeline has only one stage:

`Query`
   * index the evidence files to make them searchable
   * query the index based on the provided search criteria that got passed in via the pipeline request dialogue.

The pipeline uses the Gitlab concepts: `Stage`and `Jobs`.

<img width="658" alt="image" src="https://github.com/user-attachments/assets/dead3fd1-3bf1-41e6-9c0a-394b5fc6c743" />


## Prerequisites

This pipeline is submitting a query against a centrally managed set of Wazi Deploy evidence files that are expected to be collected on the same machine where this pipeline will be executed.

The directory of Wazi Deploy evidence files is referenced by within the .gitlab-ci.yml file by the wdEvidencesRoot variable.



## Setup of template

This is a standalone pipeline that can be used to query the evidence file. To get started,

   * Create a GitLab project
   * Copy the queryTemplate.yml and renderer.yml file into the root of the project.
   * Review the .gitlab-ci.yml file with your Gitlab administrator.

The renderer is optional and can be in various formats like SQL, .csv, html, JSON etc. Incase the renderer is not specified, the retrieved data is presented in a raw YAML format.
please find more documentation here [IBM Wazi Deploy documentation](https://www.ibm.com/docs/en/developer-for-zos/17.0.0?topic=deploy-getting-started-analysis-deployment-results))

### CLI Parameter and description

The following variables need to be updated within the pipeline definition file: `.gitlab-ci.yml`.

CLI Parameter | Description
--- | ---
templateFile |  path to the query file that contains the extraction criteria for the analysis.
reportFile | path to store the output file produced as a result of running the Wazi deploy query.
rendererFile | (optional) path to the renderer file that transforms the analysis results into a specified output format such as HTML, JSON or txt 


## Pipeline usage

This pipleine implements the [wazi-deploy-evidence command](https://www.ibm.com/docs/en/developer-for-zos/17.0.0?topic=commands-wazi-deploy-evidence-command) for easy use of deployment analysis. 

Please check the below parameters for which this pipeline is executed.

Parameter | Description
--- | ---
application | Specify the name of your application (eg: retirementCalculator, Mortgage-SA)
module | The program name
type | Specify the type of artifacts (eg: COBOL,JCL)
environment | Specify the environment (eg: integration, acceptance)


### Implementation of the pipeline

To query the Wazi deploy evidence index, the developer triggers the pipeline manually. When requesting the pipeline, multiple filter criteria can be configured to limit the search on element name, element type, application or environment.

### Sample output 
This can be downloaded and viewed after a successful run of the pipeline.
Eg:

```
# Artifacts of type JCL for application retirementCalculator in environment EOLEB7-Integration:



Environment          Type     Artifact               App Name                 Version                                Packaging Timestamp          Deploy Timestamp
-------------------  -------  ---------------------  -----------------------  --------------------------------------  ---------------------------  -------------------------
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11803        20250115.073112.416        20250115.073121.868
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11804        20250115.073619.607        20250115.073629.313
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11811        20250123.110036.146        20250123.110058.709
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11818        20250124.142002.958        20250124.142033.268
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11856        20250305.080255.537        20250305.080316.195
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11858        20250305.101043.715        20250305.101118.107
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11876        20250317.084317.014        20250317.085150.733
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11877        20250317.092005.774        20250317.092014.648
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11878        20250317.092748.151        20250317.092755.492
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11881        20250317.111746.650        20250317.111755.437
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11882        20250317.121257.352        20250317.121306.330
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     retirementCalculator.build-11884        20250317.160431.210        20250317.160447.597
EOLEB7-Integration  JCL      EBUD0RUN.JCL           retirementCalculator     20250320.090959.868                     20250320.081249.425        20250320.081258.437

```

