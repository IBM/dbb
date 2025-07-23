# IBM Wazi Deploy - Reporting Pipeline Template

This template contains  [queryTemplate.yml](queryTemplate.yml), [renderer.yml](renderer.yml) and a GitLab pipeline definition file for querying IBM Wazi Deploy evidence files and generating detailed reports of deployed artifacts.

## Overview and capabilities

This query template is analyzing the evidence file, to list the details of the deployed artifacts that are created as a result of the deployment and generate a deployment report of the deployed artifacts. The renderer helps in the further customization of the query result.

## Features

 Parses and indexes Wazi Deploy evidence files
  * Filters deployed artifacts by criteria (application, module, type, environment)
  * Outputs clean, customizable deployment reports
  * Uses [queryTemplate.yml](queryTemplate.yml) for filtering logic
  * Supports custom renderers via [renderer.yml](renderer.yml)

## Usage

Use this template when you want to:

   * Track deployment activity of specific applications or modules
   * Audit deployment history across environments
   * Analyze and troubleshoot artifact delivery

## Getting Started

To use this template as a pipeline:

   1. Create a new GitLab project
   2. Copy the following files into the root of your new project:
      * [queryTemplate.yml](queryTemplate.yml) 
      * [renderer.yml](renderer.yml) 
      * [`Gitlab/`](./Gitlab/) directory  

   3. Follow the detailed instructions in [Gitlab/README.md](./Gitlab/README.md) to configure and run the pipeline.


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

```

