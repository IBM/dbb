# Pipeline Templates

This category provides templates to implement a CI/CD pipeline covering the entire development lifecycle. These assets are developed based on [The Git-based process you need for Mainframe development](https://ibm.github.io/z-devops-acceleration-program/docs/git-branching-model-for-mainframe-dev) and help you to quickly setup your pipeline environment. Please make yourself familiar with the outlined Git branching model.

## Table of Contents 
Asset | Description | Documentation Link
--- | --- | ---
Common-Backend-Scripts | Core asset to simplify defining the pipeline orchestration by providing central services for the various stages of the CI/CD pipeline. Especially useful for pipeline architectures which don't provide a native runner .  | [Common-Backend-Scripts/README.md](Common-Backend-Scripts/README.md)
Azure DevOps Pipeline Template | Template to setup a [AzureDevOps pipeline](https://learn.microsoft.com/en-us/azure/devops/pipelines/?view=azure-devops&viewFallbackFrom=azure-pipelines) to build, package and deploy Azure Repos Git. | [AzureDevOpsPipeline/README.md](AzureDevOpsPipeline/README.md)
Gitlab CI/CD Pipeline Template | .gitlab-ci.yml template to setup a [Gitlab CI/CD pipeline](https://docs.gitlab.com/ee/ci/pipelines/) to build, package and deploy Gitlab platform. | [GitlabCIPipeline/README.md](GitlabCIPipeline/README.md)
GitHub Actions Pipeline Template | Template to setup a [GitHub Actions Pipeline](https://docs.github.com/en/actions) to build, package, and deploy a GitHub repository. | [GitHubActionsPipeline/README.md]

We are expecting additional templates in this category for the various pipeline orchestrators such as AzureDevOps leveraging the Common Backend scripts. 