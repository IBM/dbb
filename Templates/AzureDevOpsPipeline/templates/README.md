# README

This folder contains [Azure DevOps Templates](https://learn.microsoft.com/en-us/azure/devops/pipelines/process/templates?view=azure-devops&pivots=templates-includes) YAML configurations, that are referenced by the main Azure pipeline template. 

Please work with your Azure DevOps administrators to agree on how and where to maintain these scripts.

## Inventory

Template | Description
--- | ---
[deployment/deployReleasePackage.yml](deployment/deployReleasePackage.yml) | Template to deploy a release package into the various controlled test environments, including production.
[tagging/createProductionReleaseTag.yml](tagging/createProductionReleaseTag.yml) | Template that uses the AZ CLI to tag the commit of the build that was deployed to the production environment (Schema `rel-1.1.1`).
[tagging/createReleaseCandidate.yml](tagging/createReleaseCandidate.yml) | Template to compute and create the release candidate tag (following semantic versioning schema `rel-1.1.1_rc00`) to improved traceability and understanding. Leverages the AZ CLI.