# DBB Community Repository

Welcome to the IBM Dependency Based Build (DBB) community repository. The helpful and handy location for finding and sharing example DBB scripts and snippets when building out DevOps pipelines for the mainframe.

## Resources

* [IBM Dependency Based Build Product Page](https://www.ibm.com/products/dependency-based-build)
* [IBM DBB Documentation](https://www.ibm.com/docs/en/dbb)
* [IBM DBB zBuilder Build Framework](https://www.ibm.com/docs/en/adffz/dbb/3.0.0?topic=zbuilder-getting-started) - DBB zBuilder is an integrated configuration-based build framework for building z/OS applications. Build configuration is defined in YAML files.
* [IBM DBB zAppBuild](https://github.com/IBM/dbb-zappbuild) - The zAppBuild project is a community-driven build framework implemented in Groovy.
* [IBM DBB Git Migration Modeler](https://github.com/IBM/dbb-git-migration-modeler/) - An asset to provide a guided approach to plan and migrate source codebase, and help to identify and document the boundaries of mainframe applications.
* [IBM Development and Pipeline Community](https://community.ibm.com/community/user/ibmz-and-linuxone/groups/topic-home?CommunityKey=f461c55d-159c-4a94-b708-9f7fe11d972b)
* [IBM DevOps Acceleration Program Solution Page](https://ibm.github.io/z-devops-acceleration-program/)

## Versions

Branches convey the purpose of their assets. [Releases](https://github.com/IBM/dbb/releases) are published frequently to highlight important updates. Tags no longer mirror product version numbers — samples document their own version requirements. Backward compatibility is not a primary objective.

> [!NOTE]
> In future releases, the repository layout will be simplified. In a next generation of the `main` branch, `main` will focus on assets targeting the use of the zBuilder framework with DBB 3.0 and later. A new epic branch will support the development of required changes. Contents of the current `main` branch will be kept available via a new `groovy-based` branch, acting as a maintenance branch, that will provide access to existing Groovy-based assets.

Until this change, the below branches' purposes are:

* [main](https://github.com/IBM/dbb/tree/main) - keeps the existing structure for both zBuilder and Groovy-based assets.

* [simplify/community-templates](https://github.com/IBM/dbb/tree/simplify/community-templates) - the new development branch focussing on the zBuilder framework with DBB 3.0 and later. This holds the simplified, designated target layout for the project.

See [releases](https://github.com/IBM/dbb/releases) for prior versions of the community assets.

## Contributing

For instructions on how to contribute new samples and bug fixes, please read the [Contributions Guidelines](CONTRIBUTIONS.md).

## Content
Sample | Description
--- | ---
[IDE/GitISPFClient](IDE/GitISPFClient) | An ISPF interface that interacts with a Git repository to allow cloning, staging, checking in, pushing and pulling as well as other git commands.
[Pipeline/AnalyzeCodeCoverageReport](Pipeline/AnalyzeCodeCoverageReport) | Sample script to extract and print Code Coverage information as collected by IBM Debug.
[Pipeline/CreateUCDComponentVersion](Pipeline/CreateUCDComponentVersion) | Post-build script to parse the DBB Build report to generate a UCD component shiplist file and to create a new UCD component version.
[Pipeline/DeployUCDComponentVersion](Pipeline/DeployUCDComponentVersion) | Sample script to trigger a UCD deployment from the pipeline, where the pipeline orchestrator does not provide standard plugins for this task.
[Pipeline/PackageBuildOutputs](Pipeline/PackageBuildOutputs) | Post-build script to create a generic package with the produced build outputs, optionally uploads results to an Artifactory repository. Artifactory deploy/download sample script.    
[Pipeline/RunIDZCodeReview](Pipeline/RunIDZCodeReview) | Post-build script to integrate IBM IDz Code Review application into a pipeline.
[Pipeline/SimplePackageDeploy](Pipeline/SimplePackageDeploy) | Post-build script to deploy the tar package contents to the target libraries.
[Scanners](Scanners) | Sample dependency scanner implementations using the extension framework of the DBB toolkit.
[Schema](Schema) | zBuilder schema used to configure YAML validation for build and application configurations in an IDE.
[Templates](Templates#pipeline-templates) | Contains Pipeline templates for various Pipeline orchestrators such as AzureDevOps, Gitlab, Github Actions and Jenkins.
[Templates/Common-Backend-Scripts](Templates/Common-Backend-Scripts) | Asset to encapsulate pipeline steps to simplify the pipeline implementation.
[Utilities/DeletePDS](Utilities/DeletePDS) | Sample script to delete PDSes on z/OS that are no longer needed.
[Utilities/Jenkins](Utilities/Jenkins) | Utility shell scripts supplied to address issues when running Jenkins remote agents on z/OS UNIX System Services (USS).
[Utilities/PermissionCheck](Utilities/PermissionCheck) | Groovy script to check the DBB role for a provided user.
[Utilities/ReadSMFRecords](Utilities/ReadSMFRecords) | Groovy scripts to read System Management Facilities (SMF) records using IBM's Dependency Based Build capabilities.
[WaziDeploy/Reporting](WaziDeploy/Reporting) | This category provides templates for querying IBM Wazi Deploy evidence files and generating detailed reports.
[WaziDeploy/Schemas](WaziDeploy/Schemas) | Wazi Deploy schemas used to configure Yaml validation for the config file, the deployment method file and the manifest file.
[WaziDeploy/zDeploy](WaziDeploy/zDeploy/) |  Wazi Deploy deployment configuration framework for both Ansible and Python, that allows maintaining application specific configuration along the core deployment configuration for Wazi Deploy.
[zBuilder extensions](zBuilder/) | IBM zBuilder extensions showcasing advanced scenarios.
[zBuilder/Cross-Application-Dependencies](zBuilder/Cross-Application-Dependencies/) | Sample configuration demonstrating zBuilder's cross-application dependency management feature, including the MortgageApplication split into five independently buildable sub-applications.
[zBuilder/MortgageApplication](zBuilder/MortgageApplication/) | Mortgage Application sample application prepared to be built with IBM DBB zBuilder.
