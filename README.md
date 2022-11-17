# DBB Community Repository
Welcome to the IBM Dependency Based Build (DBB) community repository. The helpful and handy location for finding and sharing example DBB scripts and snippets.

## Resources
* [IBM Dependency Based Build Product Page](https://www.ibm.com/products/dependency-based-build)
* [IBM DBB Documentation](https://www.ibm.com/docs/en/dbb)
* [IBM zDevOps Community](https://community.ibm.com/community/user/ibmz-and-linuxone/groups/topic-home?CommunityKey=f36c1ced-7e79-43cd-897c-e798acfef4a4)
* [IBM DBB Community](https://community.ibm.com/community/user/ibmz-and-linuxone/groups/topic-home/blog-entries?communitykey=20c9b889-9450-4ab6-8f11-8a5eb2b3342d)
* [IBM DBB zAppBuild](https://github.com/IBM/dbb-zappbuild)


## Contributing
For instructions on how to contribute new samples and bug fixes, please read the [Contributions Guidelines](CONTRIBUTIONS.md).

## Content
Sample | Description
--- | ---
[Build/HelloWorld](Build/HelloWorld) | The HelloWorld sample provides simple source file types and the Groovy build scripts to compile them
[Build/MultiThreadMVSJob](Build/MultiThreadMVSJob) | Sample showing how to modilfy a compilation script to run in a multi-thread environment.
[IDE/GitISPFClient](IDE/GitISPFClient) | An ISPF interface that interacts with a Git repository to allow cloning, staging, checking in, pushing and pulling as well as other git commands.
[Migration/jcl](Migration/jcl) | Sample script to migrate JCL from a Z/OS dataset to a local Groovy script.
[Migration/sclm](Migration/sclm) | This sample provides scripts to migrate source members to local Git repository and convert the build information in SCLM into build Groovy scripts.
[Pipeline/AnalyzeCodeCoverageReport](Pipeline/AnalyzeCodeCoverageReport) | Sample script to extract and print Code Coverage information as collected by IBM Debug.
[Pipeline/CreateUCDComponentVersion](Pipeline/CreateUCDComponentVersion) | Post-build script to parse the DBB Build report to generate a UCD component shiplist file and to create a new UCD component version.
[Pipeline/DeployUCDComponentVersion](Pipeline/DeployUCDComponentVersion) | Sample script to trigger a UCD deployment from the pipeline, where the pipeline orchestrator does not provide standard plugins for this task.
[Pipeline/PackageBuildOutputs](Pipeline/PackageBuildOutputs) | Post-build script to create a generic package with the produced build outputs, optionally uploads results to an Artifactory repository. Artifactory deploy/download sample script.    
[Pipeline/PublishSharedInterfaces](Pipeline/PublishSharedInterfaces) | Post-build script to publish shared copybooks to a shared git repository managing all shared copybooks. 
[Pipeline/RunIDZCodeReview](Pipeline/RunIDZCodeReview) | Post-build script to integrate IBM IDz Code Review application into a pipeline.
[Pipeline/SimplePackageDeploy](Pipeline/SimplePackageDeploy) | Post-build script to deploy the tar package contents to the target libraries.
[Utilities/DeletePDS](Utilities/DeletePDS) | Sample script to delete PDSes on z/OS that are no longer needed.
[Utilities/Jenkins](Utilities/Jenkins) | Utility shell scripts supplied to address issues when running Jenkins remote agents on z/OS UNIX System Services (USS).
[Utilities/ReadSMFRecords](Utilities/ReadSMFRecords) | Groovy scripts to read System Management Facilities (SMF) records using IBM's Dependency Based Build capabilities.
[Utilities/Validation](Utilities/Validation) | Groovy scripts that can aid in the validation of product installs.
