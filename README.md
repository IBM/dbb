# DBB Community Repository
Welcome to the IBM Dependency Based Build (DBB) community repository. The helpful and handy location for finding and sharing example DBB scripts and snippets.

## Resources
* [IBM Dependency Based Build Product Page](https://www.ibm.com/products/dependency-based-build)
* [IBM DBB Knowledge Center](https://www.ibm.com/docs/en/dbb/1.1.0)
* [IBM zDevOps Community](https://community.ibm.com/community/user/ibmz-and-linuxone/groups/topic-home?CommunityKey=f36c1ced-7e79-43cd-897c-e798acfef4a4)
* [IBM DBB Community](https://community.ibm.com/community/user/ibmz-and-linuxone/groups/topic-home/blog-entries?communitykey=20c9b889-9450-4ab6-8f11-8a5eb2b3342d)


## Contributing
For instructions on how to contribute new samples and bug fixes, please read the [Contributions Guidelines](CONTRIBUTIONS.md).

## Content
Sample | Description
--- | ---
[Build/BuildReport](Build/BuildReport) | Sample showing how to extend the BuildReport to provide additional functionality
[Build/HelloWorld](Build/HelloWorld) | The HelloWorld sample provides simple source file types and the Groovy build scripts to compile them
[Build/IDE](Build/IDE) | A collection of 3 samples from PLI, COBOL, and ASM created by the IDz/ZOD team with instructions to run them on IDz/ZOD.
[Build/MortgageApplication](Build/MortgageApplication) | Sample application demonstrating many of the  build functions provided by DBB.
Build/MortgageApplicationV2 | MortgageApplicationV2 has been moved to the new zAppBuild repository at https://github.com/IBM/dbb-zappbuild.
[Build/MultiThreadMVSJob](Build/MultiThreadMVSJob) | Sample showing how to modilfy a compilation script to run in a multi-thread environment.
Build/PublishLoadModules | Sample demonstrating how to publish load modules to Artifactory after a successful build. Removed and superseded by [Pipeline/PackageBuildOutputs](Pipeline/PackageBuildOutputs).
Build/zAppBuild | zAppBuild has been moved to its own stand alone repository at https://github.com/IBM/dbb-zappbuild. 
[IDE/GitISPFClient](IDE/GitISPFClient) | An ISPF interface that interacts with a Git repository to allow cloning, staging, checking in, pushing and pulling as well as other git commands.
[Migration/sclm](Migration/sclm) | This sample provides scripts to migrate source members to local Git repository and convert the build information in SCLM into build Groovy scripts.
[Pipeline/CreateUCDComponentVersion](Pipeline/CreateUCDComponentVersion) | Post-build script to parse the DBB Build report to generate a UCD component shiplist file and to create a new UCD component version. 
[Pipeline/DeployUCDComponentVersion](Pipeline/DeployUCDComponentVersion) | Sample script to trigger a UCD deployment from the pipeline, where the pipeline orchestrator does not provide standard plugins for this task.
[Pipeline/PackageBuildOutputs](Pipeline/PackageBuildOutputs) | Post-build script to create a generic package with the produced build outputs, optionally uploads results to an Artifactory repository. Artifactory deploy/download sample script.    
[Pipeline/PublishSharedInterfaces](Pipeline/PublishSharedInterfaces) | Post-build script to publish shared copybooks to a shared git repository managing all shared copybooks. 
[Pipeline/RunIDZCodeReview](Pipeline/RunIDZCodeReview) | Post-build script to integrate IBM IDz Code Review application into a pipeline.
[Snippets/InteractiveGateway](Snippets/InteractiveGateway) | Example showing how to use the new ISPFExec/TSOExec Interactive Gateway support added in DBB v1.0.2
[Snippets/PropertyMappings](Snippets/PropertyMappings) | Example showing how to use the new PropertyMappings class to perform aggregate functions on DBB BuildProperties.
[Snippets/zUnitTestCase](Snippets/zUnitTestCase) | Example showing how to use the JCLExec command added in DBB v1.0.1
[Utilities/BuildManager](Utilities/BuildManager) | Background process for queueing and managing build scripts without additional JVM start-up overhead.
[Utilities/BuildReportPruner](Utilities/BuildReportPruner) | Utility scripts demonstating rule based BuildReport pruning and preserving.
[Utilities/Jenkins](Utilities/Jenkins) | Utility shell scripts supplied to address issues when running Jenkins remote agents on z/OS UNIX System Services (USS).
[Utilities/ReadSMFRecords](Utilities/ReadSMFRecords) | Groovy scripts to read System Management Facilities (SMF) records using IBM's Dependency Based Build capabilities.
[Utilities/Validation](Utilities/Validation) | Groovy scripts that can aid in the validation of product installs.
[Utilities/WebAppCleanUp](Utilities/WebAppCleanUp) | Groovy script used to delete DBB web application collections and build groups that are no longer needed
