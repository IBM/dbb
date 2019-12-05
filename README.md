# DBB Community Repository
Welcome to the IBM Dependency Based Build (DBB) community repository. The helpful and handy location for finding and sharing example DBB scripts and snippets.

## Resources
* [IBM Dependency Based Build Product Page](https://developer.ibm.com/mainframe/products/ibm-dependency-based-build/)
* [IBM DBB Community](https://www.ibm.com/developerworks/community/groups/service/html/communitystart?communityUuid=eb5571db-e187-47c1-bd64-d5da2bd73e73)
* [IBM DBB Knowledge Center](https://www.ibm.com/support/knowledgecenter/SS6T76_1.0.2/welcome.html)

## Contributing
For instructions on how to contribute new samples and bug fixes, please read the [Contributions Guidelines](CONTRIBUTIONS.md).

## Content
Sample | Description
--- | ---
[Automation/idManagement](Automation/idManagement) | Create and delete ids on a z/OS system.
[Automation/Merge](Automation/Merge) | A set of three samples to demonstrate how to use the Z Open Automation Utilities (ZOAU) to merge two datasets using shell commands, Java APIs and Python APIs.
[Build/BuildReport](Build/BuildReport) | Sample showing how to extend the BuildReport to provide additional functionality
[Build/HelloWorld](Build/HelloWorld) | The HelloWorld sample provides simple source file types and the Groovy build scripts to compile them
[Build/IDE](Build/IDE) | A collection of 3 samples from PLI, COBOL, and ASM created by the IDz/ZOD team with instructions to run them on IDz/ZOD.
[Build/MortgageApplication](Build/MortgageApplication) | Sample application demonstrating many of the  build functions provided by DBB.
[Build/MortgageApplicationV2](Build/MortgageApplicationV2) | Contains two updated versions of the MortgageApplication sample (single repository and shared across multiple repositories) designed to be built by [zAppBuild](Build/zAppBuild)
[Build/MultiThreadMVSJob](Build/MultiThreadMVSJob) | Sample showing how to modilfy a compilation script to run in a multi-thread environment.
[Build/PublishLoadModules](Build/PublishLoadModules) | Sample demonstrating how to publish load modules to Artifactory after a successful build.
[Build/zAppBuild](Build/zAppBuild) | zAppBuild is a generic build solution for building z/OS applications using Apache Groovy build scripts and IBM Dependency Based Build (DBB) APIs. 
[IDE/GitISPFClient](IDE/GitISPFClient) | An ISPF interface that interacts with a Git repository to allow cloning, staging, checking in, pushing and pulling as well as other git commands.
[Migration/sclm](Migration/sclm) | This sample provides scripts to migrate source members to local Git repository and convert the build information in SCLM into build Groovy scripts.
[Snippets/InteractiveGateway](Snippets/InteractiveGateway) | Example showing how to use the new ISPFExec/TSOExec Interactive Gateway support added in DBB v1.0.2
[Snippets/PropertyMappings](Snippets/PropertyMappings) | Example showing how to use the new PropertyMappings class to perform aggregate functions on DBB BuildProperties.
[Snippets/zUnitTestCase](Snippets/zUnitTestCase) | Example showing how to use the JCLExec command added in DBB v1.0.1
[Utilities/BuildManager](Utilities/BuildManager) | Background process for queueing and managing build scripts without additional JVM start-up overhead.
[Utilities/BuildReportPruner](Utilities/BuildReportPruner) | Utility scripts demonstating rule based BuildReport pruning and preserving.
[Utilities/Jenkins](Utilities/Jenkins) | Utility shell scripts supplied to address issues when running Jenkins remote agents on z/OS UNIX System Services (USS).
[Utilities/ReadSMFRecords](Utilities/ReadSMFRecords) | Groovy scripts to read System Management Facilities (SMF) records using IBM's Dependency Based Build capabilities.
[Utilities/Validation](Utilities/Validation) | Groovy scripts that can aid in the validation of product installs.