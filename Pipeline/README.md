# Pipeline Integration Samples
This category contains examples of using DBB APIs to integrate additional parts of the pipeline.

## Table of Contents 
Sample | Description | Documentation Link
--- | --- | ---
AnalyzeCodeCoverageReport | This sample extracts and prints Code Coverage information as collected by IBM Debug. | [AnalyzeCodeCoverageReport/README.md](AnalyzeCodeCoverageReport/README.md)
CreateUCDComponentVersion | This sample reads the DBB Build Report to generate a UCD Shiplist and to run the BUZTOOL.sh command to create a new UCD Component version. Both Codestation and the external Artifact Repository is supported. | [CreateUCDComponentVersion/README.md](CreateUCDComponentVersion/README.md)
DeployUCDComponentVersion | This sample can be use to deploy an application component version into a specific environment. | [DeployUCDComponentVersion/README.md](DeployUCDComponentVersion/README.md)
PackageBuildOutputs | This sample creates a TAR file with the build outputs referenced in a DBB Build Report. Additionally, it contains a sample to Upload/Download to Artifactory. | [PackageBuildOutputs/README.md](PackageBuildOutputs/README.md)
PublishSharedInterfaces | This sample script implements a publishing mechanism of interfaces to a common git repository for shared interfaces. | [PublishSharedInterfaces/README.md](PublishSharedInterfaces/README.md)
RunIDZCodeReview | This sample reads the DBB Build Report, assembles an JCL to run IDZ Code Review on Z/OS in Batch. | [RunIDZCodeReview/README.md](RunIDZCodeReview/README.md)

