# Package Build Outputs in an independent tar format
This sample shows how to create a tar-file with the build outputs based on the DBB Build Report after a successful build, which can be published to an artifact repository and used in a scripted deployment.
Another area, where this script is beneficial as a sample, is to adapt this script in publishing shared copybooks to an artifact repository. 

The ArtifactoryHelpers allow you to upload and download packages from Artifactory. The ArtifactoryHelpers are a very simple implementation sufficient for a show case, we recommend to rather use the Artifactory Publishers which are available by your CI pipeline coordinator.

## Prerequisites
'PackageBuildOutputs.groovy' is a sample of an post-build sample and relies on the a DBB Build Report as an input .

## Package Build Outputs

### Packaging

1. After a successful DBB build, 'PackageBuildOutputs.groovy' reads the build report and retrieves all outputs from the build report. It excludes outputs without a `deployType` as well as those labeled `ZUNIT-TESTCASE` 
2. It then invokes CopyToHFS to copy the outputs from the libraries to a temporary directory on zFS. Please check the COPYMODE list, which maps last level qualifiers to the copymode of CopyToHFS  
3. It packages these load files into a tar file, and adds the BuildReport.json to it.

### (Optional) Uploading package
4. Publishes the tar file to the Artifactory repository based on the given configuration.

## Sample Invocation

### Package only
```
groovyz /var/jenkins/pipeline/PackageBuildOutputs.groovy --workDir /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005


** PackageBuildOutputs start at 20210726.051010.010
** Properties at startup:
   verbose -> false
   startTime -> 20210726.051010.010
   publish -> false
   workDir -> /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005
** Read build report data from /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/BuildReport.json
** Copying BuildOutputs to temporary package dir.
*** Number of build outputs to publish: 10
     Copying JENKINS.DBB.SAMP.BUILD.BMS.COPY(EPSMORT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.BMS.COPY with DBB Copymode TEXT
     Copying JENKINS.DBB.SAMP.BUILD.BMS.COPY(EPSMLIS) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.BMS.COPY with DBB Copymode TEXT
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMORT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMLIS) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCSMRT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMPMT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCMORT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCSMRD) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMLIST) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.DBRM(EPSCMORT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.DBRM with DBB Copymode BINARY
** Creating tar file at /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/build.20210726.050505.005.tar.
** Adding BuildReport.json to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/build.20210726.050505.005.tar.
** Package successfully created at /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/build.20210726.050505.005.tar.
** Build finished
```

### Package and Publish to Artifactory
```
groovyz /var/jenkins/pipeline/PackageBuildOutputs.groovy --workDir /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005 --propertyFile /var/jenkins/pipeline/publish.properties -v MortgageRelease_1.0 --verbose --publish


** PackageBuildOutputs start at 20210726.050608.006
** Properties at startup:
   workDir -> /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005
   startTime -> 20210726.050608.006
   publish -> true
   versionName -> MortgageRelease_1.0
   verbose -> true
   artifactory.password -> xxxxx
   artifactory.user -> xxxxx
   artifactory.repo -> basicRepository
   artifactory.url -> http://10.3.20.231:8081/artifactory
** Read build report data from /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/BuildReport.json
** Copying BuildOutputs to temporary package dir.
*** Number of build outputs to publish: 10
     Copying JENKINS.DBB.SAMP.BUILD.BMS.COPY(EPSMORT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.BMS.COPY with DBB Copymode TEXT
     Copying JENKINS.DBB.SAMP.BUILD.BMS.COPY(EPSMLIS) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.BMS.COPY with DBB Copymode TEXT
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMORT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMLIS) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCSMRT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMPMT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCMORT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCSMRD) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMLIST) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.DBRM(EPSCMORT) to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/tempPackageDir/JENKINS.DBB.SAMP.BUILD.DBRM with DBB Copymode BINARY
** Creating tar file at /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/build.20210726.050505.005.tar.
** Adding BuildReport.json to /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/build.20210726.050505.005.tar.
** Package successfully created at /var/jenkins/workspace/MortgageApplication/build.20210726.050505.005/build.20210726.050505.005.tar.
** Uploading package to Artifactory http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.0/build.20210726.050505.005.tar.
** Headers: [Expect: 100-continue, Connection: Keep-Alive]
** Request: PUT http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.0/build.20210726.050505.005.tar HTTP/1.1
** Response: HttpResponseProxy{HTTP/1.1 201 Created [Server: Artifactory/6.6.5, X-Artifactory-Id: 6e0b564c45b20ed4:-57a85152:1783ac71376:-8000, Location: http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.0/build.20210726.050505.005.tar, Content-Type: application/vnd.org.jfrog.artifactory.storage.ItemCreated+json;charset=ISO-8859-1, Transfer-Encoding: chunked, Date: Mon, 26 Jul 2021 16:06:10 GMT] ResponseEntityProxy{[Content-Type: application/vnd.org.jfrog.artifactory.storage.ItemCreated+json;charset=ISO-8859-1,Chunked: true]}}
** Build finished
```



## Command Line Options Summary

```
usage: PackageBuildOutputs.groovy [options]

 -w,--workDir <dir>                    Absolute path to the DBB build
                                       output directory
Optional: 
                                      
 -p,--publish                          Flag to indicate package upload to
                                       the provided Artifactory server.
                                       (Optional)
 -prop,--propertyFile <propertyFile>   Absolute path of a property file
                                       containing application specific
                                       Artifactory details. (Optional)
 -v,--versionName <versionName>        Name of the package tar file
                                       (Optional)
 -verb,--verbose                       Flag to provide more log output.
                                       (Optional)

 -h,--help                             Prints this message
```