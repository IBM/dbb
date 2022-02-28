# Package Build Outputs in tar format

This sample shows how to create a tar-file with the build outputs based on the DBB Build Report after a successful build.

The package can be uploaded to an artifact repository and used in a scripted deployment. Another area, where this script is beneficial as a sample, is to adapt this script in publishing shared copybooks to an artifact repository and to pull them into the build process.
The `ArtifactoryHelpers.groovy` allow you to upload and download packages from Artifactory. 

The ArtifactoryHelpers is a very simple implementation sufficient for a show case, **_we recommend_** to use the Artifactory publishers which are available with your CI pipeline coordinator.

## Prerequisites
`PackageBuildOutputs.groovy` is a sample of an post-build script relying at least on the a DBB Build Report as an input.

## Package Build Outputs Process

### Packaging

1. After a successful DBB build, `PackageBuildOutputs.groovy` reads the build report and retrieves all outputs from the build report. It excludes outputs without a `deployType` as well as those labeled `ZUNIT-TESTCASE` 
2. It then invokes CopyToHFS API to copy the outputs from the libraries to a temporary directory on zFS. It will set the file tags based on the ZLANG setting (Note: A workaround is implemented to tag files as binary); all files require to be tagged. Please check the COPYMODE list, which maps last level qualifiers to the copymode of CopyToHFS.  
3. It packages these load files into a tar file, and adds the BuildReport.json and optionally other build logs from the build workspace.
4. (Optional) Publishes the tar file to the Artifactory repository based on the given configuration using the ArtifactoryHelpers.

## Invocation samples 

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

### Package only including *.log files from build workspace

```
groovyz dbb/Pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy --workDir /var/jenkins/workspace/MortgageApplication/BUILD-2 --packagingPropertiesFile dbb/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties --includeLogs *.log

** PackageBuildOutputs start at 20220222.112956.029
** Properties at startup:
   verbose -> false
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD"]
   startTime -> 20220222.112956.029
   publish -> false
   includeLogs -> *.log
   workDir -> /var/jenkins/workspace/MortgageApplication/BUILD-2
** Read build report data from /var/jenkins/workspace/MortgageApplication/BUILD-2/BuildReport.json
** Removing Output Records without deployType or with deployType=ZUNIT-TESTCASE
** Copying BuildOutputs to temporary package dir.
*** Number of build outputs to publish: 8
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMORT) to /var/jenkins/workspace/MortgageApplication/BUILD-2/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMLIS) to /var/jenkins/workspace/MortgageApplication/BUILD-2/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCSMRT) to /var/jenkins/workspace/MortgageApplication/BUILD-2/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMPMT) to /var/jenkins/workspace/MortgageApplication/BUILD-2/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCMORT) to /var/jenkins/workspace/MortgageApplication/BUILD-2/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCSMRD) to /var/jenkins/workspace/MortgageApplication/BUILD-2/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMLIST) to /var/jenkins/workspace/MortgageApplication/BUILD-2/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.DBRM(EPSCMORT) to /var/jenkins/workspace/MortgageApplication/BUILD-2/tempPackageDir/JENKINS.DBB.SAMP.BUILD.DBRM with DBB Copymode BINARY
** Creating tar file at /var/jenkins/workspace/MortgageApplication/BUILD-2/build.20220222.021034.010.tar.

** Adding BuildReport.json to /var/jenkins/workspace/MortgageApplication/BUILD-2/build.20220222.021034.010.tar.

** Adding *.log to /var/jenkins/workspace/MortgageApplication/BUILD-2/build.20220222.021034.010.tar.

** Package successfully created at /var/jenkins/workspace/MortgageApplication/BUILD-2/build.20220222.021034.010.tar.
** Build finished
```


### Package and Publish to Artifactory
```
groovyz /var/jenkins/pipeline/PublishLoadModule.groovy --workDir /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034 --artifactoryPropertiesFile publish.properties -v MortgageRelease_1.0 -t myPackage.tar --verbose --publish


** PackageBuildOutputs start at 20210727.042032.020
** Properties at startup:
   workDir -> /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034
   startTime -> 20210727.042032.020
   publish -> true
   versionName -> MortgageRelease_1.0
   verbose -> false
   artifactory.password -> xxxxx
   artifactory.user -> xxxxx
   artifactory.repo -> basicRepository
   tarFileName -> myPackage.tar
   artifactory.url -> http://10.3.20.231:8081/artifactory
** Read build report data from /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/BuildReport.json
** Removing Output Records without deployType or with deployType=ZUNIT-TESTCASE
** Copying BuildOutputs to temporary package dir.
*** Number of build outputs to publish: 10
     Copying JENKINS.DBB.SAMP.BUILD.BMS.COPY(EPSMORT) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.BMS.COPY with DBB Copymode TEXT
     Copying JENKINS.DBB.SAMP.BUILD.BMS.COPY(EPSMLIS) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.BMS.COPY with DBB Copymode TEXT
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMORT) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMLIS) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCSMRT) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMPMT) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCMORT) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSCSMRD) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.LOAD(EPSMLIST) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.LOAD with DBB Copymode LOAD
     Copying JENKINS.DBB.SAMP.BUILD.DBRM(EPSCMORT) to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/tempPackageDir/JENKINS.DBB.SAMP.BUILD.DBRM with DBB Copymode BINARY
** Creating tar file at /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/myPackage.tar.

** Adding BuildReport.json to /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/myPackage.tar.

** Package successfully created at /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034/myPackage.tar.
** Uploading package to Artifactory http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.0/myPackage.tar.
** Headers: [Expect: 100-continue, Connection: Keep-Alive]
** Request: PUT http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.0/myPackage.tar HTTP/1.1
** Response: HttpResponseProxy{HTTP/1.1 201 Created [Server: Artifactory/6.6.5, X-Artifactory-Id: 6e0b564c45b20ed4:-57a85152:1783ac71376:-8000, Location: http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.0/myPackage.tar, Content-Type: application/vnd.org.jfrog.artifactory.storage.ItemCreated+json;charset=ISO-8859-1, Transfer-Encoding: chunked, Date: Tue, 27 Jul 2021 15:20:34 GMT] ResponseEntityProxy{[Content-Type: application/vnd.org.jfrog.artifactory.storage.ItemCreated+json;charset=ISO-8859-1,Chunked: true]}}
** Build finished
```

### Only Upload or Download to/from Artifactory

```
groovyz  /var/jenkins/pipeline/ArtifactoryHelpers.groovy --url http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.1/build.20210727.073406.034.tar --user xxxxx --password xxxxx --fileToUpload /var/jenkins/workspace/MortgageApplication//build.20210727.073406.034.tar --verbose

** Headers: [Expect: 100-continue, Connection: Keep-Alive]
** Request: PUT http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.1/build.20210727.073406.034.tar HTTP/1.1
** Response: HttpResponseProxy{HTTP/1.1 201 Created [Server: Artifactory/6.6.5, X-Artifactory-Id: 6e0b564c45b20ed4:-57a85152:1783ac71376:-8000, Location: http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.1/build.20210727.073406.034.tar, Content-Type: application/vnd.org.jfrog.artifactory.storage.ItemCreated+json;charset=ISO-8859-1, Transfer-Encoding: chunked, Date: Tue, 27 Jul 2021 06:37:30 GMT] ResponseEntityProxy{[Content-Type: application/vnd.org.jfrog.artifactory.storage.ItemCreated+json;charset=ISO-8859-1,Chunked: true]}}
** Build finished
 
```

## Command Line Options Summary - PackageBuildOutputs

```
  usage: PackageBuildOutputs.groovy [options]
 
  -w,--workDir <dir>                             Absolute path to the DBB build
                                                 output directory
  -properties,--packagingPropertiesFile <file>   Absolute path of a property file
                                                 containing application specific
                                                 packaging details. 
                                                                                                                                          
  Optional:
  -t,--tarFileName <filename>                    Name of the package tar file.
                                                 (Optional)
  -d,--deployTypes <deployTypes>                 Comma-seperated list of deployTypes
                                                 to filter on the scope of the tar
                                                 file. (Optional)
  -verb,--verbose                                Flag to provide more log output.
                                                 (Optional)
  -il,--includeLogs                              Comma-separated list of files/patterns
                                                 from the USS build workspace                                               
                                        
  Optional Artifactory Upload opts:
 
  -p,--publish                                   Flag to indicate package upload to
                                                 the provided Artifactory server.
                                                 (Optional)
  -artifactory,
    --artifactoryPropertiesFile <propertyFile>   Absolute path of a property file
                                                 containing application specific
                                                 Artifactory details. (Optional)
  -v,--versionName <versionName>                 Name of the Artifactory version.
                                                 (Optional)
 
 
  -h,--help                                      Prints this message
```

## Command Line Options Summary - ArtifactoryHelpers

```
usage: ArtifactoryHelpers.groovy [options]

 -fD,--fileToDownload <arg>   The full path of the file to download
 -fU,--fileToUpload <arg>     The full path of the file to upload
 -h,--help                    Prints this message
 -P,--password <arg>          Artifactory password
 -u,--url <arg>               Artifactory file uri location
 -U,--user <arg>              Artifactory user id
 -v,--verbose                 Flag to turn on script trace
```


## Useful reference material

#### TAR on USS 

This sample implementation makes use of tar on USS. Please see IBM Docs for further details on [tar](https://www.ibm.com/docs/en/zos/2.4.0?topic=scd-tar-manipulate-tar-archive-files-copy-back-up-file)

The implementation preserves the file tags for further processing.

```
tar -tvf justloads.jar -L T
USTAR Version 00
                    drwxr-xr-x   1 BPXROOT  DB2USR         0 Jul 28 13:47 JENKINS.DBB.SAMP.BUILD.LOAD/
b binary      T=off -rwxr-xr-x   1 BPXROOT  DB2USR     32768 Jul 28 13:47 JENKINS.DBB.SAMP.BUILD.LOAD/EPSMPMT
t UTF-8       T=on  -rw-r--r--   1 BPXROOT  DB2USR     18326 Jul 28 13:47 BuildReport.json
```

#### Jenkins Integration  

As mentioned in the introductions of this sample, we recommend to use the existing plugins of your binary artifact repository manager. You find useful material at:
- [Documentation Jenkins Artifactory Plug-in](https://www.jfrog.com/confluence/display/JFROG/Jenkins+Artifactory+Plug-in)
- [Configuring Jenkins Artifactory Plug-in](https://www.jfrog.com/confluence/display/JFROG/Configuring+Jenkins+Artifactory+Plug-in)
- [Jenkins Sample provided by JFrog](https://github.com/jfrog/project-examples/tree/master/jenkins-examples/pipeline-examples)


Below is a snippet to use PackageBuildOutputs.groovy along with the Jenkins Artifactory plugins, which provides the Build Info in Artifactory

```
stage("Package & Upload to Artifactory") {
		sh "${groovyz}  $pipelineScripts/PackageBuildOutputs.groovy --workDir ${WORKSPACE}/BUILD-${BUILD_NUMBER}"	

        artifactoryServer.credentialsId = artifactoryCredentialsId
        def buildInfo = Artifactory.newBuildInfo()
        buildInfo.name = buildName // tbd - for example {application-branch}

        // Upload DBB build outputs to Artifactory
        artifactoryServer.upload buildInfo: buildInfo, spec:
        """{
                "files": [
                    {
                        "pattern": "${WORKSPACE}/BUILD-${BUILD_NUMBER}/*.tar",
                        "target": "${artifactoryRepository}/${buildName}/${env.BUILD_NUMBER}/"
                    }
                ]
            }"""

        // Publish the build
        artifactoryServer.publishBuildInfo buildInfo      
	}  
```

 