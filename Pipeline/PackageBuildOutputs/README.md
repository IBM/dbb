# Package Build Outputs in tar format

## Summary

This sample shows how to create a tar-file with the build outputs based on the DBB Build Report after a successful build.

The package can be uploaded to an artifact repository and used in a scripted deployment. Another area, where this script is beneficial as a sample, is to adapt this script in publishing shared copybooks to an artifact repository and to pull them into the build process. The `ArtifactoryHelpers.groovy` allow you to upload and download packages from Artifactory. 
The `ArtifactoryHelpers` script is a very simple implementation sufficient for a show case, **_we recommend_** to use the Artifactory publishers which are available with your CI pipeline coordinator.

This sample Groovy script to package build outputs:

- Extracts information about the build outputs from the Dependency Based Build (DBB) `BuildReport.json`. The script is able to take a single DBB build report or multiple build reports to build a cumulative package across multiple incremental builds. 
- Copies outputs to a temporary directory on Unix System Services and creates a tar file based on the temporary directory.
## Package Build Outputs Process - High-level Processing Flow

This section provides a more detailed explanation of how the PackageBuildOutputs script works and what it does.

1. **Initialization**
   1. Read [command line parameters](#command-line-options-summary---packagebuildoutputs).
   1. Read the properties file that is passed via `--packagingPropertiesFile`.

1. **Process the DBB build report(s)**
   1. If one or multiple DBB build reports are passed to the script via either `--buildReportOrder` or `--buildReportOrderFile`, the script loops through the provided DBB build reports. If no build report is specified, the script reads DBB's `BuildReport.json` file from the pipeline work directory specified by the `--workDir` parameter. For each build report, the following steps are performed:
      1. Parse and extract build output information for records of type *ExecuteRecord* and *CopyToPDSRecord*. (Requires at least DBB 1.0.8.)
      1. Remove output entries that have no `deployType` set and remove unwanted outputs such as outputs with the `deployType` equal to `ZUNIT-TESTCASE`.
   1. If processing multiple build reports, a cumulative hashmap of output records is created to be able to combine outputs from multiple pipeline builds into a single tar file.

1. **Create Tar-file**
    1. It then invokes CopyToHFS API to copy the outputs from the libraries to a temporary directory on zFS. It will set the file tags based on the ZLANG setting (Note: A workaround is implemented to tag files as binary); all files require to be tagged. Please check the COPYMODE list, which maps last level qualifiers to the copymode of CopyToHFS. When specifying the option `--addExtension`, the `deployType` will be appended as the file extension to the file.
    1. It packages these load files into a tar file, and adds the BuildReport.json and optionally other build logs from the build workspace.

1. **(Optional) Publish to Artifactory**
    1. Publishes the tar file to the Artifactory repository based on the given configuration using the ArtifactoryHelpers.

## Invocation samples 

### Package only
```
groovyz /var/pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy --workDir /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949
```

<details>
  <summary>Console log</summary>

PackageBuildOutputs console output

```
** PackageBuildOutputs start at 20220901.025517.055
** Properties at startup:
   verbose -> false
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD"]
   packagingPropertiesFile -> /var/pipeline/PackageBuildOutputs/packageBuildOutputs.properties
   startTime -> 20220901.025517.055
   publish -> false
   workDir -> /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949
   buildReportOrder -> [/u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/BuildReport.json]
   addExtension -> false
** Read build report data from /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/BuildReport.json
** Removing Output Records without deployType or with deployType=ZUNIT-TESTCASE
** Files detected in /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/BuildReport.json
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMORT), MAPLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMLIS), MAPLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCSMRT), CICSLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMPMT), LOAD
   GITLAB.ZAPP.CLEAN.MAIN.DBRM(EPSCMORT), DBRM
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCMORT), CICSLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCSMRD), CICSLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMLIST), CICSLOAD
*** Number of build outputs to package: 8
** Copying BuildOutputs to temporary package dir.
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCSMRT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSCSMRT with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCSMRD) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSCSMRD with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMORT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSMORT with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMPMT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSMPMT with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMLIST) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSMLIST with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMLIS) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSMLIS with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCMORT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSCMORT with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.DBRM(EPSCMORT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.DBRM/EPSCMORT with DBB Copymode BINARY
** Copying /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/BuildReport.json to temporary package dir.
** Copying /var/pipeline/PackageBuildOutputs/packageBuildOutputs.properties to temporary package dir.
** Creating tar file at /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/build.20220614.084654.046.tar.
** Package successfully created at /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/build.20220614.084654.046.tar.

```
</details>

### Package only including adding deployType to files in tar

```
+ groovyz /var/pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy --workDir /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949 --addExtension
```

<details>
  <summary>Console log</summary>

PackageBuildOutputs console output

```

** PackageBuildOutputs start at 20220901.025846.058
** Properties at startup:
   verbose -> false
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD"]
   packagingPropertiesFile -> /var/pipeline/PackageBuildOutputs/packageBuildOutputs.properties
   startTime -> 20220901.025846.058
   publish -> false
   workDir -> /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949
   buildReportOrder -> [/u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/BuildReport.json]
   addExtension -> true
** Read build report data from /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/BuildReport.json
** Removing Output Records without deployType or with deployType=ZUNIT-TESTCASE
** Files detected in /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/BuildReport.json
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMORT), MAPLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMLIS), MAPLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCSMRT), CICSLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMPMT), LOAD
   GITLAB.ZAPP.CLEAN.MAIN.DBRM(EPSCMORT), DBRM
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCMORT), CICSLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCSMRD), CICSLOAD
   GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMLIST), CICSLOAD
*** Number of build outputs to package: 8
** Copying BuildOutputs to temporary package dir.
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCSMRT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSCSMRT.CICSLOAD with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCSMRD) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSCSMRD.CICSLOAD with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMORT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSMORT.MAPLOAD with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMPMT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSMPMT.LOAD with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMLIST) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSMLIST.CICSLOAD with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSMLIS) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSMLIS.MAPLOAD with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.LOAD(EPSCMORT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.LOAD/EPSCMORT.CICSLOAD with DBB Copymode LOAD
     Copying GITLAB.ZAPP.CLEAN.MAIN.DBRM(EPSCMORT) to /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/tempPackageDir/GITLAB.ZAPP.CLEAN.MAIN.DBRM/EPSCMORT.DBRM with DBB Copymode BINARY
** Copying /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/BuildReport.json to temporary package dir.
** Copying /var/pipeline/PackageBuildOutputs/packageBuildOutputs.properties to temporary package dir.
** Creating tar file at /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/build.20220614.084654.046.tar.
** Package successfully created at /u/gitlab/gitlab-runner/zos/builds/dbb-zappbuild/BUILD-5949/build.20220614.084654.046.tar.

```
</details>


### Package only processing multiple build reports

```
+ groovyz /var/pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy --workDir /var/pipeline/work --buildReportOrder /var/pipeline/retirementCalculator/BuildReport_1.json,/var/pipeline/retirementCalculator/BuildReport_2.json --tarFileName rel-1.0.0.tar --packagingPropertiesFile /var/pipeline/PackageBuildOutputs/packageBuildOutputs.properties --verbose
```

<details>
  <summary>Console log</summary>

PackageBuildOutputs console output

```

** PackageBuildOutputs start at 20220901.030431.004
** Properties at startup:
   workDir -> /var/pipeline/work
   startTime -> 20220901.030431.004
   publish -> false
   verbose -> true
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "COBOL":"TEXT"]
   packagingPropertiesFile -> /var/pipeline/PackageBuildOutputs/packageBuildOutputs.properties
   buildReportOrder -> [/var/pipeline/retirementCalculator/BuildReport_1.json, /var/pipeline/retirementCalculator/BuildReport_2.json]
   addExtension -> false
   tarFileName -> rel-1.0.0.tar
** Read build report data from /var/pipeline/retirementCalculator/BuildReport_1.json
** Removing Output Records without deployType or with deployType=ZUNIT-TESTCASE
**  Files detected in /var/pipeline/retirementCalculator/BuildReport_1.json
   JENKINS.ZDAT.RETIRE.LOAD(EBUD01), LOAD
   JENKINS.ZDAT.RETIRE.LOAD(EBUD03), LOAD
   JENKINS.ZDAT.RETIRE.LOAD(EBUD0RUN), LOAD
   JENKINS.ZDAT.RETIRE.LOAD(EBUD02), LOAD
** Read build report data from /var/pipeline/retirementCalculator/BuildReport_2.json
** Removing Output Records without deployType or with deployType=ZUNIT-TESTCASE
**  Files detected in /var/pipeline/retirementCalculator/BuildReport_2.json
   JENKINS.ZDAT.RETIRE.COPY(LINPUT), COPY
   JENKINS.ZDAT.RETIRE.LOAD(EBUD02), CICSLOAD
   JENKINS.ZDAT.RETIRE.COBOL(EBUD02), COBOL
*** Number of build outputs to package: 6
** Copying BuildOutputs to temporary package dir.
     Copying JENKINS.ZDAT.RETIRE.LOAD(EBUD0RUN) to /var/pipeline/work/tempPackageDir/JENKINS.ZDAT.RETIRE.LOAD/EBUD0RUN with DBB Copymode LOAD
     Copying JENKINS.ZDAT.RETIRE.LOAD(EBUD02) to /var/pipeline/work/tempPackageDir/JENKINS.ZDAT.RETIRE.LOAD/EBUD02 with DBB Copymode LOAD
     Copying JENKINS.ZDAT.RETIRE.LOAD(EBUD03) to /var/pipeline/work/tempPackageDir/JENKINS.ZDAT.RETIRE.LOAD/EBUD03 with DBB Copymode LOAD
     Copying JENKINS.ZDAT.RETIRE.LOAD(EBUD01) to /var/pipeline/work/tempPackageDir/JENKINS.ZDAT.RETIRE.LOAD/EBUD01 with DBB Copymode LOAD
     Copying JENKINS.ZDAT.RETIRE.COBOL(EBUD02) to /var/pipeline/work/tempPackageDir/JENKINS.ZDAT.RETIRE.COBOL/EBUD02 with DBB Copymode TEXT
     Copying JENKINS.ZDAT.RETIRE.COPY(LINPUT) to /var/pipeline/work/tempPackageDir/JENKINS.ZDAT.RETIRE.COPY/LINPUT with DBB Copymode TEXT
** Copying /var/pipeline/retirementCalculator/BuildReport_1.json to temporary package dir as 001_BuildReport_1.json. Executing [sh, -c, cp /var/pipeline/retirementCalculator/BuildReport_1.json /var/pipeline/work/tempPackageDir/001_BuildReport_1.json]:
** Copying /var/pipeline/retirementCalculator/BuildReport_2.json to temporary package dir as 002_BuildReport_2.json. Executing [sh, -c, cp /var/pipeline/retirementCalculator/BuildReport_2.json /var/pipeline/work/tempPackageDir/002_BuildReport_2.json]:
** Copying /var/pipeline/PackageBuildOutputs/packageBuildOutputs.properties to temporary package dir.executing [sh, -c, cp /var/pipeline/PackageBuildOutputs/packageBuildOutputs.properties /var/pipeline/work/tempPackageDir]:
** Creating tar file at /var/pipeline/work/rel-1.0.0.tar.
executing [sh, -c, tar cUXf /var/pipeline/work/rel-1.0.0.tar *]:

** Package successfully created at /var/pipeline/work/rel-1.0.0.tar.
**   List package contents.
executing [sh, -c, tar tvf /var/pipeline/work/rel-1.0.0.tar]:
-rw-------   1 BPXROOT  TIVUSR     22489 Sep  1 16:04 001_BuildReport_1.json
-rw-------   1 BPXROOT  TIVUSR     18708 Sep  1 16:04 002_BuildReport_2.json
drwxr-xr-x   1 BPXROOT  TIVUSR         0 Sep  1 16:04 JENKINS.ZDAT.RETIRE.COBOL/
-rw-r--r--   1 BPXROOT  TIVUSR      2107 Sep  1 16:04 JENKINS.ZDAT.RETIRE.COBOL/EBUD02
drwxr-xr-x   1 BPXROOT  TIVUSR         0 Sep  1 16:04 JENKINS.ZDAT.RETIRE.COPY/
-rw-r--r--   1 BPXROOT  TIVUSR       333 Sep  1 16:04 JENKINS.ZDAT.RETIRE.COPY/LINPUT
drwxr-xr-x   1 BPXROOT  TIVUSR         0 Sep  1 16:04 JENKINS.ZDAT.RETIRE.LOAD/
-rwxr-xr-x   1 BPXROOT  TIVUSR     49152 Sep  1 16:04 JENKINS.ZDAT.RETIRE.LOAD/EBUD0RUN
-rwxr-xr-x   1 BPXROOT  TIVUSR     49152 Sep  1 16:04 JENKINS.ZDAT.RETIRE.LOAD/EBUD02
-rwxr-xr-x   1 BPXROOT  TIVUSR     86016 Sep  1 16:04 JENKINS.ZDAT.RETIRE.LOAD/EBUD03
-rwxr-xr-x   1 BPXROOT  TIVUSR     49152 Sep  1 16:04 JENKINS.ZDAT.RETIRE.LOAD/EBUD01
-rw-r--r--   1 BPXROOT  TIVUSR        38 Sep  1 16:04 buildReportOrder.txt
-rw-------   1 BPXROOT  TIVUSR      1015 Sep  1 16:04 packageBuildOutputs.properties

** Build finished

```
</details>


### Package and Publish to Artifactory
```
groovyz /var/jenkins/pipeline/PublishLoadModule.groovy --workDir /var/jenkins/workspace/MortgageApplication/build.20210727.073406.034 --artifactoryPropertiesFile publish.properties -v MortgageRelease_1.0 -t myPackage.tar --verbose --publish
```

<details>
  <summary>Console log</summary>

PackageBuildOutputs console output

```

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
</details>

### Only Upload or Download to/from Artifactory

```
groovyz  /var/jenkins/pipeline/ArtifactoryHelpers.groovy --url http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.1/build.20210727.073406.034.tar --user xxxxx --password xxxxx --fileToUpload /var/jenkins/workspace/MortgageApplication//build.20210727.073406.034.tar --verbose
```
<details>
  <summary>Console log</summary>

ArtifactoryHelpers console output

```
** Headers: [Expect: 100-continue, Connection: Keep-Alive]
** Request: PUT http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.1/build.20210727.073406.034.tar HTTP/1.1
** Response: HttpResponseProxy{HTTP/1.1 201 Created [Server: Artifactory/6.6.5, X-Artifactory-Id: 6e0b564c45b20ed4:-57a85152:1783ac71376:-8000, Location: http://10.3.20.231:8081/artifactory/basicRepository/MortgageRelease_1.1/build.20210727.073406.034.tar, Content-Type: application/vnd.org.jfrog.artifactory.storage.ItemCreated+json;charset=ISO-8859-1, Transfer-Encoding: chunked, Date: Tue, 27 Jul 2021 06:37:30 GMT] ResponseEntityProxy{[Content-Type: application/vnd.org.jfrog.artifactory.storage.ItemCreated+json;charset=ISO-8859-1,Chunked: true]}}
** Build finished
 
```
</details>

## Command Line Options Summary - PackageBuildOutputs

```
  usage: PackageBuildOutputs.groovy [options]
 
  -w,--workDir <dir>                             Absolute path to the DBB build
                                                 output directory
  -properties,--packagingPropertiesFile <file>   Absolute path of a property file
                                                 containing application specific
                                                 packaging details. 
                                                                                                                                          
  Optional:
  -boFile,--buildReportOrderFile <file>          Name of the buildReportOrder file, used to specify
                                                 buildReport.json files to be processed.
  -bO,--buildReportOrder <buildReports>          Additional build reports to be processed. If -boFile and -bO 
                                                 are used together, the build reports from -bO are 
                                                 appended to the build reports from -boFile.
  -t,--tarFileName <filename>                    Name of the package tar file.
                                                 (Optional unless using --buildReportOrder or --buildReportOrderFile)
  -d,--deployTypes <deployTypes>                 Comma-seperated list of deployTypes
                                                 to filter on the scope of the tar
                                                 file. (Optional)
  -verb,--verbose                                Flag to provide more log output. (Optional)
  -il,--includeLogs                              Comma-separated list of files/patterns
                                                 from the USS build workspace
  -ae,--addExtension                             Flag to add the deploy type extension to the member
                                                 in the package tar file. (Optional)                                                                                              
                                        
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

 
