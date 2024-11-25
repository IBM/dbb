# Package Build Outputs in TAR format

## Summary

This sample shows how to create a TAR file with the build outputs based on the DBB Build Report after a successful build.

The package can be uploaded to an artifact repository and used in a scripted deployment. Another area, where this script is beneficial as a sample, is to adapt this script in publishing shared copybooks to an artifact repository and to pull them into the build process. The `ArtifactRepositoryHelpers.groovy` allow you to upload and download packages from Artifactory. 
The `ArtifactRepositoryHelpers` script is a simple implementation to support download and upload from/to an Artifact Repository server.
However, it is recommend to consider using APIs or CLI utilities provided by Artifact Repositories distributions, which are available with your CI/CD pipeline coordinator.

This sample Groovy script can be used to package build outputs:

- It extracts information about the build outputs from the Dependency Based Build (DBB) `BuildReport.json`. The script is able to take a single DBB build report or multiple build reports to build a cumulative package across multiple incremental builds. 
  - It processes the MVSExec, CopyToPDS and the USS_Record types
- It optionally generates the [Wazi Deploy application manifest](https://www.ibm.com/docs/en/developer-for-zos/16.0?topic=files-application-manifest-file) file.
- It copies outputs to a temporary directory on Unix System Services and creates a TAR file based on the temporary directory.
- If the path to the application's Git repository is provided, the script will search for the Application Descriptor file and process it. Include Files classified as `public` will be automatically added to the package into the subdirectory `include/src`. The script will also process programs that are classified as `internal submodule` or `service submodule` and will copy the corresponding object decks to either the `lib` directory or the `include/bin` subfolder for the tar file.
- If a baseline package is provided through the corresponding CLI option, the baseline package is expanded in the temporary directory first:
  - All the subfolders are removed except the subfolder that contains interfaces definitions, by default located in the `include` subfolder.
  - More information can be found in the [Baseline Packages](#baseline-packages) section.

The support for zFS files in the packaging process is performed through the use of an `USS_RECORD` type record in the DBB BuildReport. 

## Package Build Outputs Process - High-level Processing Flow

This section provides a more detailed explanation of how the PackageBuildOutputs script works and what it does.

1. **Initialization**
   1. Read [command line parameters](#command-line-options-summary---packagebuildoutputs).
   2. Read the properties file that is passed via `--packagingPropertiesFile`.

2. **Process the DBB build report(s)**
   1. If one or multiple DBB build reports are passed to the script via either `--buildReportOrder` or `--buildReportOrderFile`, the script loops through the provided DBB build reports. If no build report is specified, the script reads DBB's `BuildReport.json` file from the pipeline work directory specified by the `--workDir` parameter. For each build report, the following steps are performed:
      1. Parse and extract build output information for records of type *ExecuteRecord*, *CopyToPDSRecord* and *USS_Record*.
      2. Remove output entries that have no `deployType` set and remove unwanted outputs such as outputs with the `deployType` equal to `ZUNIT-TESTCASE`.
   2. If processing multiple build reports, a cumulative hashmap of output records is created to be able to combine outputs from multiple pipeline builds into a single TAR file.
   	  1. The key of the map, used in the calculation of the artifacts to be deployed, is the combination of the member name and the deploy type.
   	  2. Artifacts having the same member name and the same deploy type will be present only once in the generated package, taking the last occurrence of the artifact, as found in the ordered list of Build Reports passed as parameters.

3. **(Optionally) Generate Software-Bill-Of-Material (SBOM) file**
   Based on the collected build outputs information, an SBOM file following the [CycloneDX](https://cyclonedx.org/) specification is created.  
   More details can be found [in this section](#software-bill-of-material-sbom-generation).

4. **(Optionally) Generate Wazi Deploy application manifest**
   Based on the collected build outputs information, the [Wazi Deploy application manifest](https://www.ibm.com/docs/en/developer-for-zos/17.0?topic=files-application-manifest-file) is generated and saved as wazideploy_manifest.yml.

5. **Create TAR file**
    1. It then invokes CopyToHFS API to copy the outputs from the libraries to a temporary directory on zFS. It will set the file tags based on the ZLANG setting (Note: A workaround is implemented to tag files as binary); all files require to be tagged. Please check the COPYMODE list, which maps last level qualifiers to the copymode of CopyToHFS. When specifying the option `--addExtension`, the `deployType` will be appended as the file extension to the file.
    2. It packages these load files into a TAR file, and adds the BuildReport.json and optionally other build logs from the build workspace.
    3. If the path to the application's Git repository is provided, it will perform additional publishing of object decks and Include Files classified as public and shared, into the `include` subfolder of the temporary directory.

6. **(Optional) Publish to Artifact Repository such as JFrog Artifactory or Sonartype Nexus**
    1. Publishes the TAR file to the artifact repository based on the given configuration using the ArtifactRepositoryHelpers script. Consider a Nexus RAW, or a Artifactory Generic as the repository type. **Please note**: The ArtifactRepositoryHelpers script is updated for DBB 2.0 and requires to run on JAVA 11. The publishing can be configured to pass in the artifact repository information as well as the path within the repository `directory/[versionName|buildLabel]/tarFileName` via the cli.

7. **(Optional) Generate IBM Concert Build manifest**
   1. Based on the collected build outputs information, the IBM Concert Build Manifest file is generated and saved as concert_build_manifest.yaml. This is a feeder file to publish build information into IBM Concert. It will only be generated if both sbom and packaging options are in effect. 

Notes: 
* The script doesn't manage the deletions of artifacts. Although they are reported in the DBB Build Reports, deletions are not handled by this script.

## Invocation samples 

### Package
```
groovyz /var/pipeline/dbb/Pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy --workDir /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator

```

<details>
  <summary>Console log</summary>

PackageBuildOutputs console output

```
** PackageBuildOutputs start at 20241028.145508.011
** Properties at startup:
   addExtension -> false
   buildReportOrder -> [/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json]
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "JCL": "TEXT", "EQALANGX" : "BINARY", "OBJ" : "BINARY"]
   fileEncoding -> UTF-8
   generateSBOM -> false
   generateWaziDeployAppManifest -> false
   packagingPropertiesFile -> /u/mdalbin/dbb-MD/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties
   publish -> false
   startTime -> 20241028.145508.011
   verbose -> false
   workDir -> /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator
** Read build report data from '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json'.
** Remove output records without deployType or with deployType=ZUNIT-TESTCASE
** Deployable artifacts detected in '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json':
        'EBUD03' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD02' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD0RUN' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD01' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
*** Number of build outputs to package: 4
** Copy build outputs to temporary package directory '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir'
        Copy 'DBEHM.MIG.LOAD(EBUD03)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD03' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD02)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD02' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD0RUN)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD0RUN' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD01)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD01' with DBB Copymode 'LOAD'
** Generate package build report order file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/buildReportOrder.txt'
** Copy packaging properties config file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/packageBuildOutputs.properties'
** Create tar file at /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/build.20241018.143823.127.tar
** Package '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/build.20241018.143823.127.tar' successfully created.
** Packaging completed successfully.
```
</details>

### Package with capturing the deployType attribute as file extension

Adding `--addExtension` is mandatory, when you plan to use Wazi Deploy as the deployment engine.

```
groovyz /var/pipeline/dbb/Pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy --workDir /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator --addExtension
```

<details>
  <summary>Console log</summary>

PackageBuildOutputs console output

```
** PackageBuildOutputs start at 20241028.145001.451
** Properties at startup:
   addExtension -> true
   buildReportOrder -> [/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json]
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "JCL": "TEXT", "EQALANGX" : "BINARY", "OBJ" : "BINARY"]
   fileEncoding -> UTF-8
   generateSBOM -> false
   generateWaziDeployAppManifest -> false
   packagingPropertiesFile -> /u/mdalbin/dbb-MD/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties
   publish -> false
   startTime -> 20241028.145001.451
   verbose -> false
   workDir -> /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator
** Read build report data from '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json'.
** Remove output records without deployType or with deployType=ZUNIT-TESTCASE
** Deployable artifacts detected in '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json':
        'EBUD03' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD02' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD0RUN' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD01' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
*** Number of build outputs to package: 4
** Copy build outputs to temporary package directory '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir'
        Copy 'DBEHM.MIG.LOAD(EBUD03)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD03.LOAD' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD02)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD02.LOAD' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD0RUN)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD0RUN.LOAD' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD01)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD01.LOAD' with DBB Copymode 'LOAD'
** Generate package build report order file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/buildReportOrder.txt'
** Copy packaging properties config file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/packageBuildOutputs.properties'
** Create tar file at /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/build.20241018.143823.127.tar
** Package '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/build.20241018.143823.127.tar' successfully created.
** Packaging completed successfully.
```

</details>

### Package to deploy with Wazi Deploy

When deploying with Wazi Deploy, and generating the Wazi Deploy Application Manifest file make the following options mandatory:

* `--generateWaziDeployAppManifest`
* `--addExtension`
* `--branch`
* `--versionName` (recommended)
* `--application` (recommended)

```
+ groovyz /var/pipeline/dbb/Pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy \
      --workDir /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator  \
      --tarFileName retirementCalculator.tar  \
      --application retirementCalculator  \
      --addExtension \
      --branch main \
      --generateWaziDeployAppManifest  \
      --includeLogs "*.log"  \
      --versionName rel-2.0.0 \
      --verbose
```

<details>
  <summary>Console log</summary>

PackageBuildOutputs console output

```
** PackageBuildOutputs start at 20241028.145841.890
** Properties at startup:
   addExtension -> true
   application -> retirementCalculator
   branch -> main
   buildReportOrder -> [/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json]
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "JCL": "TEXT", "EQALANGX" : "BINARY", "OBJ" :
"BINARY"]
   fileEncoding -> UTF-8
   generateSBOM -> false
   generateWaziDeployAppManifest -> true
   includeLogs -> *.log
   packagingPropertiesFile -> /u/mdalbin/dbb-MD/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties
   publish -> false
   startTime -> 20241028.145841.890
   tarFileName -> retirementCalculator.tar
   verbose -> true
   versionName -> rel-2.0.0
   workDir -> /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator
** Read build report data from '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json'.
** Remove output records without deployType or with deployType=ZUNIT-TESTCASE
** Deployable artifacts detected in '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json':
        'EBUD03' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD02' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD0RUN' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD01' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
*** Number of build outputs to package: 4
** Copy build outputs to temporary package directory '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir'
        Copy 'DBEHM.MIG.LOAD(EBUD03)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD
03.LOAD' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD02)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD
02.LOAD' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD0RUN)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EB
UD0RUN.LOAD' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD01)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD
01.LOAD' with DBB Copymode 'LOAD'
** Generate Wazi Deploy Application Manifest file to /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/wa
zideploy_manifest.yml
---
apiVersion: "wazideploy.ibm.com/v1"
kind: "ManifestState"
metadata:
  name: "retirementCalculator"
  description: "retirementCalculator"
  version: "rel-2.0.0"
  annotations:
    creationTimestamp: "20241028.145841.890"
    scmInfo:
      type: "git"
      uri: null
      branch: "main"
      shortCommit: "581f315a70a69454fcd880b6c3a4bb59f991414d"
    packageInfo: null
artifacts:
- name: "EBUD03"
  description: "RetirementCalculator/src/cobol/ebud03.cbl"
  properties:
  - key: "path"
    value: "bin/LOAD/EBUD03.LOAD"
  - key: "githash"
    value: "581f315a70a69454fcd880b6c3a4bb59f991414d"
  type: "LOAD"
  hash: "581f315a70a69454fcd880b6c3a4bb59f991414d"
- name: "EBUD02"
  description: "RetirementCalculator/src/cobol/ebud02.cbl"
  properties:
  - key: "path"
    value: "bin/LOAD/EBUD02.LOAD"
  - key: "githash"
    value: "581f315a70a69454fcd880b6c3a4bb59f991414d"
  type: "LOAD"
  hash: "581f315a70a69454fcd880b6c3a4bb59f991414d"
- name: "EBUD0RUN"
  description: "RetirementCalculator/src/cobol/ebud0run.cbl"
  properties:
  - key: "path"
    value: "bin/LOAD/EBUD0RUN.LOAD"
  - key: "githash"
    value: "581f315a70a69454fcd880b6c3a4bb59f991414d"
  type: "LOAD"
  hash: "581f315a70a69454fcd880b6c3a4bb59f991414d"
- name: "EBUD01"
  description: "RetirementCalculator/src/cobol/ebud01.cbl"
  properties:
  - key: "path"
    value: "bin/LOAD/EBUD01.LOAD"
  - key: "githash"
    value: "581f315a70a69454fcd880b6c3a4bb59f991414d"
  type: "LOAD"
  hash: "581f315a70a69454fcd880b6c3a4bb59f991414d"

** Generate package build report order file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/buildRe
portOrder.txt'
** Copy packaging properties config file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/packageBui
ldOutputs.properties'
** Create tar file at /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/retirementCalculator.tar
   Executing [sh, -c, tar cUXf /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/retirementCalculator.tar *]
** Package '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/retirementCalculator.tar' successfully created.
** Add files with file pattern '*.log' from '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator' to '/u/mdalbin/Migration
-Modeler-MDLB-work/logs/RetirementCalculator/retirementCalculator.tar'
   Executing [sh, -c, tar rUXf /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/retirementCalculator.tar *.log]
** List package contents.
   Executing [sh, -c, tar tvf /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/retirementCalculator.tar]
-rw-rw-rw-   1 BPXROOT  TIVUSR      9169 Oct 28 11:29 001_BuildReport.json
drwxr-xr-x   1 BPXROOT  TIVUSR         0 Oct 28 14:58 bin/
drwxr-xr-x   1 BPXROOT  TIVUSR         0 Oct 28 14:58 bin/LOAD/
-rwxr-xr-x   1 BPXROOT  TIVUSR     86016 Oct 28 14:58 bin/LOAD/EBUD03.LOAD
-rwxr-xr-x   1 BPXROOT  TIVUSR     49152 Oct 28 14:58 bin/LOAD/EBUD02.LOAD
-rwxr-xr-x   1 BPXROOT  TIVUSR     49152 Oct 28 14:58 bin/LOAD/EBUD0RUN.LOAD
-rwxr-xr-x   1 BPXROOT  TIVUSR     49152 Oct 28 14:58 bin/LOAD/EBUD01.LOAD
-rw-r--r--   1 BPXROOT  TIVUSR        22 Oct 28 14:58 buildReportOrder.txt
-rw-r--r--   1 BPXROOT  ZSECURE     2502 Oct 28 14:54 packageBuildOutputs.properties
-rw-r--r--   1 BPXROOT  TIVUSR      1509 Oct 28 14:58 wazideploy_manifest.yml
-rw-r--r--   1 BPXROOT  TIVUSR      2296 Oct 18 16:38 build-preview-RetirementCalculator.log
-rw-r--r--   1 BPXROOT  TIVUSR      4411 Oct 18 16:38 packaging-preview-RetirementCalculator.log

** Packaging completed successfully.
```
</details>

### Package using multiple build reports

```
+ groovyz /var/pipeline/dbb/Pipeline/PackageBuildOutputs/PackageBuildOutputs.groovy \
        --workDir /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator \
        --buildReportOrder /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/previous_BuildReport.json,/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/last_BuildReport.json \
        --tarFileName rel-1.2.0.tar \
        --verbose
```

<details>
  <summary>Console log</summary>

PackageBuildOutputs console output

```
** PackageBuildOutputs start at 20241028.150801.393
** Properties at startup:
   addExtension -> false
   buildReportOrder -> [/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/previous_BuildReport.json, /u/mdalbin/Migratio
n-Modeler-MDLB-work/logs/RetirementCalculator/last_BuildReport.json]
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "JCL": "TEXT", "EQALANGX" : "BINARY", "OBJ" :
"BINARY"]
   fileEncoding -> UTF-8
   generateSBOM -> false
   generateWaziDeployAppManifest -> false
   packagingPropertiesFile -> /u/mdalbin/dbb-MD/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties
   publish -> false
   startTime -> 20241028.150801.393
   tarFileName -> rel-1.2.0.tar
   verbose -> true
   workDir -> /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator
** Read build report data from '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/previous_BuildReport.json'.
** Remove output records without deployType or with deployType=ZUNIT-TESTCASE
** Deployable artifacts detected in '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/previous_BuildReport.json':
        'EBUD03' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD02' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD0RUN' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD01' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
** Read build report data from '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/last_BuildReport.json'.
** Remove output records without deployType or with deployType=ZUNIT-TESTCASE
** Deployable artifacts detected in '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/last_BuildReport.json':
        'EBUD03' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD02' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD0RUN' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD01' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
*** Number of build outputs to package: 4
** Copy build outputs to temporary package directory '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir'
        Copy 'DBEHM.MIG.LOAD(EBUD03)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD03' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD02)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD02' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD0RUN)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD0RUN' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD01)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD01' with DBB Copymode 'LOAD'
** Generate package build report order file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/buildReportOrder.txt'
** Copy packaging properties config file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/packageBuildOutputs.properties'
** Create tar file at /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/rel-1.2.0.tar
   Executing [sh, -c, tar cUXf /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/rel-1.2.0.tar *]
** Package '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/rel-1.2.0.tar' successfully created.
** List package contents.
   Executing [sh, -c, tar tvf /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/rel-1.2.0.tar]
-rw-------   1 BPXROOT  TIVUSR      9888 Oct 18 16:38 001_previous_BuildReport.json
-rw-rw-rw-   1 BPXROOT  TIVUSR      9169 Oct 28 11:29 002_last_BuildReport.json
drwxr-xr-x   1 BPXROOT  TIVUSR         0 Oct 28 15:08 bin/
drwxr-xr-x   1 BPXROOT  TIVUSR         0 Oct 28 15:08 bin/LOAD/
-rwxr-xr-x   1 BPXROOT  TIVUSR     86016 Oct 28 15:08 bin/LOAD/EBUD03
-rwxr-xr-x   1 BPXROOT  TIVUSR     49152 Oct 28 15:08 bin/LOAD/EBUD02
-rwxr-xr-x   1 BPXROOT  TIVUSR     49152 Oct 28 15:08 bin/LOAD/EBUD0RUN
-rwxr-xr-x   1 BPXROOT  TIVUSR     49152 Oct 28 15:08 bin/LOAD/EBUD01
-rw-r--r--   1 BPXROOT  TIVUSR        58 Oct 28 15:08 buildReportOrder.txt
-rw-r--r--   1 BPXROOT  ZSECURE     2502 Oct 28 14:54 packageBuildOutputs.properties

** Packaging completed successfully.
```
</details>


### Package and Publish to Artifactory / Nexus

Overview of the various ways to specify the structure within the repository: 

* When no version or directory name is provided, the package will be uploaded into `(buildLabel)/(tarFileName)`
* Specifying `version (-v)` uploads the package into  `(version)/(tarFileName)`
* Specifying `directory (-ad)` uploads the package into `(directory)/(buildLabel)/(tarFileName)`
* Specifying `version (-v)` and `directory (-ad)` uploads the package into `(directory)/(versionName)/(tarFileName)`

The password for the artifact repository can also represent the APIKey. It is recommended to store that inside the secret store of your pipeline orchestrator.

```
groovyz /var/pipeline/dbb/Pipeline/PackageBuildOutputs.groovy \
        --workDir /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator \
		-p --artifactRepositoryUrl http://10.3.20.231:8081/artifactory \
		--artifactRepositoryUser admin \
		--artifactRepositoryPassword xxxxxxxx \
		--artifactRepositoryName RetirementCalculator
```

<details>
  <summary>Console log</summary>

PackageBuildOutputs console output

```
** PackageBuildOutputs start at 20241028.151247.498
** Properties at startup:
   addExtension -> false
   artifactRepository.password -> xxxxxx
   artifactRepository.repo -> RetirementCalculator
   artifactRepository.url -> http://10.3.20.231:8081/artifactory
   artifactRepository.user -> admin
   buildReportOrder -> [/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json]
   copyModeMap -> ["COPYBOOK": "TEXT", "COPY": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "JCL": "TEXT", "EQALANGX" : "BINARY", "OBJ" :
"BINARY"]
   fileEncoding -> UTF-8
   generateSBOM -> false
   generateWaziDeployAppManifest -> false
   packagingPropertiesFile -> /u/mdalbin/dbb-MD/Pipeline/PackageBuildOutputs/packageBuildOutputs.properties
   publish -> true
   startTime -> 20241028.151247.498
   verbose -> false
   workDir -> /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator
** Read build report data from '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json'.
** Remove output records without deployType or with deployType=ZUNIT-TESTCASE
** Deployable artifacts detected in '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/BuildReport.json':
        'EBUD03' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD02' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD0RUN' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
        'EBUD01' from 'DBEHM.MIG.LOAD' with Deploy Type 'LOAD'
*** Number of build outputs to package: 4
** Copy build outputs to temporary package directory '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir'
        Copy 'DBEHM.MIG.LOAD(EBUD03)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD03' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD02)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD02' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD0RUN)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD0RUN' with DBB Copymode 'LOAD'
        Copy 'DBEHM.MIG.LOAD(EBUD01)' to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/bin/LOAD/EBUD01' with DBB Copymode 'LOAD'
** Generate package build report order file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/buildReportOrder.txt'
** Copy packaging properties config file to '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/tempPackageDir/packageBuildOutputs.properties'
** Create tar file at /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/build.20241018.143823.127.tar
** Package '/u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/build.20241018.143823.127.tar' successfully created.
** Upload package to Artifact Repository 'http://10.3.20.231:8081/artifactory/RetirementCalculator/build.20241018.143823.127/build.20241018.143823.127.tar'.
** ArtifactRepositoryHelper started for upload of /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/build.20241018.143823.127.tar to http://10.3.20.231:8081/artifactory/RetirementCalculator/build.20241018.143823.127/build.20241018.143823.127.tar
** Uploading /u/mdalbin/Migration-Modeler-MDLB-work/logs/RetirementCalculator/build.20241018.143823.127.tar to http://10.3.20.231:8081/artifactory/RetirementCalculator/build.20241018.143823.127/build.20241018.143823.127.tar...
** Upload completed.
** Packaging completed successfully.
```
</details>

## Configuration through properties files

Limiting the cli options can be a desired strategy to maintain files under version control or implement central control mechanisms to enforce standards. 

The PackageBuildOutputs script can be configured using the [packageBuildOutputs.properties](packageBuildOutputs.properties) file.
In any case, the `packageBuildOutputs.properties` is loaded, because it at least specifies the `copyModeMap`. 

The properties file allows you to control the following settings:


Parameter | Description
---------- | ----------------------------------------------------------------------------------------
`copyModeMap` | configures the mapping of last level qualifier and the necessary copymode from PDS to USS.
`deployTypesFilter` | to limit the scope of DBB deployTypes that are added to the package
`includeLogs` | List of file patterns from the workDir, that should be addded to the package, such as build logs
`addExtension` | Boolean flag to append the DBB deployType as the file extension to provide information about the deployment 
`generateWaziDeployAppManifest` |  Boolean flag to indicate if the Wazi Deploy Application Manifest file should be created
`generateSBOM` |  Boolean flag to indicate if a CycloneDX SBOM file should be created
`publish` |  Boolean flag to indicate if the created TAR file should be uploaded to an Artifact Repository
`fileEncoding` | File encoding for files generated as part of the process 

Additionally, the ArtifactRepositoryHelpers accept a properties like [appArtifactRepository.properties](appArtifactRepository.properties) file to define:

Parameter | Description
---------- | ----------------------------------------------------------------------------------------
`artifactRepository.url` | URL to the Artifact server, e.q. https://your-artifactory-url/artifactory
`artifactRepository.repo` | Artifact repository name to store the build, e.q. sys-zos-application-local
`artifactRepository.directory` | Artifact repository directory to distinguish between prelimiary versions and release candidates, e.q. rel-1.0.0
`artifactRepository.user` | User name
`artifactRepository.password` | Password, Personal Access Token
`artifactRepository.httpClientVersion` | HttpClient.Version setting to override the HTTP protocol version (Optional)


## Command Line Options Summary - PackageBuildOutputs

```
  usage: PackageBuildOutputs.groovy [options]
 usage: PackageBuildOutputs.groovy [options]
 -a,--application <application>
      The name of the application
 -ad,--artifactRepositoryDirectory <repoDirectory>
      Directory path in the repository to store the build . (Optional)
 -ae,--addExtension
      Flag to add the deploy type extension to the member in the package tar file. (Optional)
 -af,--applicationFolderPath <applicationFolderPath>
      Path to the Application's Git repository folder
 -ah,--artifactRepositoryHttpClientProtocolVersion
 <httpClientProtocolVersion>
      HttpClient.Version setting to override the HTTP protocol version. (Optional)
 -aP,--artifactRepositoryPassword <password>
      Password to connect to the Artifact repository server. (Optional)
 -aprop,--artifactRepositoryPropertyFile <propertyFile>
      Path of a property file containing application specific artifact  repository details. (Optional) ** (Deprecated)
 -ar,--artifactRepositoryName <repoName>
      Artifact repository name to store the build. (Optional)
 -au,--artifactRepositoryUrl <url>
      URL to the Artifact repository server. (Optional)
 -aU,--artifactRepositoryUser <user>
      User to connect to the Artifact repository server. (Optional)
 -b,--branch <branch>
      The git branch processed by the pipeline
 -bO,--buildReportOrder <buildReportOrder>
      List of build reports in order of processing
 -boFile,--buildReportOrderFile <buildReportOrderFile>
      A file that lists build reports in order of processing
 -bp,--baselinePackage <baselinePackageFilePath>
      Path to a baseline Package. (Optional)
 -d,--deployTypes <deployTypes>
      Comma-seperated list of deployTypes to filter on the scope of the tar file. (Optional)
 -h,--help
      Prints this message
 -il,--includeLogs <includeLogs>
      Comma-separated list of files/patterns from the USS build workspace.  (Optional)
 -o,--owner <owner>
      Owner of the packaged artifacts
 -p,--publish
      Flag to indicate package upload to the provided Artifact Repository server. (Optional)
 -properties,--packagingPropertiesFile <packagingPropertiesFile>
      Path of a property file containing application specific packaging details.
 -s,--sbom
      Flag to control the generation of SBOM
 -sa,--sbomAuthor <sbomAuthor>
      Author of the SBOM, in form "Name <email>"
 -t,--tarFileName <filename>
      Name of the package tar file. (Optional unless using --buildReportOrder or --buildReportOrderFile)
 -v,--versionName <versionName>
      Name of the version/package on the Artifact repository server. (Optional)
 -verb,--verbose
      Flag to provide more log output. (Optional)
 -w,--workDir <dir>
      Absolute path to the DBB build output directory
 -wd,--generateWaziDeployAppManifest
      Flag indicating to generate and add the Wazi Deploy Application Manifest file.
```

## Command Line Options Summary - ArtifactRepositoryHelpers

```
usage: ArtifactRepositoryHelpers.groovy [options]

 -fD,--fileToDownload <arg>   The full path of the file to download
 -fU,--fileToUpload <arg>     The full path of the file to upload
 -h,--help                    Prints this message
 -P,--password <arg>          Artifactory password
 -u,--url <arg>               Artifactory file uri location
 -U,--user <arg>              Artifactory user id
 -v,--verbose                 Flag to turn on script trace
 -ht,--httpClientVersion      Http Client Protocol Version (Optional)
```


## Software-Bill-Of-Material (SBOM) generation

This `PackageBuildOutputs.groovy` script is able to generate an SBOM file based on the information contained in the DBB Build Report.
It will process the different records of the DBB Build Report, to collect each deployable artifact's required properties and dependencies when documenting a valid SBOM file.

This sample script is using the [CycloneDX](https://cyclonedx.org/) specification to document the necessary elements of the SBOM file.
The output file is written in JSON, following the [Cyclone DX 1.5](https://cyclonedx.org/docs/1.5/json/) schema.

To implement the correct objects when generating an SBOM file, the script uses the [CycloneDX Java library](https://github.com/CycloneDX/cyclonedx-core-java).
This library makes use of other libraries like [Jackson](https://github.com/FasterXML/jackson), which also comes with dependencies.
The list of required libraries are:

- cyclonedx-core-java (8.x version)
- jackson-core (tested with 2.16.1 version)
- jackson-annotations (tested with 2.16.1 version)
- jackson-databind (tested with 2.16.1 version)
- jackson-dataformat-xml (tested with 2.16.1 version)
- json-schema-validator (tested with 1.2.0 version)
- packageurl-java (tested with 1.5.0 version)         

These libraries (available as JAR files) must be made available on z/OS Unix System Services.
The easiest way it to download the JAR packages manually (or through maven) and upload them to a specific location on z/OS, where the script can use them.

Also, these libraries must be available in the Java CLASSPATH.
A convenient way is to specify the paths to these libraries through the `-cp` flag when invoking DBB.

To enable the generation of the SBOM file, the `-s/--sbom` flag must be passed.
It is recommended to specify an author for the SBOM, even when generated through the pipeline, through the`-sa/--sbomAuthor` parameter.
For instance, it could be the Release Manager or the Application Owner of the application for which the pipeline is running. 

As an example, you can invoke the SBOM generation with the following command:

~~~~
/usr/lpp/dbb/v2r0/bin/groovyz -cp /u/mdalbin/SBOM/cyclonedx-core-java-8.0.3.jar:/u/mdalbin/SBOM/jackson-annotations-2.16.1.jar:/u/mdalbin/SBOM/jackson-core-2.16.1.jar:/u/mdalbin/SBOM/jackson-databind-2.16.1.jar:/u/mdalbin/SBOM/jackson-dataformat-xml-2.16.1.jar:/u/mdalbin/SBOM/json-schema-validator-1.2.0.jar:/u/mdalbin/SBOM/packageurl-java-1.5.0.jar /u/mdalbin/SBOM/dbb/Pipeline/PackageBuildOutputs/PackageBuildOutputsWithSBOM.groovy --workDir /u/ado/workspace/MortgageApplication/feature/consumeRetirementCalculatorServiceImpacts/build-20240312.1/logs --tarFileName MortgageApplication.tar --addExtension -s -sa "David Gilmour <david.gilmour@pinkfloyd.com>"
~~~~ 

By default, the SBOM file is generated in the `tempPackageDir` and named `<buildnumber>_sbom.json`.
This way, it is automatically packaged in the TAR file that is created by the script, ensuring the package and its content are not tampered and correctly documented. 

## IBM Concert Build manifest generation

This `PackageBuildOutputs.groovy` script is able to generate an IBM Concert Build manifest based on the information contained in the DBB Build Report and the published package information. The output is a YAML file that adheres to IBM Concert Build specification YAML format. The generation of the CycloneDX SBOM is a pre-requisite as the IBM Concert Build manifest will reference the CycloneDX SBOM file for detailed information about the build outputs.

To enable the generation of the IBM Concert Build manifest, the `-ic/--generateConcertBuildManifest` flag must be passed.

As an example, you can invoke the generation of an IBM Concert Build manifest with the following command:

~~~~
/usr/lpp/dbb/v2r0/bin/groovyz -cp /u/mdalbin/SBOM/cyclonedx-core-java-8.0.3.jar:/u/mdalbin/SBOM/jackson-annotations-2.16.1.jar:/u/mdalbin/SBOM/jackson-core-2.16.1.jar:/u/mdalbin/SBOM/jackson-databind-2.16.1.jar:/u/mdalbin/SBOM/jackson-dataformat-xml-2.16.1.jar:/u/mdalbin/SBOM/json-schema-validator-1.2.0.jar:/u/mdalbin/SBOM/packageurl-java-1.5.0.jar /u/mdalbin/SBOM/dbb/Pipeline/PackageBuildOutputs/PackageBuildOutputsWithSBOM.groovy --workDir /u/ado/workspace/MortgageApplication/feature/consumeRetirementCalculatorServiceImpacts/build-20240312.1/logs --tarFileName MortgageApplication.tar --addExtension -s -sa "David Gilmour <david.gilmour@pinkfloyd.com>" -ic
~~~~ 

By default, the IBM Concert Build manifest file is generated in the `tempPackageDir` and named `concert_build_manifest.yaml`. 

## Publishing interfaces

The CLI option `applicationFolderPath` is used to point to the application's Git repository on z/OS Unix System Services. When specified, this parameter enables the PackageBuildOutputs with additional capabilities, like the packaging of object decks and Include Files based on their usage.

When this CLI option is provided, the script will search for an [Application Descriptor file](https://github.com/IBM/dbb-git-migration-modeler/?tab=readme-ov-file#output-files), named `applicationDescriptor.yml` and located at the root level of the application's Git repository. This file contains information on the usage of each artifact of the application: programs can be "internal submodules" or "service submodule" if statically linked by other programs, include files can be public or shared, if referenced by programs from other applications.

If an Application Descriptor file is found, the required information is leveraged, to include in the created archive:
- the object decks that were created during the previous build process, when they are issued from a program that is identified as an "internal submodule" or a "service submodule".
- the public or shared includes files present in the application's Git repository.

These artifacts are placed in the `include` subfolder of the archive, within specific subfolders to segregate artifacts based on their nature:
- the `include/bin` subfolder contains the object decks that other programs can statically link.
- the `include/src` subfolder contains the public and shared include files, that other programs can reference.

When used in conjunction with [baseline packages](#baseline-packages), it is possible to have an archive that contains all the interfaces of an application, combining previous versions of object decks (when they have not changed) with the newest material that was just built or changed in the application's Git repository.

## Baseline Packages

The CLI option `baselinePackage` can be used to specify a path to an existing package on z/OS Unix System Services. This package will then be used as a baseline, on which new artifacts documented in the provided build report(s) will be copied, potentially replacing the content of the baseline package.

During the packaging process, the baseline package is expanded. The `include` subfolder remains intact and contains artifacts that represent interfaces of the application, that other applications can consume or reference (typically, public/shared Include Files like COBOL copybooks, and object decks that are statically linked by "consuming" loadmodules). All other subfolders are removed.

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

 
