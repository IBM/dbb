# IBM UCD Packaging Based on IBM DBB Build Report

## Summary

An important step in the pipeline is to generate a deployable package. This sample Groovy script:

- Extracts information about the build outputs from the Dependency Based Build (DBB) `BuildReport.json`. The script is able to take a single DBB build report or multiple build reports to build a cumulative package across multiple incremental builds. 
- Generates the UrbanCode Deploy (UCD) shiplist `shiplist.xml` file.
- Invokes the `buztool.sh` with the appropriate configuration to store the binary package either in UCD packaging format v1 or v2 in the artifact repository and to register a new UCD component version. To use UCD packaging format v2, pass the CLI option `--ucdV2PackageFormat`.

## High-level Processing Flow

This section provides a more detailed explanation of how the CreateUCDComponentVersion script works and what it does.

1. **Initialization**
   1. Read [command line parameters](#command-line-options-summary).
   1. Read any application and global properties files that are passed as a comma-separated list via `--packagingPropFiles`.

1. **Process the DBB build report(s)**
   1. If one or multiple DBB build reports are passed to the script via either `--buildReportOrder` or `--buildReportOrderFile`, the script loops through the provided DBB build reports. If no build report is specified, the script reads DBB's `BuildReport.json` file from the pipeline work directory specified by the `--workDir` parameter. For each build report, the following steps are performed:
      1. Parse and extract build output information for records of type *ExecuteRecord* and *CopyToPDSRecord*. (Requires at least DBB 1.0.8.)
      1. Parse and extract the build output information for deleted build outputs of type *Delete_Record* written to the build report by [zAppBuild (release 2.4.0 onwards)](https://github.com/IBM/dbb-zappbuild/releases/tag/2.4.0). (Requires at least DBB 1.1.3, as the script uses the AnyTypeRecord API introduced in this version.)
      1. Remove output entries that have no `deployType` set and remove unwanted outputs such as outputs with the `deployType` equal to `ZUNIT-TESTCASE`.
   1. If processing multiple build reports, a cumulative list of output records is created to be able to combine outputs from multiple pipeline builds into one UCD component version.

1. **Generate the UCD `shiplist.xml` file and invoke the UCD packaging step**
   1. Generate the UCD's shiplist records (known as "container records" in UCD documentation) related to build outputs in partitioned datasets. (For more details, see the [IBM Docs UCD Shiplist](https://www.ibm.com/docs/en/urbancode-deploy/7.2.2?topic=SS4GSP_7.2.2/com.ibm.udeploy.doc/topics/zos_shiplistfiles.html)
   1. Write `shiplist.xml` to the build directory:
      1. (Optional) Add links to the UCD component version for the relevant continuous integration (CI) pipeline build and Git pull request.
      1. (Optional) Add UCD artifact-level properties for bind properties such as `bind_collectionID`, `bind_packageOwner`, `bind_qualifier` captured in the zAppBuild framework through generic PropertyRecords for members with deployType `DBRM` - see [generateDb2BindInfoRecord configuration in zAppBuild](https://github.com/IBM/dbb-zappbuild/blob/06ff114ee22b4e41a09aa0640ac75b7e56c70521/build-conf/build.properties#L79-L89).  
      1. (Optional) Add UCD artifact-level properties to trace changes back to the version control system (via Git hashes).
      1. (Optional) Add information about the input source files (and optionally links to the version control system) from the DBB Dependency Sets.
   1. Invoke `buztool.sh` on USS with the generated shiplist file and passed command line interface (CLI) options.

## Invocation Samples

Example invocation (default):
```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion
```

Example to build a cumulative package across multiple build reports via `buildReportOrder`:

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --buildReportOrder /u/ibmuser/sample_buildreports/BuildReport_1.json,/u/ibmuser/sample_buildreports/BuildReport_2.json,/u/ibmuser/sample_buildreports/BuildReport_3.json --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion
```

Example to build a cumulative package across multiple build reports via `buildReportOrderFile`. The file contains the references to the locations of the build reports:

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --buildReportOrderFile /u/ibmuser/sample_buildreports/BuildReportOrderFile.txt --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion
```

- Contents of `/u/ibmuser/sample_buildreports/BuildReportOrderFile.txt`:

  ```
  /u/ibmuser/sample_buildreports/BuildReport_1.json
  /u/ibmuser/sample_buildreports/BuildReport_2.json
  /u/ibmuser/sample_buildreports/BuildReport_3.json 
  ```

Example to leverage [UCD packaging format v2](https://www.ibm.com/docs/en/urbancode-deploy/7.2.1?topic=czcv-creating-zos-component-version-using-v2-package-format):

- Note: This requires setting the `deployType` attribute on the UCD shiplist container level, which are defined via the `containerMapping` property passed via `--packagingPropFiles`. The property maps the last level qualifiers to the `deployType`. A sample is provided at [applicationRepositoryProps.properties](applicationRepositoryProps.properties). Additionally, the Buztool properties file requires the mapping of `deployType` to `copyType` to configure how files are copied from the PDS to the temporary directory on USS for the packaging process of the v2 format.

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --ucdV2PackageFormat
```

Example to establish link to the pipeline URL:

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/
```

Example to establish link to the pipeline URL and links for each deployable artifact to the Git provider (see sample `applicationRepositoryProps.properties`):

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --packagingPropFiles /var/dbb/extensions/ucd-packaging/mortgageRepositoryProps.properties 
```

Example to establish links to the pipeline URL, the Git branch, and the pull request in the UCD component version:

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --pullRequestURL https://github.com/IBM/dbb/pull/102 --gitBranch development
```

## Command Line Options Summary

```
$DBB_HOME/bin/groovyz <ussLocation>/dbb-ucd-packaging.groovy [options]

required options:

 -b,--buztool <file>                            Absolute path to UrbanCode Deploy
                                                buztool.sh script

 -c,--component <name>                          Name of the UCD component to
                                                create version in

 -w,--workDir <dir>                             Absolute path to the DBB build
                                                output directory

optional cli options :

 -bo,--buildReportOrder <arg>                   Build a cumulative package based on a comma separated list of
                                                one or multiple DBB build reports processed in the provided order (Optional).

 -boFile,--buildReportOrderFile <arg>           Build a cumulative package based on an input file that lists
                                                one or multiple build reports defining the order of processing (Optional).

 buztool parameters : 

 -prop,--propertyFile <arg>                     Absolute path to UCD buztool property file. 
                                                From UCD v7.1.x and greater it replaces the -ar option.

 -ar,--artifactRepository <arg>                 Absolute path to Artifact Respository Server
                                                Server connection file (** Deprecated, 
                                                Please use --propertyFile instead **)

 -v,--versionName <arg>                         Name of the UCD component version
 
 packaging script parameters : 

 -ppf,--packagingPropFiles <arg>                Comma separated list of property files to configure
                                                the dbb-ucd-packaging script

 -rpFile,--repositoryInfoPropertiesFile <arg>   Absolute path to the property file containing
                                                URL prefixes to git provider (** Deprecated,
                                                please use cli option --packagingPropFiles **)

 -pURL,--pipelineURL <arg>                      URL to the pipeline build result

 -prURL,--pullRequestURL <arg>                  URL to the Pull Request

 -g,--gitBranch <arg>                           Name of the git branch


 -p,--preview                                   Preview mode generate shiplist, but do
                                                not run buztool.sh


utility options :

 -help,--help                                   Prints this message
 ```

## Sample Console Log for Processing a Single Build Report

A sample invocation that stores the application package in an external artifact repository in UCD packaging format v2, including all traceability links:

```
/var/jenkins/workspace/zunit-retirementCalculator/dbb/Pipeline/CreateUCDComponentVersion/dbb-ucd-packaging.groovy --buztool /var/ucd-agent/bin/buztool.sh --workDir /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035 --component retirementCalculatorGithub --prop /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties --versionName 135_20220531.113630.036 --packagingPropFiles /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalulcatur.packaging.properties --ucdV2PackageFormat --pipelineURL http://jenkins-server/job/zunit-retirementCalculator/135/ --gitBranch main
```

<details>
  <summary>Console log</summary>


**dbb-ucd-packaging script output**

```
** Create version start at 20220531.013637.036
** Properties at startup:
   buztoolPropertyFile -> /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties
   workDir -> /var/jenkins/workspace/zunit-retirementCalculator/BUILD-120/build.20220531.013558.035
   startTime -> 20220531.013637.036
   versionName -> 120_20220531.113630.036
   git_commitURL_prefix -> https://github.ibm.com/zDevOps/retirementCalculator/commit
   git_treeURL_prefix -> https://github.ibm.com/zDevOps/retirementCalculator/tree
   ucdV2PackageFormat -> true
   preview -> true
   gitBranch -> main
   containerMapping -> ["LOAD": "LOAD", "LOADLIB": "LOAD", "COPY" : "TEXT", "DBRM" : "DBRM", "JCL" : "TEXT"]
   buildReportOrder -> [/var/jenkins/workspace/zunit-retirementCalculator/BUILD-120/build.20220531.013558.035/BuildReport.json]
   pipelineURL -> http://jenkins-server/job/zunit-retirementCalculator/120/
   buztoolPath -> /var/ucd-agent/bin/buztool.sh
   component -> retirementCalculatorGithub
* Buildrecord type TYPE_COPY_TO_PDS is supported with DBB toolkit 1.0.8 and higher. Extracting build records for TYPE_COPY_TO_PDS might not be available and skipped. Identified DBB Toolkit version 1.1.3.
**  Reading provided build report(s).
*** Parsing DBB build report /var/jenkins/workspace/zunit-retirementCalculator/BUILD-120/build.20220531.013558.035/BuildReport.json.
**  Deployable files detected in /var/jenkins/workspace/zunit-retirementCalculator/BUILD-120/build.20220531.013558.035/BuildReport.json
   JENKINS.ZDAT.RETIRE.LOAD(EBUD01), LOAD
** Generate UCD ship list file
   Creating general UCD component version properties.
   Storing DBB Build result properties as general component version properties due to single build report.
   Creating shiplist record for build output JENKINS.ZDAT.RETIRE.LOAD(EBUD01) with recordType EXECUTE.
** Write ship list file to  /var/jenkins/workspace/zunit-retirementCalculator/BUILD-120/build.20220531.013558.035/shiplist.xml
** Following UCD buztool cmd will be invoked
/var/ucd-agent/bin/buztool.sh createzosversion2 -c retirementCalculatorGithub -s /var/jenkins/workspace/zunit-retirementCalculator/BUILD-120/build.20220531.013558.035/shiplist.xml -o /var/jenkins/workspace/zunit-retirementCalculator/BUILD-120/build.20220531.013558.035/buztool.output -prop /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties -v "120_20220531.113630.036" 

```
</details>

## Sample Console Log for Processing Multiple Build Reports to Assemble a Cumulative Package

A sample invocation that builds a cumulative package across multiple build reports leveraging the `--buildReportOrder` CLI option:

```
groovyz /var/jenkins/workspace/zunit-retirementCalculator/dbb/Pipeline/CreateUCDComponentVersion/dbb-ucd-packaging.groovy --buztool /var/ucd-agent/bin/buztool.sh --workDir /var/jenkins/workspace/zunit-retirementCalculator/BUILD-139/build.20220602.100747.007 --buildReportOrder /var/jenkins/tmp/BuildReport-134.json,/var/jenkins/tmp/BuildReport-135.json,/var/jenkins/tmp/BuildReport-137.json,/var/jenkins/tmp/BuildReport-138.json,/var/jenkins/tmp/BuildReport-139.json --component retirementCalculatorGithub --prop /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties --versionName 139_20220602.080800.008 --packagingPropFiles /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalulcatur.packaging.properties --ucdV2PackageFormat --pipelineURL http://jenkins-server/job/zunit-retirementCalculator/139/ --gitBranch main 
```

<details>
  <summary>Console log and sample shiplist file</summary>


**dbb-ucd-packaging script output**

```

** Create version start at 20220602.100807.008
** Properties at startup:
   buztoolPropertyFile -> /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties
   workDir -> /var/jenkins/workspace/zunit-retirementCalculator/BUILD-139/build.20220602.100747.007
   startTime -> 20220602.100807.008
   versionName -> 139_20220602.080800.008
   git_commitURL_prefix -> https://github.ibm.com/zDevOps/retirementCalculator/commit
   git_treeURL_prefix -> https://github.ibm.com/zDevOps/retirementCalculator/tree
   ucdV2PackageFormat -> true
   preview -> false
   gitBranch -> main
   containerMapping -> ["LOAD": "LOAD", "LOADLIB": "LOAD", "COPY" : "TEXT", "DBRM" : "DBRM", "JCL" : "TEXT"]
   buildReportOrder -> [/var/jenkins/tmp/BuildReport-134.json, /var/jenkins/tmp/BuildReport-135.json, /var/jenkins/tmp/BuildReport-137.json, /var/jenkins/tmp/BuildReport-138.json, /var/jenkins/tmp/BuildReport-139.json]
   pipelineURL -> http://jenkins-server/job/zunit-retirementCalculator/139/
   buztoolPath -> /var/ucd-agent/bin/buztool.sh
   component -> retirementCalculatorGithub
* Buildrecord type TYPE_COPY_TO_PDS is supported with DBB toolkit 1.0.8 and higher. Extracting build records for TYPE_COPY_TO_PDS might not be available and skipped. Identified DBB Toolkit version 1.1.3.
**  Reading provided build report(s).
*** Parsing DBB build report /var/jenkins/tmp/BuildReport-134.json.
**  Deployable files detected in /var/jenkins/tmp/BuildReport-134.json
   JENKINS.ZDAT.RETIRE.LOAD(EBUD0RUN), LOAD
*** Parsing DBB build report /var/jenkins/tmp/BuildReport-135.json.
**  Deployable files detected in /var/jenkins/tmp/BuildReport-135.json
   JENKINS.ZDAT.RETIRE.LOAD(EBUD01), LOAD
*** Parsing DBB build report /var/jenkins/tmp/BuildReport-137.json.
**  Deployable files detected in /var/jenkins/tmp/BuildReport-137.json
   JENKINS.ZDAT.RETIRE.LOAD(EBUD0RUN), LOAD
*** Parsing DBB build report /var/jenkins/tmp/BuildReport-138.json.
**  No items to package in /var/jenkins/tmp/BuildReport-138.json.
*** Parsing DBB build report /var/jenkins/tmp/BuildReport-139.json.
**  Deployable files detected in /var/jenkins/tmp/BuildReport-139.json
   JENKINS.ZDAT.RETIRE.LOAD(EBUD03), LOAD
** Generate UCD ship list file
   Creating general UCD component version properties.
   Creating shiplist record for build output JENKINS.ZDAT.RETIRE.LOAD(EBUD0RUN) with recordType EXECUTE.
   Creating shiplist record for build output JENKINS.ZDAT.RETIRE.LOAD(EBUD03) with recordType EXECUTE.
   Creating shiplist record for build output JENKINS.ZDAT.RETIRE.LOAD(EBUD01) with recordType EXECUTE.
** Write ship list file to  /var/jenkins/workspace/zunit-retirementCalculator/BUILD-139/build.20220602.100747.007/shiplist.xml
** Following UCD buztool cmd will be invoked
/var/ucd-agent/bin/buztool.sh createzosversion2 -c retirementCalculatorGithub -s /var/jenkins/workspace/zunit-retirementCalculator/BUILD-139/build.20220602.100747.007/shiplist.xml -o /var/jenkins/workspace/zunit-retirementCalculator/BUILD-139/build.20220602.100747.007/buztool.output -prop /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties -v "139_20220602.080800.008" 
** Create version by running UCD buztool
```
**generated shiplist file**

```
<?xml version="1.0" encoding="CP037"?>
<manifest type='MANIFEST_SHIPLIST'>
  <property name='ci-pipelineUrl' value='http://jenkins-server/job/zunit-retirementCalculator/139/' />
  <property name='ci-gitBranch' value='main' />
  <container name='JENKINS.ZDAT.RETIRE.LOAD' type='PDS' deployType='LOAD'>
    <resource name='EBUD0RUN' type='PDSMember' deployType='LOAD'>
      <property name='dbb-buildResultUrl' label='build.20220602.054657.046' value='https://10.3.20.96:10443/dbb/rest/buildResult/93067' />
      <property name='impactBuild' value='true' />
      <property name='filesProcessed' value='1' />
      <property name=':githash:retirementCalculator' value='f7ee158c9f790006778d650abfff415aa1f83149' />
      <property name=':giturl:retirementCalculator' value='git@github.ibm.com:zDevOps/retirementCalculator.git' />
      <property name='buildcommand' value='IEWBLINK' />
      <property name='buildoptions' value='RENT,REUS=RENT,LIST,XREF,AMODE=31,RMODE=ANY' />
      <property name='githash' value='f7ee158c9f790006778d650abfff415aa1f83149' />
      <property name='git-link-to-commit' value='https://github.ibm.com/zDevOps/retirementCalculator/commit/f7ee158c9f790006778d650abfff415aa1f83149' />
      <inputs url='https://github.ibm.com/zDevOps/retirementCalculator/tree/f7ee158c9f790006778d650abfff415aa1f83149/retirementCalculator/cobol/EBUD0RUN.cbl'>
        <input name='retirementCalculator/cobol/EBUD0RUN.cbl' compileType='Main' url='https://github.ibm.com/zDevOps/retirementCalculator/tree/f7ee158c9f790006778d650abfff415aa1f83149/retirementCalculator/cobol/EBUD0RUN.cbl' />
        <input name='retirementCalculator/copy/LINPUT.cpy' compileType='COPY' url='https://github.ibm.com/zDevOps/retirementCalculator/tree/f7ee158c9f790006778d650abfff415aa1f83149/retirementCalculator/copy/LINPUT.cpy' />
      </inputs>
    </resource>
  </container>
  <container name='JENKINS.ZDAT.RETIRE.LOAD' type='PDS' deployType='LOAD'>
    <resource name='EBUD03' type='PDSMember' deployType='LOAD'>
      <property name='dbb-buildResultUrl' label='build.20220602.100747.007' value='https://10.3.20.96:10443/dbb/rest/buildResult/93087' />
      <property name='impactBuild' value='true' />
      <property name='filesProcessed' value='1' />
      <property name=':githash:retirementCalculator' value='db183e92ca33433a558bac1d7a84bf2f5857ab65' />
      <property name=':giturl:retirementCalculator' value='git@github.ibm.com:zDevOps/retirementCalculator.git' />
      <property name='buildcommand' value='IEWBLINK' />
      <property name='buildoptions' value='RENT,REUS=RENT,LIST,XREF,AMODE=31,RMODE=ANY' />
      <property name='githash' value='db183e92ca33433a558bac1d7a84bf2f5857ab65' />
      <property name='git-link-to-commit' value='https://github.ibm.com/zDevOps/retirementCalculator/commit/db183e92ca33433a558bac1d7a84bf2f5857ab65' />
      <inputs url='https://github.ibm.com/zDevOps/retirementCalculator/tree/db183e92ca33433a558bac1d7a84bf2f5857ab65/retirementCalculator/cobol/EBUD03.cbl'>
        <input name='retirementCalculator/cobol/EBUD03.cbl' compileType='Main' url='https://github.ibm.com/zDevOps/retirementCalculator/tree/db183e92ca33433a558bac1d7a84bf2f5857ab65/retirementCalculator/cobol/EBUD03.cbl' />
      </inputs>
    </resource>
  </container>
  <container name='JENKINS.ZDAT.RETIRE.LOAD' type='PDS' deployType='LOAD'>
    <resource name='EBUD01' type='PDSMember' deployType='LOAD'>
      <property name='dbb-buildResultUrl' label='build.20220531.013558.035' value='https://10.3.20.96:10443/dbb/rest/buildResult/93008' />
      <property name='impactBuild' value='true' />
      <property name='filesProcessed' value='2' />
      <property name=':githash:retirementCalculator' value='11b9be5c88f3888d69b4b1a9c883734196c24958' />
      <property name=':giturl:retirementCalculator' value='git@github.ibm.com:zDevOps/retirementCalculator.git' />
      <property name='buildcommand' value='IEWBLINK' />
      <property name='buildoptions' value='RENT,REUS=RENT,LIST,XREF,AMODE=31,RMODE=ANY' />
      <property name='githash' value='11b9be5c88f3888d69b4b1a9c883734196c24958' />
      <property name='git-link-to-commit' value='https://github.ibm.com/zDevOps/retirementCalculator/commit/11b9be5c88f3888d69b4b1a9c883734196c24958' />
      <inputs url='https://github.ibm.com/zDevOps/retirementCalculator/tree/11b9be5c88f3888d69b4b1a9c883734196c24958/retirementCalculator/cobol/EBUD01.cbl'>
        <input name='retirementCalculator/cobol/EBUD01.cbl' compileType='Main' url='https://github.ibm.com/zDevOps/retirementCalculator/tree/11b9be5c88f3888d69b4b1a9c883734196c24958/retirementCalculator/cobol/EBUD01.cbl' />
      </inputs>
    </resource>
  </container>
</manifest>
```


</details>
 