# IBM UCD Packaging Based on IBM DBB Build Report

## Summary

An important step in the pipeline is to generate a deployable package. This sample Groovy script:

- Extracts information about the build outputs from the Dependency Based Build (DBB) `BuildReport.json`. The script is able to take a single DBB build report or multiple build reports to build a cumulative package across multiple incremental builds. 
- Generates the UrbanCode Deploy (UCD) shiplist `shiplist.xml` file.
- Invokes the `buztool.sh` with the appropriate configuration to store the binary package in the artifact repository and to register a new UCD component version.

## High-level Processing Flow

This section provides a more detailed explanation of how the CreateUCDComponentVersion script works and what it does.

1. **Initialization**
   1. Read command line parameters.
   1. Read any application and global properties files that are passed as a comma-separated list via `--packagingPropFiles`.
       (This parameter is optional for UCD package format v1, but required when using UCD package format v2 - see `--ucdV2PackageFormat`.)

1. **Process the DBB build report(s)**
   1. Either read DBB's `BuildReport.json` from the pipeline work directory, or loop through the list of provided DBB build reports (using the `--buildReportOrder` or `--buildReportOrderFile` option).
   1. Parse and extract build output information for records of type *ExecuteRecord* and *CopyToPDSRecord*. (Requires at least DBB 1.0.8.)
   1. Parse and extract the build output information for deleted build outputs of type *Delete_Record* written to the build report by zAppBuild. (Requires at least DBB 1.1.3, as the script uses the AnyTypeRecord API introduced in this version.)

1. **Generate the UCD `shiplist.xml` file and invoke the UCD packaging step**
   1. Generate the container records for the UCD shiplist for build outputs in partitioned datasets. (For more details, see the [IBM Docs UCD Shiplist](https://www.ibm.com/docs/en/urbancode-deploy/7.2.2?topic=SS4GSP_7.2.2/com.ibm.udeploy.doc/topics/zos_shiplistfiles.html) page.)
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

- Note: This requires setting the deployTypes attribute on the UCD shiplist container level, which are defined via the `containerMapping` property passed via `--packagingPropFiles` . The property maps the last level qualifiers to the deployType. A sample is provided at [applicationRepositoryProps.properties](applicationRepositoryProps.properties). Additionally, the Buztool properties file requires the mapping of deployTypes to copyTypes to configure how files are copied from the PDS to the temporary directory on USS for the packaging process of the v2 format.

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
groovyz /var/jenkins/workspace/zunit-retirementCalculator/dbb/Pipeline/CreateUCDComponentVersion/dbb-ucd-packaging.groovy --buztool /var/ucd-agent/bin/buztool.sh --workDir /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035 --buildReportOrder /var/jenkins/tmp/BuildReport-134.json,/var/jenkins/tmp/BuildReport-135.json --component retirementCalculatorGithub --prop /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties --versionName 135_20220531.113630.036 --packagingPropFiles /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalulcatur.packaging.properties --ucdV2PackageFormat --pipelineURL http://jenkins-server/job/zunit-retirementCalculator/135/ --gitBranch main
```

<details>
  <summary>Console log</summary>


**dbb-ucd-packaging script output**

```
** Create version start at 20220531.013644.036
** Properties at startup:
   buztoolPropertyFile -> /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties
   workDir -> /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035
   startTime -> 20220531.013644.036
   versionName -> 135_20220531.113630.036
   git_commitURL_prefix -> https://github.ibm.com/zDevOps/retirementCalculator/commit
   git_treeURL_prefix -> https://github.ibm.com/zDevOps/retirementCalculator/tree
   ucdV2PackageFormat -> true
   preview -> false
   gitBranch -> main
   containerMapping -> ["LOAD": "LOAD", "LOADLIB": "LOAD", "COPY" : "TEXT", "DBRM" : "DBRM", "JCL" : "TEXT"]
   buildReportOrder -> [/var/jenkins/tmp/BuildReport-134.json, /var/jenkins/tmp/BuildReport-135.json]
   pipelineURL -> http://jenkins-server/job/zunit-retirementCalculator/135/
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
** Generate UCD ship list file
   Creating general UCD component version properties.
   Creating shiplist record for build output JENKINS.ZDAT.RETIRE.LOAD(EBUD0RUN) with recordType EXECUTE.
   Creating shiplist record for build output JENKINS.ZDAT.RETIRE.LOAD(EBUD01) with recordType EXECUTE.
** Write ship list file to  /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035/shiplist.xml
** Following UCD buztool cmd will be invoked
/var/ucd-agent/bin/buztool.sh createzosversion2 -c retirementCalculatorGithub -s /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035/shiplist.xml -o /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035/buztool.output -prop /var/jenkins/workspace/zunit-retirementCalculator/retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties -v "135_20220531.113630.036" 
```
</details>
 