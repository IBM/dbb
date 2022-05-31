# IBM UCD Packaging based on IBM DBB Build Report

## Summary

An important step in the pipeline is to generate a deployable package. This sample groovy script
- extracts information about the build outputs from the Dependency Based Build ```BuildReport.json```. The script is able to take a single DBB Build report or multiple Build reports to build a cumulative package across multiple incremental builds. 
- generates the UCD shiplist ```shiplist.xml``` file
- invokes the ```buztool.sh``` with the approriate configuration to store the binary package in the artifact repository and to register a new UCD component version.

## High-level Processing flow
**Initialization**
- Read command line parameters. 
- Read application and global properties which are supposed to be passed via `--packagingPropFiles` (Optionally)

**Process the DBB Build report(s)**
- Either reads DBB's BuildReport.json from the pipeline work directory or loops through the list of provided DBB Build reports (```--buildReportOrder``` or ```--buildReportOrderFile```).
- Parse and extract build output information of records of type *ExecuteRecord* and *CopyToPDSRecord* (requires at least DBB 1.0.8)
- Parse and extract the build output information for deleted build outputs of type *Delete_Record* written to the BuildReport by zAppBuild leveraging the AnyTypeRecord API which got introced with IBM Dependency Based Build 1.1.3. (requires at least DBB 1.1.3)

**Generates the UCD shiplist.xml file and invokes UCD packaging step**
- Write the shiplist.xml to the build directory
    - Optionally, adds links back to the ci pipeline build, the git pull request to UCD component version.
    - Adds UCD artifact level properties for bind information captured in the zAppBuild framework through generic PropertyRecords for DBRM members, such as bind_collectionID,bind_packageOwner,bind_qualifier on the element level - see [generateDb2BindInfoRecord configuration in zAppBuild](https://github.com/IBM/dbb-zappbuild/blob/06ff114ee22b4e41a09aa0640ac75b7e56c70521/build-conf/build.properties#L79-L89) (Optional).  
    - Adds UCD artifact level properties to trace changes back to the version control system (via git hashes) (Optional).
    - Adds source input information (and optionally links to the version control system) about the input files from the DBB Dependency Sets.
- Invokes buztool.sh on USS with the generated shiplist file and passed cli options.
## Invocation samples

Example invocation (default):
```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion
```

Example to build a cumulative package across multiple build reports via `buildReportOrder`: 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --buildReportOrder /u/ibmuser/sample_buildreports/BuildReport_1.json,/u/ibmuser/sample_buildreports/BuildReport_2.json,/u/ibmuser/sample_buildreports/BuildReport_3.json --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion
```

Example to build a cumulative package across multiple build reports via `buildReportOrderFile`. The file contains the references to the locations of the buildReport: 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --buildReportOrderFile /u/ibmuser/sample_buildreports/BuildReportOrderFile.txt --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion
```
/u/ibmuser/sample_buildreports/BuildReportOrderFile.txt
```
/u/ibmuser/sample_buildreports/BuildReport_1.json
/u/ibmuser/sample_buildreports/BuildReport_2.json
/u/ibmuser/sample_buildreports/BuildReport_3.json 
```

Example to leverage [UCD packaging format v2](https://www.ibm.com/docs/en/urbancode-deploy/7.2.1?topic=czcv-creating-zos-component-version-using-v2-package-format): 

* Please note that this requires to define the mapping of the copyModes through the buztool properties file.

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --ucdV2PackageFormat
```

Example to establish link to the pipeline url: 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/
```

Example to establish link to the pipeline url and links for each deployable artifact to the git provider (see sample applicationRepositoryProps.properties): 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --propertyFile /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --packagingPropFiles /var/dbb/extensions/ucd-packaging/mortgageRepositoryProps.properties 
```

Example to establish links to the pipeline url, the git branch and the pull request in the UCD component version: 

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


 ## Sample console log for processing a single BuildReport.json
A sample invocation which stores the application package in an external artifact repository in UCD packaging format v2, including all traceability links.

```
/var/jenkins/workspace/zunit-retirementCalculator/dbb/Pipeline/CreateUCDComponentVersion/dbb-ucd-packaging.groovy --buztool /var/ucd-agent/bin/buztool.sh --workDir /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035 --component retirementCalculatorGithub --prop /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties --versionName 135_20220531.113630.036 --packagingPropFiles /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalulcatur.packaging.properties --ucdV2PackageFormat --pipelineURL http://jenkins-server/job/zunit-retirementCalculator/135/ --gitBranch main
```

<details>
  <summary>Console log</summary>


**dbb-ucd-packaging script output**

```
** Create version start at 20220531.013637.036
** Properties at startup:
   buztoolPropertyFile -> /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties
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
/var/ucd-agent/bin/buztool.sh createzosversion2 -c retirementCalculatorGithub -s /var/jenkins/workspace/zunit-retirementCalculator/BUILD-120/build.20220531.013558.035/shiplist.xml -o /var/jenkins/workspace/zunit-retirementCalculator/BUILD-120/build.20220531.013558.035/buztool.output -prop /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties -v "120_20220531.113630.036" 

```
</details>

 ## Sample console log for processing multiple BuildReports to assemble a cumulative package.

A sample invocation which builds a cumulative package across multiple build reports leveraging the `--buildReportOrder` cli option:

```
groovyz /var/jenkins/workspace/zunit-retirementCalculator/dbb/Pipeline/CreateUCDComponentVersion/dbb-ucd-packaging.groovy --buztool /var/ucd-agent/bin/buztool.sh --workDir /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035 --buildReportOrder /var/jenkins/tmp/BuildReport-134.json,/var/jenkins/tmp/BuildReport-135.json --component retirementCalculatorGithub --prop /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties --versionName 135_20220531.113630.036 --packagingPropFiles /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalulcatur.packaging.properties --ucdV2PackageFormat --pipelineURL http://jenkins-server/job/zunit-retirementCalculator/135/ --gitBranch main
```

<details>
  <summary>Console log</summary>


**dbb-ucd-packaging script output**

```
** Create version start at 20220531.013644.036
** Properties at startup:
   buztoolPropertyFile -> /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties
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
/var/ucd-agent/bin/buztool.sh createzosversion2 -c retirementCalculatorGithub -s /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035/shiplist.xml -o /var/jenkins/workspace/zunit-retirementCalculator/BUILD-135/build.20220531.013558.035/buztool.output -prop /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties -v "135_20220531.113630.036" 
```
</details>
 