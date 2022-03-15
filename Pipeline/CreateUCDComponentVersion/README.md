# IBM UCD Packaging based on IBM DBB Build Report

## Summary

An important step in the pipeline is to generate a deployable package. This sample groovy script
- extracts information about the build outputs from the Dependency Based Build ```BuildReport.json```
- generates the UCD shiplist ```shiplist.xml``` file
- invokes the ```buztool.sh``` with the approriate configuration to store the binary package in the artifact repository and to register a new UCD component version.

## High-level Processing flow
Initialization
- Read command line parameters
- Read Application level properties are supposed to be passed via `--packagingPropFiles` (Optionally)

Process the DBB Build report
- Read DBB's BuildReport.json from the pipeline work directory
- Parse and extract build output information of records of type *ExecuteRecord* and *CopyToPDSRecord* (requires at least DBB 1.0.8)
- Parse and extract the build output information for deleted build outputs of type *Delete_Record* written to the BuildReport by zAppBuild leveraging the AnyTypeRecord API which got introced with IBM Dependency Based Build 1.1.3.

Generates the UCD shiplist.xml file and invokes UCD packaging step
- Write the shiplist.xml to the build directory
    - Adds links back to the ci pipeline build to UCD component version (Optional).
    - Adds UCD properties for bind information captured in the zAppBuild framework through generic PropertyRecords for DBRM members, such as bind_collectionID,bind_packageOwner,bind_qualifier on the element level - see [generateDb2BindInfoRecord configuration in zAppBuild](https://github.com/IBM/dbb-zappbuild/blob/06ff114ee22b4e41a09aa0640ac75b7e56c70521/build-conf/build.properties#L79-L89) (Optional).  
    - Adds UCD properties to changes (git hashes) within the version control system (Optional).
    - Adds source input information about the input files from the DBB Dependency Sets.
- Invokes buztool.sh on USS with the generated shiplist file and passed cli options.
## Invocation samples

Example invocation (default):
```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --prop /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion
```

Example to leverage [UCD packaging format v2](https://www.ibm.com/docs/en/urbancode-deploy/7.2.1?topic=czcv-creating-zos-component-version-using-v2-package-format): 

* Please note that this requires to define the mapping of the copyModes through the buztool properties file.

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --prop /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --ucdV2PackageFormat
```

Example to establish link to the pipeline url: 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --prop /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/
```

Example to establish link to the pipeline url and links for each deployable artifact to the git provider: 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --prop /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --packagingPropFiles /var/dbb/extensions/ucd-packaging/mortgageRepositoryProps.properties 
```

Example to establish link to the pipeline url and git branch: 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --prop /var/ucd/agent/conf/artifactrepository/myapp.artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --gitBranch development
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

 -g,--gitBranch <arg>                           Name of the git branch

 -p,--preview                                   Preview mode generate shiplist, but do
                                                not run buztool.sh


utility options :

 -help,--help                                   Prints this message
 ```


 ## Sample script log
A sample invocation which stores the application package in an external artifact repository in UCD packaging format v2, including all traceability links.

<details>
  <summary>Console outputs</summary>


**dbb-ucd-packaging script output**

```
+ /usr/lpp/dbb/v1r0/bin/groovyz /var/jenkins/workspace/zunit-retirementCalculator/dbb/Pipeline/CreateUCDComponentVersion/dbb-ucd-packaging.groovy --buztool /var/ucd-agent/bin/buztool.sh --workDir /var/jenkins/workspace/zunit-retirementCalculator/BUILD-112/build.20220315.080246.002 --component retirementCalculator --prop /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties --versionName 112_20220315.050325.003 --packagingPropFiles /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalulcatur.packaging.properties --ucdV2PackageFormat --pipelineURL http://jenkins-server/job/zunit-retirementCalculator/112/ --gitBranch main 
** Create version start at 20220315.080327.003
** Properties at startup:
   workDir -> /var/jenkins/workspace/zunit-retirementCalculator/BUILD-112/build.20220315.080246.002
   startTime -> 20220315.080327.003
   versionName -> 112_20220315.050325.003
   git_commitURL_prefix -> https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/commit
   git_treeURL_prefix -> https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree
   ucdV2PackageFormat -> true
   preview -> false
   gitBranch -> main
   containerMapping -> ["LOAD": "LOAD", "LOADLIB": "LOAD", "COPY" : "TEXT", "DBRM" : "DBRM", "JCL" : "TEXT"]
   pipelineURL -> http://jenkins-server/job/zunit-retirementCalculator/112/
   buztoolPath -> /var/ucd-agent/bin/buztool.sh
   propertyFileSettings -> /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties
   component -> retirementCalculator
** Read build report data from /var/jenkins/workspace/zunit-retirementCalculator/BUILD-112/build.20220315.080246.002/BuildReport.json
** Find deployable outputs in the build report 
   * Buildrecord type TYPE_COPY_TO_PDS is supported with DBB toolkit 1.0.8 and higher. Extracting build records for TYPE_COPY_TO_PDS might not be available and skipped. Identified DBB Toolkit version 1.1.2.
** Deployable files
   JENKINS.ZDAT.RETIRE.LOAD(EBUD01), LOAD
   JENKINS.ZDAT.RETIRE.LOAD(EBUD03), LOAD
   JENKINS.ZDAT.RETIRE.LOAD(EBUD0RUN), LOAD
   JENKINS.ZDAT.RETIRE.LOAD(EBUD02), LOAD
** Deleted files
** Generate UCD ship list file
** Write ship list file to  /var/jenkins/workspace/zunit-retirementCalculator/BUILD-112/build.20220315.080246.002/shiplist.xml
** Following UCD buztool cmd will be invoked
/var/ucd-agent/bin/buztool.sh createzosversion2 -c retirementCalculator -s /var/jenkins/workspace/zunit-retirementCalculator/BUILD-112/build.20220315.080246.002/shiplist.xml -o /var/jenkins/workspace/zunit-retirementCalculator/BUILD-112/build.20220315.080246.002/buztool.output -prop /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties -v 112_20220315.050325.003 
** Create version by running UCD buztool
zOS toolkit config   : /var/ucd-agent/ (7.2.1.0,20211011-0758)
zOS toolkit binary   : /var/ucd-agent/ (7.2.1.0,20211011-0758)
zOS toolkit data set : RATCFG.UCD.V7R2M1 (7.2.1.0,20211011-0758)
Reading parameters:
....Command : createzosversion2
....Component : retirementCalculator
....Version : 112_20220315.050325.003
....Shiplist file : /var/jenkins/workspace/zunit-retirementCalculator/BUILD-112/build.20220315.080246.002/shiplist.xml
....Buztool Properties File : /var/jenkins/workspace/zunit-retirementCalculator//retirementCalculator/retirementCalculator/application-conf/retirementCalculator.ucd.properties
....Output File:/var/jenkins/workspace/zunit-retirementCalculator/BUILD-112/build.20220315.080246.002/buztool.output
Verifying version
....Repository location : /var/ucd-agent/var/repository/retirementCalculator/112_20220315.050325.003
Pre-processing shiplist:
....Shiplist after processing :/var/ucd-agent/var/repository/retirementCalculator/112_20220315.050325.003/shiplist.xml
Packaging data sets:
....Shiplist : /var/ucd-agent/var/repository/retirementCalculator/112_20220315.050325.003/shiplist.xml
....Location to store zip :  /var/ucd-agent/var/repository/retirementCalculator/112_20220315.050325.003
....Zip name : package.zip
....verbose : false
....Datasets copied succesfully
Post-processing package:
PackageManifest file post-processing completed. 
Create version and store package:
....Uploading to artifactory using details from prop file
....Executing request PUT http://artifactory-server/artifactory/RetirementCalculator/112_20220315.050325.003.zip HTTP/1.1
....Version artifacts stored to ARTIFACTORY server
....Version created in UCD server
....Version:112_20220315.050325.003 created
Elapsed time: 2.0 seconds.

** buztool output properties
   version.url -> https://ucd.dat.ibm.com:8443//#version/c9f2f810-8314-4baa-af79-937a57f81c51
   version.repository.type -> CODESTATION
   version.name -> 112_20220315.050325.003
   version.id -> c9f2f810-8314-4baa-af79-937a57f81c51
   component.name -> retirementCalculator
   version.shiplist -> /var/jenkins/workspace/zunit-retirementCalculator/BUILD-112/build.20220315.080246.002/shiplist.xml
** Build finished
```

**Generated Shiplistfile.xml**

```
<?xml version="1.0" encoding="CP037"?>
<manifest type='MANIFEST_SHIPLIST'>
  <property name='dbb-buildResultUrl' value='https://10.3.20.96:10443/dbb/rest/buildResult/90543' />
  <property name='ci-pipelineUrl' value='http://jenkins-server/job/zunit-retirementCalculator/112/' />
  <property name='ci-gitBranch' value='main' />
  <property name='filesProcessed' value='8' />
  <property name=':githash:retirementCalculator' value='532647316aecd2acd4b3a545e8985080c97415db' />
  <property name='fullBuild' value='true' />
  <property name=':giturl:retirementCalculator' value='git@github.ibm.com:zDevOps-Acceleration/retirementCalculator.git' />
  <container name='JENKINS.ZDAT.RETIRE.LOAD' type='PDS' deployType='LOAD'>
    <resource name='EBUD01' type='PDSMember' deployType='LOAD'>
      <property name='buildcommand' value='IEWBLINK' />
      <property name='buildoptions' value='RENT,REUS=RENT,LIST,XREF,AMODE=31,RMODE=ANY' />
      <property name='githash' value='532647316aecd2acd4b3a545e8985080c97415db' />
      <property name='git-link-to-commit' value='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/commit/532647316aecd2acd4b3a545e8985080c97415db' />
      <inputs url='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree/532647316aecd2acd4b3a545e8985080c97415db/retirementCalculator/cobol/EBUD01.cbl'>
        <input name='retirementCalculator/cobol/EBUD01.cbl' compileType='Main' url='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree/532647316aecd2acd4b3a545e8985080c97415db/retirementCalculator/cobol/EBUD01.cbl' />
      </inputs>
    </resource>
  </container>
  <container name='JENKINS.ZDAT.RETIRE.LOAD' type='PDS' deployType='LOAD'>
    <resource name='EBUD03' type='PDSMember' deployType='LOAD'>
      <property name='buildcommand' value='IEWBLINK' />
      <property name='buildoptions' value='RENT,REUS=RENT,LIST,XREF,AMODE=31,RMODE=ANY' />
      <property name='githash' value='532647316aecd2acd4b3a545e8985080c97415db' />
      <property name='git-link-to-commit' value='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/commit/532647316aecd2acd4b3a545e8985080c97415db' />
      <inputs url='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree/532647316aecd2acd4b3a545e8985080c97415db/retirementCalculator/cobol/EBUD03.cbl'>
        <input name='retirementCalculator/cobol/EBUD03.cbl' compileType='Main' url='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree/532647316aecd2acd4b3a545e8985080c97415db/retirementCalculator/cobol/EBUD03.cbl' />
      </inputs>
    </resource>
  </container>
  <container name='JENKINS.ZDAT.RETIRE.LOAD' type='PDS' deployType='LOAD'>
    <resource name='EBUD0RUN' type='PDSMember' deployType='LOAD'>
      <property name='buildcommand' value='IEWBLINK' />
      <property name='buildoptions' value='RENT,REUS=RENT,LIST,XREF,AMODE=31,RMODE=ANY' />
      <property name='githash' value='532647316aecd2acd4b3a545e8985080c97415db' />
      <property name='git-link-to-commit' value='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/commit/532647316aecd2acd4b3a545e8985080c97415db' />
      <inputs url='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree/532647316aecd2acd4b3a545e8985080c97415db/retirementCalculator/cobol/EBUD0RUN.cbl'>
        <input name='retirementCalculator/cobol/EBUD0RUN.cbl' compileType='Main' url='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree/532647316aecd2acd4b3a545e8985080c97415db/retirementCalculator/cobol/EBUD0RUN.cbl' />
        <input name='retirementCalculator/copy/LINPUT.cpy' compileType='COPY' url='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree/532647316aecd2acd4b3a545e8985080c97415db/retirementCalculator/copy/LINPUT.cpy' />
      </inputs>
    </resource>
  </container>
  <container name='JENKINS.ZDAT.RETIRE.LOAD' type='PDS' deployType='LOAD'>
    <resource name='EBUD02' type='PDSMember' deployType='LOAD'>
      <property name='buildcommand' value='IEWBLINK' />
      <property name='buildoptions' value='RENT,REUS=RENT,LIST,XREF,AMODE=31,RMODE=ANY' />
      <property name='githash' value='532647316aecd2acd4b3a545e8985080c97415db' />
      <property name='git-link-to-commit' value='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/commit/532647316aecd2acd4b3a545e8985080c97415db' />
      <inputs url='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree/532647316aecd2acd4b3a545e8985080c97415db/retirementCalculator/cobol/EBUD02.cbl'>
        <input name='retirementCalculator/cobol/EBUD02.cbl' compileType='Main' url='https://github.ibm.com/zDevOps-Acceleration/retirementCalculator/tree/532647316aecd2acd4b3a545e8985080c97415db/retirementCalculator/cobol/EBUD02.cbl' />
      </inputs>
    </resource>
  </container>
</manifest>
```
</details>
 