# IBM UCD Packaging based on IBM DBB Build Report

An important step in the pipeline is to generate a deployable package. This sample groovy script
- extracts information about the build outputs from the DBB BuildReport.json
- generates the UCD shiplist ```shiplist.xml``` file
- invokes the ```buztool.sh``` of UCD

Example invocation (default):
```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --prop /var/ucd/agent/conf/artifactrepository/artifactory.properties --versionName MyVersion
```

Example to establish link to the pipeline url: 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --prop /var/ucd/agent/conf/artifactrepository/artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/
```

Example to establish link to the pipeline url and links for each deployable artifact to the git provider: 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --prop /var/ucd/agent/conf/artifactrepository/artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --repositoryInfoPropertiesFile /var/dbb/extensions/ucd-packaging/mortgageRepositoryProps.properties 
```

Example to establish link to the pipeline url and git branch: 

```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/build/job/dbb-outputdir --component MYCOMP --prop /var/ucd/agent/conf/artifactrepository/artifactory.properties --versionName MyVersion --pipelineURL https://ci-server/job/MortgageApplication/34/ --gitBranch development
```

## Processing flow
- Read command line parameters
- Read DBB's BuildReport.json from the pipeline work directory
- Parse and extract build output information of records of type *ExecuteRecord* and *CopyToPDSRecord*
- Optionally adds generic PropertyRecords, which are linked to the buildfile
- Optionally adds links back to the ci pipeline build 
- Optionally adds links to changes (git hashes) within the version control system
- Generates shiplist.xml file
- Invokes buztool.sh on USS with the generated options

## Command Line Options Summary
```
$DBB_HOME/bin/groovyz <ussLocation>/dbb-ucd-packaging.groovy [options]

required options:

 -b,--buztool <file>               		Absolute path to UrbanCode Deploy
                                    		buztool.sh script
 -c,--component <name>              		Name of the UCD component to
                                    		create version in
 -w,--workDir <dir>                 		Absolute path to the DBB build
                                    		output directory

optional options:
 -ar,--artifactRepository <arg>     		Absolute path to Artifact Respository Server
                                    		Server connection file
 -prop,--propertyFile <arg>         		Absolute path to property file. 
                                    		From UCD v7.1.x and greater it replace the -ar option.
 -v,--versionName <arg>             		Name of the UCD component version
 -pURL,--pipelineURL <arg>			URL to the pipeline build result
 -g,--gitBranch <arg>					Name of the git branch
 -rpFile,--repositoryInfoPropertiesFile <arg>	Absolute path to the property file containing URL prefixes to git provider
 -p,--preview                       		Preview mode generate shiplist, but do
                                    		not run buztool.sh


utility options
 -help,--help             Prints this message
 ```