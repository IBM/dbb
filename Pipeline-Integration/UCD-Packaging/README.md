# IBM UCD Packaging based on IBM DBB Build Report

An important step in the pipeline is to generate a deployable package. This sample groovy script
- extracts information about the build outputs from the DBB BuildReport.json
- generates the UCD shiplist ```shiplist.xml``` file
- invokes the ```buztool.sh``` of UCD

Example invocation:
```
$DBB_HOME/bin/groovyz dbb-ucd-packaging.groovy --buztool /var/ucd/agent/bin/buztool.sh --workDir /var/ucd/packaging-samples --component MYCOMP --ar /var/ucd/agent/conf/artifactrepository/artifactory.properties --versionName MyVersion
```

## Processing flow
- Read command line parameters
- Read DBB's BuildReport.json from the pipeline work directory
- Parse and extract build output information from ExecuteRecords 
- Optionally adds generic PropertyRecords, which are linked to the buildfile
- Generates shiplist.xml file
- Invokes buztool.sh on USS with the generated options

## Command Line Options Summary
```
$DBB_HOME/bin/groovyz <ussLocation>/dbb-ucd-packaging.groovy [options]

required options:

 -b,--buztool <file>                Absolute path to UrbanCode Deploy
                                    buztool.sh script
 -c,--component <name>              Name of the UCD component to
                                    create version in
 -w,--workDir <dir>                 Absolute path to the DBB build
                                    output directory

optional options:
 -ar,--artifactRepository <arg>     Absolute path to Artifact Respository Server
                                    Server connection file
 -v,--versionName <arg>             Name of the UCD component version
 -p,--preview                       Preview mode generate shiplist, but do
                                    not run buztool.sh


utility options
 -help,--help             Prints this message
 ```