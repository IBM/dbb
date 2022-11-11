# Simple Build Report Deploy (Deploy the package to target libraries)
## Description
This "SimplePackageDeploy.groovy" sample script is capable of untaring the tar package produced by [PackageBuildOutputs](../PackageBuildOutputs/) and deploy the tar package contents to the target libraries. The deployment is done based on the contents in BuildReport.json file.

Potential scenarios include:
* Deploying the tar package to the target environment libraries.
* If the user wants to do shift-left testing using IBM Z Virtual Test Platform, and subsystem updates are not required.

As of now, this script does not perform any activation activities like a CICS NEWCOPY or a DB2 BIND. Also, it does not support any type of rollback process.
This script works only for packages of PackageBuildOutputs based on a single BuildReport.json and does not support cumulative packages based on multiple build reports.

### Prerequisites

* All the target environment libraries are existing.
* The deployment package is available in USS. (For example, it could be a downloaded copy from the binary artifact repository.)
* The user invoking the script has proper permissions to write to the target datasets.

### Installation
* Copy the sample scripts into USS files under the dbb-zappbuild folder.
* Update the permission bits to allow for execute. Ex: chmod 755 *

### Customization/Configuration
* Review the notes sections in each sample script prologue for more information on customization.

### Sample invocation

#### Deploy from package only output from [PackageBuildOutputs](../PackageBuildOutputs/)
```
groovyz SimplePackageDeploy.groovy --workDir /u/ibmuser/workspace/MortgageApplication/out --tarFileName /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithoutExtn.tar --hlq IBMUSER.SIT.MORTGAGE
```

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20221111.064802.048
** Created tar file extract directory /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048
** Untar file at /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithoutExtn.tar.

Package untar done to /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048

** Deploying the contents in /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048/BuildReport.json

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048/IBMUSER.MORT0001.LOAD/EPSMORT is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMORT to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048/IBMUSER.MORT0001.LOAD/EPSMLIS is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMLIS to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048/IBMUSER.MORT0001.LOAD/EPSCSMRT is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCSMRT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048/IBMUSER.MORT0001.DBRM/EPSCMORT is of type DBRM
Copied source file - IBMUSER.MORT0001.DBRM/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.DBRM

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048/IBMUSER.MORT0001.LOAD/EPSCMORT is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048/IBMUSER.MORT0001.LOAD/DATEVAL is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/DATEVAL to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048/IBMUSER.MORT0001.LOAD/LENPGM is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/LENPGM to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Cleaning up the temporary folder - /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064802.048

** Build finished
```
</details>


#### Deploy from package only including adding deployType to files in tar output from [PackageBuildOutputs](../PackageBuildOutputs/)
```
groovyz SimplePackageDeploy.groovy --workDir /u/ibmuser/workspace/MortgageApplication/out --tarFileName /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithExtn.tar --hlq IBMUSER.SIT.MORTGAGE --packageWithExtension
```

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20221111.064958.049
** Created tar file extract directory /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049
** Untar file at /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithExtn.tar.

Package untar done to /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049

** Deploying the contents in /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049/BuildReport.json

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049/IBMUSER.MORT0001.LOAD/EPSMORT.MAPLOAD is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMORT to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049/IBMUSER.MORT0001.LOAD/EPSMLIS.MAPLOAD is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMLIS to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049/IBMUSER.MORT0001.LOAD/EPSCSMRT.CICSLOAD is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCSMRT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049/IBMUSER.MORT0001.DBRM/EPSCMORT.DBRM is of type DBRM
Copied source file - IBMUSER.MORT0001.DBRM/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.DBRM

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049/IBMUSER.MORT0001.LOAD/EPSCMORT.CICSLOAD is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049/IBMUSER.MORT0001.LOAD/DATEVAL.LOAD is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/DATEVAL to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049/IBMUSER.MORT0001.LOAD/LENPGM.LOAD is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/LENPGM to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Cleaning up the temporary folder - /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.064958.049

** Build finished
```
</details>

#### Error when BuildReport.json file is not found in the package

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20221111.065624.056
** Created tar file extract directory /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065624.056
** Untar file at /u/ibmuser/workspace/MortgageApplication/out/multiBuildPackageWithoutExtn.tar.

Package untar done to /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065624.056
** Build report data at /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065624.056/BuildReport.json not found
** Deployment stopped

Cleaning up the temporary folder - /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065624.056

** Build finished
```
</details>


#### Error when copy mode for a deploy type is not defined

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20221111.065252.052
** Created tar file extract directory /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065252.052
** Untar file at /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithoutExtn.tar.

Package untar done to /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065252.052

** Deploying the contents in /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065252.052/BuildReport.json

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065252.052/IBMUSER.MORT0001.LOAD/EPSMORT is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMORT to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065252.052/IBMUSER.MORT0001.LOAD/EPSMLIS is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMLIS to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065252.052/IBMUSER.MORT0001.LOAD/EPSCSMRT is of type CICSLOAD
ERROR: DEPLOYMENT FAILED
ERROR: SOURCE FILE NOT DEPLOYED : IBMUSER.MORT0001.LOAD/EPSCSMRT
ERROR: DBB COPY MODE NOT DEFINED FOR DEPLOY TYPE : CICSLOAD


Cleaning up the temporary folder - /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221111.065252.052

** Build finished
```
</details>


### Command Line Options Summary - SimplePackageDeploy
```
  usage: SimplePackageDeploy.groovy [options]
 
  -w,--workDir <dir>                             Absolute path to the package untar 
                                                 temporary folder
  -t,--tarFileName <filename>                    Name of the package tar file with path
  -q,--hlq <hlq>                                 HLQ of the target environment libraries 
                                                 for the deployment 
                                                                                                                                          
  Optional:
  
  -e,--packageWithExtension                      Flag to show the package contains 
                                                 extension 
  -h,--help                                      Prints this message
```

### Sample(s) Inventory

Artifact | Description
---------- | ----------------------------------------------------------------------------------------
[SimplePackageDeploy.groovy](SimplePackageDeploy.groovy) | Groovy program to deploy the package to target libraries
[SimplePackageDeploy.properties](SimplePackageDeploy.properties) | Properties file to customize copy mode and target libraries LLQ
