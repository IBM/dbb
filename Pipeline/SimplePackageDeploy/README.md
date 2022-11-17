# Simple Package Deploy (Deploy the package to target libraries)
## Description
This "SimplePackageDeploy.groovy" sample script is capable of untaring the tar package produced by [PackageBuildOutputs](../PackageBuildOutputs/) and deploy the tar package contents to the target libraries. The deployment is done based on the contents in BuildReport.json file.

Potential scenarios include:
* Deploying the tar package to the target environment libraries.
* If the user wants to do shift-left testing using IBM Z Virtual Test Platform, and subsystem updates are not required.

As of now, this script does not perform any activation activities like a CICS NEWCOPY or a DB2 BIND. Also, it does not support any type of rollback process.
This script works only for packages of [PackageBuildOutputs](../PackageBuildOutputs/) based on a single BuildReport.json and does not support cumulative packages based on multiple build reports.

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
** SimplePackageDeploy start at 20221117.051945.019
** Properties at startup:
     targetLibLLQMap -> {"JCL": "JCL", "DBRM": "DBRM", "LOAD": "LOAD", "CICSLOAD": "CICSLOAD", "MAPLOAD": "MAPLOAD", "IMSLOAD": "IMSLOAD"}
     packageWithExtension -> false
     tarFileName -> /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithoutExtn.tar
     copyModeMap -> {"JCL": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "CICSLOAD": "LOAD", "MAPLOAD": "LOAD", "IMSLOAD": "LOAD"}
     startTime -> 20221117.051945.019
     hlq -> IBMUSER.SIT.MORTGAGE
     workDir -> /u/ibmuser/workspace/MortgageApplication/out

** Created tar file extract directory /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.051945.019
** Untarring file /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithoutExtn.tar to /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.051945.019.

** Deploying the contents in /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.051945.019/BuildReport.json
     Copied file IBMUSER.MORT0001.LOAD/EPSMORT to target library IBMUSER.SIT.MORTGAGE.MAPLOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.LOAD/EPSMLIS to target library IBMUSER.SIT.MORTGAGE.MAPLOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.LOAD/EPSCSMRT to target library IBMUSER.SIT.MORTGAGE.CICSLOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.DBRM/EPSCMORT to target library IBMUSER.SIT.MORTGAGE.DBRM using DBB CopyMode BINARY
     Copied file IBMUSER.MORT0001.LOAD/EPSCMORT to target library IBMUSER.SIT.MORTGAGE.CICSLOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.LOAD/DATEVAL to target library IBMUSER.SIT.MORTGAGE.LOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.LOAD/LENPGM to target library IBMUSER.SIT.MORTGAGE.LOAD using DBB CopyMode LOAD
** Cleaning up the temporary folder - /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.051945.019 

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
** SimplePackageDeploy start at 20221117.052226.022
** Properties at startup:
     targetLibLLQMap -> {"JCL": "JCL", "DBRM": "DBRM", "LOAD": "LOAD", "CICSLOAD": "CICSLOAD", "MAPLOAD": "MAPLOAD", "IMSLOAD": "IMSLOAD"}
     packageWithExtension -> true
     tarFileName -> /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithExtn.tar
     copyModeMap -> {"JCL": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "CICSLOAD": "LOAD", "MAPLOAD": "LOAD", "IMSLOAD": "LOAD"}
     startTime -> 20221117.052226.022
     hlq -> IBMUSER.SIT.MORTGAGE
     workDir -> /u/ibmuser/workspace/MortgageApplication/out

** Created tar file extract directory /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052226.022
** Untarring file /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithExtn.tar to /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052226.022.

** Deploying the contents in /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052226.022/BuildReport.json
     Copied file IBMUSER.MORT0001.LOAD/EPSMORT to target library IBMUSER.SIT.MORTGAGE.MAPLOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.LOAD/EPSMLIS to target library IBMUSER.SIT.MORTGAGE.MAPLOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.LOAD/EPSCSMRT to target library IBMUSER.SIT.MORTGAGE.CICSLOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.DBRM/EPSCMORT to target library IBMUSER.SIT.MORTGAGE.DBRM using DBB CopyMode BINARY
     Copied file IBMUSER.MORT0001.LOAD/EPSCMORT to target library IBMUSER.SIT.MORTGAGE.CICSLOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.LOAD/DATEVAL to target library IBMUSER.SIT.MORTGAGE.LOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.LOAD/LENPGM to target library IBMUSER.SIT.MORTGAGE.LOAD using DBB CopyMode LOAD
** Cleaning up the temporary folder - /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052226.022 

** Build finished
```
</details>

#### Error when BuildReport.json file is not found in the package

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20221117.052620.026
** Properties at startup:
     targetLibLLQMap -> {"JCL": "JCL", "DBRM": "DBRM", "LOAD": "LOAD", "CICSLOAD": "CICSLOAD", "MAPLOAD": "MAPLOAD", "IMSLOAD": "IMSLOAD"}
     packageWithExtension -> false
     tarFileName -> /u/ibmuser/workspace/MortgageApplication/out/multiBuildPackageWithoutExtn.tar
     copyModeMap -> {"JCL": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "CICSLOAD": "LOAD", "MAPLOAD": "LOAD", "IMSLOAD": "LOAD"}
     startTime -> 20221117.052620.026
     hlq -> IBMUSER.SIT.MORTGAGE
     workDir -> /u/ibmuser/workspace/MortgageApplication/out

** Created tar file extract directory /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052620.026
** Untarring file /u/ibmuser/workspace/MortgageApplication/out/multiBuildPackageWithoutExtn.tar to /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052620.026.

*! Build report data at /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052620.026/BuildReport.json not found
*! Deployment stopped
** Cleaning up the temporary folder - /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052620.026 

** Build finished
```
</details>


#### Error when copy mode for a deploy type is not defined

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20221117.052757.027
** Properties at startup:
     targetLibLLQMap -> {"JCL": "JCL", "DBRM": "DBRM", "LOAD": "LOAD", "CICSLOAD": "CICSLOAD", "MAPLOAD": "MAPLOAD", "IMSLOAD": "IMSLOAD"}
     packageWithExtension -> false
     tarFileName -> /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithoutExtn.tar
     copyModeMap -> {"JCL": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD", "MAPLOAD": "LOAD", "IMSLOAD": "LOAD"}
     startTime -> 20221117.052757.027
     hlq -> IBMUSER.SIT.MORTGAGE
     workDir -> /u/ibmuser/workspace/MortgageApplication/out

** Created tar file extract directory /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052757.027
** Untarring file /u/ibmuser/workspace/MortgageApplication/out/build.20220920.053231.032/packageWithoutExtn.tar to /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052757.027.

** Deploying the contents in /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052757.027/BuildReport.json
     Copied file IBMUSER.MORT0001.LOAD/EPSMORT to target library IBMUSER.SIT.MORTGAGE.MAPLOAD using DBB CopyMode LOAD
     Copied file IBMUSER.MORT0001.LOAD/EPSMLIS to target library IBMUSER.SIT.MORTGAGE.MAPLOAD using DBB CopyMode LOAD
     !ERROR: DEPLOYMENT FAILED
     !ERROR: SOURCE FILE NOT DEPLOYED : IBMUSER.MORT0001.LOAD/EPSCSMRT
     !ERROR: DBB COPY MODE NOT DEFINED FOR DEPLOY TYPE : CICSLOAD
** Cleaning up the temporary folder - /u/ibmuser/workspace/MortgageApplication/out/DeployFiles_20221117.052757.027 

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
