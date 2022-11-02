# Simple Build Report Deploy (Deploy the package to target libraries)
## Description
This "SimplePackageDeploy.groovy" sample script is capable of untaring the tar package produced by [PackageBuildOutputs](../PackageBuildOutputs/) and deploy the tar package contents to the target libraries. 

Potential scenarios include:
* Deploying the tar package to the target environment libraries.
* If the user wants to do shift-left testing using IBM Z Virtual Test Platform, and subsystem updates are not required.

As of now, this script does not perform any activation activities like a CICS NEWCOPY or a DB2 BIND.  Also, it does not support any type of rollback process.

### Prerequisites

* All the target environment libraries are existing.
* The deployment package is available in USS. (For example, it could be a downloaded copy from the binary artifact repository.)
* The user invoking the script has proper permissions to write to the target datasets.

### Installation
* Copy the below sample scripts into USS files under the dbb-zappbuild folder.
* Update the permission bits to allow for execute. Ex: chmod 755 *

### Customization/Configuration
* Review the notes sections in each sample script prologue for more information on customization.

### Sample invocation

#### Deploy from package only output from [PackageBuildOutputs](../PackageBuildOutputs/)
```
groovyz SimplePackageDeploy.groovy --workDir /u/ibmuser/workspace/zAppBuild_Hogan/out --tarFileName /u/ibmuser/workspace/zAppBuild_Hogan/out/build.20220920.053231.032/packageWithoutExtn.tar --hlq IBMUSER.SIT.MORTGAGE
```

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20220920.073414.034
** Created tar file extract directory /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034
** Untar file at /u/ibmuser/workspace/zAppBuild_Hogan/out/build.20220920.053231.032/packageWithoutExtn.tar.

Package untar done to /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034

** Deploying the contents in /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034/BuildReport.json

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034/IBMUSER.MORT0001.LOAD/EPSMORT is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMORT to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034/IBMUSER.MORT0001.LOAD/EPSMLIS is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMLIS to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034/IBMUSER.MORT0001.LOAD/EPSCSMRT is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCSMRT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034/IBMUSER.MORT0001.DBRM/EPSCMORT is of type DBRM
Copied source file - IBMUSER.MORT0001.DBRM/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.DBRM

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034/IBMUSER.MORT0001.LOAD/EPSCMORT is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034/IBMUSER.MORT0001.LOAD/DATEVAL is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/DATEVAL to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034/IBMUSER.MORT0001.LOAD/LENPGM is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/LENPGM to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Deleted the temporary folder - /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073414.034

** Build finished
```
</details>


#### Deploy from package only including adding deployType to files in tar output from [PackageBuildOutputs](../PackageBuildOutputs/)
```
groovyz SimplePackageDeploy.groovy --workDir /u/ibmuser/workspace/zAppBuild_Hogan/out --tarFileName /u/ibmuser/workspace/zAppBuild_Hogan/out/build.20220920.053231.032/packageWithExtn.tar --hlq IBMUSER.SIT.MORTGAGE --packageWithExtension
```

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20220920.073751.037
** Created tar file extract directory /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037
** Untar file at /u/ibmuser/workspace/zAppBuild_Hogan/out/build.20220920.053231.032/packageWithExtn.tar.

Package untar done to /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037

** Deploying the contents in /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037/BuildReport.json

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037/IBMUSER.MORT0001.LOAD/EPSMORT.MAPLOAD is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMORT to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037/IBMUSER.MORT0001.LOAD/EPSMLIS.MAPLOAD is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMLIS to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037/IBMUSER.MORT0001.LOAD/EPSCSMRT.CICSLOAD is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCSMRT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037/IBMUSER.MORT0001.DBRM/EPSCMORT.DBRM is of type DBRM
Copied source file - IBMUSER.MORT0001.DBRM/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.DBRM

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037/IBMUSER.MORT0001.LOAD/EPSCMORT.CICSLOAD is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037/IBMUSER.MORT0001.LOAD/DATEVAL.LOAD is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/DATEVAL to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037/IBMUSER.MORT0001.LOAD/LENPGM.LOAD is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/LENPGM to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Deleted the temporary folder - /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.073751.037

** Build finished
```
</details>


#### Deploy from package only processing multiple build reports output from [PackageBuildOutputs](../PackageBuildOutputs/)
```
groovyz SimplePackageDeploy.groovy --workDir /u/ibmuser/workspace/zAppBuild_Hogan/out --tarFileName /u/ibmuser/workspace/zAppBuild_Hogan/out/multiBuildPackageWithoutExtn.tar --hlq IBMUSER.SIT.MORTGAGE
```

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20220920.074004.040
** Created tar file extract directory /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040
** Untar file at /u/ibmuser/workspace/zAppBuild_Hogan/out/multiBuildPackageWithoutExtn.tar.

Package untar done to /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040

** Deploying the contents in /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/001_BuildReport.json

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0001.LOAD/EPSMORT is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMORT to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0001.LOAD/EPSMLIS is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMLIS to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0001.LOAD/EPSCSMRT is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCSMRT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0001.DBRM/EPSCMORT is of type DBRM
Copied source file - IBMUSER.MORT0001.DBRM/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.DBRM

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0001.LOAD/EPSCMORT is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0001.LOAD/DATEVAL is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/DATEVAL to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0001.LOAD/LENPGM is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/LENPGM to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

** Deploying the contents in /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/002_BuildReport.json

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0002.LOAD/EPSCSMRD is of type CICSLOAD
Copied source file - IBMUSER.MORT0002.LOAD/EPSCSMRD to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0002.LOAD/EPSMPMT is of type LOAD
Copied source file - IBMUSER.MORT0002.LOAD/EPSMPMT to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040/IBMUSER.MORT0002.LOAD/EPSMLIST is of type CICSLOAD
Copied source file - IBMUSER.MORT0002.LOAD/EPSMLIST to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Deleted the temporary folder - /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074004.040

** Build finished
```
</details>


#### Deploy from package only processing multiple build reports including adding deployType to files in tar output from [PackageBuildOutputs](../PackageBuildOutputs/)
```
groovyz SimplePackageDeploy.groovy --workDir /u/ibmuser/workspace/zAppBuild_Hogan/out --tarFileName /u/ibmuser/workspace/zAppBuild_Hogan/out/multiBuildPackageWithExtn.tar --hlq IBMUSER.SIT.MORTGAGE --packageWithExtension
```

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20220920.074508.045
** Created tar file extract directory /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045
** Untar file at /u/ibmuser/workspace/zAppBuild_Hogan/out/multiBuildPackageWithExtn.tar.

Package untar done to /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045

** Deploying the contents in /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/001_BuildReport.json

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0001.LOAD/EPSMORT.MAPLOAD is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMORT to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0001.LOAD/EPSMLIS.MAPLOAD is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMLIS to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0001.LOAD/EPSCSMRT.CICSLOAD is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCSMRT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0001.DBRM/EPSCMORT.DBRM is of type DBRM
Copied source file - IBMUSER.MORT0001.DBRM/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.DBRM

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0001.LOAD/EPSCMORT.CICSLOAD is of type CICSLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSCMORT to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0001.LOAD/DATEVAL.LOAD is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/DATEVAL to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0001.LOAD/LENPGM.LOAD is of type LOAD
Copied source file - IBMUSER.MORT0001.LOAD/LENPGM to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

** Deploying the contents in /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/002_BuildReport.json

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0002.LOAD/EPSCSMRD.CICSLOAD is of type CICSLOAD
Copied source file - IBMUSER.MORT0002.LOAD/EPSCSMRD to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0002.LOAD/EPSMPMT.LOAD is of type LOAD
Copied source file - IBMUSER.MORT0002.LOAD/EPSMPMT to Target PDS - IBMUSER.SIT.MORTGAGE.LOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045/IBMUSER.MORT0002.LOAD/EPSMLIST.CICSLOAD is of type CICSLOAD
Copied source file - IBMUSER.MORT0002.LOAD/EPSMLIST to Target PDS - IBMUSER.SIT.MORTGAGE.CICSLOAD

Deleted the temporary folder - /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.074508.045

** Build finished
```
</details>


#### Error when copy mode for a deploy type is not defined

<details>
  <summary>Console log</summary>

SimplePackageDeploy console output

```
** SimplePackageDeploy start at 20220920.075237.052
** Created tar file extract directory /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.075237.052
** Untar file at /u/ibmuser/workspace/zAppBuild_Hogan/out/build.20220920.053231.032/packageWithoutExtn.tar.

Package untar done to /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.075237.052

** Deploying the contents in /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.075237.052/BuildReport.json

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.075237.052/IBMUSER.MORT0001.LOAD/EPSMORT is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMORT to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.075237.052/IBMUSER.MORT0001.LOAD/EPSMLIS is of type MAPLOAD
Copied source file - IBMUSER.MORT0001.LOAD/EPSMLIS to Target PDS - IBMUSER.SIT.MORTGAGE.MAPLOAD

Extracted file /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.075237.052/IBMUSER.MORT0001.LOAD/EPSCSMRT is of type CICSLOAD
ERROR: DEPLOYMENT FAILED
ERROR: SOURCE FILE NOT DEPLOYED : IBMUSER.MORT0001.LOAD/EPSCSMRT
ERROR: DBB COPY MODE NOT DEFINED FOR DEPLOY TYPE : CICSLOAD


Deleted the temporary folder - /u/ibmuser/workspace/zAppBuild_Hogan/out/DeployFiles_20220920.075237.052

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
