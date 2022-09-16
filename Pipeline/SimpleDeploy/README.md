# Simple Deploy (Deploy the package to target libraries)
## Description
This "SimpleDeploy.groovy" sample script is capable of untaring the tar package produced by [PackageBuildOutputs](../PackageBuildOutputs/) and deploy the tar package contents to the target libraries. 

Potential scenarios include:
* Deploying the tar package to the target environment libraries.
* If the user wants to do shift-left testing using IBM Z Virtual Test Platform, and subsystem updates are not required.

As of now, this script does not perform any activation activities like a CICS NEWCOPY or a DB2 BIND.  Currently this script does not support rollback process also.

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

```
groovyz SimpleDeploy.groovy --workDir /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy --tarFileName /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/dbb-logs/build.20220513.020006.000/fullBuild.tar --hlq ANUPRA.SIT.MORTGAGE
** SimpleDeploy start at 20220531.121014.010
** Created tar file extract directory /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy/DeployFiles_20220531.121014.010
** Untar file at /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/dbb-logs/build.20220513.020006.000/fullBuild.tar.

Package untar done to /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy/DeployFiles_20220531.121014.010
** Read build report data from /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy/DeployFiles_20220531.121014.010/BuildReport.json

Extracted file ANUPRA.MORTGAGE.LOAD/EPSMORT is of type MAPLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSMORT to Target PDS - ANUPRA.SIT.MORTGAGE.MAPLOAD

Extracted file ANUPRA.MORTGAGE.LOAD/EPSMLIS is of type MAPLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSMLIS to Target PDS - ANUPRA.SIT.MORTGAGE.MAPLOAD

Extracted file ANUPRA.MORTGAGE.LOAD/EPSCSMRT is of type CICSLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSCSMRT to Target PDS - ANUPRA.SIT.MORTGAGE.CICSLOAD

Extracted file ANUPRA.MORTGAGE.DBRM/EPSCMORT is of type DBRM
Copied source file - ANUPRA.MORTGAGE.DBRM/EPSCMORT to Target PDS - ANUPRA.SIT.MORTGAGE.DBRM

Extracted file ANUPRA.MORTGAGE.LOAD/EPSCMORT is of type CICSLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSCMORT to Target PDS - ANUPRA.SIT.MORTGAGE.CICSLOAD

Extracted file ANUPRA.MORTGAGE.DBRM/EPSCMORT is of type DBRM
Copied source file - ANUPRA.MORTGAGE.DBRM/EPSCMORT to Target PDS - ANUPRA.SIT.MORTGAGE.DBRM

Extracted file ANUPRA.MORTGAGE.LOAD/EPSCMORT is of type CICSLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSCMORT to Target PDS - ANUPRA.SIT.MORTGAGE.CICSLOAD

Extracted file ANUPRA.MORTGAGE.LOAD/EPSCSMRD is of type CICSLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSCSMRD to Target PDS - ANUPRA.SIT.MORTGAGE.CICSLOAD

Extracted file ANUPRA.MORTGAGE.LOAD/EPSMPMT is of type LOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSMPMT to Target PDS - ANUPRA.SIT.MORTGAGE.LOAD

Extracted file ANUPRA.MORTGAGE.LOAD/EPSMLIST is of type CICSLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSMLIST to Target PDS - ANUPRA.SIT.MORTGAGE.CICSLOAD

Extracted file ANUPRA.MORTGAGE.LOAD/LENPGM is of type LOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/LENPGM to Target PDS - ANUPRA.SIT.MORTGAGE.LOAD

Extracted file ANUPRA.MORTGAGE.LOAD/DATEVAL is of type LOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/DATEVAL to Target PDS - ANUPRA.SIT.MORTGAGE.LOAD

Deleted the temporary folder - /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy/DeployFiles_20220531.121014.010

** Build finished
```

If the copy mode for a deploy type is not defined:

```
groovyz SimpleDeploy.groovy --workDir /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy --tarFileName /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/dbb-logs/build.20220513.020006.000/fullBuild.tar --hlq ANUPRA.SIT.MORTGAGE
** SimpleDeploy start at 20220531.121349.013
** Created tar file extract directory /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy/DeployFiles_20220531.121349.013
** Untar file at /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/dbb-logs/build.20220513.020006.000/fullBuild.tar.

Package untar done to /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy/DeployFiles_20220531.121349.013
** Read build report data from /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy/DeployFiles_20220531.121349.013/BuildReport.json

Extracted file ANUPRA.MORTGAGE.LOAD/EPSMORT is of type MAPLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSMORT to Target PDS - ANUPRA.SIT.MORTGAGE.MAPLOAD

Extracted file ANUPRA.MORTGAGE.LOAD/EPSMLIS is of type MAPLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSMLIS to Target PDS - ANUPRA.SIT.MORTGAGE.MAPLOAD

Extracted file ANUPRA.MORTGAGE.LOAD/EPSCSMRT is of type CICSLOAD
Copied source file - ANUPRA.MORTGAGE.LOAD/EPSCSMRT to Target PDS - ANUPRA.SIT.MORTGAGE.CICSLOAD

Extracted file ANUPRA.MORTGAGE.DBRM/EPSCMORT is of type DBRM
ERROR: DEPLOYMENT FAILED
ERROR: SOURCE FILE NOT DEPLOYED : ANUPRA.MORTGAGE.DBRM/EPSCMORT
ERROR: DBB COPY MODE NOT DEFINED FOR DEPLOY TYPE : DBRM


Deleted the temporary folder - /u/anupra/workspace/MortgageApplication/zAppBuild_Hogan/Temp-Deploy/DeployFiles_20220531.121349.013

** Build finished
```

### Sample(s) Inventory

Artifact | Description
---------- | ----------------------------------------------------------------------------------------
[SimpleDeploy.groovy](SimpleDeploy.groovy) | Groovy program to deploy the package to target libraries
[simpleDeploy.properties](simpleDeploy.properties) | Properties file to customize copy mode and target libraries LLQ
