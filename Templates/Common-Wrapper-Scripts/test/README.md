# Test driver
Test driver is a shell script to run the available [backend scripts](../README.md#4---script-inventory) remotely on a x86 system to show how each backend script is run using either Zowe or SSH command.

## Pre-requisite
* Installed and configured DBB 2.0, [zAppBuild](https://github.com/IBM/dbb-zappbuild) and the [pipeline scripts](https://github.com/IBM/dbb/tree/main/Pipeline) from the IBM DBB Community 
* Installed the pipeline backend scripts as [described here](../README.md#installation).
* (Optional) Setup JFrog Artifactory Setup for the Mortgage Application. See [packageBuildOutputs.config](../packageBuildOutputs.config)
* (Optional) Setup UCD environment for MortgageApplication

## Sample repository
The **MortgageApplication** sample application was configured with several branches to test the scripts to support the outlined development workflows **The Git-based process you need for Mainframe development**.

The application is available at - https://github.com/dennis-behm/MortgageApplication, which is referenced in the test script. Please review the [branch setup](https://github.com/dennis-behm/MortgageApplication/branches), the [tag](https://github.com/dennis-behm/MortgageApplication/tags) for configuring the baseline reference.

## Instruction to run
1. Locate testGenericWrapper.sh on your local machine.
2. Review and update the test configuration section in the script, and update it accordingly to your needs.
   * dbbBuildRootDir
   * sshConnection and sshEnvironmentProfile when invoking the scripts via SSH
3. Invoke the script as example below

Sample Invocation for Zowe CLI

```
testGenericWrapper.sh zowe
```

Sample Invocation for ssh

```
testGenericWrapper.sh ssh
```

The sample outputs are located at [logs/testGenericWrapper-zowe.log](logs/testGenericWrapper-zowe.log) and [logs/testGenericWrapper-uss.log](logs/testGenericWrapper-uss.log)

Test test driver outputs a test summary at the end of the execution:

```
Test Summary
============
 testMortgageApplication-Main-Bld-Build-0 	 passed , rc=0
 testMortgageApplication-Main-Bld-Build-1 	 passed , rc=0
 testMortgageApplication-Main-Rel-Build-2 	 passed , rc=0
 testMortgageApplication-Main-Prev-Build-3 	 passed , rc=0
 testMortgageApplication-Feature-Setmainbuildbranch-Build-1 	 passed , rc=0
 testMortgageApplication-Release-Rel100-Build-1 	 passed , rc=0
 testMortgageApplication-Hotfix-Release-Rel100-Build-1 	 passed , rc=0
 testMortgageApplication-Epic-implementAI-Build-1 	 passed , rc=0
 testMortgageApplication-Epic-Feature-implementAI-Build-1 	 passed , rc=0
```

## Test scenarios

Test | Description
---------- | ----------------------------------------------------------------------------------------
testMortgageApplication-Main-Bld-Build-0 | Full Build
testMortgageApplication-Main-Bld-Build-1 | Build pipeline: Impact build with baseline, packaging
testMortgageApplication-Main-Rel-Build-2 | Release pipeline: Impact build with baseline, packaging, deployment
testMortgageApplication-Main-Prev-Build-3 | Preview Build
testMortgageApplication-Feature-Setmainbuildbranch-Build-1 | Feature branch pipeline, packaging
testMortgageApplication-Release-Rel100-Build-1  | Build pipeline: Impact build with baseline, packaging
testMortgageApplication-Hotfix-Release-Rel100-Build-1 | Feature branch pipeline (overriding mainBuildBranch), packaging (preliminary package)
testMortgageApplication-Epic-implementAI-Build-1 | Build pipeline: Impact build with baseline, packaging
testMortgageApplication-Epic-Feature-implementAI-Build-1 | Feature branch pipeline (overriding mainBuildBranch), packaging (preliminary package)

## Assumptions / Limitations
* The scripts use Mortgage application as sample application.
* `testMortgageApplication-Main-Bld-Build-0` is performing a full build for initializing the DBB dependency data
* Due to the architecture of the MortgageApplication (generated Copybooks, sub-modules that are linked), re-configure `cobol_compileSyslibConcatenation` and `cobol_linkEditSyslibConcatenation` in [COBOL.properties](https://github.com/dennis-behm/MortgageApplication/blob/feature/setmainbuildbranch/application-conf/Cobol.properties) in the different configurations or make use of the output of the full build.