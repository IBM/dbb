# zAppBuild
zAppBuild is a generic build solution for building z/OS applications using Apache Groovy build scripts and IBM Dependency Based Build (DBB) APIs. It is intended to be cloned to a single location on USS and used to build all of your z/OS applications. This is done by simply copying the supplied `application-conf` folder to the application source repository you want to build and then verify/update the contained default configuration property values to ensure they meet the build requirements of your application. See [MortgageApplicationV2](../MortgageApplicationV2) for two examples of applications that are designed to be built by zAppBuild.  Instructions on how to create a stand alone Git repository for zAppBuild is included in [INSTALL.md](INSTALL.md).

**IMPORTANT** : The [datasets.properties](build-conf/datasets.properties) must be configured for your build machine before executing a build!  See [build-conf/README.md](build-conf/README.md) for more information.

## Supported Languages
The zAppBuild sample provides the following *language* build scripts by default:
* Assembler.groovy
* BMS.groovy
* Cobol.groovy
* LinkEdit.groovy (for building link cards)
* PLI.groovy
* DBDgen.groovy
* PSBgen.groovy
* MFS.groovy

All language scripts both compile and optionally link-edit programs. The language build scripts are intended to be useful out of the box but depending on the complexity of your applications' build requirements, may require modifications to meet your development team's needs.  By following the examples used in the existing language build scripts of keeping all application specific references out of the build scripts and instead using configuration properties with strong default values, the zAppBuild sample can continue to be a generic build solution for all of your specific applications.

## Build Scope
The build scope of zAppBuild is an application which is loosely defined as one or more Git repositories containing all the z/OS source files required to build the application.  There are no specific rules as to the structure of the repositories except that one repository must contain the high level `application-conf` folder provided by zAppBuild which contains all of the configuration properties for building the application programs.  

**NOTE:** All source repositories that make up the application must be cloned on the build machine under a common *workspace*  directory prior to calling build.groovy.

zAppBuild supports a number of build scenarios:
* **Single Program** - Build a single program in the application.
* **List of Programs** - Build a list of programs provided by a text file.
* **Full Build** - Build all programs (or buildable files) of an application.
* **Impact Build** - Build only programs impacted by source files that have changed since the last successful build.
* **Topic Branch Build** - Detects when building a topic branch for the first time and will automatically clone the dependency data collections from the main build branch in order to avoid having to rescan the entire application.
* **Scan Only** - Skip the actual building and only scan source files for dependency data.

Links to additional documentation is provided in the table below.  Instructions on invoking a zAppBuild is included in [BUILD.md](BUILD.md).

## Repository Legend
Folder/File | Description | Documentation Link
--- | --- | ---
application/application-conf | The `application-conf` folder contains application specific configuration properties used by build.groovy and language build scripts.  It is intended to be copied as a high level folder to the application repository and configured to meet the build requirments of the application. Ex. `myAppRepository/application-conf` | [application/application-conf/README.md](application/application-conf/README.md)
build-conf | This folder contains global configuration properties used by build.groovy and language build scripts. | [build-conf/README.md](build-conf/README.md)
languages | This folder contains the language specific build scripts that are associated to build files via script mappings (see `application/application-conf/files.properties`) and called by build.groovy. | [languages/README.md](languages/README.md)
utilities | This folder contains utility scripts which provide common utility functions used by the various zAppBuild build scripts. | [utilities/README.md](utilities/README.md)
build.groovy | This is the main build script that is called to start the build process. | [BUILD.md](BUILD.md)

